(ns fs.watch
  "A controller to monitor filesystem changes, with appropriate callback hooks"
  (:require [extensions.zip :refer [prewalk!]]
            [clj-time.core :as time]
            [clojure.zip :refer [node]]
            [fs.core :refer [directory? path-zip ->path files]])
  (:import [java.nio.file Path WatchEvent WatchService FileSystems
            StandardWatchEventKinds WatchKey Paths]
           [org.apache.commons.vfs2.impl DefaultFileMonitor]
           [org.apache.commons.vfs2 FileListener VFS]))

(def event-keys
  {StandardWatchEventKinds/ENTRY_CREATE :create
   StandardWatchEventKinds/ENTRY_DELETE :delete
   StandardWatchEventKinds/ENTRY_MODIFY :modify})

(defn register
  [path watcher]
  (.register ^Path path
             ^WatchService watcher
             (into-array (keys event-keys))))

(defn append-keys!
  "Return a function which when given a path, recursively appends watchers"
  [watcher watch-keys]
  (fn [path]
    (prewalk! (path-zip path)
              (fn [loc] (let [path (node loc)]
                          (when (directory? path)
                            (let [key (register path watcher)]
                              (swap! watch-keys conj [key path]))))))))

(defn handler
  [key watch-keys callback-map]
  (let [path (@watch-keys key)
        watch-events (.pollEvents key)]
    (doseq [event watch-events]
      (let [base-name (.context event)
            path (.resolve path base-name)
            action (event-keys (.kind event))]
        (when-let [callback (callback-map action)]
          (callback path))
        (.reset key)))))

(defn url->path [url]
  (Paths/get (.toURI url)))

(defn file-object->path [file-object]
  (let [url (.getURL file-object)]
    (url->path url)))

(defn path->file-object [path]
  (let [uri (.toUri path)
        uri-string (str uri)
        fs (VFS/getManager)]
    (.resolveFile fs uri-string)))

(defn poll-watch
  "Callback functions for :create :delete :modify events on a path.
   Each callback takes a java.nio.file.Path, called for side effects.
   Return a function of no arguments, which closes the watcher"
  [path callback-map]
  (let [file-object (path->file-object path)
        callback
        (fn [callback-key]
          (fn [event]
            (let [path (file-object->path (.getFile event))]
              (when-let [f (callback-key callback-map)]
                (f path)))))
        
        monitor
        (DefaultFileMonitor.
          (reify FileListener
            (fileChanged [this event]
              ((callback :modify) event))
            (fileCreated [this event]
              ((callback :create) event))
            (fileDeleted [this event]
              ((callback :delete) event))))]
    (.setRecursive monitor true)
    (.addFile monitor file-object)
    (.start monitor)
    (fn [] (.stop monitor))))

(defn watch
  "Register callback functions for :create :delete :modify events on a path.
   Each callback function has a single java.nio.file.Path argument, called for
   side effects"
  [path callback-map]
  (let [watcher (.newWatchService (FileSystems/getDefault))
        watch-keys (atom {})
        watching? (atom true)
        appender! (append-keys! watcher watch-keys)
        callback-map (assoc callback-map :create (fn [path]
                                                   (when (directory? path)
                                                     (appender! path))))]

    (appender! path)

    (future
      (while @watching?
        (let [key ^WatchKey (.take watcher)]
          (handler key watch-keys callback-map))))
    
    (fn []
      (reset! watching? false)
      (.close watcher))))

(def path-list (atom []))

#_(poll-watch (->path "/tmp/")
              {:modify (fn [path]
                         (swap! path-list conj
                                (str "<MODIFY>" (.toString path) "</MODIFY>")))})

(defn seconds->millis [s]
  (-> s time/secs .toStandardDuration .getMillis))

(defonce watching?
  (atom false))

(defn schedule [f s]
  "Calls function f every s seconds for side effects"
  (future
    (while @watching?
      (do (Thread/sleep (seconds->millis s))
          (f)))))

(defn force-watch [path callback s]
  "Sometimes even polling is not enough, for each file directly in path, call
   callback on the file every s seconds"
  (reset! watching? true)
  (schedule (fn []
              (doseq [path (files (->path path))]
                (callback path)))
            s))

#_(force-watch "/tmp" (fn [path] (println path)) 3)

(defn stop-force-watch []
  (reset! watching? false))

#_(stop-force-watch)
