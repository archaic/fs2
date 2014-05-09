(ns fs.core
  (:require [extension.zip :as azip :refer [postwalk!]]
            [clojure.zip :as zip :refer [zipper]])
  (:import [java.nio.file Files LinkOption Path Paths CopyOption StandardCopyOption]
           [java.nio.file.attribute FileAttribute]))

(defn ->path
  [file]
  (cond (string? file) (Paths/get file (into-array String []))
        (instance? Path file) file))

(defn file?
  [path]
  (Files/isRegularFile (->path path) (LinkOption/values)))

(defn directory?
  [path]
  (Files/isDirectory (->path path) (LinkOption/values)))

(defn children
  [path]
  (iterator-seq (.iterator (Files/newDirectoryStream path))))

(defn path-zip
  "Returns a (non editable) zipper for path elements, given a root element"
  [root]
  (zipper directory? children nil root))

(defn directories
  [path]
  (filter directory? (children path)))

(defn rename
  "Renames all regular files in path from keys in m to values"
  [path m]
  (doseq [[source target] m]
    (try (Files/move (.resolve path source)
                     (.resolve path target) (into-array CopyOption []))
         (catch Exception e (println (.getMessage e))))))

(defn mkdir
  ([path] (Files/createDirectory (->path path) (into-array FileAttribute [])))
  ([path dir] (mkdir (.resolve (->path path) dir))))

(defn rmdir
  [path]
  (let [path (->path path)]
    (when (directory? path)
      (postwalk! (path-zip path)
                 (fn [loc]
                   (Files/delete (zip/node loc)))))))

(defn cp
  [from to]
  (Files/copy (->path from) (->path to) (into-array CopyOption [])))

(defn create
  "Creates directories in path"
  [path dirs]
  (doseq [dir dirs]
    (mkdir path dir)))

(defn files
  "A sequence of paths within path which are regular files, path must
   be a path to a directory"
  [path]
  (let [children (children path)]
    (filter file? children)))

(defn mass-files
  "A sequence of paths within path which are regular files, recursive"
  [path]
  (azip/keep (fn [loc]
               (let [node (zip/node loc)]
                 (when (file? node)
                   node)))
             (path-zip (->path path))))

(defn mv
  [src dst & [options]]
  (try (let [options-array
             (if options
               (if (:overwrite? options)
                 [StandardCopyOption/REPLACE_EXISTING]
                 [])
               [])
             copy-options (into-array CopyOption options-array)]
         (Files/move (->path src) (->path dst) copy-options))
       (catch Exception ex (println (.getMessage ex)))))

(defn mass-rename
  "Renames file-names that satisfy pred by transform, ancestor root"
  [{:keys [root pred transform test? overwrite?]}]
  (postwalk! (path-zip (->path root))
             (fn [loc]
               (let [node (zip/node loc)]
                 (when (file? node)
                   (let [file-name (str (last node))]
                     (when (pred file-name)
                       (let [new-file-name (transform file-name)
                             directory (.getParent node)
                             new-path (.resolve directory (->path new-file-name))]
                         (if test?
                           (println "mv: " node " -> " new-path)
                           (mv node new-path {:overwrite? overwrite?}))))))))))

(defn rm [file]
  (let [path (->path file)]
    (when (file? path)
      (Files/delete path))))

(defn rm-files [{:keys [root pred test?]}]
  (doseq [file (files (->path root))]
    (let [suffix (str (last file))]
      (when (pred suffix)
        (if test?
          (println "rm: " suffix)
          (rm file))))))
