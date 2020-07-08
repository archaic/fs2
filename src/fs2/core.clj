(ns fs2.core
  (:require [clojure.java.io :as io]
            [clojure.zip :as zip]
            [fs2.core :as fs])
  (:import [java.io FileInputStream]
           [java.net URL URI]
           [java.nio.file Files LinkOption Path Paths CopyOption]
           [java.nio.file.attribute FileAttribute]))

(extend Path
  io/IOFactory
  (assoc io/default-streams-impl
    :make-input-stream
    (fn [^Path x opts]
      (io/make-input-stream (FileInputStream. (.toString x))
                            opts))))

(defn ->path
  [x]
  (cond (instance? Path x) x
        (and (string? x)
             (re-find #"(?i)^file:"
                      x))
        (->path (URL. x))
        (string? x) (Paths/get x (into-array String []))
        (instance? URI x) (Paths/get x)
        (instance? URL x) (Paths/get (.toURI ^URL x))))

(defn file?
  [path]
  (Files/isRegularFile (->path path)
                       (LinkOption/values)))

(defn directory?
  [path]
  (Files/isDirectory (->path path)
                     (LinkOption/values)))

(defn children
  [path]
  (iterator-seq (.iterator (Files/newDirectoryStream path))))

(defn path-zip
  [root]
  (zip/zipper directory? children nil root))

(defn directories
  [path]
  (filter directory? (children path)))

(defn mkdir
  [^Path path]
  (Files/createDirectory (->path path)
                         (into-array FileAttribute [])))

(defn mkdirs
  [^Path path]
  (Files/createDirectories (->path path)
                           (into-array FileAttribute [])))

(defn cp
  [^Path from ^Path to]
  (Files/copy from
              to
              #^"[Ljava.nio.file.CopyOption;" (into-array CopyOption [])))

(defn files
  "A sequence of paths within path which are regular files, path must
  be a path to a directory"
  [path]
  (let [children (children path)]
    (filter file? children)))

(defn rm
  [path]
  (when (file? path)
    (Files/delete path)))

(defn mv
  [^Path from ^Path to]
  (Files/move from
              to
              #^"[Ljava.nio.file.CopyOption;" (into-array CopyOption [])))

(defn create-tmp
  [prefix]
  (Files/createTempDirectory prefix
                             (into-array FileAttribute
                                         [])))

(defn symlink?
  [path]
  (Files/isSymbolicLink (->path path)))

(defn symlink
  [link target]

  (let [link-path
        (->path link)

        target-path
        (->path target)]

    (when (symlink? link-path)
      (Files/deleteIfExists link-path))

    (Files/createSymbolicLink link-path
                              target-path
                              (into-array FileAttribute
                                          []))))

(defn size-in-bytes
  [path]
  (Files/size path))

(defn to-bytes
  [path]
  (Files/readAllBytes (->path path)))
