(ns fs2.core
  (:require [clojure.java.io :as io]
            [clojure.zip :as zip])
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
  (cond (string? x) (Paths/get x (into-array String []))
        (instance? Path x) x
        (instance? URI x) (Paths/get x)
        (instance? URL x) (Paths/get (.toURI ^URL x))))

(defn file?
  [path]
  (Files/isRegularFile path (LinkOption/values)))

(defn directory?
  [path]
  (Files/isDirectory path (LinkOption/values)))

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
  (Files/createDirectory path (into-array FileAttribute [])))

(defn mkdirs
  [^Path path]
  (Files/createDirectories path (into-array FileAttribute [])))

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
