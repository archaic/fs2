(defproject com.eoneq/fs2 "0.7.3"

  :description "File System Utilities"

  :global-vars {*warn-on-reflection* true}

  :lein-tools-deps/config {:config-files [:install :user :project]}

  :middleware [lein-tools-deps.plugin/resolve-dependencies-with-deps-edn]

  :plugins [[lein-tools-deps "0.4.1"]])
