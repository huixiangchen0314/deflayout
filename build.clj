(ns build
  (:require [clojure.tools.build.api :as b]))

(def lib 'top.kzre/deflayout)
(def version "0.1.0")
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))
(def jar-file "target/deflayout-0.1.0.jar")            ;; 硬编码，与 Makefile 一致
(def uber-file "target/deflayout-0.1.0-standalone.jar")

(defn clean [_]
      (b/delete {:path "target"}))

(defn compile-java [_]
      (b/javac {:src-dirs ["src"]
                :class-dir class-dir
                :basis basis}))

(defn- copy-clj-sources []
       (let [src-dir (java.io.File. "src")
             target-dir (java.io.File. class-dir)]
            (when (.exists src-dir)
                  (doseq [^java.io.File f (file-seq src-dir)
                          :when (and (.isFile f) (.endsWith (.getName f) ".clj"))]
                         (let [rel-path (-> (.toPath src-dir) (.relativize (.toPath f)))
                               dest (java.io.File. target-dir (.toString rel-path))]
                              (.mkdirs (.getParentFile dest))
                              (java.nio.file.Files/copy (.toPath f) (.toPath dest)
                                                        (make-array java.nio.file.CopyOption 0)))))))


(defn compile-clj [_]
      (b/compile-clj {:basis basis
                      :src-dirs ["src"]
                      :class-dir class-dir}))

(defn jar [_]
      (clean nil)
      ;(compile-java nil)
      (compile-clj nil)
      (b/write-pom {:class-dir class-dir
                    :lib lib
                    :version version
                    :basis basis
                    :src-dirs ["src"]
                    :scm {:url "https://github.com/topkzre/deflayout"
                          :connection "scm:git:git://github.com/topkzre/deflayout.git"
                          :developerConnection "scm:git:ssh://git@github.com:topkzre/deflayout.git"}})
      (copy-clj-sources)
      (b/copy-dir {:src-dirs [ "resources"] :target-dir class-dir})
      (b/jar {:class-dir class-dir
              :jar-file jar-file})
      (println "Jar created:" jar-file))


