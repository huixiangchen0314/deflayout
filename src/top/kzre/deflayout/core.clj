(ns top.kzre.deflayout.core
  (:import (clojure.lang Keyword)))

;; ---------- 数据记录 ----------

(defrecord Field
  [^Keyword name
   ^long length
   comp-names])

(defrecord LayoutDesc
  [^boolean unsynchronized-mutable
   ^Keyword storage])

(defrecord Structure
  [^Keyword name
   ^Keyword type
   fields
   ^long stride
   ^LayoutDesc desc])

(defrecord Layout
  [^Keyword name
   structs
   ^boolean ext?
   ^LayoutDesc default-desc])

;; ---------- 解析逻辑 ----------

(defn- parse-desc
  [{:keys [unsynchronized-mutable storage]
    :or   {unsynchronized-mutable false
           storage               :array}}]
  (LayoutDesc. (boolean unsynchronized-mutable) storage))

(defn- parse-field-vec [field-vec]
  (mapv (fn [[k v]]
          (if (vector? v)
            (Field. k (count v) v)
            (Field. k v nil)))
        (partition 2 field-vec)))

(defn- compute-stride [fields]
  (reduce + 0 (map :length fields)))

(defn- parse-struct [group-name group-def ^LayoutDesc default-desc]
  (let [[type-kw field-vec & opts-kvs] group-def
        opts-map (apply hash-map opts-kvs)
        mutable (get opts-map :unsynchronized-mutable (:unsynchronized-mutable default-desc))
        storage (get opts-map :storage (:storage default-desc))
        desc    (LayoutDesc. (boolean mutable) storage)
        fields  (parse-field-vec field-vec)
        stride  (compute-stride fields)]
    (Structure. group-name type-kw fields stride desc)))

(defn- parse-structs [layout ^LayoutDesc default-desc]
  (vec (for [[gname gdef] layout]
         (parse-struct gname gdef default-desc))))

(defn- parse-layout ^Layout [name layout opts]
  (let [ext?         (boolean (:ext opts false))
        default-desc (parse-desc opts)
        structs      (parse-structs layout default-desc)]
    (Layout. name structs ext? default-desc)))

;; ---------- 生成 deftype 字段 ----------

(defn- emit-type-fields [lo]
  (let [structs (:structs lo)
        fields (mapcat (fn [s]
                         (let [sym      (symbol (name (:name s)))
                               mutable? (:unsynchronized-mutable (:desc s))
                               type-kw  (:type s)
                               tag      (case type-kw
                                          ;; 转符号
                                          :float 'floats       ;; 使用普通引用
                                          :int   'ints
                                          :byte  'bytes
                                          (throw (ex-info (str "Unsupported field type: " type-kw)
                                                          {:structure s})))
                               meta-map (if mutable?
                                          {:tag tag :unsynchronized-mutable true}
                                          {:tag tag})]
                           [(with-meta sym meta-map)]))
                       structs)]
    (if (:ext? lo)
      (conj (vec fields) 'ext)
      (vec fields))))

;; ---------- 数组版宏（默认，直接操作数组） ----------
(defn- field-offsets [fields]
  (map vector fields (reductions + 0 (map :length fields))))

(defn- emit-array-accessors [lo]
  (vec
    (mapcat (fn [s]
              (let [stride (:stride s)
                    offsets (field-offsets (:fields s))]
                (mapcat (fn [[f offset]]
                          (let [sem        (:name f)
                                comp-names (or (:comp-names f)
                                               (mapv #(keyword (str %)) (range (:length f))))]
                            (map-indexed (fn [ci comp-name]
                                           (let [accessor-name (symbol (str (name sem) "-" (name comp-name)))
                                                 idx-expr      `(+ (* ~'idx ~stride) ~(+ offset ci))]
                                             `(defmacro ~accessor-name [~'arr ~'idx]
                                                (aget ~'arr ~idx-expr))))
                                         comp-names)))
                        offsets)))
            (:structs lo))))

(defn- emit-array-setters [lo]
  (vec
    (mapcat (fn [s]
              (let [stride (:stride s)
                    offsets (field-offsets (:fields s))]
                (mapcat (fn [[f offset]]
                          (let [sem        (:name f)
                                comp-names (or (:comp-names f)
                                               (mapv #(keyword (str %)) (range (:length f))))]
                            (map-indexed (fn [ci comp-name]
                                           (let [setter-name (symbol (str "set-" (name sem) "-" (name comp-name) "!"))
                                                 idx-expr    `(+ (* ~'idx ~stride) ~(+ offset ci))]
                                             `(defmacro ~setter-name [~'arr ~'idx ~'val]
                                                (aset ~'arr ~idx-expr ~'val))))
                                         comp-names)))
                        offsets)))
            (:structs lo))))

(defn- emit-array-whole-accessors [lo]
  (vec
    (mapcat (fn [s]
              (let [stride (:stride s)
                    offsets (field-offsets (:fields s))]
                (map (fn [[f offset]]
                       (let [field-sym (symbol (name (:name f)))
                             len        (:length f)
                             asets      (for [ci (range len)]
                                          `(aset ~'out (+ ~'idx ~ci)
                                                 (aget ~'arr
                                                       (+ (* ~'idx ~stride) ~(+ offset ci)))))]
                         `(defmacro ~field-sym [~'out ~'arr ~'idx]
                            ~@asets)))
                     offsets)))
            (:structs lo))))

(defn- emit-array-whole-setters [lo]
  (vec
    (mapcat (fn [s]
              (let [stride (:stride s)
                    offsets (field-offsets (:fields s))]
                (map (fn [[f offset]]
                       (let [field-sym (symbol (str "set-" (name (:name f)) "!"))
                             len        (:length f)
                             params     (into ['arr 'idx] (repeatedly len gensym))
                             asets      (for [ci (range len)]
                                          `(aset ~'arr
                                                 (+ (* ~'idx ~stride) ~(+ offset ci))
                                                 ~(nth params (+ 2 ci))))]
                         `(defmacro ~field-sym ~(vec params)
                            ~@asets)))
                     offsets)))
            (:structs lo))))


;; ---------- 主宏 ----------

(defmacro deflayout
  [name layout & opts]
  (let [lo (parse-layout name layout (apply hash-map opts))
        _ (println (pr-str lo))]
    `(do
       (deftype ~name ~(emit-type-fields lo))
       ;; 数组版（默认高效）
       ~@(emit-array-accessors lo)
       ~@(emit-array-setters lo)
       ~@(emit-array-whole-accessors lo)
       ~@(emit-array-whole-setters lo)
       )))

(comment
  ;; 示例布局
  (deflayout Object
             {:f0 [:float [:vertex 3 :normal 3 :uv0 2]
                   :unsynchronized-mutable false]
              :f1 [:float [:weights 4]]}
             :ext true
             :unsynchronized-mutable true)

  ;; 生成的类型
  (deftype Object [^floats f0
                   ^:unsynchronized-mutable ^floats f1
                   ext])

  ;; ===== 数组版本（高性能） =====
  (let [obj (Object. (float-array 100) (float-array 100) nil)
        f0  (.f0 obj)
        out (float-array 3)]
    (vertex-x f0 0)                ; 读取分量
    (set-vertex-x! f0 0 1.0)       ; 设置分量
    (vertex out f0 0)              ; 整体拷贝
    (set-vertex! f0 0 1.0 2.0 3.0); 整体设置
    (weights out (.f1 obj) 5))     ; 另一个分组

  ;; ===== 对象版本（便捷，obj- 前缀） =====
  (let [obj (Object. (float-array 100) (float-array 100) nil)
        out (float-array 3)]
    (obj-vertex-x obj 0)                   ; 分量读取
    (obj-set-vertex-x! obj 0 1.0)          ; 分量设置
    (obj-vertex out obj 0)                 ; 整体拷贝
    (obj-set-vertex! obj 0 1.0 2.0 3.0))  ; 整体设置
  )