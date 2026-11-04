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
   ^boolean unchecked-math?
   type
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

(defn parse-layout ^Layout [name layout
                            {:keys [ext? type unchecked-math?] :as opts
                             :or   {ext?             false
                                    type             :type
                                    unchecked-math? false}}]
  (let [default-desc (parse-desc opts)
        structs      (parse-structs layout default-desc)]
    (Layout. name structs (boolean ext?) (boolean unchecked-math?) type default-desc)))

;; ---------- 类型提示工具（仅用于 deftype 字段生成） ----------
(defn- type-tag-hint [type-kw]
  (case type-kw
    :float "[F"
    :int   "[I"
    :byte  "[B"
    (throw (ex-info (str "Unsupported field type: " type-kw)
                    {:type type-kw}))))

;; ---------- 生成 deftype 字段 ----------
(defn emit-type-fields [lo]
  (let [structs (:structs lo)
        fields (mapcat (fn [s]
                         (let [sym      (symbol (name (:name s)))
                               mutable? (:unsynchronized-mutable (:desc s))
                               tag      (type-tag-hint (:type s))
                               meta-map (if mutable?
                                          {:tag tag :unsynchronized-mutable true}
                                          {:tag tag})]
                           [(with-meta sym meta-map)]))
                       structs)]
    (if (:ext? lo)
      (conj (vec fields) 'ext)
      (vec fields))))

;; ---------- 偏移计算 ----------
(defn field-offsets [fields]
  (map vector fields (reductions + 0 (map :length fields))))

;; ---------- 访问器宏：单个字段组件 ----------
(defn emit-array-accessors [lo unchecked?]
  (vec
    (mapcat (fn [s]
              (let [stride  (:stride s)
                    type-kw (:type s)
                    offsets (field-offsets (:fields s))]
                (mapcat (fn [[f offset]]
                          (let [sem        (:name f)
                                comp-names (or (:comp-names f)
                                               (mapv #(keyword (str %)) (range (:length f))))]
                            (map-indexed (fn [ci comp-name]
                                           (let [accessor-name (symbol (str (name sem) "-" (name comp-name)))
                                                 final-offset  (+ offset ci)]
                                             (case type-kw
                                               :float
                                               `(defmacro ~accessor-name [ ~'arr ~'idx]
                                                  (if ~unchecked?
                                                    `(aget ^"[F" ~~'arr (unchecked-add (unchecked-multiply ~~'idx ~~stride) ~~final-offset))
                                                   `(aget ^"[F" ~~'arr (+ (* ~~'idx ~~stride) ~~final-offset))))
                                               :int
                                               `(defmacro ~accessor-name [~'arr ~'idx]
                                                  (if ~unchecked?
                                                    `(aget ^"[I" ~~'arr (unchecked-add (unchecked-multiply ~~'idx ~~stride) ~~final-offset))
                                                    `(aget ^"[I" ~~'arr (+ (* ~~'idx ~~stride) ~~final-offset))))
                                               :byte
                                               `(defmacro ~accessor-name [ ~'arr ~'idx]
                                                  (if ~unchecked?
                                                    `(aget ^"[B" ~~'arr (unchecked-add (unchecked-multiply ~~'idx ~~stride) ~~final-offset))
                                                    `(aget ^"[B" ~~'arr (+ (* ~~'idx ~~stride) ~~final-offset)))))))
                                         comp-names)))
                        offsets)))
            (:structs lo))))

(defn emit-array-accessors-offset [lo unchecked?]
  (vec
    (mapcat (fn [s]
              (let [stride  (:stride s)
                    type-kw (:type s)
                    offsets (field-offsets (:fields s))]
                (mapcat (fn [[f offset]]
                          (let [sem        (:name f)
                                comp-names (or (:comp-names f)
                                               (mapv #(keyword (str %)) (range (:length f))))]
                            (map-indexed (fn [ci comp-name]
                                           (let [accessor-name
                                                 (symbol (str (name sem) "-" (name comp-name) "-offset"))
                                                 final-offset  (+ offset ci)]
                                             (case type-kw
                                               :float
                                               `(defmacro ~accessor-name [ ~'arr ~'idx ~'offset]
                                                  (if ~unchecked?
                                                    `(aget ^"[F" ~~'arr (unchecked-add
                                                                          ~'offset
                                                                          (unchecked-add
                                                                            (unchecked-multiply ~~'idx ~~stride) ~~final-offset)))
                                                    `(aget ^"[F" ~~'arr (+ ~'offset (* ~~'idx ~~stride) ~~final-offset))))
                                               :int
                                               `(defmacro ~accessor-name [~'arr ~'idx ~'offset]
                                                  (if ~unchecked?
                                                    `(aget ^"[I" ~~'arr (unchecked-add
                                                                          ~'offset
                                                                          (unchecked-add
                                                                            (unchecked-multiply ~~'idx ~~stride) ~~final-offset)))
                                                    `(aget ^"[I" ~~'arr (+ ~'offset (* ~~'idx ~~stride) ~~final-offset))))
                                               :byte
                                               `(defmacro ~accessor-name [ ~'arr ~'idx ~'offset]
                                                  (if ~unchecked?
                                                    `(aget ^"[B" ~~'arr (unchecked-add
                                                                          ~'offset
                                                                          (unchecked-add (unchecked-multiply ~~'idx ~~stride) ~~final-offset)))
                                                    `(aget ^"[B" ~~'arr (+ ~'offset (* ~~'idx ~~stride) ~~final-offset)))))))
                                         comp-names)))
                        offsets)))
            (:structs lo))))


(defn emit-array-setters [lo unchecked?]
  (vec
    (mapcat (fn [s]
              (let [stride  (:stride s)
                    type-kw (:type s)
                    offsets (field-offsets (:fields s))]
                (mapcat (fn [[f offset]]
                          (let [sem        (:name f)
                                comp-names (or (:comp-names f)
                                               (mapv #(keyword (str %)) (range (:length f))))]
                            (map-indexed (fn [ci comp-name]
                                           (let [setter-name  (symbol (str "set-" (name sem) "-" (name comp-name) "!"))
                                                 final-offset (+ offset ci)]
                                             (case type-kw
                                               :float
                                               `(defmacro ~setter-name [ ~'arr ~'idx ~'val]
                                                  (if ~unchecked?
                                                    `(aset ^"[F" ~~'arr (unchecked-add (unchecked-multiply ~~'idx ~~stride) ~~final-offset) ~~'val)
                                                    `(aset ^"[F" ~~'arr (+ (* ~~'idx ~~stride) ~~final-offset) ~~'val)))
                                               :int
                                               `(defmacro ~setter-name [~'arr ~'idx ~'val]
                                                  (if ~unchecked?
                                                    `(aset ^"[I" ~~'arr (unchecked-add (unchecked-multiply ~~'idx ~~stride) ~~final-offset) ~~'val)
                                                    `(aset ^"[I" ~~'arr (+ (* ~~'idx ~~stride) ~~final-offset) ~~'val)))
                                               :byte
                                               `(defmacro ~setter-name [ ~'arr ~'idx ~'val]
                                                  (if ~unchecked?
                                                    `(aset ^"[B" ~~'arr (unchecked-add (unchecked-multiply ~~'idx ~~stride) ~~final-offset) ~~'val)
                                                    `(aset ^"[B" ~~'arr (+ (* ~~'idx ~~stride) ~~final-offset) ~~'val))))))
                                         comp-names)))
                        offsets)))
            (:structs lo))))


;; ---------- 设置器宏：单个字段组件 ----------
(defn emit-array-setters-offset [lo unchecked?]
  (vec
    (mapcat (fn [s]
              (let [stride  (:stride s)
                    type-kw (:type s)
                    offsets (field-offsets (:fields s))]
                (mapcat (fn [[f offset]]
                          (let [sem        (:name f)
                                comp-names (or (:comp-names f)
                                               (mapv #(keyword (str %)) (range (:length f))))]
                            (map-indexed (fn [ci comp-name]
                                           (let [setter-name  (symbol (str "set-" (name sem) "-" (name comp-name) "-offset" "!"))
                                                 final-offset (+ offset ci)]
                                             (case type-kw
                                               :float
                                               `(defmacro ~setter-name [ ~'arr ~'idx ~'val ~'offset]
                                                  (if ~unchecked?
                                                    `(aset ^"[F" ~~'arr
                                                           (unchecked-add
                                                             ~'offset
                                                             (unchecked-add
                                                               (unchecked-multiply ~~'idx ~~stride) ~~final-offset)) ~~'val)
                                                    `(aset ^"[F" ~~'arr (+ ~'offset (* ~~'idx ~~stride) ~~final-offset) ~~'val)))
                                               :int
                                               `(defmacro ~setter-name [~'arr ~'idx ~'val ~'offset]
                                                  (if ~unchecked?
                                                    `(aset ^"[I" ~~'arr
                                                           (unchecked-add
                                                             ~'offset
                                                             (unchecked-add
                                                               (unchecked-multiply ~~'idx ~~stride) ~~final-offset)) ~~'val)
                                                    `(aset ^"[I" ~~'arr (+ ~'offset (* ~~'idx ~~stride) ~~final-offset) ~~'val)))
                                               :byte
                                               `(defmacro ~setter-name [ ~'arr ~'idx ~'val ~'offset]
                                                  (if ~unchecked?
                                                    `(aset ^"[B" ~~'arr
                                                           (unchecked-add
                                                             ~'offset
                                                             (unchecked-add
                                                               (unchecked-multiply ~~'idx ~~stride) ~~final-offset)) ~~'val)
                                                    `(aset ^"[B" ~~'arr (+ ~'offset (* ~~'idx ~~stride) ~~final-offset) ~~'val))))))
                            comp-names)))
                offsets)))
    (:structs lo))))

;; ---------- 整字段读取宏 ----------


(defn emit-array-whole-accessors [lo unchecked?]
  (vec
    (mapcat (fn [s]
              (let [stride  (:stride s)
                    type-kw (:type s)
                    offsets (field-offsets (:fields s))]
                (map (fn [[f offset]]
                       (let [field-sym (symbol (name (:name f)))
                             len       (:length f)]
                         (case type-kw
                           :float
                           `(defmacro ~field-sym [~'out ~'arr ~'idx]
                              `(do ~@(for [ci# (range ~len)]
                                       (if ~unchecked?
                                         `(aset ^"[F" ~~'out (unchecked-add ~~'idx ~ci#)
                                                (aget ^"[F" ~~'arr (unchecked-add (unchecked-add (unchecked-multiply ~~'idx ~~stride) ~~offset) ~ci#)))
                                         `(aset ^"[F" ~~'out (+ ~~'idx ~ci#)
                                                (aget ^"[F" ~~'arr (+ (* ~~'idx ~~stride) ~~offset ~ci#)))))))
                           :int
                           `(defmacro ~field-sym [~'out ~'arr ~'idx]
                              `(do ~@(for [ci# (range ~len)]
                                       (if ~unchecked?
                                         `(aset ^"[I" ~~'out (unchecked-add ~~'idx ~ci#)
                                                (aget ^"[I" ~~'arr (unchecked-add (unchecked-add (unchecked-multiply ~~'idx ~~stride) ~~offset) ~ci#)))
                                         `(aset ^"[I" ~~'out (+ ~~'idx ~ci#)
                                                (aget ^"[I" ~~'arr (+ (* ~~'idx ~~stride) ~~offset ~ci#)))))))
                           :byte
                           `(defmacro ~field-sym [~'out ~'arr ~'idx]
                              `(do ~@(for [ci# (range ~len)]
                                       (if ~unchecked?
                                         `(aset ^"[B" ~~'out (unchecked-add ~~'idx ~ci#)
                                                (aget ^"[B" ~~'arr (unchecked-add (unchecked-add (unchecked-multiply ~~'idx ~~stride) ~~offset) ~ci#)))
                                         `(aset ^"[B" ~~'out (+ ~~'idx ~ci#)
                                                (aget ^"[B" ~~'arr (+ (* ~~'idx ~~stride) ~~offset ~ci#))))))))))
                     offsets)))
            (:structs lo))))

(defn emit-array-whole-accessors-offset [lo unchecked?]
  (vec
    (mapcat (fn [s]
              (let [stride  (:stride s)
                    type-kw (:type s)
                    offsets (field-offsets (:fields s))]
                (map (fn [[f off]]
                       (let [field-sym (symbol (str (name (:name f)) "-offset"))
                             len       (:length f)]
                         (case type-kw
                           :float
                           `(defmacro ~field-sym [~'out ~'arr ~'idx
                                                  ~'offset]
                              `(do ~@(for [ci# (range ~len)]
                                       (if ~unchecked?
                                         `(aset ^"[F" ~~'out (unchecked-add ~~'idx ~ci#)
                                                (aget ^"[F" ~~'arr
                                                      (unchecked-add
                                                        ~~'offset
                                                        (unchecked-add
                                                          (unchecked-add (unchecked-multiply ~~'idx ~~stride) ~~off)
                                                          ~ci#))))
                                         `(aset ^"[F" ~~'out (+ ~~'idx ~ci#)
                                                (aget ^"[F" ~~'arr (+  ~~'offset (* ~~'idx ~~stride) ~~off ~ci#)))))))
                           :int
                           `(defmacro ~field-sym [~'out ~'arr ~'idx ~'offset]
                              `(do ~@(for [ci# (range ~len)]
                                       (if ~unchecked?
                                         `(aset ^"[I" ~~'out (unchecked-add ~~'idx ~ci#)
                                                (aget ^"[I" ~~'arr
                                                      (unchecked-add
                                                        ~~'offset
                                                        (unchecked-add
                                                          (unchecked-add (unchecked-multiply ~~'idx ~~stride) ~~off)
                                                          ~ci#))))
                                         `(aset ^"[I" ~~'out (+ ~~'idx ~ci#)
                                                (aget ^"[I" ~~'arr (+  ~~'offset (* ~~'idx ~~stride) ~~off ~ci#)))))))
                           :byte
                           `(defmacro ~field-sym [~'out ~'arr ~'idx ~'offset]
                              `(do ~@(for [ci# (range ~len)]
                                       (if ~unchecked?
                                         `(aset ^"[B" ~~'out (unchecked-add ~~'idx ~ci#)
                                                (aget ^"[B" ~~'arr
                                                      (unchecked-add
                                                        ~~'offset
                                                        (unchecked-add
                                                          (unchecked-add (unchecked-multiply ~~'idx ~~stride) ~~off)
                                                          ~ci#))))
                                         `(aset ^"[B" ~~'out (+ ~~'idx ~ci#)
                                                (aget ^"[B" ~~'arr (+ ~~'offset (* ~~'idx ~~stride) ~~off ~ci#))))))))))
                     offsets)))
            (:structs lo))))

;; ---------- 整字段写入宏 ----------
(defn emit-array-whole-setters [lo unchecked?]
  (vec
    (mapcat (fn [s]
              (let [stride  (:stride s)
                    type-kw (:type s)
                    offsets (field-offsets (:fields s))]
                (map (fn [[f offset]]
                       (let [field-sym (symbol (str "set-" (name (:name f)) "!"))
                             len       (:length f)
                             val-syms  (repeatedly len gensym)]
                         (case type-kw
                           :float
                           `(defmacro ~field-sym [~'arr ~'idx ~@val-syms]
                              `(do ~@(map-indexed (fn [ci# val#]
                                                    (if ~unchecked?
                                                      `(aset ^"[F" ~~'arr (unchecked-add (unchecked-add (unchecked-multiply ~~'idx ~~stride) ~~offset) ~ci#) ~val#)
                                                      `(aset ^"[F" ~~'arr (+ (* ~~'idx ~~stride) ~~offset ~ci#) ~val#)))
                                                  (list ~@val-syms))))
                           :int
                           `(defmacro ~field-sym [~'arr ~'idx ~@val-syms]
                              `(do ~@(map-indexed (fn [ci# val#]
                                                    (if ~unchecked?
                                                      `(aset ^"[I" ~~'arr (unchecked-add (unchecked-add (unchecked-multiply ~~'idx ~~stride) ~~offset) ~ci#) ~val#)
                                                      `(aset ^"[I" ~~'arr (+ (* ~~'idx ~~stride) ~~offset ~ci#) ~val#)))
                                                  (list ~@val-syms))))
                           :byte
                           `(defmacro ~field-sym [~'arr ~'idx ~@val-syms]
                              `(do ~@(map-indexed (fn [ci# val#]
                                                    (if ~unchecked?
                                                      `(aset ^"[B" ~~'arr (unchecked-add (unchecked-add (unchecked-multiply ~~'idx ~~stride) ~~offset) ~ci#) ~val#)
                                                      `(aset ^"[B" ~~'arr (+ (* ~~'idx ~~stride) ~~offset ~ci#) ~val#)))
                                                  (list ~@val-syms)))))))
                offsets)))
    (:structs lo))))

(defn emit-array-whole-setters-offset [lo unchecked?]
  (vec
    (mapcat (fn [s]
              (let [stride  (:stride s)
                    type-kw (:type s)
                    offsets (field-offsets (:fields s))]
                (map (fn [[f off]]
                       (let [field-sym (symbol (str "set-" (name (:name f)) "-offset" "!"))
                             len       (:length f)
                             val-syms  (repeatedly len gensym)]
                         (case type-kw
                           :float
                           `(defmacro ~field-sym [~'arr ~'idx ~@val-syms ~'offset]
                              `(do ~@(map-indexed (fn [ci# val#]
                                                    (if ~unchecked?
                                                      `(aset ^"[F" ~~'arr
                                                             (unchecked-add
                                                               ~~'offset
                                                               (unchecked-add
                                                                 (unchecked-add
                                                                   (unchecked-multiply ~~'idx ~~stride) ~~off) ~ci#) )
                                                             ~val#)
                                                      `(aset ^"[F" ~~'arr (+ ~~'offset (* ~~'idx ~~stride) ~~off ~ci#) ~val#)))
                                                  (list ~@val-syms))))
                           :int
                           `(defmacro ~field-sym [~'arr ~'idx ~@val-syms ~'offset]
                              `(do ~@(map-indexed (fn [ci# val#]
                                                    (if ~unchecked?
                                                      `(aset ^"[I" ~~'arr
                                                             (unchecked-add
                                                               ~~'offset
                                                               (unchecked-add
                                                                 (unchecked-add (unchecked-multiply ~~'idx ~~stride) ~~off)
                                                                 ~ci#) )
                                                             ~val#)
                                                      `(aset ^"[I" ~~'arr (+ ~~'offset (* ~~'idx ~~stride) ~~off ~ci#) ~val#)))
                                                  (list ~@val-syms))))
                           :byte
                           `(defmacro ~field-sym [~'arr ~'idx ~@val-syms ~'offset]
                              `(do ~@(map-indexed (fn [ci# val#]
                                                    (if ~unchecked?
                                                      `(aset ^"[B" ~~'arr
                                                             (unchecked-add
                                                               ~~'offset
                                                               (unchecked-add
                                                                 (unchecked-add (unchecked-multiply ~~'idx ~~stride) ~~off)
                                                                 ~ci#) )
                                                             ~val#)
                                                      `(aset ^"[B" ~~'arr (+ ~~'offset (* ~~'idx ~~stride) ~~off ~ci#) ~val#)))
                                                  (list ~@val-syms)))))))
                     offsets)))
            (:structs lo))))


;; ---------- obj- 访问器宏：单个字段组件 ----------
(defn emit-obj-accessors [lo unchecked?]
  (vec
    (mapcat (fn [s]
              (let [stride    (:stride s)
                    type-kw   (:type s)
                    field-sym-quoted `(symbol (name (:name ~s)))  ;; deftype 字段名
                    offsets   (field-offsets (:fields s))]
                (mapcat (fn [[f offset]]
                          (let [sem        (:name f)
                                comp-names (or (:comp-names f)
                                               (mapv #(keyword (str %)) (range (:length f))))]
                            (map-indexed (fn [ci comp-name]
                                           (let [accessor-name (symbol (str "obj-" (name sem) "-" (name comp-name)))
                                                 final-offset  (+ offset ci)
                                                 ]
                                             (case type-kw
                                               :float
                                               `(defmacro ~accessor-name [~'obj ~'idx]
                                                  (let [~'sym ~field-sym-quoted]
                                                    (if ~unchecked?
                                                      `(aget ^"[F" (. ~~'obj ~~'sym)
                                                             (unchecked-add (unchecked-multiply ~~'idx ~~stride) ~~final-offset))
                                                      `(aget ^"[F" (. ~~'obj ~~'sym)
                                                             (+ (* ~~'idx ~~stride) ~~final-offset)))))
                                               :int
                                               `(defmacro ~accessor-name [~'obj ~'idx]
                                                  (let [~'sym ~field-sym-quoted]
                                                    (if ~unchecked?
                                                      `(aget ^"[I" (. ~~'obj ~~'sym)
                                                             (unchecked-add (unchecked-multiply ~~'idx ~~stride) ~~final-offset))
                                                      `(aget ^"[I" (. ~~'obj ~~'sym)
                                                             (+ (* ~~'idx ~~stride) ~~final-offset)))))
                                               :byte
                                               `(defmacro ~accessor-name [~'obj ~'idx]
                                                  (let [~'sym ~field-sym-quoted]
                                                    (if ~unchecked?
                                                      `(aget ^"[B" (. ~~'obj ~~'sym)
                                                             (unchecked-add (unchecked-multiply ~~'idx ~~stride) ~~final-offset))
                                                     `(aget ^"[B" (. ~~'obj ~~'sym)
                                                            (+ (* ~~'idx ~~stride) ~~final-offset))))))))
                                         comp-names)))
                        offsets)))
            (:structs lo))))

;; ---------- obj- 设置器宏：单个字段组件 ----------
(defn emit-obj-setters [lo unchecked?]
  (vec
    (mapcat (fn [s]
              (let [stride    (:stride s)
                    type-kw   (:type s)
                    field-sym-quoted `(symbol (name (:name ~s)))
                    offsets   (field-offsets (:fields s))]
                (mapcat (fn [[f offset]]
                          (let [sem        (:name f)
                                comp-names (or (:comp-names f)
                                               (mapv #(keyword (str %)) (range (:length f))))]
                            (map-indexed (fn [ci comp-name]
                                           (let [setter-name  (symbol (str "obj-set-" (name sem) "-" (name comp-name) "!"))
                                                 final-offset (+ offset ci)]
                                             (case type-kw
                                               :float
                                               `(defmacro ~setter-name [~'obj ~'idx ~'val]
                                                  (let [~'sym ~field-sym-quoted]
                                                    (if ~unchecked?
                                                      `(aset ^"[F" (. ~~'obj ~~'sym)
                                                             (unchecked-add (unchecked-multiply ~~'idx ~~stride) ~~final-offset)
                                                             ~~'val)
                                                      `(aset ^"[F" (. ~~'obj ~~'sym)
                                                             (+ (* ~~'idx ~~stride) ~~final-offset)
                                                             ~~'val))))
                                               :int
                                               `(defmacro ~setter-name [~'obj ~'idx ~'val]
                                                  (if ~unchecked?
                                                    (let [~'sym ~field-sym-quoted]
                                                      `(aset ^"[I" (. ~~'obj ~~'sym)
                                                             (unchecked-add (unchecked-multiply ~~'idx ~~stride
                                                                  ) ~~final-offset)
                                                             ~~'val))
                                                   (let [~'sym ~field-sym-quoted]
                                                     (if ~unchecked?
                                                       `(aset ^"[I" (. ~~'obj ~~'sym)
                                                              (unchecked-add (unchecked-multiply ~~'idx ~~stride) ~~final-offset)
                                                              ~~'val)
                                                       `(aset ^"[I" (. ~~'obj ~~'sym)
                                                              (+ (* ~~'idx ~~stride) ~~final-offset)
                                                              ~~'val)))))
                                               :byte
                                               `(defmacro ~setter-name [~'obj ~'idx ~'val]
                                                  (let [~'sym ~field-sym-quoted]
                                                    (if ~unchecked?
                                                      `(aset ^"[B" (. ~~'obj ~~'sym)
                                                             (unchecked-add (unchecked-multiply ~~'idx ~~stride) ~~final-offset)
                                                             ~~'val)
                                                      `(aset ^"[B" (. ~~'obj ~~'sym)
                                                             (+ (* ~~'idx ~~stride) ~~final-offset)
                                                             ~~'val)))))))
                                         comp-names)))
                        offsets)))
            (:structs lo))))

;; ---------- obj- 整字段读取宏 ----------
(defn emit-obj-whole-accessors [lo unchecked?]
  (vec
    (mapcat (fn [s]
              (let [stride    (:stride s)
                    type-kw   (:type s)
                    field-sym-quoted `(symbol (name (:name ~s)))
                    offsets   (field-offsets (:fields s))]
                (map (fn [[f offset]]
                       (let [field-sym* (symbol (name (:name f)))   ;; 这个宏的名字
                             len        (:length f)]
                         (case type-kw
                           :float
                           `(defmacro ~field-sym* [~'out ~'obj ~'idx]
                              (let [~'sym ~field-sym-quoted]
                                `(do ~@(for [ci# (range ~len)]
                                        (if ~unchecked?
                                          `(aset ^"[F" ~~'out (unchecked-add ~~'idx ~ci#)
                                                 (aget ^"[F" (. ~~'obj ~~'sym)
                                                       (unchecked-add (unchecked-add (unchecked-multiply ~~'idx ~~stride) ~~offset) ~ci#)))
                                          `(aset ^"[F" ~~'out (+ ~~'idx ~ci#)
                                                 (aget ^"[F" (. ~~'obj ~~'sym)
                                                       (+ (* ~~'idx ~~stride) ~~offset ~ci#))))))))
                           :int
                           `(defmacro ~field-sym* [~'out ~'obj ~'idx]
                              (let [~'sym ~field-sym-quoted]
                                `(do ~@(for [ci# (range ~len)]
                                         (if ~unchecked?
                                           `(aset ^"[I" ~~'out (unchecked-add ~~'idx ~ci#)
                                                  (aget ^"[I" (. ~~'obj ~~'sym)
                                                        (unchecked-add (unchecked-add (unchecked-multiply ~~'idx ~~stride) ~~offset) ~ci#)))
                                           `(aset ^"[I" ~~'out (+ ~~'idx ~ci#)
                                                  (aget ^"[I" (. ~~'obj ~~'sym)
                                                        (+ (* ~~'idx ~~stride) ~~offset ~ci#))))))))
                           :byte
                           `(defmacro ~field-sym* [~'out ~'obj ~'idx]
                              (let [~'sym ~field-sym-quoted]
                                `(do ~@(for [ci# (range ~len)]
                                         (if ~unchecked?
                                           `(aset ^"[B" ~~'out (unchecked-add ~~'idx ~ci#)
                                                  (aget ^"[B" (. ~~'obj ~~'sym)
                                                        (unchecked-add (unchecked-add (unchecked-multiply ~~'idx ~~stride) ~~offset) ~ci#)))
                                           `(aset ^"[B" ~~'out (+ ~~'idx ~ci#)
                                                  (aget ^"[B" (. ~~'obj ~~'sym)
                                                        (+ (* ~~'idx ~~stride) ~~offset ~ci#)))))))))))
                     offsets)))
            (:structs lo))))

;; ---------- obj- 整字段写入宏 ----------
(defn emit-obj-whole-setters [lo unchecked?]
  (vec
    (mapcat (fn [s]
              (let [stride    (:stride s)
                    type-kw   (:type s)
                    field-sym-quoted `(symbol (name (:name ~s)))
                    offsets   (field-offsets (:fields s))]
                (map (fn [[f offset]]
                       (let [field-sym* (symbol (str "obj-set-" (name (:name f)) "!"))
                             len        (:length f)
                             val-syms   (repeatedly len gensym)]
                         (case type-kw
                           :float
                           `(defmacro ~field-sym* [~'obj ~'idx ~@val-syms]
                              (let [~'sym ~field-sym-quoted]
                                `(do ~@(map-indexed (fn [ci# val#]
                                                      (if ~unchecked?
                                                        `(aset ^"[F" (. ~~'obj ~~'sym)
                                                               (unchecked-add (unchecked-add (unchecked-multiply ~~'idx ~~stride) ~~offset) ~ci#)
                                                               ~val#)
                                                        `(aset ^"[F" (. ~~'obj ~~'sym)
                                                               (+ (* ~~'idx ~~stride) ~~offset ~ci#)
                                                               ~val#)))
                                                    (list ~@val-syms)))))
                           :int
                           `(defmacro ~field-sym* [~'obj ~'idx ~@val-syms]
                              (let  [~'sym ~field-sym-quoted]
                                `(do ~@(map-indexed (fn [ci# val#]
                                                      (if ~unchecked?
                                                        `(aset ^"[I" (. ~~'obj ~~'sym)
                                                               (unchecked-add (unchecked-add (unchecked-multiply ~~'idx ~~stride) ~~offset) ~ci#)
                                                               ~val#)
                                                        `(aset ^"[I" (. ~~'obj ~~'sym)
                                                               (+ (* ~~'idx ~~stride) ~~offset ~ci#)
                                                               ~val#)))
                                                    (list ~@val-syms)))))
                           :byte
                           `(defmacro ~field-sym* [~'obj ~'idx ~@val-syms]
                              (let [~'sym ~field-sym-quoted]
                                `(do ~@(map-indexed (fn [ci# val#]
                                                      (if ~unchecked?
                                                        `(aset ^"[B" (. ~~'obj ~~'sym)
                                                               (unchecked-add (unchecked-add (unchecked-multiply ~~'idx ~~stride) ~~offset) ~ci#)
                                                               ~val#)
                                                        `(aset ^"[B" (. ~~'obj ~~'sym)
                                                               (+ (* ~~'idx ~~stride) ~~offset ~ci#)
                                                               ~val#)))
                                                    (list ~@val-syms))))))))
                     offsets)))
            (:structs lo))))

(defn emit-type-definition [name ^Layout lo body]
  (let [type (:type lo)]
    (case type
      :type `(deftype ~name ~(emit-type-fields lo)
               ~@body)
      :record `(defrecord ~name ~(emit-type-fields lo)
                 ~@body))))

;; ---------- 主宏 ----------
(defmacro deflayout
  [name layout opts & body]
  (let [lo (parse-layout name layout opts)
        unchecked?  (:unchecked-math? lo)
        _  (println (pr-str lo))]
    `(do
       ~(emit-type-definition name lo body)
       ;; 原始数组版本
       ~@(emit-array-accessors lo unchecked?)
       ~@(emit-array-setters lo unchecked?)
       ~@(emit-array-whole-accessors lo unchecked?)
       ~@(emit-array-whole-setters lo unchecked?)
       ;; 原始数组 offset 版本
       ~@(emit-array-accessors-offset lo unchecked?)
       ~@(emit-array-setters-offset lo unchecked?)
       ~@(emit-array-whole-accessors-offset lo unchecked?)
       ~@(emit-array-whole-setters-offset lo unchecked?)
       ;; obj 版本
       ~@(emit-obj-accessors lo unchecked?)
       ~@(emit-obj-setters lo unchecked?)
       ~@(emit-obj-whole-accessors lo unchecked?)
       ~@(emit-obj-whole-setters lo unchecked?)
       )))