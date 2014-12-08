(ns crawler.logger
  (:gen-class))

(declare make-node add-node)

(defn init-log []
  (make-node :root))

(defn add-record [record place]
  (let [node (make-node record)]
    (add-node node place)
    node))

(defn print-tree
  ([log-root]
    (println)
    (print-tree (@log-root :children) 0))
  ([nodes level]
    (let [indent (apply str (repeat (* level 4) " "))]
      (doseq [node nodes]
        (println indent (@node :val))
        (print-tree (@node :children) (+ level 1))))))

(defn make-node [val]
  (atom {:val val :children []}))

(defn add-node [node place]
  (swap! place assoc :children (conj (@place :children) node)))
