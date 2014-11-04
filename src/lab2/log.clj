(ns lab2.log
  (:gen-class))

(defn make-node [val]
  (atom {:val val :children []}))

(defn add-node [node place]
  (swap! place assoc :children (conj (@place :children) node)))

(defn print-tree
  ([node]
    (print-tree node 0))
  ([node level]
    (let [indent (apply str (repeat (* level 4) " "))]
      (println indent (@node :val)))
    (doseq [child (@node :children)]
      (print-tree child (+ level 1)))))
