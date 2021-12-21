(ns graph.core
  (:require [clojure.data.priority-map :refer [priority-map priority-map-keyfn-by]])
  (:require [clojure.pprint :refer [pprint]]))

(def infinity Double/POSITIVE_INFINITY)


;;
;; Part 1.
;; I decided to use an adjacency map to ensure unique vertices, with weighted edge maps.
;;

(def example-graph
  {:01 {:02 2, :04 1},
   :02 {:03 2, :04 3},
   :03 {},
   :04 {:05 1},
   :05 {}})


;;
;; Part 2.
;;

(defn rand-weight
  "Returns a random edge weight from 1 to max inclusive."
  [max]
  (inc (rand-int max)))


(defn add-vertex
  "Adds a vertex with key 'v' to a graph, ensuring that it is edge connected
   to an existing node (if any) with random weighting."
  [g v w]
  (if (empty? g)
    (assoc {} v {})
    (let [prev-key (rand-nth (keys g))
          prev-edges (get g prev-key {})]
      (-> g
          (assoc prev-key
                 (assoc prev-edges v (rand-weight w)))
          (assoc v {})))))


(defn add-edge
  "Randomly adds an edge within the graph with a random weight, ensuring
   that no edges are duplicated."
  [g vert-keys w]
  (loop []
    (let [vert-idx (rand-int (dec (count vert-keys)))       ;; skip last vert
          rest-keys (into [] (drop (inc vert-idx) vert-keys))
          vert (nth vert-keys vert-idx)
          edges (get g vert {})
          spare-keys (into [] (clojure.set/difference (set rest-keys) (set (keys edges))))]

      (if (> (count spare-keys) 0)
        (assoc g vert
                 (assoc edges (rand-nth spare-keys) (rand-weight w)))
        ;; try again if we need to
        (recur)))))


(defn make-graph
  "Generates a directed graph with 'n' vertices and 's' edges, with each
   edge given a random weight between 1 and 's'."
  [n s]
  (let [vert-keys (mapv #(keyword (format "%02d" %)) (range 1 (inc n)))]

    ;; add required number of verts ensuring connectedness
    (loop [g (reduce #(add-vertex %1 %2 s) {} vert-keys)
           e (- s (dec n))]

      (if (not= e 0)
        ;; add remaining edges iteratively
        (recur (add-edge g vert-keys s)
               (dec e))
        (into (sorted-map) g)))))


;;
;; Part 3.
;;

(defn initial-costs
  "Creates a cost map with vertices/nodes mapped to a weight and path. This
  is implemented as a priority map sorted by ascending weight."
  [graph start]
  (-> (into (priority-map-keyfn-by :weight <)
            (map vector (keys graph) (repeat {:weight infinity :path []})))
      (assoc start {:weight 0 :path []})))


(defn result-path
  "Extracts the path to a node and resulting weight from a cost map entry."
  [node key]
  [(when-let [path (:path node)]
     (conj path key))
   (or (:weight node) infinity)])


(defn update-costs
  "Updates the costs map by adding the current (with the lowest weight) node's weight
  to neighbouring nodes, and removes the current node from the map."
  [graph costs]
  (let [[current-node {:keys [weight path]}] (peek costs)

        ;; adds the current node weight to connected neighbours
        update-weights (fn [weights [node edge-weight]]
                         (let [min-weight (comp first #(sort-by :weight [%1 %2]))
                               cost {:weight (+ weight edge-weight)
                                     :path   (conj (or path []) current-node)}]
                           (update weights node min-weight cost)))

        ;; the nodes to update
        neighbours (select-keys (graph current-node) (keys costs))

        costs' (reduce update-weights costs neighbours)]

    ;; done with current node so remove from 'unvisited'
    (dissoc costs' current-node)))


(defn shortest-path
  "Finds the shortest path between two nodes in a directed graph using edge weights."
  [graph start end]
  (loop [costs (initial-costs graph start)]

    ;; return result if reached end node or all costs unchanged
    (if (or (= end (-> costs peek first))
            (every? #(= infinity (:weight %)) (vals costs)))
      (result-path (costs end) end)
      (recur (update-costs graph costs)))))


;;
;; Part 4.
;;

(defn eccentricity
  "Calculates the eccentricity of a graph node, or the maximum distance (by weight)
  to another node."
  [graph node]
  (->> (remove #{node} (keys graph))
       (map (partial shortest-path graph node))
       (map (fn [[_ cost]] cost))
       (remove #(= % infinity))                             ;; remove 'leaf' node weights
       (sort >)
       (first)))


(defn compare-eccentricity
  "Compares eccentricity for all nodes in a graph using the comparator and returns
  the first of the sorted list."
  [graph comparator]
  (->> (map (partial eccentricity graph) (keys graph))
       (remove nil?)
       (sort comparator)
       (first)))


(defn radius
  "Calculates the minimum eccentricity of a graph."
  [graph]
  (compare-eccentricity graph <))


(defn diameter
  "Calculates the maximum eccentricity of a graph."
  [graph]
  (compare-eccentricity graph >))


;(def random-graph (make-graph 10 10))
;(shortest-path random-graph (first (keys random-graph)) (last (keys random-graph)))
;(eccentricity random-graph (first (keys random-graph)))
;(radius random-graph)
;(diameter random-graph)
