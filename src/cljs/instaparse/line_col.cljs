(ns instaparse.line-col
  (:require instaparse.transform))

; Function to annotate parse-tree with line and column metadata.

(defrecord Cursor [^int index ^long line ^long column])

(defn- advance-cursor [^Cursor cursor ^String text new-index]
  (let [new-index (int new-index)]
    (assert (<= (.-index cursor) new-index))
    (if (= (.-index cursor) new-index) cursor
      (loop [index (.-index cursor), line (.-line cursor), column (.-column cursor)]
        (cond 
          (= index new-index) (Cursor. index line column)
          (= (.charAt text index) \newline) (recur (inc index) (inc line) 1)
          :else (recur (inc index) line (inc column)))))))
         
(defn- make-line-col-fn
  "Given a string `text`, returns a function that takes an index into the string,
and returns a cursor, including line and column information.  For efficiency,
inputs must be fed into the function in increasing order."
  [^String text]
  (let [cursor-state (atom (Cursor. 0 1 1))]
    (fn line-col [i]
      (swap! cursor-state advance-cursor text i)
      @cursor-state)))                        

(defn- hiccup-add-line-col-spans
  [line-col-fn parse-tree]
  (let [m (meta parse-tree), 
        start-index (:instaparse.gll/start-index m), 
        end-index (:instaparse.gll/end-index m)]
    (if (and start-index end-index)
      (let [start-cursor (line-col-fn start-index),
            children (doall (map (partial hiccup-add-line-col-spans line-col-fn) (next parse-tree))),
            end-cursor (line-col-fn end-index)]
        (with-meta
          (into [(first parse-tree)] children)
          (merge (meta parse-tree) 
                 {:instaparse.gll/start-line (:line start-cursor)
                  :instaparse.gll/start-column (:column start-cursor)
                  :instaparse.gll/end-line (:line end-cursor)
                  :instaparse.gll/end-column (:column end-cursor)})))
      parse-tree)))

(defn- enlive-add-line-col-spans
  [line-col-fn parse-tree]
  (let [m (meta parse-tree), 
        start-index (:instaparse.gll/start-index m), 
        end-index (:instaparse.gll/end-index m)]
    (if (and start-index end-index)
      (let [start-cursor (line-col-fn start-index),
            children (doall (map (partial enlive-add-line-col-spans line-col-fn) (:content parse-tree))),
            end-cursor (line-col-fn end-index)]
        (with-meta
          (assoc parse-tree :content children)
          (merge (meta parse-tree) 
                 {:instaparse.gll/start-line (:line start-cursor)
                  :instaparse.gll/start-column (:column start-cursor)
                  :instaparse.gll/end-line (:line end-cursor)
                  :instaparse.gll/end-column (:column end-cursor)})))
      parse-tree)))
  
(defn add-line-col-spans
  "Given a string `text` and a `parse-tree` for text, return parse tree
with its metadata annotated with line and column info. The info can
then be found in the metadata map under the keywords:
 
:instaparse.gll/start-line, :instaparse.gll/start-column,
:instaparse.gll/end-line, :instaparse.gll/end-column

The start is inclusive, the end is exclusive. Lines and columns are 1-based."
  [text parse-tree]
  (let [line-col-fn (make-line-col-fn text)]
    (cond
      (nil? parse-tree) nil
      
      (and (map? parse-tree) (:tag parse-tree))
      ; This is an enlive tree-seq
      (enlive-add-line-col-spans line-col-fn parse-tree)     
      
      (and (vector? parse-tree) (keyword? (first parse-tree)))
      ; This is a hiccup tree-seq
      (hiccup-add-line-col-spans line-col-fn parse-tree)
      
      (and (sequential? parse-tree) (map? (first parse-tree)) (:tag (first parse-tree)))
      ; This is an enlive tree with hidden root tag
      (instaparse.transform/map-preserving-meta 
        (partial enlive-add-line-col-spans line-col-fn) parse-tree)
      
      (and (sequential? parse-tree) (vector? (first parse-tree)) (keyword? (first (first parse-tree))))
      ; This is a hiccup tree with hidden root tag
      (instaparse.transform/map-preserving-meta 
        (partial hiccup-add-line-col-spans line-col-fn) parse-tree)

      (instance? instaparse.gll.Failure parse-tree)
      ; pass failures through unchanged
      parse-tree
    
      :else
      (throw "Invalid parse-tree, not recognized as either enlive or hiccup format."))))
