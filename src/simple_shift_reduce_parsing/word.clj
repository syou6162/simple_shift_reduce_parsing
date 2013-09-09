(ns simple_shift_reduce_parsing.word)

(defrecord Word [^String surface ^String lemma
                 ^String pos-tag ^String cpos-tag
                 ^long idx ^long head])

(defn normalize-num-expr [^String s]
  (if (.matches s "[0-9]+|[0-9]+\\.[0-9]+|[0-9]+[0-9,]+")
    "<num>"
    s))

(defn make [^String surface ^String pos-tag ^long idx ^long head]
  (let [lemma (if (< 5 (count surface))
                (-> surface (subs 0 5) normalize-num-expr))
        cpos-tag (subs pos-tag 0 1)]
    (Word. (normalize-num-expr surface) lemma
           pos-tag cpos-tag
           idx head)))
