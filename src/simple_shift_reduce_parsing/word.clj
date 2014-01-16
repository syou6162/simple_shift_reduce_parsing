(ns simple_shift_reduce_parsing.word)
(import Jenkins)

(defrecord Word [^String surface ^long surface-id
                 ^String lemma ^long lemma-id
                 ^String pos-tag ^long pos-tag-id
                 ^String cpos-tag ^long cpos-tag-id
                 ^long idx ^long head])

(defn normalize-num-expr [^String s]
  (if (.matches s "[0-9]+|[0-9]+\\.[0-9]+|[0-9]+[0-9,]+")
    "<num>"
    s))

(defn make [^String surface' ^String pos-tag ^long idx ^long head]
  (let [surface (normalize-num-expr surface')
        lemma (if (< 5 (count surface))
                (-> surface (subs 0 5) normalize-num-expr)
                (-> surface normalize-num-expr))
        cpos-tag (subs pos-tag 0 1)]
    (Word. surface (Jenkins/hashString surface)
           lemma (Jenkins/hashString lemma)
           pos-tag (Jenkins/hashString pos-tag)
           cpos-tag (Jenkins/hashString cpos-tag)
           idx head)))
