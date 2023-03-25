(req
 (std "std")
 (switch-fun "std" switch-fun)
 (testing "testing")
 (fntest- "testing" fntest))

(defn switch (exp argz) (return `(progn (def args ~exp) ~(/std/switch-fun argz))))

(defn fntest (body expect) (return (/testing/fntest- body expect)))

(export
 (switch switch)
 (fntest fntest))
