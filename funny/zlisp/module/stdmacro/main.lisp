(req
 (std "std")
 (switch-fun "std" switch-fun))

(defn switch (exp argz) (return `(progn (def args ~exp) ~(std/switch-fun argz))))

(export
 (switch switch))
