(req
 (std "std")
 (switch-fun "std" switch-fun)
 (not- "std" not)
 (eq- "std" eq)
 (type- "std" type)
 (length- "std" length)
 (head- "std" head)
 (tail- "std" tail)
 (cons- "std" cons)
 (panic- "std" panic)
 (testing "testing")
 (fntest- "testing" fntest))

(defn switch (exp argz) (return (list 'progn (list 'def 'args exp) (/std/switch-fun argz))))

(defn fntest (body expect) (return (/testing/fntest- body expect)))

(defn backquote (exp)
  (if (/std/not- (/std/eq- (/std/type- exp) :list))
      (return (list 'quote exp))
    (if (/std/not- exp)
        (return (list 'quote exp))
      (if (/std/eq- (/std/length- exp) 2)
          (if (/std/eq- (/std/head- exp) 'tilde)
              (return (/std/head- (/std/tail- exp)))
            (return (list '/std/cons (../backquote (/std/head- exp)) (../backquote (/std/tail- exp)))))
        (return (list '/std/cons (../backquote (/std/head- exp)) (../backquote (/std/tail- exp))))))))

(defn defn2 (name args body)
  (progn
  (return
   (list
    'progn
    (list
     'defn name '()
     (list 'progn
           (list 'defn 'original-func args body)
           (list 'original-func '(at mut) '(at pre) '(at 0) '(at up2))
           (list 'return :shouldnt-go-here)))
    (list name '(at mut) '(at 0))))))
