// Written around September 21, 2013.
// An implementation of a Turing machine.

(defn turing-machine [rules]
  rules)

(defn step [machine [state cursor tape]]
  (let [under-cursor (get tape cursor) 
        matching? (fn [rule] (= (take 2 rule) [state under-cursor])) 
        [a b new-state new-under-cursor movement :as rule] (first (filter matching? machine))]
    (if rule
      (let [new-tape (if (= cursor -1)
                       (into [new-under-cursor] tape) 
                       (assoc tape cursor new-under-cursor))
            new-cursor- ({:left (dec cursor) :right (inc cursor) :nop cursor} movement)
            new-cursor (if (= cursor -1)
                         (inc new-cursor-)
                         new-cursor-)]
        [new-state new-cursor new-tape]) 
      nil)))

;;; s-c-t stands for state-cursor-tape
(defn until [final-states machine s-c-t]
 (loop [[new-state new-cursor new-tape :as new-s-c-t] s-c-t]
   (if (final-states new-state)
     new-s-c-t
     (recur (step machine new-s-c-t)))))

;;; TESTS ARE COMING!

(def plus-one (turing-machine
                [[:normal 1 :normal 0 :left]
                 [:normal 0 :stop 1 :nop]
                 [:normal nil :stop 1 :nop]]))

(def tapes [[0]
            [1]
            [1 0]
            [1 1]
            [1 0 1 1 0 1 1]
            [1 1 1]])

(doseq [tape tapes]
  (let [s-c-t [:normal (dec (count tape)) tape]
        new-s-c-t (until #{:stop} plus-one s-c-t)]
    (println tape (new-s-c-t 2))))

