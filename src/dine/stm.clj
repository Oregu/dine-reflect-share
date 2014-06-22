(ns dine.stm)

(def free-fork -1)
(defn free? [fork] (= fork free-fork))

(defn millis
  ([ ] (System/currentTimeMillis))
  ([x] (System/currentTimeMillis)))

(defmacro stats [myname desc stats]
  `(println ~desc "for" ~myname "is"
      "min" (apply min ~stats)
      "max" (apply max ~stats)
      "avg" (quot (apply + ~stats) (count ~stats))))

(defn report [data]
  (let [{myname :myname,
         time-thinking :thinking,
         dine-attempts :attempts} data]
    (stats myname "Thinking time" time-thinking)
    (stats myname "Dine attempts" dine-attempts)))

(defn dine [idx myname forks total]
  (future
    (let [time-start (millis)
          time-last-eat   (atom time-start)
          eat-wait-times  (atom [])
          current-wait-count (atom 0)
          eat-wait-counts (atom [])
          left-fork-idx  idx
          right-fork-idx (mod (inc idx) total)
          left-fork  (nth forks left-fork-idx)
          right-fork (nth forks right-fork-idx)]

      (loop [state :reflexing]
        (condp = state

          :reflexing
          (let [time-to-think (+ (rand 100) 100)]
            (println myname "starts thinking")
            (Thread/sleep time-to-think)
            (recur :hungry))

          :hungry
          (do
            (println myname "hungry")
            (if
              (dosync
                (when (and
                        (free? (ensure left-fork))
                        (free? (ensure right-fork)))
                  (do
                    (ref-set left-fork idx)
                    (ref-set right-fork idx))))
              (recur :dining)
              (do
                (swap! current-wait-count inc)
                (Thread/sleep (+ (rand 50) 50))
                (recur :hungry))))

          :dining
          (let [time-to-eat (+ (rand 50) 50)]
            (println myname "eating with forks" left-fork-idx right-fork-idx)
            (Thread/sleep time-to-eat)
            (println myname "full")

            (dosync
              (ref-set left-fork free-fork)
              (ref-set right-fork free-fork))

            (swap! eat-wait-counts conj @current-wait-count)
            (swap! eat-wait-times conj (- (millis) @time-last-eat))
            (swap! current-wait-count (constantly 0))
            (swap! time-last-eat millis)

            (if (< (- (millis) time-start) 10000)
              (recur :reflexing)
              (do
                (println myname "done!")
                {:myname myname
                 :thinking @eat-wait-times
                 :attempts @eat-wait-counts}))))))))

(defn go []
  (let [folks [:Aristotle :Kant :Spinoza :Marx :Russel]
        n (count folks)
        forks (vec (map ref (replicate n free-fork)))]
    (map (comp report deref)
      (doall (map-indexed #(dine %1 %2 forks n) folks)))))
