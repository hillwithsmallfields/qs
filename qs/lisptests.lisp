(+ (* 3 4) 5)

(* (+ 3 4) 5)

(let ((a (+ 3 4))
      (b (* 3 4)))
  (/ b a))

(let ((financisto (read-canonical "~/common/qs/qs-scratch/financisto-full.csv"))
      (handelsbanken (read-canonical "~/common/qs/qs-scratch/handelsbanken-full.csv")))
  (print financisto)
  (print handelsbanken))
