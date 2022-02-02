(let
  ((compose
    (lambda (f g)
      (lambda (x) (f (g x)))))
  (double
    (lambda (x) (mul x x)))
  (add1
    (lambda (x) (add x 1))))
  ((compose double add1) 5))