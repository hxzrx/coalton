;;; A program to (inefficiently) generate an "infinite" stream of
;;; primes. See
;;;
;;;     https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf
;;;
;;; for details.

(cl:in-package #:small-coalton-programs)

(coalton-toplevel
  (define-type (LazyStream :t)
    (LCons :t (Unit -> LazyStream :t)))

  (define (extract n l)
    "Take `n` primes from the stream `l`."
    (if (<= n 0)
        Nil
        (match l
          ((LCons x xs) (Cons x (extract (- n 1) (xs)))))))

  (define (numbers-from n)
    "Produce a stream of ascending integers starting from `n`."
    (LCons n (fn () (numbers-from (+ n 1)))))

  (define (drop-if f l)
    "Filter the stream `l`, removing values that satisfy `f`."
    (match l
      ((LCons x xs) (if (f x)
                        (drop-if f (xs))
                        (LCons x (fn () (drop-if f (xs))))))))

  (define (multiple? m x)
    "Is `x` a multple of `m`?"
    (== 0 (mod x m)))
  
  (define primes
    "A stream of prime numbers."
    (let ((drop-multiples
            (compose drop-if multiple?))
          (sieve
            (fn (l)
              (match l
                ((LCons p xs)
                 (LCons p (fn () (sieve (drop-multiples p (xs))))))))))
      (sieve (numbers-from 2)))))
