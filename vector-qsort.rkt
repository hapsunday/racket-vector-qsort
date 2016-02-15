#lang typed/racket

(require racket/vector)
;; (require racket/fixnum)
(require racket/unsafe/ops)

(: vector-qsort! (All (T) (-> (Vectorof T) (-> T T Boolean) (Vectorof T))))
(define (vector-qsort! A Less?)
  (: Equal? (-> T T Boolean))
  (define (Equal? x y)
    (and (not (Less? x y))
         (not (Less? y x))))
  (: -swap (-> Fixnum Fixnum Void))
  (define (-swap i j)
    (unless (unsafe-fx= i j)
      (let ([tmp (vector-ref A i)])
        (vector-set! A i (vector-ref A j))
        (vector-set! A j tmp))))
  (: -do-when (-> Fixnum (-> Fixnum Boolean) Fixnum Fixnum))
  (define (-do-when idx ok? d)
    (if (ok? idx) (-do-when (unsafe-fx+ d idx) ok? d)
        idx))
  (: -do-split (-> Fixnum Fixnum (values Fixnum Fixnum)))
  (define (-do-split L R)
    (let ([pivot (vector-ref A R)])
      (: --add (-> Fixnum Fixnum Fixnum Fixnum))
      (define (--add i e d)
        (if (Equal? (vector-ref A i) pivot)
            (begin (-swap i e) (unsafe-fx+ d e))
            e))
      (let loop ([_L : Fixnum L]
                 [_R : Fixnum (unsafe-fx- R 1)]
                 [Le : Fixnum L]
                 [Re : Fixnum (unsafe-fx- R 1)])
        (let ([Li (-do-when _L (lambda ([v : Fixnum]) : Boolean
                                 (Less? (vector-ref A v) pivot)) 1)]
              [Ri (-do-when _R
                            (lambda ([v : Fixnum]) : Boolean
                              (and (> v _L) (Less? pivot (vector-ref A v))))
                            -1)])
          (if (unsafe-fx< Li Ri)
              (begin (-swap Li Ri)
                     (loop Li Ri (--add Li Le 1) (--add Ri Re -1)))
              (values (let g ([k : Fixnum (unsafe-fx- Li 1)] [z : Fixnum L])
                        (if (unsafe-fx< z Le)
                            (begin (-swap z k) (g (unsafe-fx- k 1) (unsafe-fx+ z 1)))
                            k))
                      (let h ([k : Fixnum Li] [z : Fixnum R])
                        (if (unsafe-fx< Re z)
                            (begin (-swap z k) (h (unsafe-fx+ k 1) (unsafe-fx- z 1)))
                            k))))))))
  (let -qsort! ([L : Fixnum 0]
                [R : Fixnum (- (vector-length A) 1)])
    (when (unsafe-fx< L R)
      (let-values ([(Le Re) (-do-split L R)])
        (-qsort! L Le)
        (-qsort! Re R))))
  A)

