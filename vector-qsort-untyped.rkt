#lang racket

(require racket/vector)
(require racket/function)
;; (require racket/fixnum)
(require racket/unsafe/ops)

;; (: vector-qsort! (All (T) (-> (Vectorof T) (-> T T Boolean) (Vectorof T))))
(define (vector-qsort! A Less?)

  ;; (: -less? (-> Fixnum Fixnum Boolean))
  (define (-less? x y)
    (Less? (unsafe-vector-ref A x)
           (unsafe-vector-ref A y)))

  ;; (: -swap (-> Fixnum Fixnum Void))
  (define (-swap i j)
    (unless (unsafe-fx= i j)
      (let ([tmp (unsafe-vector-ref A i)])
        (unsafe-vector-set! A i (unsafe-vector-ref A j))
        (unsafe-vector-set! A j tmp))))
  
  ;; (: -do-when (-> Fixnum (-> Fixnum Boolean) Fixnum Fixnum))
  (define (-do-when idx ok? d)
    (if (ok? idx)
        (-do-when (unsafe-fx+ d idx) ok? d)
        idx))

  ;; (: -choose-pivot (-> Fixnum Fixnum T))
  (define (-choose-pivot L R)
    (let ([M (unsafe-fxquotient (unsafe-fx+ L R) 2)])
      (and (-less? M L) (-swap L M))
      (and (-less? R L) (-swap L R))
      (and (-less? M R) (-swap M R))
      (unsafe-vector-ref A R)))
  
  ;; (: -do-split (-> Fixnum Fixnum (values Fixnum Fixnum)))
  (define (-do-split L R)
    (let ([pivot (-choose-pivot L R)])
      
      (let loop ([_L L]
                 [_R (unsafe-fx- R 1)]
                 [Le L]
                 [Re (unsafe-fx- R 1)])
        (let ([Li (let g ([k _L])
                    (if (Less? (unsafe-vector-ref A k) pivot)
                        (g (unsafe-fx+ 1 k))
                        k))]
              [Ri (let g ([k _R])
                    (if (and (unsafe-fx< _L k) (Less? pivot (unsafe-vector-ref A k)))
                        (g (unsafe-fx- k 1))
                        k))])
          (if (unsafe-fx< Li Ri)
              (begin (-swap Li Ri)
                     (loop Li Ri
                           (if (Less? (unsafe-vector-ref A Li) pivot)
                               Le (begin (-swap Li Le) (unsafe-fx+ 1 Le)))
                           (if (Less? pivot (unsafe-vector-ref A Ri))
                               Re (begin (-swap Ri Re) (unsafe-fx- Re 1)))))
              (values (let g ([k (unsafe-fx- Li 1)] [z L])
                        (if (unsafe-fx< z Le)
                            (begin (-swap z k) (g (unsafe-fx- k 1) (unsafe-fx+ z 1)))
                            k))
                      (let h ([k Li] [z R])
                        (if (unsafe-fx< Re z)
                            (begin (-swap z k) (h (unsafe-fx+ k 1) (unsafe-fx- z 1)))
                            k))))))))
  (define (-bubble-sort L R)
    (when (let loop ([pi L] [i (unsafe-fx+ L 1)] [tag #f])
            (if (unsafe-fx<= i R)
                (if (-less? i pi)
                    (begin (-swap i pi)
                           (loop i (unsafe-fx+ i 1) #t))
                    (loop i (unsafe-fx+ i 1) tag))
                tag))
      (-bubble-sort L (unsafe-fx- R 1))))
  (let -qsort! ([L 0]
                [R (- (vector-length A) 1)])
    (if (unsafe-fx< R (unsafe-fx+ L 8))
        (-bubble-sort L R)
        (when (unsafe-fx< L R)
          (let-values ([(Le Re) (-do-split L R)])
            (-qsort! L Le)
            (-qsort! Re R)))))
  A)


(require math/base)
(define (test-sort N)
  (let ([S (build-vector N
                         (lambda (x) (random-integer -100000 1000000000)))])
    (time (let ([A (vector-copy S)])
            (vector-qsort! A unsafe-fx<)
            (printf "vector ~a\n" (vector-copy A 0 10))))
    (printf "begin to list sort S[~a..]\n" (vector-copy S 0 10))
    (time (let ([A (list->vector (sort (vector->list S) unsafe-fx<))])
            (printf "list  ~a\n" (vector-copy A 0 10))))))

