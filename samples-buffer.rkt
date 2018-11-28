#lang racket

; todo: type guards

(define (my-nosample) +nan.f)

(define (my-fl? x)
  (single-flonum? x))

(define (->my-fl x)
  (real->single-flonum x))

(define samples-buf%
  (class object%
    (define VAL-NO-SAMPLE (my-nosample))
    (define SAMPLE-LOG    (make-hasheq))
    (define IX-LAST       0)
    (define SEM           (make-semaphore 1))
    (super-new)

    (define/public (do-sem fn-do)
      (call-with-semaphore SEM fn-do))

    ; get sample count
    (define/public (size?) IX-LAST)

    (define (coordinate ix)
      (let ([bin (arithmetic-shift ix -16)]
            [ix  (bitwise-and ix #xFFFF)])
        (values bin ix)))
    
    (define/public (tuple-ref ix)
      (let*-values ([(bin ix) (coordinate ix)]
                    [(vec)    (hash-ref SAMPLE-LOG bin #f)])
        (cond
          [(false? vec) '()]
          [else         (vector-ref vec ix)])
        ))

    (define (unsafe-push lst-tuple)
      (do-sem
       (lambda ()
         (let*-values ([(bin ix) (coordinate IX-LAST)]
                       [(vec)    (hash-ref SAMPLE-LOG bin #f)])
           
           ; create new bin
           (cond [(false? vec)
                  (set! vec (make-vector (+ 1 #xFFFF) #f))
                  (hash-set! SAMPLE-LOG bin vec)])

           ; update value
           (vector-set! vec ix lst-tuple)
           (set! IX-LAST (+ IX-LAST 1))
           ))         
       ))

    ; push; push a tuples list into the samples log
    (define/public (push-nums lst-tuple)
      (unsafe-push
       (for/list ([val (in-list lst-tuple)])
         (cond
           [(my-fl? val) val]
           [(real? val)  (->my-fl val)]
           [else         VAL-NO-SAMPLE]))
       ))

    (define/public (push-string str)
      (push-nums
       (for/list ([vs (in-list (string-split (string-trim str)))])
         (string->number vs))
       ))

    (define/public (walk-old ix-start n-count fn-do)
      (let ([len (size?)])
        (for ([step (in-range    0 n-count)]
              [ix   (in-naturals ix-start)]
              #:break (>= ix len))
          (fn-do step ix (tuple-ref ix))
          )))

    ; determine min/max values across sample range
    ; todo: mutation-free version
    (define/public (minmax? ix-start n-count)
      (let* ([min VAL-NO-SAMPLE]
             [max VAL-NO-SAMPLE])
        (walk
         ix-start n-count
         (lambda (_ ix lst-tuple)
           (for ([val (in-list lst-tuple)])
             (cond
               [(not (nan? val))
                (cond [(or (nan? min) (< val min)) (set! min val)])
                (cond [(or (nan? max) (> val max)) (set! max val)])])
             )))        
        (if (nan? min) (values (->my-fl -1) (->my-fl 1)) (values min max))
        ))

    ; (time (send B minmax-ex? 0 100000))
    ; (time (send B walk-ex 0 100000 (lambda (ct ix tup z) (+ z 1)) 1))
    (define/public (walk ix-start i-count fn-do)
      (cond
        [(< ix-start 0)
         (error 'walk "ix-start (~a) must be >= 0" ix-start)]
        [(< i-count 0)
         (error 'walk "i-count (~a) must be positive" i-count)]
        [else
         (let*-values ([(ix-end)     (min IX-LAST (+ ix-start i-count))]
                       [(bin-a ix-a) (coordinate ix-start)]
                       [(bin-b ix-b) (coordinate ix-end)]
                       [(step)       0])
           (for ([ixBin (in-range bin-a (+ bin-b 1))])
             (let* ([BVEC (hash-ref SAMPLE-LOG ixBin #f)]
                    [i-start (if (= ixBin bin-a) ix-a 0)]
                    [i-end   (if (= ixBin bin-b) ix-b #f)])
               (cond
                 [(vector? BVEC)
                  (for ([v (in-vector BVEC i-start i-end)])
                    (fn-do step (+ step ix-start) v)
                    (set! step (+ step 1))
                    )])
               ))
           )]
        ))
    ))

#|
(define B (new samples-buf%))

(for ([i (in-range 0 100000)])
  (send B push-nums
  (for/list ([i (in-range 4)]) (->my-fl (- (random 1 1000000) 500000)))
  ))
|#

(provide my-nosample)
(provide my-fl?)
(provide ->my-fl)
(provide samples-buf%)
