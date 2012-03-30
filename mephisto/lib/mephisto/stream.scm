;; stream utility
;; $Id: stream.scm,v 1.6 2006/03/13 18:02:42 torus Exp $

(define-module mephisto.stream
  (use gauche.collection)
  (use util.stream)

  (export-all)
  )

(select-module mephisto.stream)

(define the-empty-stream stream-null)
(define cons-stream stream-cons)


(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (integers-from n)
  (cons-stream n (integers-from (+ n 1))))

(define (constant-stream n)
  (cons-stream n (constant-stream n)))

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let* ((s1car (stream-car s1))
                (s2car (stream-car s2))
                (weight1 (weight s1car))
                (weight2 (weight s2car)))
           (if (< weight1 weight2)
               (cons-stream s1car
                            (merge-weighted (stream-cdr s1)
                                            s2
                                            weight))
               (cons-stream s2car
                            (merge-weighted s1
                                            (stream-cdr s2)
                                            weight)))))))

(define (stream-concat s t)
  (if (stream-null? s)
      t
      (cons-stream
       (stream-car s)
       (stream-concat (stream-cdr s) t))))

(define-syntax stream-concat-stream
  (syntax-rules ()
    ((_ s ss)
     (stream-concat-stream-sub s (delay ss)))))

(define (stream-concat-stream-sub s ss)
  (if (stream-null? s)
      (if (stream-null? (force ss))
	  the-empty-stream
	  (stream-concat-stream (stream-car (force ss)) (stream-cdr (force ss))))
      (cons-stream
       (stream-car s)
       (stream-concat-stream (stream-cdr s) (force ss)))))

(define (stream-thin n stream)
  (define (stream-n-cdr x stream)
    (cond ((= x 0) stream)
	  ((stream-null? stream) the-empty-stream)
	  (else
	   (stream-n-cdr (- x 1) (stream-cdr stream)))))

  (if (stream-null? stream)
      the-empty-stream
      (cons-stream
       (stream-car stream)
       (stream-thin n (stream-n-cdr n stream)))))

(provide "mephisto.stream")
