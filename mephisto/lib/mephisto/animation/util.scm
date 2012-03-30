; $Id: util.scm,v 1.4 2006/03/13 18:02:42 torus Exp $

(define-module mephisto.animation.util
  (use util.stream)
  (use mephisto.stream)
  (export-all)
  )

(select-module mephisto.animation.util)

(define (make-time-stream)
  (let loop ((n 0))
    (stream-cons n (loop (+ n 1)))))

(define (make-painter-stream painter time-stream)
  (letrec ((painter-stream (cons-stream painter painter-stream)))
    (stream-map cons
		time-stream
		painter-stream)))

(define (accelerate n stream)
  (stream-map (lambda (s t)
		(cons t (cdr s)))
	      (stream-thin n stream)
	      (make-time-stream)))

(define (align-painters painter coords)
  (lambda ()
    (for-each (lambda (coord)
		((apply painter-translate painter coord)))
	      coords)))

(define (camera-moving-stream src dest cur-time total-time time-stream)
  (if (= cur-time total-time)
      the-empty-stream
      (cons-stream
       (cons (stream-car time-stream)
	     (lambda ()
	       (let* ((view-point
		       (smooth-moving-list dest src cur-time total-time))
		      (look-at (map (lambda (a b) (+ a b)) view-point '(0 0 -15))))
		 (gl-load-identity)
		 (apply glu-look-at (append view-point look-at '(0 1 0))))))

       (camera-moving-stream src dest
			     (+ cur-time 1) total-time
			     (stream-cdr time-stream)))))

(provide "mephisto.animation.util")
