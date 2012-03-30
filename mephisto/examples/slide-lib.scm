;; $Id: slide-lib.scm,v 1.2 2006/08/25 23:38:49 torus Exp $

(use srfi-13)

(define (paint-bold painter)
  (lambda ()
    (gl-push-matrix)
    (painter)
    (gl-translate 0.1 0 0)
    (painter)
    (gl-pop-matrix))
  )

(define (draw-text-painter text-list substr-proc)
  (let ((lines (map (lambda (line)
		      (let* ((lst (string->list line))
			     (len (length lst)))
			(cons len lst)))
		    text-list)))
    (lambda (x)
      (let loop ((line 0)
		 (lines lines))
	  (if (null? lines)
	      'done
	      (let ((len (caar lines))
		    (lst (cdar lines)))
		(move-and-text-painter -35 (+ 45 (* line -4)) 10
				       (substr-proc lst len x))
		(loop (+ line 1) (cdr lines))))))))

(define (move-and-text-painter x y z text)
  ((move-to (point4f x y z)
	    (paint-yellow
	     (paint-bold
	      (scale 0.025
		     (stroke-char-painter GLUT_STROKE_ROMAN
					  text)))))))

(define slide-current-time
  (let-values (((start-sec start-microsec) (sys-gettimeofday)))
    (lambda ()
      (let-values (((sec microsec) (sys-gettimeofday)))
	(+ (* 1000000 (- sec start-sec)) (- microsec start-microsec))
	))))

(define (stream-until end-time)
  (if (> (slide-current-time) end-time)
      stream-null
      (stream-cons #t (stream-until end-time))))

(define (slide-draw-string str drawing-interval interval)
  (let ((lines (string-split str "\n")))
    (let* ((start-time (slide-current-time))
	   (end-time (+ start-time interval)))
    (append-painter-streams!
     (stream-map (lambda (s1 s2) s1)
		 (procedure-stream
		  (let1 draw-proc (draw-text-painter lines
						     (lambda (lst len x)
						       (take lst (floor (* len x)))))
		    (lambda ()
		      (let ((duration (- (slide-current-time) start-time)))
			(draw-proc (if (< duration drawing-interval)
				       (/ duration drawing-interval)
				       1))
			))))
		 (stream-until end-time)
		 )))))

;; (slide-draw-string
;;  "(MEPHISTO\non Gauche)")

(define *slide-total-time* 0)

(define-macro (eval&draw text sec)
  `(eval&draw/alt ,text ,sec ,(read-from-string text)))

(define-macro (eval&draw/alt text sec altbody)
  (set! *slide-total-time* (+ *slide-total-time* sec))
  #?=(list sec *slide-total-time* text)
  `(begin
     (slide-draw-string ,text 1000000 ,(- (* sec 1000000) 500000))
     (sys-sleep 1)
     ,altbody
     (sys-sleep ,(- sec 1))
     ))
