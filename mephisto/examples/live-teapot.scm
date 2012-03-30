;; with live-paint sample
;; $Id: live-teapot.scm,v 1.1 2006/06/04 09:17:21 torus Exp $

(load "./live-paint")

;; If you are in the Emacs, run the main procedure now to get into the
;; interactive environment.

;(main '())

(define-wires count p1 p2)

(attach-constraint!
 (count => p1)
 (/ (remainder count 30) 3))

;(wires-set-value! (p1 3))

(append-painter-streams!
 (procedure-stream
  (lambda/constraint
   (p1)
   (glut-solid-teapot p1))))

(append-painter-streams!
 (stream-map (lambda (x)
	       x
	       (lambda ()
		 (wires-set-value! (count x))))
	     (make-time-stream)))

;(wire-reset! p1)
