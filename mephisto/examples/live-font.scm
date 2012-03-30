;; Drowing font sample with live-paint
;; $Id: live-font.scm,v 1.3 2006/08/21 07:31:58 torus Exp $

(load "./live-paint")
(load "./slide-lib")

;; If you are in the Emacs, run the main procedure now to get into the
;; interactive environment.

;(interactive-main '())

(define-wires count x y)

(append-painter-streams!
 (stream-map (lambda (x)
	       x
	       (lambda ()
		 (wires-set-value! (count x))))
	     (make-time-stream)))

(append-painter-streams!
 (procedure-stream
  (move-to (point4f -15 10 0)
	   (paint-yellow
	    (bitmap-char-painter GLUT_BITMAP_TIMES_ROMAN_24 "MEPHISTO on Gauche.")))
;; 	    (bitmap-char-painter GLUT_BITMAP_HELVETICA_18 "MEPHISTO on Gauche.")))
  ))

(eval&draw
"(attach-constraint!
 (count => x)
 (* 10 (cos (/ count 30))))"
)

;; (attach-constraint!
;;  (count => x)
;;  (* 10 (cos (/ count 30))))

(attach-constraint!
 (count => y)
 (* 10 (sin (/ count 100)))
 )

(append-painter-streams!
 (procedure-stream
  (lambda/constraint
   (x y)
   ((move-to (point4f (* x 1.5) (* 0.5 y) -2)
	     (paint-red cube))))
 ))

(append-painter-streams!
 (procedure-stream
  (lambda/constraint
   (x y)
   ((move-to (point4f (+ x 1.5) (* 0.5 y) -2)
	     (bitmap-char-painter GLUT_BITMAP_HELVETICA_10 #`"x = ,x y = ,y"))))
 ))

(append-painter-streams!
 (procedure-stream
  (lambda/constraint
   (x y)
   ((move-to (point4f (+ x 1.5) (* 0.5 y) -2)
	     (lambda ()
	       (gl-push-matrix)
	       (gl-normal 0 0 1)
	       (gl-scale 0.1 0.1 0.1)
	       (glut-stroke-character GLUT_STROKE_ROMAN (char->integer #\@))
	       (gl-pop-matrix)))))
 ))
