;; pixel drawing sample
;; $Id: live-pixel.scm,v 1.2 2006/08/07 15:17:08 torus Exp $

(use gl.simple-image)

(load "./live-paint")

;; If you are in the Emacs, run the main procedure now to get into the
;; interactive environment.

;(main '())

(define-wires x y img)

(append-painter-streams!
 (procedure-stream
  (lambda/constraint
   (x y img)
   ((move-to (point4f x y -2)
	     (pixel-painter img))))))

(wires-set-value!
 (x -3)
 (y 0)
 (img (read-sgi-image "./sample-img.sgi")))
