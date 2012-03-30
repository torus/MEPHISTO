(add-load-path "../lib")

(use srfi-1)
(use srfi-11)
(use util.stream)

(use gauche.sequence)
(use gauche.collection)
(use gauche.threads)

(use gl)
(use gl.glut)
(use gl.math3d)

(use mephisto.constraint)
(use mephisto.stream)
(use mephisto.animation)
(use mephisto.animation.util)
(use mephisto.model)
(use mephisto.picture)

(load "../examples/paint-primitives")

(define model (prepare-display-list (read-obj-model "./figure.obj")))

(define (main args)
  (mephisto-add-cut! (make-painter-stream
		      (move-to (point4f 0 -10 0)
			       (scale 20 model))
		      (make-time-stream)))

  (glut-init args)
  (glut-init-display-mode (logior GLUT_DOUBLE GLUT_DEPTH GLUT_RGB))

  (glut-init-window-size (* 80 16) (* 80 9))
  (glut-init-window-position 0 0)
  (glut-create-window "MEPHISTO DEMO")
  (mephisto-init!)

  (glut-main-loop)

  0)
