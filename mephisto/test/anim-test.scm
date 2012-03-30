; Tests for animation feature
; $Id: anim-test.scm,v 1.1 2006/06/01 06:46:08 torus Exp $

(add-load-path "../lib")

(use gauche.test)

(use gl)
(use gl.glut)

(use mephisto.stream)
(use mephisto.animation)
(use mephisto.animation.util)

(test-start "animation")

(test-module 'mephisto.stream)
(test-module 'mephisto.animation)

(define cube
  (lambda ()
     (glut-solid-cube 5)
     ))

(mephisto-add-cut! (make-painter-stream cube (make-time-stream)))

(mephisto-main '())
