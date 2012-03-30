;; A minimum demo to use gl.simple.viewer

(use gl)
(use gl.glut)
(use gl.simple.viewer)

(add-load-path "../mephisto/lib")
(use mephisto.model)

(define (main args)
  (glut-init args)
  (simple-viewer-display (read-obj-model "./miku/miku01.obj"))
  (simple-viewer-window 'demo)
  (simple-viewer-run)
  0)
