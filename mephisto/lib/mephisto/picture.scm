(define-module mephisto.picture
  (use srfi-1)

  (use gl)
  (use gl.glut)

  (use gauche.uvector)

  (export-all)
  )

(select-module mephisto.picture)

(define (cube)
  (glut-solid-cube 1.0))

(define (paint-color painter r g b)
  (lambda ()
    (gl-material GL_FRONT GL_DIFFUSE (f32vector r g b 1.0))
    (gl-material GL_FRONT GL_AMBIENT
		 (f32vector (/ r 10) (/ g 10) (/ b 10) 1.0))
    (painter)))

(define (paint-red painter)
  (paint-color painter 1.0 0.0 0.0))

(define (paint-green painter)
  (paint-color painter 0.0 1.0 0.0))

(define (paint-yellow painter)
  (paint-color painter 1.0 1.0 0.0))

(define (paint-gray painter)
  (paint-color painter 0.5 0.5 0.5))

(define (paint-matte-white painter)
  (lambda ()
    (gl-material GL_FRONT GL_DIFFUSE '#f32(1 1 1 1))
    (gl-material GL_FRONT GL_AMBIENT '#f32(0.3 0.3 0.3 1))
    (painter)))

(define (move-to coord painter)
  (lambda ()
    (gl-push-matrix)
    (apply gl-translate (take (point4f->list coord) 3))
    (painter)
    (gl-pop-matrix)
    ))

(define (scale n painter)
  (lambda ()
    (gl-push-matrix)
    (gl-scale n n n)
    (painter)
    (gl-pop-matrix)))

(provide "mephisto.picture")
