(use srfi-11)

(use util.match)

(use gauche.time)
(use gl)
(use gl.glut)
(use gl.math3d)
(use gl.simple-image)

(add-load-path "../mephisto/lib")
(use mephisto.model)

(define *window-width* 640)
(define *window-height* 480)

(define (mephisto-main args)
  (glut-init args)
  (glut-init-display-mode (logior GLUT_DOUBLE GLUT_DEPTH GLUT_RGB))

  (glut-init-window-size *window-width* *window-height*)
  (glut-init-window-position 0 0)
  (glut-create-window "TANK!")
  (mephisto-init!)

  (set! *draw-model* (prepare-model))

  (glut-keyboard-func keyboard)
  (glut-display-func render)
  (glut-idle-func render)
  (glut-main-loop)
)

(define (main args)
  (mephisto-main args))


(define (keyboard key x y)
  (case key
    ((27) (exit 0))))

(define (clear-screen!)
  (gl-clear-color 0 0 0 1)
  (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  )

(define *draw-model* #f)

(define (mephisto-init!)
  (gl-shade-model GL_SMOOTH)

  (gl-light GL_LIGHT0 GL_POSITION '#f32(-100.0 100.0 0.0 0.0))
  (gl-light GL_LIGHT0 GL_DIFFUSE '#f32(1.0 1.0 1.0 1.0))
  (gl-light GL_LIGHT0 GL_AMBIENT '#f32(0.3 0.3 0.3 1.0))

  (gl-light GL_LIGHT1 GL_POSITION '#f32(100.0 -100.0 0.0 0.0))
  (gl-light GL_LIGHT1 GL_DIFFUSE '#f32(1.0 1.0 1.0 1.0))
  (gl-light GL_LIGHT1 GL_AMBIENT '#f32(0.3 0.3 0.3 1.0))

  (gl-enable GL_LIGHTING)
  (gl-enable GL_LIGHT0)
  (gl-enable GL_LIGHT1)

  (gl-enable GL_DEPTH_TEST)
  (gl-enable GL_NORMALIZE)
  )

(define *texname* #f)

(define (prepare-model)
  (match (time (read-sgi-image "./miku/textures/mtl_001_diff.sgi"))
	 ((width height channels tex)
    (let ((l (gl-gen-lists 1))
	  (model (time (read-obj-model "./miku/miku01.obj"))))
      (let1 texnames (gl-gen-textures 1)
	(set! *texname* (ref texnames 0))
	(gl-bind-texture GL_TEXTURE_2D *texname*))
      (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT)
      (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT)
      (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
      (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
      (gl-tex-image-2d GL_TEXTURE_2D 0 GL_RGB8
		       width height
		       0 GL_RGB GL_UNSIGNED_BYTE tex)

      (gl-new-list l GL_COMPILE)
      (model)
      (gl-end-list)

      (lambda ()
	(gl-call-list l)))
    )))

(define render
  (let ((alpha 0))
    (lambda ()
      (clear-screen!)

      (gl-enable GL_TEXTURE_2D)
      (gl-tex-env GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_DECAL)
      (gl-bind-texture GL_TEXTURE_2D *texname*)

      (gl-push-matrix)
      (glu-perspective 20 4/3 5 25)
      (glu-look-at 0 5 10 0 0 0 0 1 0)

      (let1 yoffset (* 0.1 (sin (* 0.3 alpha)))
	(gl-push-matrix)
	(gl-translate 0 yoffset 0)
	(gl-rotate alpha 0 1 0)
	(*draw-model*)
	(gl-pop-matrix)
	)

      (gl-pop-matrix)

      (glut-swap-buffers)

      (set! alpha (+ alpha 0.1))
      )))
