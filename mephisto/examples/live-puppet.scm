;; puppet model with live-paint
;; $Id: live-puppet.scm,v 1.1 2006/08/25 23:38:49 torus Exp $

(load "./live-paint")
(load "./live-puppet-lib")

;(main '())

(define (main args)
  (mephisto-add-cut! (make-live-stream))

  (glut-init args)
  (glut-init-display-mode (logior GLUT_DOUBLE GLUT_DEPTH GLUT_RGB))

  ;; Game mode (full screen)
;;   (glut-game-mode-string "width=640 height=480 bpp~24 hertz=30")
;;   (glut-enter-game-mode)

  ;; Window
  (glut-init-window-size 640 480)
  (glut-init-window-position 0 0)
  (glut-create-window "MEPHISTO DEMO")

  (mephisto-init!)

  (thread-start! (make-thread (lambda () (puppet-main))))

  (glut-main-loop)

  0)

;; camera control
(define (puppet-main)
  (define puppet1 #f)
  (define puppet2 #f)
  (define puppet3 #f)
  (define puppet4 #f)
  (define puppet5 #f)
  (define puppet6 #f)
  (define puppet7 #f)
  (define puppet8 #f)
  (define headmat #f)
  (define chest #f)

  (append-painter-streams!
   (stream (lambda ()
;; 	     (print (vm-get-stack-trace-lite))
	     (gl-matrix-mode GL_PROJECTION)
	     (gl-load-identity)
	     (gl-frustum -20 20 -15 15 50 150)
	     (gl-matrix-mode GL_MODELVIEW)
	     (gl-load-identity)
	     (glu-look-at 0.0 60.0 100.0 0.0 20.0 0.0 0.0 1.0 0.0)
	     )))

  (set! puppet1 (make-puppet))
  (sys-sleep 1)
  (set! puppet2 (make-puppet))
  (sys-sleep 1)
  (set! puppet3 (make-puppet))
  (sys-sleep 1)
  (set! puppet4 (make-puppet))

  (set! puppet5 (make-puppet))
  (set! puppet6 (make-puppet))
  (set! puppet7 (make-puppet))
  (set! puppet8 (make-puppet))

  (set! headmat (puppet1 'head:matrix))
  (set! chest (puppet1 'chest))

  (append-painter-streams!
   (procedure-stream
    (lambda/constraint
     (headmat chest)
     (gl-push-matrix)
     (apply gl-translate (take (point4f->list chest) 3))
     (gl-mult-matrix headmat)
     (gl-translate 2 0 1)
     ((paint-red cube))
     (gl-pop-matrix)
     )))

  (let1 u (* 10 pi)
    (wire-set-value! (puppet1 'count) 0)
    (wire-set-value! (puppet2 'count) u)
    (wire-set-value! (puppet3 'count) (* 2 u))
    (wire-set-value! (puppet4 'count) (* 3 u))
    (wire-set-value! (puppet5 'count) (* 4 u))
    (wire-set-value! (puppet6 'count) (* 5 u))
    (wire-set-value! (puppet7 'count) (* 6 u))
    (wire-set-value! (puppet8 'count) (* 7 u))
    )

  (wire-set-value! (puppet1 'r:next) puppet8)
  (wire-set-value! (puppet1 'l:next) puppet2)
  (wire-set-value! (puppet2 'r:next) puppet1)
  (wire-set-value! (puppet2 'l:next) puppet3)
  (wire-set-value! (puppet3 'r:next) puppet2)
  (wire-set-value! (puppet3 'l:next) puppet4)
  (wire-set-value! (puppet4 'r:next) puppet3)
  (wire-set-value! (puppet4 'l:next) puppet5)
  (wire-set-value! (puppet5 'r:next) puppet4)
  (wire-set-value! (puppet5 'l:next) puppet6)
  (wire-set-value! (puppet6 'r:next) puppet5)
  (wire-set-value! (puppet6 'l:next) puppet7)
  (wire-set-value! (puppet7 'r:next) puppet6)
  (wire-set-value! (puppet7 'l:next) puppet8)
  (wire-set-value! (puppet8 'r:next) puppet7)
  (wire-set-value! (puppet8 'l:next) puppet1)

  (wire-reset! (puppet1 'motion:run))
  (wire-reset! (puppet2 'motion:run))
  (wire-reset! (puppet3 'motion:run))
  (wire-reset! (puppet4 'motion:run))
  (wire-reset! (puppet5 'motion:run))
  (wire-reset! (puppet6 'motion:run))
  (wire-reset! (puppet7 'motion:run))
  (wire-reset! (puppet8 'motion:run))

  (wire-set-value! (puppet1 'motion:pair) #t)
  (wire-set-value! (puppet2 'motion:pair) #t)
  (wire-set-value! (puppet3 'motion:pair) #t)
  (wire-set-value! (puppet4 'motion:pair) #t)
  (wire-set-value! (puppet5 'motion:pair) #t)
  (wire-set-value! (puppet6 'motion:pair) #t)
  (wire-set-value! (puppet7 'motion:pair) #t)
  (wire-set-value! (puppet8 'motion:pair) #t)

  (wire-set-value! (puppet1 'partner) puppet2)
  (wire-set-value! (puppet2 'partner) puppet1)
  (wire-set-value! (puppet3 'partner) puppet4)
  (wire-set-value! (puppet4 'partner) puppet3)
  (wire-set-value! (puppet5 'partner) puppet6)
  (wire-set-value! (puppet6 'partner) puppet5)
  (wire-set-value! (puppet7 'partner) puppet8)
  (wire-set-value! (puppet8 'partner) puppet7)

  (append-painter-streams!
   (procedure-stream
    (lambda/constraint
     (chest)
     ((move-to (point4f-add chest (vector4f 0 8 0))
	       (paint-red
		(bitmap-char-painter GLUT_BITMAP_HELVETICA_18 "Mayim Mayim")))
      )
     )))
  )
