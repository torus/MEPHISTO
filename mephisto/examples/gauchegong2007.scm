;; puppet model with live-paint
;; $Id: live-puppet.scm,v 1.1 2006/08/25 23:38:49 torus Exp $

(use gauche.time)
(use srfi-27)

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
  (gl-clear-color 0.0 0.3 1.0 0.0)

  (thread-start! (make-thread (lambda () (puppet-main-2))))
;   (thread-start! (make-thread (lambda () (puppet-main))))

  (glut-mouse-func mouse)
  (glut-main-loop)

  0)

(define *mouse-input* #f)

(define (mouse button state x y)
  (set! *mouse-input* (list button state x y)))

(define (make-input-stream)
  (stream-cons (if *mouse-input*
		   (let1 i *mouse-input*
		     (set! *mouse-input* #f)
		     i)
		   #f)
	       (make-input-stream)))

(define *mouse-handle* '())

(define (make-linear-path source target)
  (lambda (param)
    (point4f-add
     (point4f 0 0 0)
     (vector4f-add (vector4f-scale target param)
		   (vector4f-scale source (- 1 param))))))

(define *tempo* 125/60)

(define (make-catch-path height puppet1 ball-path source target timer)
  (time-counter-stop! timer)
  (time-counter-reset! timer)

  (let ((r (wire-get-value (puppet1 'r:wrist)))
	(l (wire-get-value (puppet1 'l:wrist))))
    (wire-set-value!
     (puppet1 'r:wrist-path)
     (make-linear-path (list->vector4f (point4f->list r))
		       (vector4f-sub target (vector4f 2 0 0))))
    (wire-set-value!
     (puppet1 'l:wrist-path)
     (make-linear-path (list->vector4f (point4f->list l))
		       (vector4f-add target (vector4f 2 0 0)))))

  (wire-set-value!
   ball-path
   (let ((factor (* height 4)))
     (lambda (param)
       (point4f-add
	(point4f 0 (- height (* factor (square (- param 0.5)))) 0)
	(vector4f-add (vector4f-scale target param)
		      (vector4f-scale source (- 1 param)))))))
  )

(define (make-catch-anim wires source target mouse-args timer)
  (define-values (puppet1 ball-path ball-param ball-position
			  motion:catch motion:handclap motion:jump
			  motion:kneel motion:slump)
    (apply values wires))

  (make-catch-path 20 puppet1 ball-path source target timer)

  (if (eq? (car mouse-args) GLUT_LEFT_BUTTON)
      (add-mouse-handler!
       (lambda (args)
	 (make-catch-anim wires
			  (vector4f 0 22 30)
			  (vector4f (+ (random-integer 11) -5)
				    (+ (random-integer 11) 17) 5)
			  args timer)))

      (add-mouse-handler!
       (lambda (args)
	 (wire-reset! motion:jump)
	 (make-kneel-anim wires timer)

	 (add-mouse-handler!
	  (lambda (args)
	    (wire-reset! motion:kneel)
	    (wire-set-value! motion:jump #t)
	    (handclap! puppet1 motion:handclap)))

	 (add-mouse-handler!
	  (lambda (args)
	    (initial-state puppet1 motion:jump)
	    ))

	 (add-mouse-handler!
	  (lambda (args)
	    (wire-reset! motion:catch)
	    ))

	 (add-mouse-handler!
	  (lambda (args)
	    (let ((waist (wire-get-value (puppet1 'waist)))
		  (chest (wire-get-value (puppet1 'chest)))
		  (r (wire-get-value (puppet1 'r:wrist)))
		  (l (wire-get-value (puppet1 'l:wrist))))

	      (wire-reset! motion:jump)
	      (wire-reset! motion:handclap)

	      (wire-reset! (puppet1 'right))
	      (wire-reset! (puppet1 'downward))
	      (wire-reset! (puppet1 'forward))

	      (wires-set-value!
	       ((puppet1 'waist) (point4f 0 16 0))
	       (motion:slump #t))
	      (make-bow-anim puppet1 ball-path (vector4f 5 14 8) timer)
	    )))

	 (add-mouse-handler!
	  (lambda (args)
	    (make-bow-anim puppet1 ball-path (vector4f -5 14 8) timer)))
	 (add-mouse-handler!
	  (lambda (args)
	    (make-bow-anim puppet1 ball-path (vector4f 0 14 8) timer)))

	 (add-mouse-handler!
	  (lambda (args)
	    (let ((wtimer (make <real-time-counter>))
		  (count (puppet1 'count))
		  (w (puppet1 'waist)))
	      (let ((src (wire-get-value w))
		    (dst (point4f 0 5 1)))

		(time-counter-start! wtimer)

		(attach-constraint!
		 (count => w)
		 (begin
		   (time-counter-stop! wtimer)
		   (let ((t (time-counter-value wtimer)))
		     (when (< t 1) (time-counter-start! wtimer))
		     (let1 p (if (< t 1) t 1)
		       (list->point4f
			(vector4f->list
			 (vector4f-add
			  (vector4f-scale src (- 1 t))
			  (vector4f-scale dst t)))))))
		 )
		))))

	 (add-mouse-handler!
	  (lambda (args)
	    (make-bow-anim puppet1 ball-path (vector4f 0 3 15) timer)))

	 ))))

(define (make-bow-anim puppet1 ball-path target timer)
  (make-catch-path 1 puppet1 ball-path
		   (vector4f 0 23 7) target timer)
  )

(define (make-kneel-anim wires timer)
  (define-values (puppet1 ball-path ball-param ball-position
			  motion:catch motion:handclap
			  motion:jump motion:kneel motion:slump)
    (apply values wires))

  (make-catch-path 10 puppet1 ball-path (vector4f 0 22 30) (vector4f 0 10 17) timer)

  (wire-set-value! motion:kneel #t)
  (wire-set-value!
   (puppet1 'chest-path)
   (lambda (param)
     (point4f-add (point4f 0 23 0)
		  (vector4f-scale (vector4f 0 -8 8) param))))
  )

(define (make-handclap-anim wires)
  (define-values (puppet1 ball-path ball-param ball-position
			  motion:catch motion:handclap motion:jump
			  motion:kneel motion:slump)
    (apply values wires))
  (wire-set-value! motion:handclap #t))

(define (add-mouse-handler! proc)
  (set! *mouse-handle* (append! *mouse-handle* (list proc))))

(define (slump-chest&right target waist up)
  (let ((t-w (point4f-sub target waist)))
    (let ((right (vector4f-normalize (vector4f-cross t-w up))))
      (let ((l-square (vector4f-dot t-w t-w))
	    (vec (vector4f-cross right t-w)))
	(let1 ll (- (/ (square body-length) l-square)
		    (square 1/2))
	  (list right
		(point4f-add
		 waist
		 (vector4f-add
		  (vector4f-scale t-w 1/2)
		  (vector4f-scale vec (sqrt (if (> ll 0) ll 0)))))))))))

(define (handclap! puppet1 motion:handclap)
  (wire-set-value!
   (puppet1 'r:wrist-path)
   (lambda (param)
     (point4f-add (point4f 0 22 3)
		  (vector4f-scale (vector4f -10 0 0) param))))

  (wire-set-value!
   (puppet1 'l:wrist-path)
   (lambda (param)
     (point4f-add (point4f 0 22 3)
		  (vector4f-scale (vector4f 10 0 0) param))))

  (wires-reset! (puppet1 'l:wrist) (puppet1 'r:wrist))
  (wire-set-value! motion:handclap #t)
  )

(define (initial-state puppet1 motion:jump)
  (wires-set-value!
   (motion:jump #t)
   ((puppet1 'forward) (vector4f 0 0 1))
   ((puppet1 'downward) (vector4f 0 -1 0))

   ((puppet1 'r:ankle) (point4f -3 0 0))
   ((puppet1 'l:ankle) (point4f 3 0 0))

   ((puppet1 'chest-path)
    (lambda (param)
      (point4f-add (point4f 0 25 0)
		   (vector4f-scale (vector4f 0 -3 0) param))))

   ((puppet1 'r:wrist) (point4f -4 12 2))
   ((puppet1 'l:wrist) (point4f 4 12 2)))
)

(define (puppet-main-2)
  (define puppet1 #f)
  (define-wires ball-position ball-path ball-param)
  (define-wires motion:handclap motion:jump motion:catch motion:kneel motion:slump)
  (define-wires paint-axis)

  (wire-set-value! paint-axis #t)

  (set! puppet1 (make-puppet))

  (wire-reset! (puppet1 'motion:run))
  (wire-set-value! (puppet1 'motion:normal) #t)

  (let ((pc (puppet1 'chest-param)))
    (attach-constraint! (motion:kneel ball-param => pc) ball-param))

  (let ((pc (puppet1 'chest-param))
	(pl (puppet1 'l:wrist-param))
	(pr (puppet1 'r:wrist-param)))
    (attach-constraint! (motion:handclap pc => pl) pc)
    (attach-constraint! (motion:handclap pc => pr) pc))

  (let ((count (puppet1 'count))
	(param (make-wire))
	(x-param (make-wire)))
    (let ((timer (make <real-time-counter>)))
      (attach-constraint!
       (count => param)
       (begin
	 (time-counter-stop! timer)
	 (let ((t (* (time-counter-value timer) *tempo*)))
	   (time-counter-start! timer)
	   (- t (floor t)))))
      (attach-constraint!
       (param => x-param)
       (let1 x (- (* param 2) 1)
	 (- 1 (* x x))))
      (let ((pc (puppet1 'chest-param)))
	(attach-constraint! (motion:jump x-param => pc) x-param))
      ))

  (let ((c&r (make-wire))
	(l (puppet1 'l:wrist))
	(r (puppet1 'r:wrist))
	(chest (puppet1 'chest))
	(waist (puppet1 'waist))
	(right (puppet1 'right)))
    (attach-constraint!
     (motion:slump ball-position waist => c&r)
     (slump-chest&right ball-position waist (vector4f 0 1 0)))

    (attach-constraint!
     (motion:slump c&r => chest)
     (cadr c&r))
    (attach-constraint!
     (motion:slump c&r => right)
     (car c&r))

    (attach-constraint!
     (motion:slump ball-position right => r)
     (point4f-add ball-position right))
    (attach-constraint!
     (motion:slump ball-position right => l)
     (point4f-sub ball-position right))
    )

  (add-mouse-handler!
   (lambda (args)
     (initial-state puppet1 motion:jump)
     ))

  (add-mouse-handler!
   (lambda (args)
     (handclap! puppet1 motion:handclap)
     ))

  (let ((count (puppet1 'count))
	(timer (make <real-time-counter>)))
    (attach-constraint!
     (ball-path ball-param => ball-position)
     (ball-path ball-param))

    (attach-constraint!
     (count => ball-param)
     (begin
       (time-counter-stop! timer)
       (let1 t (* (time-counter-value timer) 1.5)
	 (if (< t 1)
	     (begin (time-counter-start! timer)
		    t)
	     1))))

    (let ((pr (puppet1 'r:wrist-param))
	  (pl (puppet1 'l:wrist-param)))
      (attach-constraint! (motion:catch ball-param => pr) (* ball-param ball-param))
      (attach-constraint! (motion:catch ball-param => pl) (* ball-param ball-param))
      (attach-constraint! (motion:slump ball-param => pr) ball-param)
      (attach-constraint! (motion:slump ball-param => pl) ball-param))

    (add-mouse-handler!
     (lambda (args)
       ;; Don't jump!
       (wire-set-value!
	(puppet1 'chest-path)
	(lambda (param)
	  (point4f-add (point4f 0 23 0)
		       (vector4f-scale (vector4f 0 -1 0) param))))

       (time-counter-start! timer)
       (make-catch-anim (list puppet1 ball-path ball-param ball-position
			      motion:catch motion:handclap motion:jump
			      motion:kneel motion:slump)
			(vector4f 0 22 30) (vector4f 0 22 3)
			args timer)

       (wire-reset! motion:handclap)
       (wire-set-value! motion:catch #t)
       )
     ))

  (append-painter-streams!
   (stream (lambda ()
	     ;; camera control
	     (gl-matrix-mode GL_PROJECTION)
	     (gl-load-identity)
	     (gl-frustum -20 20 -15 15 50 150)
	     (gl-matrix-mode GL_MODELVIEW)
	     (gl-load-identity)
	     (glu-look-at 20.0 60.0 100.0 0.0 10.0 0.0 0.0 1.0 0.0)
	     ))

   (stream-map
    (lambda (i)
      (lambda ()
	(when (and i (eq? (cadr i) GLUT_UP) (pair? *mouse-handle*))
	  ((car *mouse-handle*) i)
	  (set! *mouse-handle* (cdr *mouse-handle*)))))
    (make-input-stream))

   ;; axis
;;    (procedure-stream
;;     (lambda/constraint
;;      (paint-axis)
;;      ((paint-green
;;        (lambda ()
;; 	 (gl-push-matrix)
;; 	 (gl-scale 10 0.5 0.5)
;; 	 (gl-translate 0.5 0 0)
;; 	 (glut-solid-cube 1)
;; 	 (gl-pop-matrix)
;; 	 (gl-push-matrix)
;; 	 (gl-scale 0.5 10 0.5)
;; 	 (gl-translate 0 0.5 0)
;; 	 (glut-solid-cube 1)
;; 	 (gl-pop-matrix)
;; 	 (gl-push-matrix)
;; 	 (gl-scale 0.5 0.5 10)
;; 	 (gl-translate 0 0 0.5)
;; 	 (glut-solid-cube 1)
;; 	 (gl-pop-matrix)
;; 	 )))))

   ;; stage
   (procedure-stream
    (paint-green
     (lambda ()
      (gl-push-matrix)
      (gl-scale 40 1 40)
      (gl-translate 0 -0.5 0)
      (glut-solid-cube 1)
      (gl-pop-matrix)
      )))

   (procedure-stream
    (lambda/constraint
     (ball-position)
     ((move-to
       ball-position
       (paint-yellow
	(lambda ()
	  (gl-push-matrix)
	  (gl-scale 2 2 2)
	  (glut-solid-icosahedron)
	  (gl-pop-matrix))))))
     ))

  (paint-flower paint-red)
  (paint-flower paint-yellow)
  (paint-flower paint-green)
)

(define (paint-flower paint-color)
  (let loop ((i 10))
    (when (> i 0)
      (let ((x (- (random-integer 30) 15))
	    (z (- (random-integer 30) 15)))
	(append-painter-streams!
	 (procedure-stream
	  (paint-color
	   (lambda ()
	     (gl-push-matrix)
	     (gl-translate x 1 z)
	     (glut-solid-cube 1)
	     (gl-pop-matrix))))
	 ))
      (loop (- i 1))))
  )

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
	     ;; camera control
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
