;; Demo program for `LL Ring 2006'
;; $Id: 2006-llgong.scm,v 1.1 2006/08/25 23:39:49 torus Exp $

(load "./live-paint")
(load "./slide-lib")
(load "./live-puppet-lib")

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

  (thread-start! (make-thread (lambda () (slide-main))))

  (glut-main-loop)

  0)

;;;;;;;;;;
(set! body->r:shoulder
      (lambda (chest right)
	(point4f -20 0 0)))

(set! body->l:shoulder
      (lambda (chest right)
	(point4f -10 0 0)))

(set! body->l:hip
      (lambda (waist right)
	(point4f 0 0 0)))

(set! body->r:hip
      (lambda (waist right)
	(point4f 10 0 0)))

;;;;;;;;;;;

(set! count->chest
      (lambda (count)
	(point4f 0 20 -30)))

(set! count->forward
      (lambda (count)
	(vector4f 0 0 1)))


(set! count->r:ankle
      (lambda (hip right downward forward count)
	(count->ankle hip right downward forward 0 0)))
(set! count->l:ankle
      (lambda (hip right downward forward count)
	(count->ankle hip right downward forward 0 0)))

(set! count->l:wrist
      (lambda (shoulder right downward forward count)
	(count->wrist shoulder right downward forward 0 0)))
(set! count->r:wrist
      (lambda (shoulder right downward forward count)
	(count->wrist shoulder right downward forward 0 0)))

;;;;;;;;;;;;;;;;

(define (slide-main)
  (define puppet1 #f)
  (define puppet2 #f)
  (define puppet3 #f)
  (define puppet4 #f)
  (define headmat #f)
  (define chest #f)

  (append-painter-streams!
   (stream (lambda ()
	     (gl-matrix-mode GL_PROJECTION)
	     (gl-load-identity)
	     (gl-frustum -20 20 -15 15 50 150)
	     (gl-matrix-mode GL_MODELVIEW)
	     (gl-load-identity)
	     (glu-look-at 0.0 40.0 100.0 0.0 20.0 0.0 0.0 1.0 0.0)
	     ))
   (procedure-stream
    (move-to (point4f 25 0 20)
	     (lambda ()
	       ((bitmap-char-painter GLUT_BITMAP_HELVETICA_18
				     (number->string (slide-current-time)))))))
   )

;;   (eval&draw
;; "(print (+ 1 2 3 4 5 6 7 8 9 10))
;; (+ 1 2 3 4 5 6 7 8 9 10)
;; (print (+ 1 2 3 4 5 6 7 8 9 10))"
;;    3)

  (slide-draw-string
   "> gosh"
  1000000 4500000)

  (sys-sleep 1)

  (slide-draw-string
   "
gosh> (MEPHISTO)"
  1000000 4500000)

  (sys-sleep 2)

  (slide-draw-string
   "


M E P H I S T O       "
  1000000 6500000)

  (sys-sleep 1)

  (slide-draw-string
   "




    o n
        G a u c h e"
  1000000 5500000)

  (sys-sleep 1)

  (slide-draw-string
   "








       T o r u  H i s a i"
  1000000 4500000)

  (sys-sleep 5)

;;;;;;;;;;;;

;;;;;;;;;;;;

  (eval&draw/alt
   "(define puppet1 (make-puppet))"
   10
   (set! puppet1 (make-puppet)))


  (eval&draw/alt
   "(define (body->r:shoulder chest right)
  (point4f-add chest
    (vector4f-scale right 3)))

(attach-constraint!
 (chest right => r:shoulder)
 (body->r:shoulder chest right))"
20
  (set! body->r:shoulder
	(lambda (chest right)
	  (point4f-add chest (vector4f-scale right 3))))
)

;;"

  (eval&draw/alt
   "(define (body->l:shoulder chest right)
  (point4f-sub chest (vector4f-scale right 3)))

(attach-constraint!
 (chest right => l:shoulder)
 (body->l:shoulder chest right))"
   10
  (set! body->l:shoulder
	(lambda (chest right)
	  (point4f-sub chest (vector4f-scale right 3))))
)


  (eval&draw/alt
   "(define (body->l:hip waist right)
  (point4f-sub waist (vector4f-scale right 2)))

(attach-constraint!
 (waist right => l:hip)
 (body->l:hip waist right))"
   10
  (set! body->l:hip
	(lambda (waist right)
	  (point4f-sub waist (vector4f-scale right 2))))
)


  (eval&draw/alt
   "(define (body->r:hip waist right)
  (point4f-add waist (vector4f-scale right 2)))

(attach-constraint!
 (waist right => r:hip)
 (body->r:hip waist right))"
   10
  (set! body->r:hip
	(lambda (waist right)
	  (point4f-add waist (vector4f-scale right 2))))
)


;;;;;;;;;;;;;

  (eval&draw/alt
"(define (count->r:ankle hip right downward forward count)
  (count->ankle hip right downward forward count 0))

(define (count->l:ankle hip right downward forward count)
  (count->ankle hip right downward forward count pi))"
20
(begin
  (set! count->r:ankle
      (lambda (hip right downward forward count)
	(count->ankle hip right downward forward count 0)))
  (set! count->l:ankle
	(lambda (hip right downward forward count)
	  (count->ankle hip right downward forward count pi))))
)

;;"

  (eval&draw/alt
"(define (count->l:wrist shoulder right downward forward count)
  (count->wrist shoulder right downward forward count 0))

(define (count->r:wrist shoulder right downward forward count)
  (count->wrist shoulder right downward forward count pi))"
10
(begin
  (set! count->l:wrist
	(lambda (shoulder right downward forward count)
	  (count->wrist shoulder right downward forward count 0)))
  (set! count->r:wrist
	(lambda (shoulder right downward forward count)
	  (count->wrist shoulder right downward forward count pi))))
)

;;;;;;;;;;;"

  (eval&draw/alt
   "(define (count->forward count)
  (let ((a (/ count 40))
        (b (/ count 5)))
    (vector4f-normalize
     (vector4f-add (vector4f (* -30 (sin a))
                             0
                             (* -30 (cos a)))
                   (vector4f (* -56 (sin b))
                             0
                             (* 56 (cos b)))))))"
   20
  (set! count->forward
	(lambda (count)
	  (let ((a (/ count 40))
		(b (/ count 5)))
	    (vector4f-normalize
	     (vector4f-add (vector4f (* -30 (sin a))
				     0
				     (* -30 (cos a)))
			   (vector4f (* -56 (sin b))
				     0
				     (* 56 (cos b))))))))

)


  (eval&draw/alt
   "(define (count->chest count)
  (let ((a (/ count 40))
        (b (/ count 5)))
    (point4f-add (point4f (* 30 (cos a))
                          (+ 20 (* 2 (sin (/ count 3))))
                          (* -30 (sin a)))
                 (vector4f (* 7 (cos b))
                           0
                           (* 7 (sin b))))))"
20
(set! count->chest
	(lambda (count)
	  (let ((a (/ count 40))
		(b (/ count 5)))
	    (point4f-add (point4f (* 30 (cos a))
				  (+ 20 (* 2 (sin (/ count 3))))
				  (* -30 (sin a)))
			 (vector4f (* 7 (cos b))
				   0
				   (* 7 (sin b)))))))
)


;;;;;;;;;;


  (eval&draw/alt
   "(define puppet2 (make-puppet))"
   20
  (set! puppet2 (make-puppet)))

  (eval&draw/alt
   "(define puppet3 (make-puppet))"
   5
  (set! puppet3 (make-puppet)))

  (eval&draw/alt
   "(define puppet4 (make-puppet))"
   5
  (set! puppet4 (make-puppet)))


  (eval&draw/alt
   "(define headmat (puppet1 'head:matrix))
(define chest (puppet1 'chest))"
   30
(begin
  (set! headmat (puppet1 'head:matrix))
  (set! chest (puppet1 'chest))))


  (eval&draw
   "(append-painter-streams!
 (procedure-stream
  (lambda/constraint
   (headmat chest)
   (gl-push-matrix)
   (apply gl-translate (take (point4f->list chest) 3))
   (gl-mult-matrix headmat)
   (gl-translate 2 0 1)
   ((paint-red cube))
   (gl-pop-matrix)
   )))"
   20)



  (eval&draw
"(wires-set-value!
   ((puppet1 'count) 0)
   ((puppet2 'count) (* 4.5 pi))
   ((puppet3 'count) (* 40 pi))
   ((puppet4 'count) (* 44.5 pi)))"
   30)


;;;;;;;;;;

  (eval&draw
"(wires-set-value!
 ((puppet1 'partner) puppet2)
 ((puppet2 'partner) puppet1)
 ((puppet3 'partner) puppet4)
 ((puppet4 'partner) puppet3))"
20)

  (eval&draw
"(wires-reset!
 (puppet1 'motion:run)
 (puppet2 'motion:run)
 (puppet3 'motion:run)
 (puppet4 'motion:run))"
10)


  (eval&draw
"(wires-set-value!
 ((puppet1 'motion:pair) #t)
 ((puppet2 'motion:pair) #t)
 ((puppet3 'motion:pair) #t)
 ((puppet4 'motion:pair) #t))"
20)

  )
