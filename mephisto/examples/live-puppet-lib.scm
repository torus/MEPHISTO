;; $Id: live-puppet-lib.scm,v 1.1 2006/08/25 23:38:49 torus Exp $

(use math.const)

(define (angle->unit-vec-x theta phi)
  (vector4f (* (cos phi) (cos theta))
	    (* (cos phi) (sin theta))
	    (sin phi)))

(define (angle->unit-vec-y theta phi)
  (vector4f (- (* (sin phi) (cos theta)))
	    (- (* (sin phi) (sin theta)))
	    (cos phi)))

(define (angle->unit-vec-z theta phi)
  (vector4f (- (sin theta))
	    (cos theta)
	    0))

(define (angle->wrist theta phi psi omega arm-len)
  (let ((elbow 
	 (vector4f-scale (angle->unit-vec-x theta phi) arm-len))
	(forearm
	 (vector4f-scale
	  (vector4f (cos omega) (* (sin omega) (cos psi)) (* (sin omega) (sin psi)))
	  arm-len
	 )))
    (vector4f-add elbow forearm)
    ))

(define (radian->degree rad)
  (* 180/pi rad))

(define (arm-painter matrix shoulder theta phi psi omega arm-len)
  (lambda ()
    (gl-push-matrix)
    (apply gl-translate (take (point4f->list shoulder) 3))
    (gl-mult-matrix matrix)
    (gl-rotate (radian->degree theta) 0 0 1)
    (gl-rotate (radian->degree phi) 0 1 0)
    (gl-rotate (radian->degree psi) 1 0 0)
    (begin
      (gl-push-matrix)
      (gl-scale arm-len 1 1)
      (gl-translate 0.5 0 0)
      (glut-solid-cube 1)
      (gl-pop-matrix))
    (gl-translate arm-len 0 0)
    (gl-rotate (radian->degree omega) 0 0 1)
    (gl-scale arm-len 1 1)
    (gl-translate 0.5 0 0)
    (glut-solid-cube 1)
    (gl-pop-matrix)
    )
  )

(define arm-length 6)
(define leg-length 8)
(define body-length 7)

(define (unit-vector->matrix4f ux uy uz)
  (matrix4f (ref ux 0) (ref ux 1) (ref ux 2) 0
	    (ref uy 0) (ref uy 1) (ref uy 2) 0
	    (ref uz 0) (ref uz 1) (ref uz 2) 0
	    0          0          0          1))

(define (body-painter chest downward forward)
  (lambda ()
    (gl-push-matrix)
    (apply gl-translate (take (point4f->list chest) 3))
    (gl-mult-matrix (unit-vector->matrix4f
		     downward forward
		     (vector4f-cross downward forward)))
    (gl-scale 7 2 2)
    (gl-translate 0.5 0 0)
    (glut-solid-cube 1)
    (gl-pop-matrix)
    )
  )

(define leg-painter arm-painter)

(define (head-painter matrix chest theta phi psi)
  (lambda ()
    (gl-push-matrix)
    (apply gl-translate (take (point4f->list chest) 3))
    (gl-mult-matrix matrix)
    (gl-rotate (radian->degree theta) 0 0 1)
    (gl-rotate (radian->degree phi) 0 1 0)
    (gl-rotate (radian->degree psi) 1 0 0)
    (gl-translate 3 0 0)
    (glut-solid-cube 3)
    (gl-pop-matrix)
    )
  )

;;
;; Static geometrical functions
;;

;; (define (body->right downward forward)
;;   (vector4f-cross downward forward))

(define (body->r:shoulder chest right)
  (point4f-add chest (vector4f-scale right 3)))

(define (body->l:shoulder chest right)
  (point4f-sub chest (vector4f-scale right 3)))

(define (body->l:hip waist right)
  (point4f-sub waist (vector4f-scale right 2)))

(define (body->r:hip waist right)
  (point4f-add waist (vector4f-scale right 2)))

(define (ankle->leg-matrix right hip ankle)
  (let* ((ux (vector4f-normalize (point4f-sub ankle hip)))
	 (uy (vector4f-normalize (vector4f-cross ux right)))
	 (uz (vector4f-cross ux uy)))
    (unit-vector->matrix4f ux uy uz)))

(define (vector->angle v)
  (let ((x (ref v 0))
	(y (ref v 1))
	(z (ref v 2)))
    (let ((h-len (sqrt (+ (square x) (square y)))))
      (let ((theta (atan (/ y x)))
	    (phi (- (atan (/ z h-len)))))
	(values theta
		(if (< x 0) (- pi phi) phi))
	))))

(define (wrist->arm-angle shoulder wrist len)
  (let* ((v (point4f-sub wrist shoulder))
	 (v-len (sqrt (vector4f-dot v v))))
    (let1 ratio (/ v-len len 2)
      (let1 a (if (< ratio 1) (acos ratio) 0)
	(list (- a)
	      0
	      0
	      (* a 2))
      ))))

(define (wrist->arm-matrix right shoulder wrist)
  (let* ((ux (vector4f-normalize (point4f-sub wrist shoulder)))
	 (uy (vector4f-normalize (vector4f-cross right ux)))
	 (uz (vector4f-cross ux uy)))
    (unit-vector->matrix4f ux uy uz)))

(define (body->head:matrix right downward forward)
  (unit-vector->matrix4f (vector4f-scale downward -1)
			 right
			 forward))

(define (attach-constraint-angle-list shoulder wrist angle theta phi psi omega length)
  (attach-constraint! (angle => theta) (car angle))
  (attach-constraint! (angle => phi) (cadr angle))
  (attach-constraint! (angle => psi) (caddr angle))
  (attach-constraint! (angle => omega) (cadddr angle))
  (attach-constraint! (shoulder wrist => angle)
		      (wrist->arm-angle shoulder wrist length))
  )

(define (attach-constraint-arm-angle right matrix shoulder wrist angle theta phi psi omega)
  (attach-constraint! (right shoulder wrist => matrix)
		      (wrist->arm-matrix right shoulder wrist))
  (attach-constraint-angle-list shoulder wrist angle theta phi psi omega arm-length)
  )

(define (attach-constraint-leg-angle right matrix hip ankle angle theta phi psi omega)
  (attach-constraint! (right hip ankle => matrix)
		     (ankle->leg-matrix right hip ankle))
  (attach-constraint-angle-list hip ankle angle theta phi psi omega leg-length)
  )

;;
;; Animation-related functions
;;

(define (count->wrist shoulder right downward forward count offset)
  (let ((angle (+ (* pi/2 (cos (+ offset (/ count 6)))) pi/4)))
    (point4f-add
     shoulder
     (vector4f-add
      (vector4f-add (vector4f-scale right 1)
		    (vector4f-scale forward 2))
      (vector4f-add (vector4f-scale forward (* 4 (cos angle)))
		    (vector4f-scale downward (* 7 (sin angle))))))))

(define (count->l:wrist shoulder right downward forward count)
  (count->wrist shoulder right downward forward count 0))
(define (count->r:wrist shoulder right downward forward count)
  (count->wrist shoulder right downward forward count pi))

(define (count->ankle hip right downward forward count offset)
  (let ((angle (+ (* pi/2 (cos (+ offset (/ count 6)))) pi/2)))
    (let1 point
	(point4f-add
	 hip
	 (vector4f-add
	  (vector4f-add (vector4f-scale right 1)
			(vector4f-scale downward 2))
	  (vector4f-add (vector4f-scale forward (* 8 (cos angle)))
			(vector4f-scale downward (* 12 (sin angle))))))
      (when (< (ref point 1) 0)
	(set! (ref point 1) 0))
      point)))

(define (count->r:ankle hip right downward forward count)
  (count->ankle hip right downward forward count 0))
(define (count->l:ankle hip right downward forward count)
  (count->ankle hip right downward forward count pi))

(define (count->chest count)
  (let1 a (/ count 40)
    (point4f (* 30 (sin a)) (+ 20 (* 2 (cos (/ count 3)))) (* -30 (cos a)))))

(define (count->forward count)
  (let1 angle (+ (/ count 40)
		 (* pi/2 (sin (/ count 5)))
		 pi/2
		 )
   (vector4f (cos angle)
	     0
	     (sin angle))))

(define paint-body-color paint-matte-white)

;; (define (paint-body-color painter)
;;   (paint-color painter
;; 	       1 1 0)
;;   )

(define-syntax define-wire-set
  (syntax-rules ()
    ((_ name wire ...)
     (begin
       (define-wires wire ...)
       (define (name w)
	 (case w
	   ((wire) wire) ...
	   (else (error "unknown wire: " w))
	   ))))))

;;
;; Pair dance
;;

(define (pair-dance-wrist my-shoulder partners-shoulder)
  (if (and my-shoulder partners-shoulder)
      (point4f-add my-shoulder
		   (vector4f-scale (point4f-sub partners-shoulder my-shoulder)
				   0.5))
      #f))

(define (pair-dance-forward my-chest partners-chest)
  (let1 vec (point4f-sub partners-chest my-chest)
    (set! (ref vec 1) 0)
    (vector4f-normalize! vec)
    vec))


;;
;; Mayim Mayim
;;

(define (mayim-mayim-wrist waist next:waist)
  (point4f (/ (+ (ref waist 0) (ref next:waist 0)) 2)
	   (/ (+ (ref waist 1) (ref next:waist 1)) 2)
	   (/ (+ (ref waist 2) (ref next:waist 2)) 2)
	   1
	   ))

(define (make-puppet)

;;
;; Wire definitions
;;

(define-wire-set wire-set
  r:arm:theta r:arm:phi r:arm:psi r:arm:omega r:shoulder r:wrist
  l:arm:theta l:arm:phi l:arm:psi l:arm:omega l:shoulder l:wrist
  r:arm:matrix l:arm:matrix

  chest waist forward downward right

  r:leg:theta r:leg:phi r:leg:psi r:leg:omega r:hip r:ankle
  l:leg:theta l:leg:phi l:leg:psi l:leg:omega l:hip l:ankle
  r:leg:matrix l:leg:matrix

  head:theta head:phi head:psi head:matrix

  l:arm-angle r:arm-angle
  l:leg-angle r:leg-angle

  r:next l:next
  partner

  ;; path-based animation
  r:wrist-path r:wrist-param
  l:wrist-path l:wrist-param
  chest-path chest-param

  motion:run
  motion:mayim-mayim
  motion:pair
  motion:normal

  count
)


;;
;; Painter streams
;;

;; right arm
(append-painter-streams!
 (procedure-stream
  (lambda/constraint
   (r:arm:matrix r:shoulder r:arm:theta r:arm:phi r:arm:psi r:arm:omega)
   ((paint-body-color
     (arm-painter r:arm:matrix r:shoulder
		  r:arm:theta r:arm:phi r:arm:psi r:arm:omega arm-length))))))

;; left arm
(append-painter-streams!
 (procedure-stream
  (lambda/constraint
   (l:arm:matrix l:shoulder l:arm:theta l:arm:phi l:arm:psi l:arm:omega)
   ((arm-painter l:arm:matrix l:shoulder
		 l:arm:theta l:arm:phi l:arm:psi l:arm:omega arm-length)))))

;; body
(append-painter-streams!
 (procedure-stream
  (lambda/constraint
   (chest downward forward)
   ((body-painter chest downward forward)))))

;; left leg
(append-painter-streams!
 (procedure-stream
  (lambda/constraint
   (l:leg:matrix l:hip l:leg:theta l:leg:phi l:leg:psi l:leg:omega)
   ((leg-painter l:leg:matrix l:hip l:leg:theta l:leg:phi l:leg:psi l:leg:omega leg-length)))))

;; right leg
(append-painter-streams!
 (procedure-stream
  (lambda/constraint
   (r:leg:matrix r:hip r:leg:theta r:leg:phi r:leg:psi r:leg:omega)
   ((leg-painter r:leg:matrix r:hip r:leg:theta r:leg:phi r:leg:psi r:leg:omega leg-length)))))

;; head
(append-painter-streams!
 (procedure-stream
  (lambda/constraint
   (head:matrix chest head:theta head:phi head:psi)
   ((head-painter head:matrix chest head:theta head:phi head:psi)))))

;; frame counter
(append-painter-streams!
 (procedure-stream
  (lambda ()
    (let1 c (wire-get-value count)
    (wire-set-value! count (+ c 1))))))


;;
;; initial values
;;

(wires-set-value! (downward (vector4f 0 -1 0))
		  (count 0)
		  (head:theta 0) (head:phi 0) (head:psi 0)
		  (motion:run #t)
		  (motion:mayim-mayim #f)
		  (motion:pair #f)
		  (motion:normal #f)
		  )

;;
;; static constraint
;;

(attach-constraint!
 (chest waist => downward)
 (vector4f-normalize (point4f-sub waist chest)))

(attach-constraint!
 (chest downward => waist)
 (point4f-add chest (vector4f-scale downward body-length)))

(attach-constraint!
 (downward forward => right) (vector4f-cross downward forward))

(attach-constraint!
 (forward right => downward) (vector4f-cross forward right))

(attach-constraint!
 (right downward => forward) (vector4f-cross right downward))

(attach-constraint!
 (chest right => l:shoulder) (body->l:shoulder chest right))

(attach-constraint!
 (chest right => r:shoulder) (body->r:shoulder chest right))

(attach-constraint!
 (waist right => r:hip) (body->r:hip waist right))

(attach-constraint!
 (waist right => l:hip) (body->l:hip waist right))

(attach-constraint-arm-angle
 right l:arm:matrix l:shoulder l:wrist
 l:arm-angle l:arm:theta l:arm:phi l:arm:psi l:arm:omega)

(attach-constraint-arm-angle
 right r:arm:matrix r:shoulder r:wrist
 r:arm-angle r:arm:theta r:arm:phi r:arm:psi r:arm:omega)

(attach-constraint-leg-angle
 right l:leg:matrix l:hip l:ankle
 l:leg-angle l:leg:theta l:leg:phi l:leg:psi l:leg:omega)

(attach-constraint-leg-angle
 right r:leg:matrix r:hip r:ankle
 r:leg-angle r:leg:theta r:leg:phi r:leg:psi r:leg:omega)

(attach-constraint!
 (right downward forward => head:matrix)
 (body->head:matrix right downward forward))

;;
;; animation constraints
;;

;; Normal
(attach-constraint!
 (motion:normal r:wrist-path r:wrist-param => r:wrist)
 (r:wrist-path r:wrist-param))

(attach-constraint!
 (motion:normal l:wrist-path l:wrist-param => l:wrist)
 (l:wrist-path l:wrist-param))

(attach-constraint!
 (motion:normal chest-path chest-param => chest)
 (chest-path chest-param))

;; Pair dance
(attach-constraint!
 (motion:pair r:shoulder partner => r:wrist)
 (pair-dance-wrist r:shoulder (wire-get-value (partner 'l:shoulder))))

(attach-constraint!
 (motion:pair l:shoulder partner => l:wrist)
 (pair-dance-wrist l:shoulder (wire-get-value (partner 'r:shoulder))))

(attach-constraint!
 (motion:pair chest partner => forward)
 (pair-dance-forward chest (wire-get-value (partner 'chest))))

(attach-constraint!
 (motion:pair count => chest) (count->chest count))

(attach-constraint!
 (motion:pair l:hip right downward forward count => l:ankle)
 (count->l:ankle l:hip right downward forward count))

(attach-constraint!
 (motion:pair r:hip right downward forward count => r:ankle)
 (count->r:ankle r:hip right downward forward count))

;; Mayim mayim

(attach-constraint!
 (motion:mayim-mayim waist r:next count => r:wrist)
 (mayim-mayim-wrist waist (wire-get-value (r:next 'waist))))

(attach-constraint!
 (motion:mayim-mayim waist l:next count => l:wrist)
 (mayim-mayim-wrist waist (wire-get-value (l:next 'waist))))

(attach-constraint!
 (motion:mayim-mayim count => forward) (count->forward count))


;; running

(attach-constraint!
 (motion:run r:shoulder right downward forward count => r:wrist)
 (count->r:wrist r:shoulder right downward forward count))

(attach-constraint!
 (motion:run l:shoulder right downward forward count => l:wrist)
 (count->l:wrist l:shoulder right downward forward count))

(attach-constraint!
 (motion:run l:hip right downward forward count => l:ankle)
 (count->l:ankle l:hip right downward forward count))

(attach-constraint!
 (motion:run r:hip right downward forward count => r:ankle)
 (count->r:ankle r:hip right downward forward count))

(attach-constraint!
 (motion:run count => chest) (count->chest count))

(attach-constraint!
 (motion:run count => forward) (count->forward count))

;;
;; misc
;;

;; marks
;; (append-painter-streams!
;;  (procedure-stream
;;   (lambda/constraint
;;    (r:wrist)
;;    ((move-to r:wrist (paint-red cube))))))

;; (append-painter-streams!
;;  (procedure-stream
;;   (lambda/constraint
;;    (l:ankle)
;;    ((move-to l:ankle (paint-red cube))))))

wire-set
)
