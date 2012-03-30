; Puppet
; $Id: puppet.scm,v 1.1 2006/06/01 06:46:08 torus Exp $

(add-load-path "../lib")

(use srfi-1)
(use srfi-11)
(use util.stream)
(use gauche.sequence)
(use gauche.collection)

(use gl)
(use gl.glut)
(use gl.math3d)

(use mephisto.constraint)
(use mephisto.stream)
(use mephisto.animation)

(load "./paint-primitives")

(define (unique-constraint)
  (make-constraint
   (a b)

   ((a => b) a)
;;    ((b => a) b)
   ))

(define (3-point-constraint)
  (make-constraint
   (pa pc la lc n pb)

   ((pa pc la lc n => pb)
    (let* ((pa->pc (point4f-sub pc pa))
	   (lb-square (vector4f-dot pa->pc pa->pc))
	   (lb (sqrt lb-square))
	   (s (let1 p (/ (+ la lb lc) 2)
		    (sqrt (* p (- p la) (- p lb) (- p lc)))))
	   (l (let1 h (/ (* 2 s) lb)
		    (sqrt (- (square lc) (square h))))))
      (if (not (real? s))
	  #f
	  (point4f-add pa
		       (vector4f-scale
			(vector4f-add (vector4f-scale pa->pc l)
				      (vector4f-scale (vector4f-normalize
					; right-handed coordinate system!
							  (vector4f-cross pa->pc n))
						      (* 2 s)))
			(/ 1 lb)))))))
  )

(define (cross-normalize-constraint)
  (make-constraint
   (vec-a vec-b vec-c)
   ((vec-a vec-b => vec-c)
    (vector4f-normalize (vector4f-cross vec-a vec-b))))
   )

(define (negative-constraint)
  (make-constraint
   (n negative-n)
   ((n => negative-n) (- n))
   ))

(define (direction-and-length-constraint)
  (make-constraint
   (p0 vec-dir len p1)

   ((p0 vec-dir len => p1)
    (point4f-add p0 (vector4f-scale vec-dir len)))
   ))

(define (hip-joint-constraint)
  (let ((c-x-norm-1 (cross-normalize-constraint))
	(c-x-norm-2 (cross-normalize-constraint))
	(c-dir-len-1 (direction-and-length-constraint))
	(c-dir-len-2 (direction-and-length-constraint))
	(c-neg (negative-constraint))
	(neg-len2 (make-wire)))
    (define (connect p0 len1 len2 vec-u vec-f p1 p2 n1 n2)
      ((c-x-norm-1 'connect) vec-u vec-f n1)
      ((c-x-norm-2 'connect) vec-u vec-f n2)
      ((c-dir-len-1 'connect) p0 n1 len1 p1)
      ((c-dir-len-2 'connect) p0 n2 neg-len2 p2)
      ((c-neg 'connect) len2 neg-len2)

      (c-neg 'wake-up)
      (c-neg 'update)

      )

    (lambda (m)
      (cond ((eq? m 'connect) connect))
      )))

(define (square x) (* x x))

(define (feet-forward-dir-constraint w)
  (make-constraint
   (pf1 pf2 vec-dir vec-u vec-f)

   ((pf1 pf2 vec-dir => vec-f)
    (let ((v (point4f-sub pf2 pf1)))
      (vector4f-scale vec-dir (abs (vector4f-dot v vec-dir)))
      ))
   )
  )

(define (feet-hip-constraint)
  (make-constraint
   (pf1 pf2 vec-u vec-f len1 len2 pc)

   ((pf1 pf2 vec-u vec-f len1 len2 => pc)
    (let* ((pf1->pf2 (point4f-sub pf2 pf1))
	   (u-dot-pf1->pf2 (vector4f-dot vec-u pf1->pf2))
	   (vec-l (vector4f-sub pf1->pf2 (vector4f-scale vec-u u-dot-pf1->pf2)))
	   (h (sqrt (- (square (* (+ len1 len2) 0.99)) (/ (vector4f-dot vec-f vec-f) 4))))
	   (h-prime (if (< u-dot-pf1->pf2 0)
			(+ h u-dot-pf1->pf2)
			h)))
      (point4f-add (point4f-add pf1 (vector4f-scale vec-l 0.5))
		   (vector4f-scale vec-u h-prime))))))

(define (make-straight-footprint-stream src step)
  (define (step-stream)
    (cons-stream step (step-stream)))
  (make-footprint-stream src (step-stream)))

(define (make-footprint-stream src step-stream)
  (cons-stream src (make-footprint-stream (point4f-add src (stream-car step-stream))
					  (stream-cdr step-stream))))

(define (make-footpath-stream src dest max-height total-time)
  (let ((vec (point4f-sub dest src)))
    (define (f x)
      (- (* max-height
	    (- (* 4
		  (square (- x 0.5)))
	       1))))

    (define (from t)
      (if (= t total-time)
	  the-empty-stream
	  (cons-stream (f (/ t total-time))
		       (from (+ t 1)))))

    (from 0)
    )
  )

(define (make-walking-stream pf1 pf2 fp-l fp-r height-vec period)
  (define (complete-curve-stream src dest t wire)
    (if (> t period)
	the-empty-stream
	(let ((x (/ t period)))
	  (let* ((v (point4f-sub dest src))
		 (p (point4f-add
		     (point4f-add src (vector4f-scale v x))
		     (vector4f-scale height-vec
				     (- 1 (* 4 (square (- x 0.5))))))))
	    (cons-stream
	     (lambda ()
	       (wire-set-value! wire p)
	       (draw-footprint dest))
	     (complete-curve-stream src dest (+ t 1) wire)
	     )))))

  (define (make-ss sl sr)
    (let ((lsrc (stream-car sl))
	  (ldest (stream-car (stream-cdr sl)))
	  (rsrc (stream-car sr))
	  (rdest (stream-car (stream-cdr sr))))
      (cons-stream (stream-concat (complete-curve-stream lsrc ldest 0 pf1)
				  (complete-curve-stream rsrc rdest 0 pf2))
		   (make-ss (stream-cdr sl) (stream-cdr sr)))))

  (let1 ss (make-ss fp-l fp-r)
    (stream-concat-stream (stream-car ss) (stream-cdr ss)))
  )

(define (make-legs)
  (let ((leg-1 (3-point-constraint))
	(leg-2 (3-point-constraint))
	(hip-joint (hip-joint-constraint))
	(forward-dir (feet-forward-dir-constraint 2))
	(feet-hip (feet-hip-constraint))

	(arm-1 (3-point-constraint))
	(arm-2 (3-point-constraint))
	(shoulder (hip-joint-constraint))
	(body (direction-and-length-constraint))

	(ph1 (make-wire))		; hip
	(ph2 (make-wire))
	(pk1 (make-wire))		; knee
	(pk2 (make-wire))
	(pf1 (make-wire))		; foot
	(pf2 (make-wire))

	(ps1 (make-wire))		; shoulder
	(ps2 (make-wire))
	(pe1 (make-wire))		; elbow
	(pe2 (make-wire))
	(pw1 (make-wire))		; wrist
	(pw2 (make-wire))

	(n1  (make-wire))
	(n2  (make-wire))
	(pc  (make-wire))

	(neck  (make-wire))
	(an1   (make-wire))
	(an2   (make-wire))
	(body-len (make-constant-wire 5))
	(arm-len1 (make-constant-wire 4))
	(arm-len2 (make-constant-wire 4))
	(shoulder-width/2 (make-constant-wire 2))

	(dir (make-wire))
	(vec-f (make-wire))
	(vec-u (make-constant-wire (vector4f 0 1 0)))
	(width/2 (make-constant-wire 1))
	(len1 (make-constant-wire 5))
	(len2 (make-constant-wire 5)))

    ((leg-1 'connect) ph1 pf1 len1 len2 n1 pk1)
    ((leg-2 'connect) ph2 pf2 len1 len2 n2 pk2)
    ((hip-joint 'connect) pc width/2 width/2 vec-u vec-f ph1 ph2 n1 n2)
    ((forward-dir 'connect) pf1 pf2 dir vec-u vec-f)
    ((feet-hip 'connect) pf1 pf2 vec-u vec-f len1 len2 pc)

    ;; 上半身はかなり適当。
    ((body 'connect) pc vec-u body-len neck)
    ((shoulder 'connect) neck shoulder-width/2 shoulder-width/2 vec-u vec-f ps1 ps2 an1 an2)
    ((arm-1 'connect) pw1 ps1 arm-len1 arm-len2 an1 pe1)
    ((arm-2 'connect) pw2 ps2 arm-len1 arm-len2 an2 pe2)

    (values ph1 ph2 pk1 pk2 pf1 pf2 n1 n2 dir
	    ps1 ps2 pe1 pe2 pw1 pw2 an1 an2
	    neck pc)
    ))


(define (make-leg p1 p2 p3 n)
  (lambda ()
    (when n
      (let ((n-dir (vector4f-normalize n)))
	(gl-material GL_FRONT GL_DIFFUSE '#f32(1.0 1.0 0.0 0.0))
	(gl-material GL_FRONT GL_AMBIENT '#f32(0.3 0.3 0.0 0.0))
	(draw-stick p1 p2 n-dir)
	(draw-stick p2 p3 n-dir)
	))
  ))

(define (leg-stream p1 p2 p3 n)
  (cons-stream
   (make-leg (p1 'get) (p2 'get) (p3 'get) (n 'get))
   (leg-stream p1 p2 p3 n)))

(define (monitor-constraint name wire)
  (lambda (m)
    (if (eq? m 'update)
	(print #`",name = ,(wire 'get)"))))

(define (dance-step-stream-1 step)
  (cons-stream (vector4f 0 0 step)
	       (cons-stream (vector4f 0 0 (- step))
			    (cons-stream (vector4f 0 0 0)
					 (dance-step-stream-1 step)))))

(define (dance-step-stream-2 step)
  (cons-stream (vector4f 0 0 0)
	       (cons-stream (vector4f 0 0 (- step))
			    (cons-stream (vector4f 0 0 step)
					 (dance-step-stream-2 step)))))

(define puppet-stand-by #f)

(let-values (((ph1 ph2 pk1 pk2 pf1 pf2 n1 n2 dir
		   ps1 ps2 pe1 pe2 pw1 pw2 an1 an2
		   neck pc)
	      (make-legs))
	     ((2-ph1 2-ph2 2-pk1 2-pk2 2-pf1 2-pf2 2-n1 2-n2 2-dir
		     2-ps1 2-ps2 2-pe1 2-pe2 2-pw1 2-pw2 2-an1 2-an2
		     2-neck 2-pc)
	      (make-legs))
	     ((uniq-1 uniq-2 hand-1 hand-2 len1)
	      (values (unique-constraint)
		      (unique-constraint)
		      (direction-and-length-constraint)
		      (direction-and-length-constraint)
		      (make-constant-wire 7)
		      ))
	     )
  (define (walk-stream footprint-stream-1 footprint-stream-2 vec-dir period)
    (cons-stream
     (cons 0 (lambda ()
	       (wires-set-value! (pf1 (stream-car footprint-stream-1))
				 (pf2 (stream-car footprint-stream-2))
				 (dir vec-dir)
				 )))
     (stream-map (lambda (t anim p1 p2 p3 p4)
		   (cons t (lambda ()
			     (draw-floor)
			     (anim)
			     (p1) (p2)
			     (p3) (p4)
			     (draw-body (neck 'get) (pc 'get) (an1 'get))
			     )))
		 (stream-enumerate-interval 0 2000)
		 (make-walking-stream pf1 pf2
				      footprint-stream-1
				      footprint-stream-2
				      (vector4f 0 1 0) period)
		 (leg-stream ph1 pk1 pf1 n1)
		 (leg-stream ph2 pk2 pf2 n2)
		 (leg-stream ps1 pe1 pw1 an1)
		 (leg-stream ps2 pe2 pw2 an2)
		 )
     ))
  (define (walk-stream-2 footprint-stream-1 footprint-stream-2 vec-dir period
			 2-footprint-stream-1 2-footprint-stream-2 2-vec-dir 2-period)
    (cons-stream
     (cons 0 (lambda ()
	       ((hand-1 'connect) ps1 dir len1 pw1)
	       ((hand-2 'connect) ps2 dir len1 pw2)
	       (wires-set-value! (pf1 (stream-car footprint-stream-1))
				 (pf2 (stream-car footprint-stream-2))
				 (dir vec-dir)
				 (2-pf1 (stream-car 2-footprint-stream-1))
				 (2-pf2 (stream-car 2-footprint-stream-2))
				 (2-dir 2-vec-dir)
				 )))
     (stream-map (lambda (t anim 2-anim p1 p2 p3 p4 2-p1 2-p2 2-p3 2-p4)
		   (cons t (lambda ()
			     (draw-floor)
			     (anim)
			     (2-anim)
			     (p1) (p2)
			     (p3) (p4)
			     (2-p1) (2-p2)
			     (2-p3) (2-p4)
			     (draw-body (neck 'get) (pc 'get) (an1 'get))
			     (draw-body (2-neck 'get) (2-pc 'get) (2-an1 'get))
			     )))
		 (stream-enumerate-interval 0 2000)
		 (make-walking-stream pf1 pf2
				      footprint-stream-1
				      footprint-stream-2
				      (vector4f 0 1 0) period)
		 (make-walking-stream 2-pf1 2-pf2
				      2-footprint-stream-1
				      2-footprint-stream-2
				      (vector4f 0 1 0) 2-period)
		 (leg-stream ph1 pk1 pf1 n1)
		 (leg-stream ph2 pk2 pf2 n2)
		 (leg-stream ps1 pe1 pw1 an1)
		 (leg-stream ps2 pe2 pw2 an2)

		 (leg-stream 2-ph1 2-pk1 2-pf1 2-n1)
		 (leg-stream 2-ph2 2-pk2 2-pf2 2-n2)
		 (leg-stream 2-ps1 2-pe1 2-pw1 2-an1)
		 (leg-stream 2-ps2 2-pe2 2-pw2 2-an2)

		 )
     ))

  (set! puppet-stand-by
	(lambda ()
  (mephisto-add-cut! (cons-stream
	     (cons 0 (lambda ()
		       (gl-load-identity)
		       (glu-look-at 5 10 10 0 5 -10 0 1 0)
		       ))
	     the-empty-stream
	     ))

  (mephisto-add-cut! (walk-stream
	     (make-straight-footprint-stream (point4f -30 0 -10) (vector4f 18 0 0))
	     (make-straight-footprint-stream (point4f -21 0 -8) (vector4f 18 0 0))
	     (vector4f 1 0 0) 20))

  (mephisto-add-cut! (walk-stream
	     (make-straight-footprint-stream (point4f -26 -1.5 -10) (vector4f 5 3 0))
	     (make-straight-footprint-stream (point4f -19 0 -10) (vector4f 5 3 0))
	     (vector4f 1 0 -1) 20))
  (mephisto-add-cut! (walk-stream
	     (make-straight-footprint-stream (point4f -26 0 -10) (vector4f 10 0 0))
	     (make-straight-footprint-stream (point4f -19 0 -10) (vector4f 10 0 0))
	     (vector4f -1 0 -1) 30))

  (mephisto-add-cut! (walk-stream
	     (make-footprint-stream (point4f -2 0 -10) (dance-step-stream-1 5))
	     (make-footprint-stream (point4f 0 0 -10) (dance-step-stream-2 5))
	     (vector4f 0 0 -1) 20))

  (mephisto-add-cut! (cons-stream
	     (cons 0 (lambda ()
		       (gl-load-identity)
		       (glu-look-at -10 10 10 0 5 0 0 1 0)
		       ((uniq-1 'connect) ps1 2-pw2)
		       ((uniq-2 'connect) 2-ph1 pw2)
		       ))
	     the-empty-stream
	     ))

  (mephisto-add-cut! (walk-stream-2
	     (make-footprint-stream (point4f -2 0 -8) (dance-step-stream-1 5))
	     (make-footprint-stream (point4f 0 0 -8) (dance-step-stream-2 5))
	     (vector4f 0 0 -1) 10
	     (make-footprint-stream (point4f 0 0 -14) (stream-thin 2 (dance-step-stream-1 5)))
	     (make-footprint-stream (point4f -2 0 -14) (dance-step-stream-2 5))
	     (vector4f 0 0 1) 10
	     ))
  ))
  )

(define (main args)
  (puppet-stand-by)

  (glut-init args)
  (glut-init-display-mode (logior GLUT_DOUBLE GLUT_DEPTH GLUT_RGB))

  (glut-init-window-size 1024 768)
  (glut-init-window-position 0 0)
  (glut-create-window "MEPHISTO DEMO")
  (mephisto-init!)
  (glut-main-loop)
  0)
