;; primitive painters
;; $Id: paint-primitives.scm,v 1.5 2006/08/21 07:31:58 torus Exp $

(use math.const)

(define (vector4f-scale v s)
  (list->vector4f (map (lambda (x) (* x s)) v)))

(define (square x) (* x x))

(define (draw-stick p1 p2 n1)
  (when (and p1 p2 n1)
    ((stick-painter p1 p2 n1 1))
    ))

(define (stick-painter p1 p2 n1 width)
  (lambda ()
    (let ((vec (point4f-sub p2 p1))
	  (s 4))
      (let ((a (/ (* 2 pi) s))
	    (n2 (vector4f-normalize (vector4f-cross vec n1))))
	(gl-begin GL_QUAD_STRIP)
	(let loop ((t 0))
	  (if (> t s)
	      'done
	      (let1 r (vector4f-add (vector4f-scale n1 (* width (cos (* a (+ t 0.5)))))
				    (vector4f-scale n2 (* width (sin (* a (+ t 0.5))))))
		(let* ((vtx1 (point4f-add p1 r))
		       (vtx2 (point4f-add vtx1 vec)))
		  (gl-normal r)
		  (gl-vertex vtx2)
		  (gl-vertex vtx1)
		  (loop (+ t 1))))))
	(gl-end)))
    )
  )

(define (draw-body p1 p2 n1)
  (when (and p1 p2 n1)
    (let ((vec (point4f-sub p2 p1))
	  (s 4))
      (let ((a (/ (* 2 pi) s))
	    (n2 (vector4f-normalize (vector4f-cross vec n1))))
	(gl-begin GL_QUAD_STRIP)
	(let loop ((t 0))
	  (if (> t s)
	      'done
	      (let1 r (vector4f-add (vector4f-scale n1 (* 2 (cos (* a (+ t 0.5)))))
				    (vector4f-scale n2 (* 1 (sin (* a (+ t 0.5))))))
		(let* ((vtx1 (point4f-add p1 r))
		       (vtx2 (point4f-add vtx1 vec)))
		  (gl-normal (vector4f-normalize r))
		  (gl-vertex vtx2)
		  (gl-vertex vtx1)
		  (loop (+ t 1))))))
	(gl-end)))))

(define (draw-footprint p)
  (let ((s 6)
	(n1 (vector4f 0 0 1))
	(n2 (vector4f 1 0 0)))
    (let ((a (/ (* 2 pi) s)))
      (gl-material GL_FRONT GL_AMBIENT_AND_DIFFUSE '#f32(1.0 0.0 0.0 0.0))
      (gl-begin GL_POLYGON)
      (gl-normal (vector4f 0 1 0))
      (let loop ((t 0))
	(if (= t s)
	    'done
	    (let1 r (vector4f-add (vector4f-scale n1 (cos (* a t)))
				  (vector4f-scale n2 (sin (* a t))))
	      (gl-vertex (point4f-add p r))
	      (loop (+ t 1)))))
      (gl-end))))

(define (prepare-display-list painter)
  (let ((first #t)
	(display-list 0))
    (lambda ()
      (when first
	(set! display-list (gl-gen-lists 1))

	(gl-new-list display-list GL_COMPILE)

	(painter)

	(gl-end-list)
	(set! first #f))

      (gl-call-list display-list))
      ))

(define draw-floor
  (prepare-display-list
   (lambda ()
     (let1 color #f
       (let yloop ((y -30))
	 (if (> y 5)
	     'done
	     (begin
	       (let xloop ((x -30))
		 (if (> x 30)
		     'done
		     (begin
		       (gl-material GL_FRONT GL_AMBIENT_AND_DIFFUSE (if color
									'#f32(0.0 1.0 0.3 0.0)
									'#f32(0.4 1.0 0.3 0.0)))
		       (gl-begin GL_QUADS)
		       (gl-normal (vector4f 0 1 0))
		       (gl-vertex x -0.1 (+ y 3))
		       (gl-vertex (+ x 3) -0.1 (+ y 3))
		       (gl-vertex (+ x 3) -0.1 y)
		       (gl-vertex x -0.1 y)
		       (gl-end)
		       (set! color (not color))
		       (xloop (+ x 3)))))
	       (yloop (+ y 3))))))
	)))

(define (char-painter proc font str)
  (lambda ()
    (gl-push-matrix)
    (gl-normal 0 0 1)
    (gl-raster-pos 0 0)
    (for-each (lambda (c) (proc font (char->integer c)))
	      (if (string? str) (string->list str) str))
    (gl-pop-matrix)
    ))

(define (bitmap-char-painter font str)
  (char-painter glut-bitmap-character font str))

(define (stroke-char-painter font str)
  (char-painter glut-stroke-character font str))

;; img must be a list returned by read-sgi-image
(define (pixel-painter img)
  (lambda ()
    (gl-normal 0 0 1)
    (gl-raster-pos 0 0)
    (gl-draw-pixels (ref img 0) (ref img 1)
		    (case (ref img 2)
;; 		      ((1) GL_????)
		      ((3) GL_RGB)
		      ((4) GL_RGBA))
		    GL_UNSIGNED_BYTE (ref img 3))))
