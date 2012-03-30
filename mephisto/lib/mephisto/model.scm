;; Simple model data importer
;; $Id: model.scm,v 1.4 2006/06/01 06:36:56 torus Exp $

(define-module mephisto.model
  (use srfi-13)

  (use util.match)
  (use gauche.charconv)
  (use gauche.sequence)
  (use gl)
  (use gl.glut)

  (export read-obj-model)
)

(select-module mephisto.model)

(define-constant *vector-size* 10000)

(define (read-obj-model path)
  (let ((vertices (cons 0 (make-vector *vector-size*)))
	(normals (cons 0 (make-vector *vector-size*)))
	(uvs (cons 0 (make-vector *vector-size*)))
	(polygons (cons 0 (make-vector *vector-size*)))
	)

    (define (add-vertex! v)
      (let1 i (car vertices)
	(set-car! vertices (+ i 1))
	(vector-set! (cdr vertices) i v)))

    (define (add-polygon! v)
      (let1 i (car polygons)
	(set-car! polygons (+ i 1))
	(vector-set! (cdr polygons) i v)))

    (define (add-normal! v)
      (let1 i (car normals)
	(set-car! normals (+ i 1))
	(vector-set! (cdr normals) i v)))

    (define (add-uv! v)
      (let1 i (car uvs)
	(set-car! uvs (+ i 1))
	(vector-set! (cdr uvs) i v)))

    (define (switch-group! g) #f)

    (with-input-from-file path
      (lambda ()
	(port-for-each
	 (lambda (line)
	   (let1 line (string-trim-right
		       (or (string-scan line #\# 'before) line))
	     (unless (string=? line "")
	       (match (string-tokenize line)
		      (("v" coord ...)
		       (add-vertex! (list->point4f (map obj-value->number coord))))
		      (("vn" coord ...)
		       (add-normal! (list->vector4f (map obj-value->number coord))))
		      (("vt" coord ...)
		       (add-uv! (map obj-value->number coord)))
		      (("f" vtx ...)
		       (add-polygon!
			(map (lambda (s)
			       (map (lambda (t) (obj-value->number t))
				    (string-split s #\/)))
			     vtx)
			))
		      (("g" group-name)
		       (switch-group! group-name))
		      ((sym args ...)
		       #f
		       )
		      ))
	     ))
	 read-line)) :encoding "shift_jis")

    (values
     (lambda ()
       (map (lambda (poly)
              (gl-begin GL_POLYGON)
              (map (lambda (p)
                     (when (number? (ref p 1))
                       (apply gl-tex-coord (ref (cdr uvs) (- (ref p 1) 1))))
                     (gl-normal (ref (cdr normals) (- (ref p 2) 1)))
                     (gl-vertex (ref (cdr vertices) (- (ref p 0) 1))))
                   poly)
              (gl-end))
            (vector->list (cdr polygons) 0 (car polygons)))
       )
     polygons vertices normals uvs
     )
    )
  )

(define (obj-value->number x)
  (with-input-from-string x read))

(provide "mephisto.model")
