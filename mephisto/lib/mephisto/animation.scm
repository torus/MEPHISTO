;; MEPHISTO animation framework
;; $Id: animation.scm,v 1.15 2006/06/01 06:36:56 torus Exp $

;; Some OpenGL related parameters are hard coded

(define-module mephisto.animation
  (use gl)
  (use gl.glut)
  (use util.stream)
  (use gauche.uvector)
  (use mephisto.stream)
  (use gauche.time)

  (use mephisto.animation.util)

  (export mephisto-init! mephisto-main mephisto-add-cut!)
  )

(select-module mephisto.animation)

(define (mephisto-init!)
  (set-main-stream
   (stream-concatenate (apply stream *stream-list*)))

  (gl-clear-color 0.0 0.0 0.0 0.0)
  (gl-shade-model GL_FLAT)

  (gl-light GL_LIGHT0 GL_POSITION '#f32(0.0 10.0 20.0 0.0))
  (gl-light GL_LIGHT0 GL_DIFFUSE '#f32(1.0 1.0 1.0 1.0))
  (gl-light GL_LIGHT0 GL_AMBIENT '#f32(1.0 1.0 1.0 1.0))
  (gl-enable GL_LIGHTING)
  (gl-enable GL_LIGHT0)

  (gl-enable GL_DEPTH_TEST)
  (gl-enable GL_NORMALIZE)

  (gl-load-identity)
  (glu-look-at 0.0 0.0 15.0 0.0 0.0 0.0 0.0 1.0 0.0)

  (glut-display-func (make-disp-func *main-stream*))
  (glut-keyboard-func keyboard)
  )

(define start-time (sys-time))
(define *current-frame* 0)
(define *time-counter* (make <real-time-counter>))
(define (make-disp-func stream)
  (let ((cur-time (car (stream-car stream))))
    (lambda ()
      (define (iter stream t)
	(if (stream-null? stream)
	    (let ((time (sys-difftime (sys-time) start-time)))
	      (print #`",*current-frame* frames / ,time secs")
	      (print #`",(/ *current-frame* time) fps")

	      (exit 0))
	    (if (not (eq? (car (stream-car stream)) t))
		(let ((func (make-disp-func stream)))
		  (glut-swap-buffers)
		  (glut-idle-func func)
		  (glut-display-func func))
		(begin
		  ((cdr (stream-car stream)))
		  (iter (stream-cdr stream) t)))))
      (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

      (inc! *current-frame*)
      (iter stream cur-time)

      (time-counter-stop! *time-counter*)
      ;; wait for 0.04 seconds
      (let ((time (- 0.04 (time-counter-value *time-counter*))))
	(when (> time 0)
	      (sys-nanosleep (* time 1000 1000 1000)))
	(time-counter-reset! *time-counter*)
	(time-counter-start! *time-counter*))
      )))

(define (reshape w h)
  (gl-viewport 0 0 w h)
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  (gl-frustum -2.0 2.0 -1.5 1.5 1.5 30.0)
  (gl-matrix-mode GL_MODELVIEW)
  )

(define (keyboard key x y)
  (cond ((= key 27) (exit 0))
	((or (= key 78) (= key 110))	; `N' or `n' key
	 (set-cut-event))
	(else #f)))

(define (mouse fn . rest)
  (cond ((= fn GLUT_LEFT_BUTTON)
	 (set-cut-event))
	(else #f)))

(define *main-stream* the-empty-stream)

(define (set-main-stream s)
  (set! *main-stream* s))

(define (mephisto-main args)
  (glut-init args)
  (glut-init-display-mode (logior GLUT_DOUBLE GLUT_DEPTH GLUT_RGB))

  (glut-init-window-size 320 240)
  (glut-init-window-position 0 0)
  (glut-create-window "MEPHISTO")
  (mephisto-init!)
  (glut-keyboard-func keyboard)

  (time-counter-start! *time-counter*)

  (glut-main-loop)
  0)


;;;;;;

(define (stream-delay stream n)
  (stream-map (lambda (f)
		(cons (+ (car f) n)
		      (cdr f)))
	      stream))

(define (stream-merge s1 . rest)
  (if (null? rest)
      s1
      (apply stream-merge (cons (merge-weighted s1 (car rest) car)
				(cdr rest)))))

;;;;;;;;;;;

(define *stream-list* '())

(define *cut-event* #f)
(define (set-cut-event)
  (set! *cut-event* #t))

(define (make-cutter-stream)
  (if *cut-event*
      the-empty-stream
      (cons-stream 1 (make-cutter-stream))))

(define (mephisto-add-cut! stream)
  (set! *stream-list*
	(append *stream-list*
		(list (stream-map (lambda (cutter painter)
				    (when *cut-event* (set! *cut-event* #f))
				    painter)
				  (make-cutter-stream)
				  stream)))))


(provide "mephisto.animation")
