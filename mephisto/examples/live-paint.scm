; Live Paint?
; $Id: live-paint.scm,v 1.5 2006/08/21 07:31:58 torus Exp $

(add-load-path "../lib")
(add-load-path ".")

(use srfi-1)
(use srfi-11)
(use util.stream)

(use gauche.sequence)
(use gauche.collection)
(use gauche.threads)

(use gl)
(use gl.glut)
(use gl.math3d)

(use mephisto.constraint)
(use mephisto.stream)
(use mephisto.animation)
(use mephisto.animation.util)
(use mephisto.model)
(use mephisto.picture)

(load "paint-primitives")


(define *new-streams* '(x))

(define (marge-live-streams streams)
    (let1 new-streams (append (cdr *new-streams*) streams)
      (set-cdr! *new-streams* '())
      (let ((next-procs (map stream-car new-streams))
	    (next-streams (remove (lambda (s) (stream-null? s))
				  (map stream-cdr new-streams))))
	(stream-cons next-procs
		     (marge-live-streams next-streams)))))

(define (make-live-stream)
  (stream-map (lambda (t lst)
		(cons t
		      (lambda ()
			(for-each (lambda (x) (x))
				  lst))))
	      (make-time-stream)
	      (marge-live-streams '())))

(define (procedure-stream proc)
  (cons-stream proc (procedure-stream proc)))

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

  (glut-main-loop)
  0)

(define (interactive-main args)
  (mephisto-add-cut! (make-live-stream))

  (glut-init args)
  (glut-init-display-mode (logior GLUT_DOUBLE GLUT_DEPTH GLUT_RGB))

  (glut-init-window-size 800 600)
  (glut-init-window-position 0 0)
  (glut-create-window "MEPHISTO DEMO")
  (mephisto-init!)

  ;; from WiLiKi
  (thread-start! (make-thread (lambda () (read-eval-print-loop))))
  (while #t
    (guard (e (else (report-error e)))
	   (glut-main-loop)))
  0)

(define (append-painter-streams! . strms)
  (append! *new-streams* strms)
  'ok)

;;
;; EXAMPLES
;;

;; Please see live-*.scm
