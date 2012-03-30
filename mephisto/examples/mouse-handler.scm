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

(define (add-mouse-handler! proc)
  (set! *mouse-handle* (append! *mouse-handle* (list proc))))

;;   (glut-mouse-func mouse)

;;    (stream-map
;;     (lambda (i)
;;       (lambda ()
;; 	(when (and i (eq? (cadr i) GLUT_UP) (pair? *mouse-handle*))
;; 	  ((car *mouse-handle*) i)
;; 	  (set! *mouse-handle* (cdr *mouse-handle*)))))
;;     (make-input-stream))
