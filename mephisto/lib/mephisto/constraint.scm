;; MEPHISTO constraint module
;; $Id: constraint.scm,v 1.22 2006/06/01 06:36:56 torus Exp $

(define-module mephisto.constraint
  (export make-wire make-constant-wire make-constraint
	  wire-reset! wires-reset! wire-active? wire-set-value! wires-set-value! wire-get-value
	  define-wires let-wires lambda/constraint attach-constraint!
	  make-hysteresis-wire
	  make-monitored-wire define-monitored-wires)
  )

(select-module mephisto.constraint)

(define (wire-set-value! wire val)
  (wire 'reset)
  ((wire 'set) val))

(define (wire-get-value wire)
  (wire 'get))

(define (wire-reset! wire)
  (wire 'reset))

(define (wires-reset! . wires)
  (for-each (cut wire-reset! <>) wires))

(define (wire-active? wire)
  (wire 'active))

(define-syntax wires-set-value!
  (syntax-rules ()
    ((_ clause ...)
     (wires-set-value!-sub (clause ...) () ()))
    ))

(define-syntax wires-set-value!-sub
  (syntax-rules ()
    ((_ () (wire ...) (val ...))
     (begin (wire 'reset) ...
	    ((wire 'set) val) ...)
     )
    ((_ ((w1 v1) clause ...) (wire ...) (val ...))
     (wires-set-value!-sub (clause ...) (w1 wire ...) (v1 val ...)))
    ))

(define (make-wire)
  (let ((active #f)
	(val #f)
	(ends '()))
    (define (connect! end)
      (set! ends (cons end ends)))

    (define (set-value! v)
      (when v
	    (set! val v)
	    (set! active #t)
	    (for-each (lambda (x) (x 'update)) ends)))

    (define (reset!)
      (set! active #f)
      (set! val #f)
      (for-each (lambda (x) (x 'update)) ends))

    (lambda (m)
      (cond ((eq? m 'connect) connect!)
	    ((eq? m 'active) active)
	    ((eq? m 'get-ends) ends)
	    ((eq? m 'get) val)
	    ((eq? m 'set) set-value!)
	    ((eq? m 'reset) (reset!))
	    (else (error "unknown message type -- wire" m))
	    )
      )))

(define (make-constant-wire v)
  (let ((val v)
	(ends '()))
    (define (connect! end)
      (set! ends (cons end ends)))

    (define (set-value! v)
      #f)

    (lambda (m)
      (cond ((eq? m 'connect) connect!)
	    ((eq? m 'active) #t)
	    ((eq? m 'get-ends) ends)
	    ((eq? m 'get) val)
	    ((eq? m 'set) set-value!)
	    ((eq? m 'reset) #f)
	    (else (error "unknown message type -- constant-wire" m))
	    ))))

(define-syntax make-constraint
  (syntax-rules ()
    ((_ (wire ...) body ...)
     (make-constraint-sub-1 () (wire ...) (wire ...) body ...))))

(define-syntax make-constraint-sub-1
  (syntax-rules ()
    ((_ (tmp ...) (w1 w2 ...) (wire ...) body ...)
     (make-constraint-sub-1 (tmp ... tmp1) (w2 ...) (wire ...) body ...))
    ((_ (tmp ...) () (wire ...) body ...)
     (make-constraint-sub-2 (tmp ...) (wire ...)
			    body ...))))

(define-syntax make-constraint-sub-2
  (syntax-rules ()
    ((_ (tmp ...) (wire ...) clause ...)
     (let ((active #t)
	   (proc #f)
	   (wire #f) ...)
       (define (connect! tmp ...)
	 (set! wire tmp) ...
	 ((tmp 'connect) proc) ...)
       (define (wake-up!)
	 (set! active #t))
       (define (sleep!)
	 (set! active #f))
       (define (update!)
	 (make-constraint-sub-2.5 active wake-up! sleep! clause ...))
       (set! proc
	     (lambda (m)
	       (cond ((eq? m 'connect) connect!)
		     ((eq? m 'update) (update!))
		     ((eq? m 'wake-up) (wake-up!))
		     ((eq? m 'sleep) (sleep!))
		     (else
		      (error "unknown message type -- constraint" m)))))
       proc
       ))))

(define-syntax make-constraint-sub-2.5
  (syntax-rules (=>)
    ((_ active wake-up! sleep! ((wire ...) expr) ...)
     (make-constraint-sub-2.5 (active wake-up! sleep!) (() (wire ...) expr) ...))
    ((_ tmp ((src ...) (=> var) expr) ...)
     (make-constraint-sub-2.5 tmp (var (src ...) expr) ...))
    ((_ tmp ((src ...) (w1 wire ...) expr) ...)
     (make-constraint-sub-2.5 tmp ((w1 src ...) (wire ...) expr) ...))

    ((_ (active wake-up! sleep!) (var (src ...) expr) ...)
     (if active
	 (cond ((all-sources-are-active? src ...)
		(make-constraint-sub-4 sleep! var (src ...) expr)) ...)
	 (begin
	   (if (not (all-sources-are-active? src ...))
	       (make-constraint-sub-5 wake-up! var)) ...)
	 ))
    ))

(define-syntax all-sources-are-active?
  (syntax-rules ()
    ((_ src ...)
     (and (src 'active) ...))
    ))

(define-syntax make-constraint-sub-4
  (syntax-rules ()
    ((_ sleep! var (src ...) expr)
     (unless (var 'active)
       (let1 val (let ((src (src 'get)) ...) expr)
	 (unless (eq? val #f)		; FIXME: It dose not work if val is a boolien value
	   (sleep!)
	   ((var 'set) val)))
	 #f))
    ))

(define (make-constraint-sub-5 wake-up! wire)
  (wake-up!)
  (wire 'reset))

(define-syntax define-wires
  (syntax-rules ()
    ((_ var ...)
     (begin
       (define var (make-wire)) ...)
     )))

(define-syntax let-wires
  (syntax-rules ()
    ((_ (var ...) body ...)
     (let ((var (make-wire))...)
       body ...
       ))))

(define-syntax lambda/constraint
  (syntax-rules ()
    ((_ (wire ...) body ...)
     (lambda ()
       (when (and (wire-active? wire) ...)
	 (let ((wire (wire 'get)) ...)
	   body ...))
       ))))

(define-syntax attach-constraint!
  (syntax-rules (=>)
    ((_ wires expr)
     (attach-constraint! () wires expr))
    ((_ (src ...) (=> dest) expr)
     (begin (((make-constraint
	       (src ... dest)
	       ((src ... => dest)
		expr))
	      'connect) src ... dest)
	    (when (and (src 'active) ...)
	      (dest 'reset))
	    'attach-constraint!-ok))
    ((_ (src ...) (src1 src2 ...) expr)
     (attach-constraint! (src ... src1) (src2 ...) expr))
    ))

;; `Hysteresis' wire that has memory of previous status.  When this
;; wire is reset, internal value is set to the previous value.

;; An example is in test/contraint-test.scm.

(define (make-hysteresis-wire name)
  (let ((w (make-wire))
	(val #f))
    (lambda (m)
      (cond ((eq? m 'reset) (set! val (w 'get)) (w 'reset))
	    ((eq? m 'previous-value) (or (w 'get) val))
	    (else (w m))))))

;; Monitored wire for debugging

(define (make-monitored-wire name)
  (let ((w (make-wire)))
    (lambda (m)
      (let ((ret (w m)))
	(print #`",(list name m) => ,ret")
	ret))))

(define-syntax define-monitored-wires
  (syntax-rules ()
    ((_ name ...)
     (begin (define name (make-monitored-wire 'name)) ...))))


(provide "mephisto.constraint")
