; testing and examples for mephisto.constraint module.
; $Id: constraint-test.scm,v 1.2 2006/08/25 23:40:23 torus Exp $

(add-load-path "../lib")

(use mephisto.constraint)

(use gauche.test)

;;;;;;;;;;;;;;;;;

(define (unique-constraint)
  (make-constraint
   (a b)
   ((a => b) a)
   ))

(define (make-multiplier)
  (make-constraint
   (m1 m2 product)
   ((m1 m2 => product) (* m1 m2))
   ((m1 product => m2) (/ product m1))
   ((m2 product => m1) (/ product m2))
   ))

(define (make-adder)
  (make-constraint
   (a1 a2 sum)
   ((a1 a2 => sum) (+ a1 a2))
   ((a1 sum => a2) (- sum a1))
   ((a2 sum => a1) (- sum a2))
   ))

(define mul1 (make-multiplier))
(define mul2 (make-multiplier))
(define adder (make-adder))

(define u (make-wire))
(define v (make-wire))
(define w (make-constant-wire 9))
(define x (make-constant-wire 5))
(define y (make-constant-wire 32))

(define c (make-wire))
(define f (make-wire))

((mul1 'connect) c w u)
((mul2 'connect) v x u)
((adder 'connect) v y f)

(test-start "mephisto.constraint")
(test-module 'mephisto.constraint)

((c 'set) 25)
(test* "c->f" 77 (f 'get))

(wires-set-value! (f 212))

(test* "f->c" 100 (c 'get))

;;;;

(define-wires a b c)

(((make-constraint (a b c)
		   ((a b => c)
		    (* a (+ b 1))))
  'connect) a b c)

(wires-set-value! (a 2) (b 3))
(test* "(* a (+ b 1))" 8 (c 'get))


;;;

(test-start "hysteresis")

(define-wires x y)
; (define-monitored-wires x y)

(define h (make-hysteresis-wire 'h))

(attach-constraint! (x => h) x)

(attach-constraint! (y => x) (/ (+ y (h 'previous-value)) 2))


(wire-set-value! x 3)
(test* "hysteresis 1" 3 (wire-get-value h))

(wire-set-value! x 5)
(test* "hysteresis 2" 5 (wire-get-value h))

(wire-reset! x)
(wire-set-value! y 15)

(test* "hysteresis 3" 15 (wire-get-value y))
(test* "hysteresis 4" 10 (wire-get-value x))

(wire-set-value! y 20)
(test* "hysteresis 5" 15 (wire-get-value x))

(wire-set-value! x 0)
(wire-set-value! y 20)
(test* "hysteresis 6" 10 (wire-get-value x))


;; not working...
;; (define-wires sw1 sw2 sw3)
;; (attach-constraint! (sw1 => sw2) #f)
;; (attach-constraint! (sw1 => sw3) #f)
;; (attach-constraint! (sw2 => sw1) #f)
;; (attach-constraint! (sw2 => sw3) #f)
;; (attach-constraint! (sw3 => sw1) #f)
;; (attach-constraint! (sw3 => sw2) #f)

;; (wire-set-value! sw1 #t)
;; (test* "sw1" #t (wire-get-value sw1))
;; (test* "sw2" #f (wire-get-value sw2))
;; (test* "sw3" #f (wire-get-value sw3))

;; (wire-set-value! sw2 #t)
;; (test* "sw2" #t (wire-get-value sw2))
;; (test* "sw1" #f (wire-get-value sw1))
;; (test* "sw3" #f (wire-get-value sw3))

(test-end)
