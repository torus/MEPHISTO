; -*- coding: euc-jp -*-
;; Simple sample with live-paint
;; $Id: live-sample.scm,v 1.1 2006/06/04 09:17:21 torus Exp $

(load "./live-paint")

;; If you are in the Emacs, run the main procedure now to get into the
;; interactive environment.

;(main '())

(define-wires count ux uy n o)

;; フレームカウンタの設定。
(append-painter-streams!
 (stream-map (lambda (x)
	       (lambda ()
		 (wires-set-value! (count x))))
	     (make-time-stream)))

;; 適当に値を入れておく。
(wires-set-value! (ux (vector4f 1 0 0))
		  (uy (vector4f 0 1 0))
		  (n 5)
		  (o (point4f 0 0 0)))

;; 正多角形の形に cube を配置する。
(append-painter-streams!
 (procedure-stream
  (lambda/constraint
   (ux uy n o)
   (let ((radius 10))
     (let loop ((i n))
       (unless (zero? i)
	 (let ((angle (/ (* 2 PI i) n)))
	   ((move-to (point4f-add (point4f-add o (vector4f-scale ux (* (cos angle) radius)))
				  (vector4f-scale uy (* (sin angle) radius)))
		     cube))
	   (loop (- i 1)))))))))

;; 正多角形の基底ベクトルを count によって変化させる。
(attach-constraint!
 (count => ux)
 (let1 angle (/ count 10)
   (vector4f-add (vector4f-scale (vector4f 1 0 0) (cos angle))
		 (vector4f-scale (vector4f 0 0 1) (sin angle)))))

;; 上の制約が動作するように、いったんリセット。
(wire-reset! ux)

(attach-constraint!
 (count => uy)
 (let1 angle (/ count 30)
   (vector4f-add (vector4f-scale (vector4f 0 1 0) (cos angle))
		 (vector4f-scale (vector4f 0 0 1) (sin angle)))))

(wire-reset! uy)

;; 多角形の角の数を変えてみる。
(wire-set-value! n 30)

;; 角の数を count にしたがって変化させる。
(attach-constraint!
 (count => n)
 (remainder (quotient count 30) 20))

(wire-reset! n)

;; 色を付けてみる。
(define-wires r g b)

(append-painter-streams!
 (procedure-stream
  (lambda/constraint
   (r g b)
   ((paint-color (lambda ()) r g b)))))

(wires-set-value! (r 0)
		  (g 1)
		  (b 1))

;; 色相で色を指定できるようにする。
;; 0 <= hue < 3
;; rgb = (list r g b)
(define-wires rgb hue)

(attach-constraint!
 (hue => rgb)
 (cond ((< hue 1)
	(list (- 1 hue) hue 1))
       ((< hue 2)
	(list 1 (- hue 1) (- 2 hue)))
       ((< hue 3)
	(list (- 3 hue) 1 (- hue 2)))
       (else
	#f)))

(attach-constraint!
 (rgb => r)
 (ref rgb 0))

(attach-constraint!
 (rgb => g)
 (ref rgb 1))

(attach-constraint!
 (rgb => b)
 (ref rgb 2))

;; 適当な値を入れてみる。
(wires-set-value! (hue 1.2))

(wire-reset! r)
(wire-reset! g)
(wire-reset! b)

;; 色相を count にしたがって変化させる。
(attach-constraint!
 (count => hue)
 (/ (remainder count 300) 100))

