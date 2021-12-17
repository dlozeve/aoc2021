(import (chicken io)
	(chicken format)
	srfi-1
	srfi-42
	srfi-152
	matchable)

(define (read-input #!optional (port (current-input-port)))
  (map (lambda (s) (map string->number (string-split (string-drop s 2) "..")))
       (string-split (string-drop (read-line port) 13) ", ")))

(define-record state
  x y vx vy)

(set! (record-printer state)
  (lambda (st out)
    (fprintf out "#,(state ~S ~S ~S ~S)" (state-x st) (state-y st) (state-vx st) (state-vy st))))

(define (too-far? target-area st)
  (match-let ((((x1 x2) (y1 y2)) target-area)
	      (($ state x y vx vy) st))
    (< y (min y1 y2))))

(define (in-target? target-area st)
  (match-let ((((x1 x2) (y1 y2)) target-area)
	      (($ state x y vx vy) st))
    (and (<= x1 x x2) (<= y1 y y2))))

(define (sign x)
  (cond
   ((zero? x) 0)
   ((> x 0) 1)
   ((< x 0) -1)))

(define (step st)
  (match-let ((($ state x y vx vy) st))
    (make-state (+ x vx) (+ y vy)
		(- vx (sign vx)) (- vy 1))))

(define (simulate target-area vx vy)
  (let lp ((st (make-state 0 0 vx vy))
	   (prev-st (make-state 0 0 vx vy))
	   (max-height 0))
    (cond
     ((in-target? target-area st) (list st (max max-height (state-y st))))
     ((too-far? target-area st) (list prev-st max-height))
     (else (lp (step st) st (max max-height (state-y st)))))))

(define (find-max-height target-area)
  (list-ec (: vx 0 500)
	   (: vy -100 1000)
	   (:let st (simulate target-area vx vy))
	   (if (in-target? target-area (car st)))
	   (cadr st)))

(let* ((target-area (read-input))
       (max-heights (find-max-height target-area)))
  (print (apply max max-heights))
  (print (length max-heights)))
