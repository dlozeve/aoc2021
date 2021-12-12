(import (chicken io)
	(chicken sort)
	srfi-1
	srfi-152)

(define (read-input #!optional (port (current-input-port)))
  (map string->number (string-split (read-line port) ",")))

(define (median lst)
  (list-ref (sort lst <=) (/ (length lst) 2)))

(define (part1 in)
  (let ((target-pos (median in)))
    (apply + (map (lambda (x) (abs (- x target-pos))) in))))

(define (cost start end)
  (let ((dist (abs (- start end))))
    (/ (* dist (+ dist 1)) 2)))

(define (total-fuel target-pos in)
  (apply + (map (lambda (x) (cost target-pos x)) in)))

(define (part2 in)
  (apply min
	 (let ((min-in (apply min in))
	       (max-in (apply max in)))
	   (let lp ((pos min-in)
		    (fuels '()))
	     (if (> pos max-in)
		 fuels
		 (lp (+ 1 pos) (cons (total-fuel pos in) fuels)))))))

(let ((in (read-input)))
  (print (part1 in))
  (print (part2 in)))
