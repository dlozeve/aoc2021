(import (chicken io)
	srfi-1
	srfi-152
	matchable)

(define (read-input #!optional (port (current-input-port)))
  (map (lambda (s) (let ((l (string-split s " ")))
		     (list (string->symbol (car l))
			   (string->number (cadr l)))))
       (read-lines port)))

(define (move steps pos depth)
  (match steps
    (() (values pos depth))
    ((('down n) . rest) (move rest pos (+ depth n)))
    ((('up n) . rest) (move rest pos (- depth n)))
    ((('forward n) . rest) (move rest (+ pos n) depth))
    (_ (error "unknown instruction"))))

(define (part1 l)
  (let-values (((pos depth) (move l 0 0)))
    (* pos depth)))

(define (move2 steps pos depth aim)
  (match steps
    (() (values pos depth))
    ((('down n) . rest) (move2 rest pos depth (+ aim n)))
    ((('up n) . rest) (move2 rest pos depth (- aim n)))
    ((('forward n) . rest) (move2 rest (+ pos n) (+ depth (* aim n)) aim))
    (_ (error "unknown instruction"))))

(define (part2 l)
  (let-values (((pos depth) (move2 l 0 0 0)))
    (* pos depth)))

(let ((in (read-input)))
  (print (part1 in))
  (print (part2 in)))
