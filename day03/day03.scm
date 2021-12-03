(import (chicken io)
	(chicken bitwise)
	srfi-1
	srfi-13)

(define (binary-string->list s)
  (map (lambda (c) (- (char->integer c) (char->integer #\0)))
       (string->list s)))

(define (list->binary-string l)
  (string-concatenate (map number->string l)))

(define (read-input #!optional (port (current-input-port)))
  (map binary-string->list (read-lines port)))

(define (part1 l)
  (let* ((n (/ (length l) 2))
	 (counts (apply map + l))
	 (most-common (map (lambda (x) (if (> x n) 1 0)) counts))
	 (gamma (string->number (list->binary-string most-common) 2))
	 (epsilon (string->number (list->binary-string (map (lambda (d) (- 1 d)) most-common)) 2)))
    (* gamma epsilon)))

(define (part2 l)
  l)

(let ((in (read-input)))
  (print (part1 in))
  (print (part2 in)))
