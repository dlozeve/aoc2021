(cond-expand
  (r7rs)
  (chicken (import r7rs)))

(import (scheme base)
	(scheme write)
	(scheme cxr)
	(srfi 1))

(define (read-input)
  (let lp ((lines '()))
    (if (and (not (null? lines)) (eof-object? (car lines)))
	(map string->number (reverse (cdr lines)))
	(lp (cons (read-line (current-input-port)) lines)))))

(define (part1 l) (count < l (cdr l)))

(define (part2 l) (count < l (cdddr l)))

(let ((in (read-input)))
  (write (part1 in))
  (newline)
  (write (part2 in))
  (newline))

