(import (chicken io)
	srfi-1
	srfi-42
	srfi-69
	srfi-152
	matchable)

(define (read-input #!optional (port (current-input-port)))
  (define (parse-entry s)
    (map (lambda (x) (map string->number (string-split x ",")))
	 (string-split s " -> ")))
  (map parse-entry (read-lines port)))

(define (horizontal? line)
  (match-let ((((x1 y1) (x2 y2)) line))
    (= x1 x2)))

(define (vertical? line)
  (match-let ((((x1 y1) (x2 y2)) line))
    (= y1 y2)))

(define (diagonal? line)
  (and (not (horizontal? line))
       (not (vertical? line))))

(define (range x1 x2)
  (iota (+ 1 (abs (- x2 x1)))
	x1
	(if (>= x2 x1) 1 -1)))

(define (all-points lines #!optional (diagonals #f))
  (define h (make-hash-table #:initial 0))
  (do-ec (: line lines)
	 (or (horizontal? line) (vertical? line))
	 (match-let ((((x1 y1) (x2 y2)) line))
	   (list-ec (: x (range x1 x2))
		    (: y (range y1 y2))
		    (hash-table-update! h (list x y) add1))))
  (when diagonals
    (do-ec (: line lines)
	   (if (diagonal? line))
	   (match-let ((((x1 y1) (x2 y2)) line))
	     (list-ec (:parallel (: x (range x1 x2))
				 (: y (range y1 y2)))
		      (hash-table-update! h (list x y) add1)))))
  h)

(define (part12 lines diagonals)
  (length
   (filter (lambda (n) (> n 1))
	   (hash-table-values (all-points lines diagonals)))))

(let ((lines (read-input)))
  (print (part12 lines #f))
  (print (part12 lines #t)))
