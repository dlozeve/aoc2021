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

(define (o2-rating numbers i)
  (if (= 1 (length numbers))
      (car numbers)
      (let* ((digits (map (lambda (l) (list-ref l i)) numbers))
	     (most-common (if (>= (apply + digits) (/ (length numbers) 2)) 1 0))
	     (new-numbers (filter (lambda (n) (= (list-ref n i) most-common)) numbers)))
	(o2-rating new-numbers (+ 1 i)))))

(define (co2-rating numbers i)
  (if (= 1 (length numbers))
      (car numbers)
      (let* ((digits (map (lambda (l) (list-ref l i)) numbers))
	     (least-common (if (>= (apply + digits) (/ (length numbers) 2)) 0 1))
	     (new-numbers (filter (lambda (n) (= (list-ref n i) least-common)) numbers)))
	(co2-rating new-numbers (+ 1 i)))))

(define (part2 l)
  (let ((o2 (string->number (list->binary-string (o2-rating l 0)) 2))
	(co2 (string->number (list->binary-string (co2-rating l 0)) 2)))
    (* o2 co2)))

(let ((in (read-input)))
  (print (part1 in))
  (print (part2 in)))
