(import (chicken io)
	srfi-1
	srfi-42
	srfi-113
	srfi-128
	srfi-152
	matchable)

(define-record point x y)

(define (read-dot s)
  (map string->number (string-split s ",")))

(define (read-fold s)
  (let ((n (string->number (string-drop s 13))))
    (match (string-ref s 11)
      (#\x (lambda (x y) (if (< x n) (list x y) (list (+ n (- n x)) y))))
      (#\y (lambda (x y) (if (< y n) (list x y) (list x (+ n (- n y))))))
      (_ (error "invalid fold axis")))))

(define (read-input #!optional (port (current-input-port)))
  (let ((in (read-lines port))
	(string-not-null? (lambda (s) (not (string-null? s)))))
    (values
     (map read-dot (take-while string-not-null? in))
     (map read-fold (cdr (drop-while string-not-null? in))))))

(define (apply-fold fold-fn dots)
  (set->list (list->set (make-default-comparator) (map (lambda (l) (apply fold-fn l)) dots))))

(define (part1 dots folds)
  (length (apply-fold (car folds) dots)))

(define (part2 dots folds)
  (if (null? folds)
      dots
      (part2 (apply-fold (car folds) dots) (cdr folds))))

(define (print-dots dots)
  (let ((xmax (+ 1 (apply max (map car dots))))
	(ymax (+ 1 (apply max (map cadr dots)))))
    (do-ec (:range y ymax)
	   (begin
	     (do-ec (:range x xmax)
		    (if (member (list x y) dots)
			(display "#")
			(display ".")))
	     (display "\n")))))

(let-values (((dots folds) (read-input)))
  (print (part1 dots folds))
  (print-dots (part2 dots folds)))
