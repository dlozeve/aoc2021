(import (chicken io)
	srfi-1
	srfi-42
	srfi-152)

(define (read-input #!optional (port (current-input-port)))
  (map string->number (string-split (read-line port) ",")))

(define (count-fishes in)
  (define c (make-list 9 0))
  (do-ec (: x in)
	 (set! (list-ref c x) (add1 (list-ref c x))))
  c)

(define (simulate counts days)
  (if (= 0 days)
      (take counts 9)
      (let ((n (car counts)))
	(set! (list-ref counts 7) (+ n (list-ref counts 7)))
	(simulate (cdr counts) (- days 1)))))

(define (total-fishes counts days)
  (apply + (take (simulate (apply circular-list counts) days) 9)))

(let* ((in '(3 4 3 1 2))
       (counts (count-fishes in)))
  (print "Test:")
  (print (total-fishes counts 18))
  (print (total-fishes counts 80))
  (print (total-fishes counts 256)))

(print)
(let* ((in (read-input))
       (counts (count-fishes in)))
  (print (total-fishes counts 80))
  (print (total-fishes counts 256)))
