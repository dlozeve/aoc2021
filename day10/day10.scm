(import (chicken io)
	(chicken sort)
	srfi-1
	matchable)

(define (read-input #!optional (port (current-input-port)))
  (read-lines port))

(define (check str)
  (let lp ((stack '())
	   (input (string->list str)))
    (match (list stack input)
      ((() ()) #t)
      (((#\( . srest) (#\) . irest)) (lp srest irest))
      (((#\[ . srest) (#\] . irest)) (lp srest irest))
      (((#\{ . srest) (#\} . irest)) (lp srest irest))
      (((#\< . srest) (#\> . irest)) (lp srest irest))
      ((_ (#\( . irest)) (lp (cons #\( stack) irest))
      ((_ (#\[ . irest)) (lp (cons #\[ stack) irest))
      ((_ (#\{ . irest)) (lp (cons #\{ stack) irest))
      ((_ (#\< . irest)) (lp (cons #\< stack) irest))
      ((_ ()) stack)
      ((_ _) (car input)))))

(define (check-score c)
  (match c
    (#\) 3)
    (#\] 57)
    (#\} 1197)
    (#\> 25137)
    (_ 0)))

(define (part1 in)
  (apply + (map check-score (map check in))))

(define (completion-value c)
  (match c
    (#\( 1)
    (#\[ 2)
    (#\{ 3)
    (#\< 4)
    (_ 0)))

(define (completion-score lst)
  (foldl (lambda (acc c) (+ (* 5 acc) (completion-value c))) 0 lst))

(define (part2 in)
  (let ((scores (map completion-score (filter list? (map check in)))))
    (list-ref (sort scores <) (/ (sub1 (length scores)) 2))))

(let ((in (read-input)))
  (print (part1 in))
  (print (part2 in)))
