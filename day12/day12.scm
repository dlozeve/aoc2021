(import (chicken io)
	srfi-1
	srfi-42
	srfi-152)

(define (read-input #!optional (port (current-input-port)))
  (map (lambda (s) (string-split s "-")) (read-lines port)))

(define (big-cave? cave)
  (char-upper-case? (string-ref cave 0)))

(define (small-cave? cave)
  (char-lower-case? (string-ref cave 0)))

(define (neighbours edges vertex)
  (let lp ((acc '())
	   (edges edges))
    (cond
     ((null? edges) acc)
     ((equal? vertex (car (car edges))) (lp (cons (cadr (car edges)) acc) (cdr edges)))
     ((equal? vertex (cadr (car edges))) (lp (cons (car (car edges)) acc) (cdr edges)))
     (else (lp acc (cdr edges))))))

(define (find-all-paths edges start end)
  (define paths '())
  (let lp ((path (list start)))
    (if (equal? (car path) end)
	(set! paths (cons (reverse path) paths))
	(do-ec (: n (neighbours edges (car path)))
	       (or (big-cave? n)
		   (not (member n path)))
	       (lp (cons n path)))))
  paths)

(define (part1 edges)
  (length (find-all-paths edges "start" "end")))

(define (small-caves-at-most-once-except-one? path)
  (let ((small-caves (filter small-cave? path)))
    (<= (- (length small-caves) (length (delete-duplicates small-caves))) 1)))

(define (find-all-paths-2 edges start end)
  (define paths '())
  (let lp ((path (list start)))
    (if (equal? (car path) end)
	(set! paths (cons (reverse path) paths))
	(do-ec (: n (neighbours edges (car path)))
	       (and (not (equal? n "start"))
		    (or (big-cave? n)
			(small-caves-at-most-once-except-one? path)))
	       (lp (cons n path)))))
  paths)

(define (part2 edges)
  (length (find-all-paths-2 edges "start" "end")))

(let ((edges (read-input)))
  (print (part1 edges))
  (print (part2 edges)))
