(import (chicken io) srfi-1)

(define (read-input)
  (map string->number
       (read-lines (current-input-port))))

(define (part1 measurements)
  (let lp ((l1 measurements)
	   (l2 (cdr measurements))
	   (acc 0))
    (cond
     ((or (null? l2) (null? l1)) acc)
     ((< (car l1) (car l2)) (lp (cdr l1) (cdr l2) (+ 1 acc)))
     (else (lp (cdr l1) (cdr l2) acc)))))

(define (sumn l n)
  (apply + (take l n)))

(define (part2 measurements)
  (let lp ((l1 measurements)
	   (l2 (cdr measurements))
	   (acc 0))
    (cond
     ((< (length l2) 3) acc)
     ((< (sumn l1 3) (sumn l2 3))
      (lp (cdr l1) (cdr l2) (+ 1 acc)))
     (else (lp (cdr l1) (cdr l2) acc)))))

(let ((measurements (read-input)))
  (print (part1 measurements))
  (print (part2 measurements)))
