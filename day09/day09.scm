(import (chicken io)
	(chicken sort)
	srfi-1
	srfi-42
	srfi-152
	generalized-arrays
	storage-classes)

(define (string->list-of-ints s)
  (map (lambda (x) (- x 48)) (map char->integer (string->list s))))

(define (read-input #!optional (port (current-input-port)))
  (nested-list->array
   (map string->list-of-ints (read-lines port))
   s8vector-storage-class
   2))

(define (neighbours grid idx)
  (let ((m (vector-ref (array-shape grid) 0))
	(n (vector-ref (array-shape grid) 1))
	(i (vector-ref idx 0))
	(j (vector-ref idx 1)))
    (list-ec (:range k (- i 1) (+ i 2))
	     (:range l (- j 1) (+ j 2))
	     (and (not (and (= k i) (= l j)))
		  (or (= k i) (= l j))
		  (>= k 0) (>= l 0) (< k m) (< l n))
	     (list (vector k l) (array-ref grid (vector k l))))))

(define (low-points grid)
  (let ((m (vector-ref (array-shape grid) 0))
	(n (vector-ref (array-shape grid) 1)))
    (list-ec (:range i m)
	     (:range j n)
	     (:let x (array-ref grid (vector i j)))
	     (:let neighb (map cadr (neighbours grid (vector i j))))
	     (if (every (lambda (n) (< x n)) neighb))
	     (list (vector i j) x))))

(define (part1 grid)
  (apply + (map add1 (map cadr (low-points grid)))))

(define (compute-basins grid)
  (define n (vector-ref (array-shape grid) 0))
  (define m (vector-ref (array-shape grid) 1))
  (define visited (make-array vector-storage-class (vector n m) #f))
  (define (basin-size idx)
    (sum-ec (: neighbour (neighbours grid idx))
	    (:let neighbour-idx (car neighbour))
	    (:let neighbour-val (cadr neighbour))
	    (not (array-ref visited neighbour-idx))
	    (begin
	      (array-set! visited neighbour-idx #t)
	      (if (= neighbour-val 9)
		  0
		  (add1 (basin-size neighbour-idx))))))
  (list-ec (: lowp-idx (map car (low-points grid)))
	   (basin-size lowp-idx)))

(define (part2 grid)
  (apply * (take (sort (compute-basins grid) >) 3)))

(let ((grid (read-input)))
  (print (part1 grid))
  (print (part2 grid)))
