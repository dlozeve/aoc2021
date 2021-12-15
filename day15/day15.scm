(import (chicken io)
	srfi-1
	srfi-42
	srfi-113
	srfi-128
	srfi-152
	matchable
	generalized-arrays
	storage-classes)

(define (string->list-of-ints s)
  (map (lambda (x) (- x 48)) (map char->integer (string->list s))))

(define (read-input #!optional (port (current-input-port)))
  (nested-list->array
   (map string->list-of-ints (read-lines port))
   u8vector-storage-class
   2))

(define (neighbours grid p)
  (match-let ((#(m n) (array-shape grid))
	      (#(x y) p))
    (list-ec (: i '(-1 0 1))
	     (: j '(-1 0 1))
	     (and (not (= i j 0))
		  (or (= i 0) (= j 0))
		  (>= (+ x i) 0)
		  (>= (+ y j) 0)
		  (< (+ x i) m)
		  (< (+ y j) n))
	     (vector (+ x i) (+ y j)))))

(define (min-unvisited-index array index-list)
  (let ((index-list (set->list index-list)))
    (fold (lambda (v acc) (if (< (array-ref array v) (array-ref array acc)) v acc))
	  (car index-list) index-list)))

(define (part1 grid)
  (define-values (m n) (match (array-shape grid)
			 (#(m n) (values m n))))
  (define unvisited (list->set (make-default-comparator)
			       (list-ec (: i m) (: j n) (vector i j))))
  (define frontier (set (make-default-comparator) #(0 0)))
  (define dist (make-array vector-storage-class (array-shape grid) (* 10 m n)))
  (array-set! dist #(0 0) 0)
  (let lp ((v #(0 0)))
    ;;(printf "~a ~a~n" v (array-ref dist v))
    (do-ec (: n (neighbours grid v))
	   (if (set-contains? unvisited n))
	   (begin
	     (array-set! dist n (min (array-ref dist n) (+ (array-ref dist v) (array-ref grid n))))
	     (set-adjoin! frontier n)))
    (set-delete! unvisited v)
    (set-delete! frontier v)
    (if (set-contains? unvisited (vector (sub1 m) (sub1 n)))
	(lp (min-unvisited-index dist frontier))
	(array-ref dist (vector (sub1 m) (sub1 n))))))

(define (expand-grid grid)
  (match-let ((#(m n) (array-shape grid)))
    (define new-grid (make-array vector-storage-class (vector (* 5 m) (* 5 n))))
    (do-ec (: k 5) (: l 5)
	   (: i m) (: j n)
	   (:let new-val (add1 (remainder (+ k l (sub1 (array-ref grid (vector i j)))) 9)))
	   (array-set! new-grid (vector (+ i (* k m)) (+ j (* l n))) new-val))
    new-grid))

(define (part2 grid)
  (part1 (expand-grid grid)))

(let ((grid (read-input)))
  (print (part1 grid))
  (print (part2 grid)))
