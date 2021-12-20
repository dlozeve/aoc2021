(import (chicken io)
	(chicken format)
	srfi-1
	srfi-42
	srfi-69
	srfi-152
	matchable)

(define (parse-line s)
  (map (lambda (c) (eq? c #\#)) (string->list s)))

(define (read-input #!optional (port (current-input-port)))
  (let* ((lines (read-lines port))
	 (algo (list->vector (parse-line (car lines))))
	 (img (map parse-line (cddr lines)))
	 (h (make-hash-table)))
    (do-ec (: line (index i) img)
	   (: x (index j) line)
	   (if x)
	   (hash-table-set! h (list i j) #t))
    (values algo h #f)))

(define (kernel img background i j)
  (fold-ec 0 (: k (- i 1) (+ i 2)) (: l (- j 1) (+ j 2))
	   (hash-table-ref/default img (list k l) background)
	   (lambda (n acc) (+ (* 2 acc) (if n 1 0)))))

(define (enhance algo img background)
  (let* ((indices (hash-table-keys img))
	 (min-i (hash-table-fold img (lambda (k v acc) (min (car k) acc)) 0))
	 (max-i (hash-table-fold img (lambda (k v acc) (max (car k) acc)) 0))
	 (min-j (hash-table-fold img (lambda (k v acc) (min (cadr k) acc)) 0))
	 (max-j (hash-table-fold img (lambda (k v acc) (max (cadr k) acc)) 0))
	 (new-img (make-hash-table))
	 (new-background (vector-ref algo (if background 511 0))))
    (do-ec (:range i (- min-i 1) (+ max-i 2))
	   (:range j (- min-j 1) (+ max-j 2))
	   (:let pixel (vector-ref algo (kernel img background i j)))
	   (hash-table-set! new-img (list i j) pixel))
    (values new-img new-background)))

(define (visualize img background)
  (let* ((indices (hash-table-keys img))
	 (is (map car indices))
	 (js (map cadr indices)))
    (do-ec (:range i (index k) (- (apply min is) 1) (+ (apply max is) 2))
	   (:range j (index l) (- (apply min js) 1) (+ (apply max js) 2))
	   (begin
	     (when (and (not (= k 0)) (= l 0)) (display "\n"))
	     (if (hash-table-ref/default img (list i j) background)
		 (display "#")
		 (display "."))))
    (display "\n")))

(define (count-pixels img)
  (hash-table-fold img (lambda (k v acc) (if v (+ 1 acc) acc)) 0))

(define (part12 algo img background times)
  (if (zero? times)
      (count-pixels img)
      (let-values (((img background) (enhance algo img background)))
	(part12 algo img background (sub1 times)))))

(let-values (((algo img background) (read-input)))
  (print (part12 algo img background 2))
  (print (part12 algo img background 50)))
