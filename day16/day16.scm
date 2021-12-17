(import (chicken io)
	(chicken format)
	srfi-1
	matchable)

(define (read-input #!optional (port (current-input-port)))
  (read-line port))

(define (hex-char->bin hex-char)
  (match hex-char
    (#\0 '(0 0 0 0))
    (#\1 '(0 0 0 1))
    (#\2 '(0 0 1 0))
    (#\3 '(0 0 1 1))
    (#\4 '(0 1 0 0))
    (#\5 '(0 1 0 1))
    (#\6 '(0 1 1 0))
    (#\7 '(0 1 1 1))
    (#\8 '(1 0 0 0))
    (#\9 '(1 0 0 1))
    (#\A '(1 0 1 0))
    (#\B '(1 0 1 1))
    (#\C '(1 1 0 0))
    (#\D '(1 1 0 1))
    (#\E '(1 1 1 0))
    (#\F '(1 1 1 1))))

(define (hex->bin hex)
  (concatenate (map hex-char->bin (string->list hex))))

(define (bin->number bin)
  (let lp ((acc 0)
	   (l bin))
    (if (null? l)
	acc
	(lp (+ (* acc 2) (car l)) (cdr l)))))

(define-record val
  version num)

(set! (record-printer val)
  (lambda (x out)
    (fprintf out "#,(val (ver ~S) ~S)" (val-version x) (val-num x))))

(define-record op
  version type args)

(set! (record-printer op)
  (lambda (x out)
    (fprintf out "#,(op (ver ~S) (type ~S) ~S)" (op-version x) (op-type x) (op-args x))))

(define (decode-version raw)
  (values (bin->number (take raw 3))
	  (drop raw 3)))

(define (decode-type-id raw)
  (values (bin->number (take raw 3))
	  (drop raw 3)))

(define (decode-num raw bits)
  (let ((new-bits (take (cdr raw) 4)))
    (if (= 1 (car raw))
	(decode-num (drop raw 5) (append bits new-bits))
	(values (bin->number (append bits new-bits)) (drop raw 5)))))

(define (decode-op version type-id raw)
  (if (zero? (car raw))
      (let* ((subpackets-length (bin->number (take (cdr raw) 15)))
	     (subpackets-raw (take (drop raw 16) subpackets-length)))
	(let-values (((subpackets _) (decode-all-packets subpackets-raw)))
	  (values (make-op version type-id subpackets) (drop raw (+ 16 subpackets-length)))))
      (let ((subpackets-count (bin->number (take (cdr raw) 11))))
	(let lp ((i subpackets-count)
		 (packets '())
		 (raw (drop raw 12)))
	  (if (zero? i)
	      (values (make-op version type-id (reverse packets)) raw)
	      (let-values (((packet new-raw) (decode-packet raw)))
		(lp (sub1 i) (cons packet packets) new-raw)))))))

(define (decode-packet raw)
  (let*-values (((version raw) (decode-version raw))
		((type-id raw) (decode-type-id raw)))
    (if (= type-id 4)
	(let-values (((num raw) (decode-num raw '())))
	  (values (make-val version num) raw))
	(let-values (((op new-raw) (decode-op version type-id raw)))
	  (values op new-raw)))))

(define (decode-all-packets raw)
  (let lp ((packets '())
	   (raw raw))
    (if (every zero? raw)
	(values (reverse packets) raw)
	(let-values (((packet new-raw) (decode-packet raw)))
	  (lp (cons packet packets) new-raw)))))

(define (version-sum packets)
  (let lp ((versions '())
	   (packets packets))
    (match packets
      (() (apply + versions))
      ((($ val ver _) . rest)
       (lp (cons ver versions) rest))
      ((($ op ver _ args) . rest)
       (lp (cons ver versions) (append args rest))))))

(define (part1 hex-str)
  (version-sum (decode-all-packets (hex->bin hex-str))))

(define (eval-packet packet)
  (match packet
    (($ val _ num) num)
    (($ op _ 0 args) (apply + (map eval-packet args)))
    (($ op _ 1 args) (apply * (map eval-packet args)))
    (($ op _ 2 args) (apply min (map eval-packet args)))
    (($ op _ 3 args) (apply max (map eval-packet args)))
    (($ op _ 5 args) (if (apply > (map eval-packet args)) 1 0))
    (($ op _ 6 args) (if (apply < (map eval-packet args)) 1 0))
    (($ op _ 7 args) (if (apply = (map eval-packet args)) 1 0))))

(define (part2 hex-str)
  (eval-packet (decode-packet (hex->bin hex-str))))

(let ((hex (read-input)))
  (print (part1 hex))
  (print (part2 hex)))
