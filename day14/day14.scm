(import (chicken io)
	srfi-69
	srfi-152
	matchable)

(define (parse-polymer str)
  (let ((str-lst (string->list str)))
    (map (lambda (c) (string->symbol (string c))) str-lst)))

(define (parse-rule str)
  (apply cons (apply cons (map parse-polymer (string-split str " -> ")))))

(define (read-input #!optional (port (current-input-port)))
  (let ((lines (read-lines port)))
    (values (parse-polymer (car lines))
	    (alist->hash-table (map parse-rule (cdr (cdr lines)))))))

(define (template->pairs template)
  (define h (make-hash-table #:initial 0))
  (let lp ((polymer template))
    (match polymer
      ((a b . rest) (hash-table-update! h (list a b) add1) (lp (cons b rest)))
      ((a) h)
      (() h))))

(define (template->counts template)
  (define h (make-hash-table #:initial 0))
  (for-each (lambda (c) (hash-table-update! h c add1)) template)
  h)

(define (polymer-step pairs counts rules)
  (define new-pairs (make-hash-table #:initial 0))
  (define new-counts (hash-table-copy counts))
  (hash-table-for-each
   pairs
   (lambda (pair v)
     (let ((elt (hash-table-ref/default rules pair #f)))
       (if elt
	   (begin (hash-table-update! new-pairs (list (car pair) elt) (lambda (x) (+ x v)))
		  (hash-table-update! new-pairs (list elt (cadr pair)) (lambda (x) (+ x v)))
		  (hash-table-update! new-counts elt (lambda (x) (+ x v))))
	   (hash-table-update! new-pairs pair add1)))))
  (values new-pairs new-counts))

(define (update-polymer pairs counts rules steps)
  (if (= steps 0)
      (values pairs counts)
      (let-values (((new-pairs new-counts) (polymer-step pairs counts rules)))
	(update-polymer new-pairs new-counts rules (- steps 1)))))

(define (part12 template rules steps)
  (define-values (pairs counts) (update-polymer (template->pairs template) (template->counts template) rules steps))
  (let ((vals (hash-table-values counts)))
    (- (apply max vals) (apply min vals))))

(let-values (((template rules) (read-input)))
  (print (part12 template rules 10))
  (print (part12 template rules 40)))
