(import (chicken io) srfi-1)

(define (read-input #!optional (port (current-input-port)))
  (map string->number
       (read-lines port)))

(define (part1 l) (count < l (cdr l)))

(define (part2 l) (count < l (cdddr l)))

(let ((in (read-input)))
  (print (part1 in))
  (print (part2 in)))
