#lang racket
(require racket/trace)

(define (es-variable? x)
  (let* ([str (if (symbol? x) (symbol->string x) x)]
         [lst (string->list str)])
    (cond
      [(empty? lst) #f]
      [else (char=? (first lst) #\?)])))

(define (es-atomo? x)
  (let* ([str (if (symbol? x) (symbol->string x) x)]
         [lst (string->list str)])
    (cond
      [(empty? lst) #f]
      [(char=? (first lst) #\?) #f]
      [else #t])))

(define (es-variable-interna? x)
  (let* ([str (if (symbol? x) (symbol->string x) x)]
         [lst (string->list str)])
    (and (char=? (first lst) #\?)
         (char=? (second lst) #\_))))

(define (asociar variable valor asociaciones)
  (if (es-variable? variable)
      (cons (cons variable valor) asociaciones)
      asociaciones)) 
(trace asociar)
