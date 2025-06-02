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

(define (es-regla? lista)
  (and (not (null? lista))
       (eq? (first lista) ':-)))

(define (es-hecho? lista)
  (let ((n (length lista)))
    (cond
      [(es-regla? lista) #f]
      [(or (= n 2) (= n 3)) #t]
      [else #f])))

(define (functor lista)
  (first lista))

(define (terminos lista)
  (cdr lista))

(define(aridad lista)
  (length (cdr lista)))

(define (encabezado-regla lista)
  (second lista))

(define (condiciones-regla lista)
  (cdr (cdr lista)))

;;(trace unificar)
;;(trace instanciar)
;;(trace buscar)
