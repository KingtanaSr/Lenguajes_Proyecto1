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
  (and (symbol? x)
       (let* ([str (symbol->string x)]
              [lst (string->list str)])
         (and (>= (length lst) 2)
              (char=? (first lst) #\?)
              (char=? (second lst) #\_)))))


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

(define (lista-variables-internas lista)
  (foldr (lambda (par acc)
           (let* ((a (car par))
                  (d (cdr par))
                  (acc1 (if (es-variable-interna? a) (cons a acc) acc))
                  (acc2 (if (es-variable-interna? d) (cons d acc1) acc1)))
             (remove-duplicates acc2)))
         '()
         lista))

(define (num-var-interna x)
  (let* ([str (if (symbol? x) (symbol->string x) x)])
    (string->number (substring str 2))))


(define (generar-variable-interna lista)
  (let* ([listav (lista-variables-internas lista)]
         [numeros (map num-var-interna listav)]
         [maximo (if (null? numeros) 0 (apply max numeros))]
         [nuevo-num (+ 1 maximo)]
         [nuevo-simbolo (string->symbol (string-append "?_" (number->string nuevo-num)))])
    nuevo-simbolo))

;(define (unificar términoA términoB asociaciones)
  
 ; (else #f))
;;(trace unificar)
;;(trace instanciar)
;;(trace buscar)
