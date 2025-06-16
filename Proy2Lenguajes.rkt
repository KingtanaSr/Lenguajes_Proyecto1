;;Chat con la explicación de todo el código: 
;;https://chatgpt.com/share/6850638f-7410-8012-a4de-81578f110c95

#lang racket
(provide ?-)

(define (es-variable? x)
  (cond
    [(symbol? x)
     (let ([str (symbol->string x)])
       (and (> (string-length str) 0)
            (char=? (string-ref str 0) #\?)))]
    [else #f]))

(define (es-átomo? x)
  (cond
    [(symbol? x)
     (let ([str (symbol->string x)])
       (and (> (string-length str) 0)
            (not (char=? (string-ref str 0) #\?))))]
    [else #f]))

(define (es-variable-interna? x)
  (and (symbol? x)
       (let ([str (symbol->string x)])
         (and (>= (string-length str) 2)
              (char=? (string-ref str 0) #\?)
              (char=? (string-ref str 1) #\_)))))

(define (es-regla? lista)
  (and (not (null? lista))
       (eq? (first lista) ':-)))

(define (es-hecho? lista)
  (let ((n (length lista)))
    (cond
      [(es-regla? lista) #f]
      [(or (= n 2) (= n 3)) #t]
      [else #f])))

(define (functor lista) (first lista))
(define (términos lista) (cdr lista))
(define(aridad lista) (length (cdr lista)))
(define (encabezado-regla lista) (second lista))
(define (condiciones-regla regla) (third regla))

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

;; UNIFICACIÓN
(define (asociar var val asociaciones)
  (let ((var (instanciar var asociaciones))
        (val (instanciar val asociaciones)))
    (cond
      [(equal? var val) #t]
      [(es-variable? var) (cons (cons var val) asociaciones)]
      [(es-variable? val) (cons (cons val var) asociaciones)]
      [(and (pair? var) (pair? val)) (unificar var val asociaciones)]
      [else #f])))

(define (instanciar elemento asociaciones)
  (cond
    [(es-variable? elemento)
     (let ((asoc (assoc elemento asociaciones)))
       (if asoc (instanciar (cdr asoc) asociaciones) elemento))]
    [(pair? elemento)
     (map (lambda (x) (instanciar x asociaciones)) elemento)]
    [else elemento]))

(define (unificar a b asociaciones)
  (let* ((a1 (instanciar a asociaciones))
        (b1 (instanciar b asociaciones)))
    (cond
      [(equal? a1 b1) asociaciones]
      [(es-variable? a1) (asociar a1 b1 asociaciones)]
      [(es-variable? b1) (asociar b1 a1 asociaciones)]
      [(and (pair? a1) (pair? b1) (= (length a1) (length b1)))
       (unificar-listas a1 b1 asociaciones)]
      [else #f])))

(define (unificar-listas la lb asociaciones)
  (cond
    [(null? la) asociaciones]
    [(null? lb) asociaciones]
    [else
     (let ((res (unificar (first la) (first lb) asociaciones)))
       (if res
           (unificar-listas (rest la) (rest lb) res)
           #f))]))

;; RESOLUCIÓN (BUSCAR)
(define (buscar meta programa incognitas asociaciones)
  (apply append
         (map (lambda (clausula)
                (resolver meta clausula programa incognitas asociaciones))
              programa)))

(define (resolver meta clausula programa incognitas asociaciones)
  (cond
    [(es-hecho? clausula)
     (let ((nueva (unificar meta clausula asociaciones)))
       (if nueva (list (filtrar-asociaciones incognitas nueva)) '()))]
    [(es-regla? clausula)
     (let* ((renombrada (renombrar-regla clausula))
            (encabezado (car renombrada))
            (condiciones (cdr renombrada))
            (nueva (unificar meta encabezado asociaciones)))
       (if nueva
           (buscar-condiciones condiciones programa incognitas nueva)
           '()))]
    [else '()]))

(define (buscar-condiciones condiciones programa incognitas asociaciones)
  (if (null? condiciones)
      (list (filtrar-asociaciones incognitas asociaciones))
      (apply append
             (map (lambda (nuevo-asoc)
                    (buscar-condiciones (cdr condiciones) programa incognitas nuevo-asoc))
                  (buscar (car condiciones) programa incognitas asociaciones)))))

(define (filtrar-asociaciones incognitas asociaciones)
  (define (instanciar-profundo var)
    (let ((val (instanciar var asociaciones)))
      (if (equal? val var) val (instanciar-profundo val))))
  (map (lambda (v)
         (cons v (instanciar-profundo v)))
       incognitas))

(define (renombrar-regla regla)
  (define (renombrar-term term tabla)
    (cond
      [(es-variable? term)
       (let ((par (assoc term tabla)))
         (if par
             (values (cdr par) tabla)
             (let* ((nuevo (generar-variable-interna tabla))
                    (nueva-tabla (cons (cons term nuevo) tabla)))
               (values nuevo nueva-tabla))))]
      [(list? term)
       (let loop ((ts term) (res '()) (t tabla))
         (if (null? ts)
             (values (reverse res) t)
             (call-with-values
                 (lambda () (renombrar-term (car ts) t))
               (lambda (nuevo-term nuevo-tabla)
                 (loop (cdr ts) (cons nuevo-term res) nuevo-tabla)))))]
      [else (values term tabla)]))

  (let ((_ (first regla))
        (encabezado (second regla))
        (condiciones (third regla)))
    (call-with-values
      (lambda () (renombrar-term encabezado '()))
      (lambda (nuevo-enc tabla-final)
        (define (renombrar-condiciones conds tabla acc)
          (if (null? conds)
              (values (reverse acc) tabla)
              (call-with-values
                  (lambda () (renombrar-term (car conds) tabla))
                (lambda (nuevo-cond nueva-tabla)
                  (renombrar-condiciones (cdr conds) nueva-tabla (cons nuevo-cond acc))))))
        (call-with-values
            (lambda () (renombrar-condiciones condiciones tabla-final '()))
          (lambda (nuevas-condiciones _)
            (cons nuevo-enc nuevas-condiciones)))))))

;; INTÉRPRETE
(define (extraer-incognitas meta)
  (define (rec term acc)
    (cond
      [(es-variable? term) (if (member term acc) acc (cons term acc))]
      [(list? term) (foldr rec acc term)]
      [else acc]))
  (rec meta '()))

(define (flatten-once lst) (apply append lst))

(define (?- meta programa)
  (let* ((incognitas (extraer-incognitas meta))
         (soluciones (buscar meta programa incognitas '())))
    (if (null? soluciones)
        #f
        (flatten-once soluciones))))
