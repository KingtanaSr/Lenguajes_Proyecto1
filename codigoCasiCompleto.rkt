#lang racket
(require racket/trace)

(define (es-variable? x)
  (cond
    [(symbol? x)
     (let ([str (symbol->string x)])
       (and (> (string-length str) 0)
            (char=? (string-ref str 0) #\?)))]
    [else #f]))

(define (es-atomo? x)
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


(define (asociar variable valor asociaciones)
  (if (es-variable? variable)
      (cons (cons variable valor) asociaciones)
      asociaciones)) 
(trace asociar)

;; UNIFICACIÓN
(define (instanciar elemento asociaciones)
  (cond
    [(es-variable? elemento)
     (let ((asoc (assoc elemento asociaciones)))
       (if asoc (instanciar (cdr asoc) asociaciones) elemento))]
    [(list? elemento)
     (map (lambda (x) (instanciar x asociaciones)) elemento)]
    [else elemento]))


(define (unificar a b asociaciones)
  (let* ((a1 (instanciar a asociaciones))
         (b1 (instanciar b asociaciones)))
    (cond
      [(equal? a1 b1) asociaciones]
      [(es-variable? a1)
       (let ((binding (assoc a1 asociaciones)))
         (if binding
             (unificar (cdr binding) b1 asociaciones)
             (asociar a1 b1 asociaciones)))]
      [(es-variable? b1)
       (unificar b1 a1 asociaciones)]
      [(and (list? a1) (list? b1)
            (= (length a1) (length b1)))
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
         (map (lambda (regla-o-hecho)
                (resolver meta regla-o-hecho programa incognitas asociaciones))
              programa)))

(define (resolver meta regla programa incognitas asociaciones)
  (cond
    [(es-hecho? regla)
     (let ((nueva (unificar meta regla asociaciones)))
       (if nueva
           (list (filtrar-asociaciones incognitas nueva))
           '()))]
    [(es-regla? regla)
     (let* ((enc (encabezado-regla regla))
            (conds (condiciones-regla regla))
            (nueva-var (generar-variable-interna asociaciones))
            (renombrada (renombrar-regla (list enc) conds nueva-var '()))
            (enc-ren (car renombrada))
            (conds-ren (cadr renombrada)))
       (let ((nuevo-asoc (unificar meta enc-ren asociaciones)))
         (if nuevo-asoc
             (buscar-condiciones conds-ren programa incognitas nuevo-asoc)
             '())))]
    [else '()]))

(define (buscar-condiciones condiciones programa incognitas asociaciones)
  (cond
    [(null? condiciones)
     (list (filtrar-asociaciones incognitas asociaciones))]
    [else
     (apply append
            (map (lambda (nuevo-asoc)
                   (buscar-condiciones (cdr condiciones) programa incognitas nuevo-asoc))
                 (buscar (car condiciones) programa incognitas asociaciones)))]))

(define (filtrar-asociaciones _ asociaciones)
  (map (lambda (p)
         (cons (car p) (instanciar (car p) asociaciones)))
       asociaciones))

(define (renombrar-regla encabezado condiciones nueva-var _)
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

  (let* ((tabla '())
         (enc-ren
          (call-with-values
              (lambda () (renombrar-term (car encabezado) tabla))
            (lambda (nuevo-enc nueva-tabla)
              (let loop ((conds condiciones) (res '()) (t nueva-tabla))
                (if (null? conds)
                    (list (list nuevo-enc) (reverse res))
                    (call-with-values
                        (lambda () (renombrar-term (car conds) t))
                      (lambda (nuevo-cond nt)
                        (loop (cdr conds) (cons nuevo-cond res) nt)))))))))
    enc-ren))

;; INTÉRPRETE
(define (extraer-incognitas meta)
  (define (rec term acc)
    (cond
      [(es-variable? term)
       (if (member term acc) acc (cons term acc))]
      [(list? term)
       (foldr rec acc term)]
      [else acc]))
  (rec meta '()))


(define (?- meta programa)
  (let* ((incognitas (extraer-incognitas meta))
         (soluciones (buscar meta programa incognitas '())))
    (if (null? soluciones)
        #f
        soluciones)))

;; PRUEBA
(define dbz
  '((igual ?x ?x)
    (padre bardock goku)
    (padre goku gohan)
    (padre goku goten)
    (padre bardock raditz)
    (padre vegeta trunks)
    (padre gohan pan)
    (madre chichi goten)
    (madre bulma trunks)
    (madre chichi gohan)
    (madre videl pan)
    (:- (progenitora ?x  ?y) ((padre ?x ?y)))
    (:- (progenitora ?x  ?y) ((madre ?x ?y)))
    (:- (ancestra ?x ?y) ((progenitora ?x ?y)))
    (:- (ancestra ?x ?y) ((ancestra ?x ?p) (progenitora ?p ?y)))
    )
  )

(?- '(padre ?x pan) dbz)
;;'((?x . gohan)) FUNCIONA
(?- '(progenitora ?x goten) dbz)
;;'((?x . goku) (?x . chichi)) -> #f
(?- '(igual a b) dbz)
;;#f FUNCIONA
(?- '(igual a a) dbz)
;;'((?x . a)) FUNCIONA
