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

(define (functor lista)
  (first lista))

(define (términos lista)
  (cdr lista))

(define(aridad lista)
  (length (cdr lista)))

(define (encabezado-regla lista)
  (second lista))

(define (condiciones-regla regla)
  (third regla))

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


(define (asociar var val asociaciones)
  (let ((var (instanciar var asociaciones))
        (val (instanciar val asociaciones)))
    (cond
      ;; Si ya son iguales después de instanciar, no hacemos nada
      [(equal? var val) #t]

      ;; Si es variable, agregar la asociación
      [(es-variable? var)
       (cons (cons var val) asociaciones)]

      ;; Si es variable el valor, hacer la asociación inversa
      [(es-variable? val)
       (cons (cons val var) asociaciones)]

      ;; Si son compuestos iguales, tratar de unificarlos
      [(and (pair? var) (pair? val))
       (unificar var val asociaciones)]

      ;; No unificables
      [else #f])))

;; UNIFICACIÓN
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
       ;;(displayln "Encabezado renombrado:")
       ;;(displayln encabezado)
       ;;(displayln "Condiciones renombradas:")
       ;;(displayln condiciones)
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
      [(es-variable? term)
       (if (member term acc) acc (cons term acc))]
      [(list? term)
       (foldr rec acc term)]
      [else acc]))
  (rec meta '()))

(define (flatten-once lst)
  (apply append lst))

(define (?- meta programa)
  (let* ((incognitas (extraer-incognitas meta))
         (soluciones (buscar meta programa incognitas '())))
    (if (null? soluciones)
        #f
        (flatten-once soluciones))))

;; ==========================
;; Consulta principal
;; ==========================



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
    (:- (progenitora ?x ?y) ((padre ?x ?y)))
    (:- (progenitora ?x ?y) ((madre ?x ?y)))
    (:- (ancestra ?x ?y) ((progenitora ?x ?y)))
    (:- (ancestra ?x ?y) ((ancestra ?x ?p) (progenitora ?p ?y)))))

(require rackunit)
(require rackunit/text-ui)
(require racket/trace)

(define pruebas-estructuras-datos
  (test-suite
   "Estructuras de datos"
   (test-case "Es regla? Caso positivo"
              (check-true
               (es-regla? '(:- (progenitora ?x  ?y) ((padre ?x ?y))))))
   (test-case "Es regla? Caso negativo"
              (check-false
               (es-regla? '(madre bulma trunks))))
   (test-case "Es regla? Lista vacía"
              (check-false
               (es-regla? '())))
   (test-case "Encabezado de regla"
              (check-equal?
               (encabezado-regla '(:- (progenitora ?x  ?y) ((padre ?x ?y))))
               '(progenitora ?x  ?y)))
   (test-case "Condiciones de regla"
              (check-equal?
               (condiciones-regla '(:- (progenitora ?x  ?y) ((padre ?x ?y))))
               '((padre ?x ?y))))
   (test-case "Es hecho? Caso negativo"
              (check-false
               (es-hecho? '(:- (progenitora ?x  ?y) ((padre ?x ?y))))))
   (test-case "Es hecho? Caso positivo"
              (check-true
               (es-hecho? '(madre bulma trunks))))
   (test-case "Es hecho? Lista vacía"
              (check-false
               (es-hecho? '())))
   (test-case "Functor de hecho"
              (check-equal?
               (functor '(padre goku gohan))
               'padre))
   (test-case "Términos de hecho"
              (check-equal?
               (términos '(padre goku gohan))
               '(goku gohan)))
   (test-case "Aridad de hecho"
              (check-equal?
               (aridad '(padre goku gohan))
               2))
   (test-case "Es variable? Con variable"
              (check-true
               (es-variable? '?x)))
   (test-case "Es variable? Con variable interna"
              (check-true
               (es-variable? '?_5)))
   (test-case "Es variable? Con número"
              (check-false
               (es-variable? 1)))
   (test-case "Es variable? Con átomo"
              (check-false
               (es-variable? 'goku)))
   (test-case "Es átomo? Con átomo"
              (check-true
               (es-átomo? 'goku)))
   (test-case "Es átomo? Con variable"
              (check-false
               (es-átomo? '?x)))
   (test-case "Es átomo? Con número"
              (check-false
               (es-átomo? 1)))
   )
  )

(define pruebas-asociaciones
  (test-suite
   "Asociaciones"
   (test-case "Asociar variable, valor es var, existente es var"
              (check-equal?
               (asociar '?r '?p '((?r . ?q) (?z . a)))
               '((?r . ?q) (?z . a) (?r . ?_1) (?p . ?_1) (?q . ?_1))))
   (test-case "Asociar variable, valor es var, existente es null"
              (check-equal?
               (asociar '?a '?b '((?x . ?_1)))
               '((?x . ?_1) (?a . ?_2) (?b . ?_2))))
   (test-case "Asociar variable, valor es var, existente es átomo"
              (check-equal?
               (asociar '?r '?q '((?y . b) (?r . c)))
               '((?y . b) (?r . c) (?r . ?_1) (?q . ?_1) (?_1 . c))))
   (test-case "Asociar variable, valor es átomo, existente es var"
              (check-equal?
               (asociar '?r 'c '((?y . b) (?r . ?q)))
               '((?q . c) (?y . b) (?r . ?q))))
   (test-case "Asociar variable, valor es átomo, existente es null"
              (check-equal?
               (asociar '?r 'c '((?y . b)))
               '((?r . c) (?y . b))))
   (test-case "Asociar variable, valor es átomo, existente es átomo y coinciden"
              (check-true
               (asociar '?r 'c '((?y . b) (?r . c)))))
   (test-case "Asociar variable, valor es átomo, existente es átomo y no coinciden"
              (check-false
               (asociar '?r 'c '((?y . b) (?r . a)))))
   (test-case "Es variable interna? positivo"
              (check-true
               (es-variable-interna? '?_1)))
   (test-case "Es variable interna? negativo"
              (check-false
               (es-variable-interna? '?x)))
   (test-case "Generar variable interna inicial"
              (check-equal?
               (generar-variable-interna '((?r . 0) (?z . a) (?y . b) (?x . c)))
               '?_1))
   (test-case "Generar variable interna siguiente lado izq"
              (check-equal?
               (generar-variable-interna '((?r . ?_1) (?_1 . ?_2) (?_2 . b)))
               '?_3))
   (test-case "Generar variable interna siguiente lado der"
              (check-equal?
               (generar-variable-interna '((?r . ?_1) (?_1 . ?_2)))
               '?_3))
   )
  )

(define pruebas-unificación
  (test-suite
   "Unificación"
   (test-case "Instanciar variables en un hecho"
              (check-equal?
               (instanciar '(padre goku ?y) '((?z . a) (?y . goten) (?x . c)))
               '(padre goku goten)))
   (test-case "Instanciar variables en una regla"
              (check-equal?
               (instanciar '(:- (progenitora ?x  ?y) ((padre ?x ?y)))
                           '((?z . a) (?x . goku)))
               '(:- (progenitora goku  ?y) ((padre goku ?y)))))
   (test-case "Instanciar variables internas en una regla"
              (check-equal?
               (instanciar '(:- (progenitora ?x  ?y) ((padre ?x ?y)))
                           '((?z . a) (?x . ?_1)))
               '(:- (progenitora ?_1  ?y) ((padre ?_1 ?y)))))
   (test-case "Instanciar variables sin asociación en una regla"
              (check-equal?
               (instanciar '(:- (progenitora ?x  ?y) ((padre ?x ?y)))
                           '((?z . a)))
               '(:- (progenitora ?x  ?y) ((padre ?x ?y)))))
   (test-case "Unificar símbolos iguales"
              (check-equal?
               (unificar 'a 'a '())
               '()))
   (test-case "Unificar símbolos diferentes"
              (check-false
               (unificar 'a 'b '())))
   (test-case "Unificar variable con símbolo"
              (check-equal?
               (unificar '?x 'a '())
               '((?x . a))))
   (test-case "Unificar símbolo con variable"
              (check-equal?
               (unificar 'a '?x '())
               '((?x . a))))
   (test-case "Unificar variable con variable"
              (check-equal?
               (unificar '?x '?y '())
               '((?x . ?_1) (?y . ?_1))))
   (test-case "Unificar términos caso 1"
              (check-equal?
               (unificar '(ancestro ?x pan) '(ancestro ?x ?y) '())
               '((?x . ?_1) (?x . ?_1) (?y . pan))))
   (test-case "Unificar términos caso 2"
              (check-equal?
               (unificar '(ancestro ?_1 ?p) '(ancestro ?x ?y) '())
               '((?_1 . ?_2) (?x . ?_2) (?p . ?_3) (?y . ?_3))))
   (test-case "Unificar términos caso 3"
              (check-equal?
               (unificar '(padre ?_2 ?_3) '(padre goku gohan) '())
               '((?_3 . gohan) (?_2 . goku))))
   )
  )

(define (same-elements? lst1 lst2)
  (equal? (set lst1) (set lst2)))

(define pruebas-consulta
  (test-suite
   "Consultas"
   (test-case "Consulta igual true"
              (check-equal?
               (?- '(igual a a) dbz)
               '()))
   (test-case "Consulta igual false"
              (check-false
               (?- '(igual a b) dbz)))
   (test-case "Consulta hecho"
              (check-true
               (same-elements?
                (?- '(padre goku ?hijo) dbz)
                '((?hijo . gohan) (?hijo . goten)))))
   (test-case "Consulta regla simple"
              (check-true
               (same-elements?
                (?- '(progenitora ?x goten) dbz)
                '((?x . goku) (?x . chichi)))))
   (test-case "Consulta regla recursiva"
              (check-true
               (same-elements?
                (?- '(padre ?x pan) dbz)
                '((?x . gohan)))))
   )
  )

(run-tests pruebas-estructuras-datos)
(run-tests pruebas-asociaciones)
(run-tests pruebas-unificación)
(run-tests pruebas-consulta)

(trace unificar)
(trace asociar)
(trace instanciar)
(trace buscar)
