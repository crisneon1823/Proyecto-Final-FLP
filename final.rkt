(provide (all-defined-out))

; especificación léxica

(define lexical-spec
  '((white-sp (whitespace) skip)
    (comment ("#" (arbno (not #\newline))) skip)

    ; identificadores
    (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)

    ; números enteros y decimales
    (number (digit (arbno digit)) integer)
    (number ("-" digit (arbno digit)) integer)
    (number (digit (arbno digit) "." digit (arbno digit)) float)
    (number ("-" digit (arbno digit) "." digit (arbno digit)) float)

    ; texto entre comillas
    (string ("\"" (arbno (not #\")) "\"") string)))


; especificación gramatical
(define grammar-spec
  '((program (expression) a-program)
    ; valores básicos
    (expression (number) lit-num-exp)
    (expression (string) lit-str-exp)
    (expression ("true") true-exp)
    (expression ("false") false-exp)
    (expression ("null") null-exp)
    (expression (identifier) var-exp)

    ; variables
    (expression ("var" (separated-list identifier "=" expression ","))
                var-decl-exp)

    (expression ("const"
                 (separated-list identifier "=" expression ","))
                const-decl-exp)

    (expression (identifier "=" expression)
                assign-exp)

    ; bloque de instrucciones
    (expression ("begin"
                 (separated-list expression ";")
                 "end")
                begin-exp)

    ; if
    (expression ("if" expression
                 "then" expression
                 "else" expression
                 "end")
                if-exp)

    ; switch
    (expression ("switch"
                 expression
                 "{"
                 (arbno "case" expression ":" expression)
                 "default" ":" expression
                 "}")
                switch-exp)

    ; ciclos
    (expression ("while"
                 expression
                 "do"
                 expression
                 "done")
                while-exp)

    (expression ("for"
                 identifier
                 "in"
                 expression
                 "do"
                 expression
                 "done")
                for-exp)

    ; funciones
    (expression ("func"
                 identifier
                 "("
                 (separated-list identifier ",")
                 ")"
                 "{"
                 (arbno expression)
                 "return"
                 expression
                 "}")
                func-exp)

    (expression (identifier
                 "("
                 (separated-list expression ",")
                 ")")
                app-exp)

    ; listas y diccionarios
    (expression ("["
                 (separated-list expression ",")
                 "]")
                list-exp)

    (expression ("{"
                 (separated-list identifier ":" expression ",")
                 "}")
                dict-exp)

    ; álgebra simbólica
    (expression ("symbol" identifier) symbol-exp)

    (expression ("simplificar"
                 "(" expression ")")
                simplificar-exp)

    (expression ("evaluar"
                 "("
                 expression
                 ","
                 "{"
                 (separated-list identifier "=" expression ",")
                 "}"
                 ")")
                evaluar-exp)

    ; operadores
    (expression
      (expression
       (or "+" "-" "*" "/" "%"
           "<" ">" "<=" ">="
           "==" "<>"
           "and" "or")
       expression)
      binary-op-exp)

    (expression
      ((or "not"
           "add1"
           "sub1"
           "print"
           "longitud"
           "cabeza"
           "cola"
           "vacio?"
           "lista?"
           "diccionario?"
           "claves"
           "valores")
       "(" expression ")") unary-op-exp
       )

    ; funciones para listas y diccionarios
    (expression ("concatenar""(" expression "," expression ")")
                concat-exp)

    (expression ("append""(" expression "," expression ")")
                append-exp)

    (expression ("ref-list" "(" expression "," expression ")")
                ref-list-exp)

    (expression ("set-list" "(" expression "," expression "," expression ")")
                set-list-exp)

    (expression ("ref-diccionario""(" expression "," expression ")")
                ref-dict-exp)

    (expression ("set-diccionario" "(" expression "," expression "," expression ")")
                set-dict-exp)))



(define-datatype expval expval?
  (num-val(num number?))
  (bool-val(bool boolean?))

  (str-val(str string?))
  (null-val)

  (list-val(lst pair?)) ; O la representación que uses para tus listas mutables
  (dict-val(dict list?)) ; Colección de pares clave-valor

  (proc-val
   (vars (list-of symbol?))
   (body (list-of expression?))
   (ret-exp expression?)
   (saved-env environment?)) ; Entorno cerrado (clausura)

  ;Para variables simbólicas puras (ej: x)
  (symbol-val (sym symbol?)) 

  (sym-expr-val
   (op string?)   ; Operador algebraico (ej: "+", "*")
   (left expval?) ; Subárbol izquierdo
   (right expval?))) ; Subárbol derecho




(define the-store '())
(define (empty-store) '())
(define (initialize-store!)
  (set! the-store (empty-store)))
(define (newref val)
  (let ((l (length the-store)))
    (set! the-store (append the-store (list val)))
    l))
(define (deref ref)
  (list-ref the-store ref))
(define (setref! ref val)
  (set! the-store
        (let loop ((store the-store) (idx 0))
          (cond
            ((null? store) (eopl:error 'setref! "Referencia inválida"))
            ((= idx ref) (cons val (cdr store)))
            (else (cons (car store) (loop (cdr store) (+ idx 1))))))))


(define-datatype environment environment?
  (empty-env)
  (extend-env
   (bvars (list-of symbol?))

   ; Guarda las posiciones del store
   (brefs (list-of integer?)) 
   (saved-env environment?)) 
  (extend-env-const
   (bvars (list-of symbol?)) 

   ; Guarda posiciones pero marcadas como inmutables
   (brefs (list-of integer?)) 
   (saved-env environment?)))

; necesitamos buscar el ambiente
(define (apply-env env search-var)
  (cases environment env
    (empty-env ()
               (eopl:error 'apply-env "Variable no encontrada: ~s" search-var))
    (extend-env (bvars brefs saved-env)
                (let ((idx (location search-var bvars)))
                  (if idx
                      (list-ref brefs idx)
                      (apply-env saved-env search-var))))
    (extend-env-const (bvars brefs saved-env)
                      (let ((idx (location search-var bvars)))
                        (if idx
                            (list-ref brefs idx)
                            (apply-env saved-env search-var))))))


;; Función auxiliar para encontrar la posición de un identificador
(define (location sym lst)
  (let loop ((lst lst) (idx 0))
    (cond
      ((null? lst) #f)
      ((eqv? sym (car lst)) idx)
      (else (loop (cdr lst) (+ idx 1))))))

;; Función auxiliar para verificar si una variable fue declarada como constante
(define (is-constant? env search-var)
  (cases environment env
    (empty-env () #f)
    (extend-env (bvars brefs saved-env)
                (let ((idx (location search-var bvars)))
                  (if idx #f (is-constant? saved-env search-var))))
    (extend-env-const (bvars brefs saved-env)
                      (let ((idx (location search-var bvars)))
                        (if idx #t (is-constant? saved-env search-var))))))

;funciones estractores para EXPVA
(define (expval->num val)
  (cases expval val
    (num-val (num) num)
    (else (eopl:error 'expval->num "El valor no es un número: ~s" val))))

(define (expval->bool val)
  (cases expval val
    (bool-val (bool) bool)
    (else (eopl:error 'expval->bool "El valor no es un booleano: ~s" val))))

(define (expval->str val)
  (cases expval val
    (str-val (str) str)
    (else (eopl:error 'expval->str "El valor no es una cadena: ~s" val))))

(define (value-of-program pgrm)
  (initialize-store!) ; Limpia memoria al iniciar
  (cases program pgrm
    (a-program (exp)
               (value-of-expression exp (empty-env)))))

(define (value-of-expression exp env)
  (cases expression exp
    ; uso de literales
    (lit-num-exp (num) (num-val num))
    (lit-str-exp (str) (str-val str))
    (true-exp () (bool-val #t))
    (false-exp () (bool-val #f))
    (null-exp () (null-val))
    
    ; uso de variables 
    (var-exp (id)
             (deref (apply-env env id)))
    
    ;declaraion de variable Mutables (var x=1, y=2) ---
    (var-decl-exp (ids exps)
                  (let* ((vals (map (lambda (e) (value-of-expression e env)) exps))
                         (refs (map newref vals)))
                    ;; Retorna null-val o el cuerpo según lo manejes, por consistencia con entornos mutables extendemos el ambiente actual y las declaraciones devuelven null-val pero afectan el entorno si se usan secuencialmente.
                    (eopl:error 'var-decl-exp "Las declaraciones múltiples requieren ser evaluadas en un entorno extendido. Usualmente se manejan bajo bloques secuenciales o let.")))
                    
    ; bloque de instrucciones con el begin y el end
    (begin-exp (exps)
               (let loop ((lst exps) (last-val (null-val)) (current-env env))
                 (if (null? lst)
                     last-val
                     ;; Si la expresión actual es una declaración, debemos propagar el nuevo ambiente extendido
                     (cases expression (car lst)
                       (var-decl-exp (ids val-exps)
                                     (let* ((vals (map (lambda (e) (value-of-expression e current-env)) val-exps))
                                            (refs (map newref vals))
                                            (new-env (extend-env ids refs current-env)))
                                       (loop (cdr lst) (null-val) new-env)))
                       (const-decl-exp (ids val-exps)
                                       (let* ((vals (map (lambda (e) (value-of-expression e current-env)) val-exps))
                                              (refs (map newref vals))
                                              (new-env (extend-env-const ids refs current-env)))
                                         (loop (cdr lst) (null-val) new-env)))
                       (else
                        (let ((val (value-of-expression (car lst) current-env)))
                          (loop (cdr lst) val current-env)))))))
    
    ;asignacion  de Variables (x = expresión) 
    (assign-exp (id rhs-exp)
                (if (is-constant? env id)
                    (eopl:error 'assign-exp "Error Semántico: No se puede reasignar un valor a la constante: ~s" id)
                    (let ((val (value-of-expression rhs-exp env)))
                      (setref! (apply-env env id) val) val))) ; Devolvemos el valor asignado como resultado de la expresión
    
    ;condicional (if)
    (if-exp (test-exp then-exp else-exp)
            (let ((val (value-of-expression test-exp env)))(if (expval->bool val)
                  (value-of-expression then-exp env)
                  (value-of-expression else-exp env))
            )
    )                       
    
    ;Próximo Bloque por Desarrollar: Operadores y Ciclos
    (else (eopl:error 'value-of-expression "Expresión no implementada aún: ~s" exp))))


; construccion del parsel
(define scan&parse
  (sllgen:make-string-parser lexical-spec grammar-spec))

(define just-scan
  (sllgen:make-string-scanner lexical-spec))

;; --- INTERFAZ DE PRUEBA ---
(define (interpretador texto)
  (value-of-program (scan&parse texto)))