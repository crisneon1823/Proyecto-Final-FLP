#lang eopl

(provide (all-defined-out))

;especificacion lexica
(define lexical-spec
  '((white-sp (whitespace) skip)
    (comment ("#" (arbno (not #\newline))) skip)

    (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol) ;identificador 

    ; creacion de los números enteros y decimales
    (number (digit (arbno digit)) integer)
    (number ("-" digit (arbno digit)) integer)
    (number (digit (arbno digit) "." digit (arbno digit)) float)
    (number ("-" digit (arbno digit) "." digit (arbno digit)) float)

    (string ("\"" (arbno (not #\")) "\"") string))) ; texto entre comillas

;gramatica 
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
    (expression ("var" (separated-list identifier "=" expression ","))var-decl-exp)

    (expression ("const" (separated-list identifier "=" expression ","))const-decl-exp)
    (expression (identifier "=" expression)assign-exp)

    ; bloque de instrucciones (del if, switch, ciclos, for, funciones)
    (expression ("begin" (separated-list expression ";") "end")
                begin-exp)
    (expression ("if" expression "then" expression "else" expression "end")
                if-exp)
    (expression ("switch" expression "{" (arbno "case" expression ":" expression) "default" ":" expression "}")
                switch-exp)
    (expression ("while" expression "do" expression "done")
                while-exp)
    (expression ("for" identifier "in" expression "do" expression "done")
                for-exp)
    (expression ("func" identifier "(" (separated-list identifier ",") ")" "{" (arbno expression) "return" expression "}")
                func-exp)

    (expression (identifier "(" (separated-list expression ",") ")")
                app-exp)

    ; listas y diccionarios
    (expression ("[" (separated-list expression ",") "]") ;
                list-exp)
    (expression ("{" (separated-list identifier ":" expression ",") "}")
                dict-exp)

    ; álgebra simbólica
    (expression ("symbol" identifier) symbol-exp)
    (expression ("simplificar" "(" expression ")")
                simplificar-exp)
    (expression ("evaluar" "(" expression "," "{" (separated-list identifier "=" expression ",") "}" ")")
                evaluar-exp)

    ; operadores binarios
    (expression (expression (or "+" "-" "*" "/" "%" "<" ">" "<=" ">=" "==" "<>" "and" "or") expression)
                binary-op-exp)

    ; operadores unarios
    (expression ((or "not" "add1" "sub1" "print" "longitud" "cabeza" "cola" "vacio?" "lista?" "diccionario?" "claves" "valores")
                 "(" expression ")") 
                unary-op-exp)

    ; funciones para listas y diccionarios
    (expression ("concatenar" "(" expression "," expression ")")concat-exp)
    (expression ("append" "(" expression "," expression ")")
                append-exp)
    (expression ("ref-list" "(" expression "," expression ")")ref-list-exp)
    (expression ("set-list" "(" expression "," expression "," expression ")")
                set-list-exp)
    (expression ("ref-diccionario" "(" expression "," expression ")")
                ref-dict-exp)

    (expression ("set-diccionario" "(" expression "," expression "," expression ")")
                set-dict-exp)))

;datatype de las expresiones val y ambiente
(define-datatype expval expval?
  (num-val (num number?))
  (bool-val (bool boolean?))
  (str-val (str string?))
  (null-val)
  (list-val (lst list?))       ;; Lista de enteros (referencias en el store)
  (dict-val (dict list?))      ;; Lista de pares (símbolo . referencia)
  (proc-val (vars (list-of symbol?))
            (body (list-of expression?))
            (ret-exp expression?)
            (saved-env environment?))
  (symbol-val (sym symbol?)) 
  (sym-expr-val (op string?)   
                (left expval?) 
                (right expval?)))

(define-datatype environment environment?
  (empty-env)
  (extend-env (bvars (list-of symbol?))
              (brefs (list-of integer?)) 

              (saved-env environment?)) 
  (extend-env-const (bvars (list-of symbol?)) (brefs (list-of integer?)) 
                    (saved-env environment?)))

;sistema de retorno de valores )
(define-datatype return-value return-value?
  (normal-val (value expval?))
  (returned-val (value expval?))
  )

(define (unwrap-result res)
  (cases return-value res
    (normal-val (v) v)
    (returned-val (v) v)))

; memoria mutable 
;; =============================================================================
(define the-store '())
(define (empty-store) '())
(define (initialize-store!) (set! the-store (empty-store)))
(define (newref val)
  (let ((l (length the-store)))
    (set! the-store (append the-store (list val)))
    l))
(define (deref ref) (list-ref the-store ref))
(define (setref! ref val)
  (set! the-store
        (let loop ((store the-store) (idx 0))
          (cond
            ((null? store) (eopl:error 'setref! "Referencia inválida"))
            ((= idx ref) (cons val (cdr store)))
            (else (cons (car store) (loop (cdr store) (+ idx 1))))))))

;operaciones dle ambiente
(define (apply-env env search-var)
  (cases environment env
    (empty-env () (eopl:error 'apply-env "Variable no encontrada: ~s" search-var))
    (extend-env (bvars brefs saved-env)
                (let ((idx (location search-var bvars)))
                  (if idx (list-ref brefs idx) (apply-env saved-env search-var))))
    (extend-env-const (bvars brefs saved-env)
                      (let ((idx (location search-var bvars)))
                        (if idx (list-ref brefs idx) (apply-env saved-env search-var))))))

(define (location sym lst)
  (let loop ((lst lst) (idx 0))
    (cond
      ((null? lst) #f)
      ((eqv? sym (car lst)) idx)
      (else (loop (cdr lst) (+ idx 1))))))

(define (is-constant? env search-var)
  (cases environment env
    (empty-env () #f)
    (extend-env (bvars brefs saved-env)
                (let ((idx (location search-var bvars)))
                  (if idx #f (is-constant? saved-env search-var))))
    (extend-env-const (bvars brefs saved-env)
                      (let ((idx (location search-var bvars)))(if idx #t (is-constant? saved-env search-var))))))

; extractores de valores (EXPVAL)
(define (expval->num val)
  (cases expval val (num-val (num) num) (else (eopl:error 'expval->num "No es un número: ~s" val))))

(define (expval->bool val)
  (cases expval val
    (bool-val (bool) bool)
    (num-val (num) (not (= num 0)))
    (str-val (str) (not (string=? str "")))
    (null-val () #f)
    (else #t)))

(define (expval->str val)
  (cases expval val (str-val (str) str) (else (eopl:error 'expval->str "No es una cadena: ~s" val))))

(define (expval->list-refs val)
  (cases expval val (list-val (lst) lst) (else (eopl:error 'expval->list-refs "No es una lista: ~s" val))))

(define (expval->dict-pairs val)
  (cases expval val (dict-val (dict) dict) (else (eopl:error 'expval->dict-pairs "No es un diccionario: ~s" val))))

; comprobacion de las igualdades
(define (equal-expval? v1 v2)
  (cond
    ((and (cases expval v1 (num-val (n) #t) (else #f)) (cases expval v2 (num-val (n) #t) (else #f)))
     (= (expval->num v1) (expval->num v2)))
    ((and (cases expval v1 (bool-val (b) #t) (else #f)) (cases expval v2 (bool-val (b) #t) (else #f)))
     (eqv? (expval->bool v1) (expval->bool v2)))
    ((and (cases expval v1 (str-val (s) #t) (else #f)) (cases expval v2 (str-val (s) #t) (else #f)))
     (string=? (expval->str v1) (expval->str v2)))
    ((and (cases expval v1 (symbol-val (s) #t) (else #f)) (cases expval v2 (symbol-val (s) #t) (else #f)))
     (cases expval v1 (symbol-val (s1) (cases expval v2 (symbol-val (s2) (eqv? s1 s2)) (else #f))) (else #f)))
    ((and (cases expval v1 (null-val) #t) (else #f)) (cases expval v2 (null-val) #t) (else #f)) #t)
    (else #f)))

;; opeeraciones binarioas y algebraica
;; =============================================================================
(define (aplicar-operacion-binaria op val1 val2)
  (let ((is-sym1? (cases expval val1 (symbol-val (s) #t) (sym-expr-val (o l r) #t) (else #f)))
        (is-sym2? (cases expval val2 (symbol-val (s) #t) (sym-expr-val (o l r) #t) (else #f))))
    (if (or is-sym1? is-sym2?)

        (sym-expr-val op val1 val2)

        (cond
          ((string=? op "+")  (num-val (+ (expval->num val1) (expval->num val2))))
          ((string=? op "-")  (num-val (- (expval->num val1) (expval->num val2))))
          ((string=? op "*")  (num-val (* (expval->num val1) (expval->num val2))))
          ((string=? op "/")  (let ((denominator (expval->num val2)))
                                (if (= denominator 0)
                                    (eopl:error 'aplicar-operacion-binaria "Error Matemático: División por cero")
                                    (num-val (/ (expval->num val1) denominator)))))
          ((string=? op "%")  (num-val (modulo (expval->num val1) (expval->num val2))))
          ((string=? op "<")  (bool-val (< (expval->num val1) (expval->num val2))))
          ((string=? op ">")  (bool-val (> (expval->num val1) (expval->num val2))))
          ((string=? op "<=") (bool-val (<= (expval->num val1) (expval->num val2))))
          ((string=? op ">=") (bool-val (>= (expval->num val1) (expval->num val2))))
          ((string=? op "==") (bool-val (equal-expval? val1 val2)))
          ((string=? op "<>") (bool-val (not (equal-expval? val1 val2))))
          ((string=? op "and") (bool-val (and (expval->bool val1) (expval->bool val2))))
          ((string=? op "or")  (bool-val (or (expval->bool val1) (expval->bool val2))))

          (else (eopl:error 'aplicar-operacion-binaria "Operador desconocido"))))))

;motor de simplificación recursiva
(define (simplificar-val val)
  (cases expval val
    (sym-expr-val (op left right)
                  (let ((l-sim (simplificar-val left))
                        (r-sim (simplificar-val right)))
                    (let ((l-num? (cases expval l-sim (num-val (n) #t) (else #f)))
                          (r-num? (cases expval r-sim (num-val (n) #t) (else #f))))
                      (if (and l-num? r-num?)
                          (aplicar-operacion-binaria op l-sim r-sim)
                          (let ((n1 (if l-num? (expval->num l-sim) #f))
                                (n2 (if r-num? (expval->num r-sim) #f)))
                            (cond
                              ((and (string=? op "+") (equal? n1 0)) r-sim)
                              ((and (string=? op "+") (equal? n2 0)) l-sim)
                              ((and (string=? op "-") (equal? n2 0)) l-sim)
                              ((and (string=? op "*") (or (equal? n1 0) (equal? n2 0))) (num-val 0))
                              ((and (string=? op "*") (equal? n1 1)) r-sim)
                              ((and (string=? op "*") (equal? n2 1)) l-sim)
                              ((and (string=? op "/") (equal? n2 1)) l-sim)
                              (else (sym-expr-val op l-sim r-sim))))))))
    (else val))
  )

;sustitucion simbolica
(define (evaluar-simbolica val targets keys-vals)
  (cases expval val
    (symbol-val (sym)
                (let loop ((t targets) (kv keys-vals))
                  (cond
                    ((null? t) val)
                    ((eqv? sym (car t)) (car kv))
                    (else (loop (cdr t) (cdr kv))))))
    (sym-expr-val (op left right)
                  (let ((l-ev (evaluar-simbolica left targets keys-vals))(r-ev (evaluar-simbolica right targets keys-vals)))
                    (simplificar-val (sym-expr-val op l-ev r-ev))))
    (else val)))

(define (pretty-print-expval val)
  (cases expval val
    (num-val (num) (display num))
    (bool-val (bool) (if bool (display "true") (display "false")))
    (str-val (str) (display str))
    (null-val () (display "null"))
    (symbol-val (sym) (display sym))
    (sym-expr-val (op left right) 
                  (display "(") (pretty-print-expval left) 
                  (display " ") (display op) (display " ") 
                  (pretty-print-expval right) (display ")"))
    (list-val (lst)
              (display "[")
              (let loop ((l lst))
                (cond
                  ((null? l) (display "]"))
                  (else (pretty-print-expval (deref (car l)))
                        (if (null? (cdr l)) (display "") (display ", "))
                        (loop (cdr l))))))
    (dict-val (dict)
              (display "{")
              (let loop ((d dict))
                (cond
                  ((null? d) (display "}"))
                  (else (display (caar d)) (display ": ")
                        (pretty-print-expval (deref (cdar d)))
                        (if (null? (cdr d)) (display "") (display ", "))
                        (loop (cdr d))))))
    (proc-val (v b r e) (display "<function>"))))

;evaluacion centralizada y unica de programas y expresiones
(define (value-of-program pgrm)
  (initialize-store!)
  (cases program pgrm
    (a-program (exp)
               (let ((result-env-pair (value-of-expression-wrapper exp (empty-env))))
                 (unwrap-result (car result-env-pair))))))
(define (value-of-expression-wrapper exp env)
  (let ((res-env-pair (value-of-expression exp env)))
    (if (return-value? (car res-env-pair))
        res-env-pair
        (cons (normal-val (car res-env-pair)) (cdr res-env-pair)))))

; value-of-expression retorna un par 
(define (value-of-expression exp env)
  (cases expression exp
    (lit-num-exp (num) (cons (num-val num) env))
    (lit-str-exp (str) (cons (str-val str) env))
    (true-exp () (cons (bool-val #t) env))
    (false-exp () (cons (bool-val #f) env))
    (null-exp () (cons (null-val) env))
    (var-exp (id) (cons (deref (apply-env env id)) env))
    (var-decl-exp (ids exps)
                  (let* ((vals (map (lambda (e) (unwrap-result (car (value-of-expression-wrapper e env)))) exps))
                         (refs (map newref vals))
                         (new-env (extend-env ids refs env)))
                    (cons (null-val) new-env)))
    (const-decl-exp (ids exps)
                    (let* ((vals (map (lambda (e) (unwrap-result (car (value-of-expression-wrapper e env)))) exps))
                           (refs (map newref vals))
                           (new-env (extend-env-const ids refs env)))
                      (cons (null-val) new-env)))
    (assign-exp (id rhs-exp)
                (if (is-constant? env id)
                    (eopl:error 'assign-exp "Error Semántico: Constante inmutable: ~s" id)
                    (let ((val (unwrap-result (car (value-of-expression-wrapper rhs-exp env)))))
                      (setref! (apply-env env id) val)
                      (cons val env))))
    (begin-exp (exps)
               (let loop ((lst exps) (current-res (normal-val (null-val))) (current-env env))
                 (if (null? lst)
                     (cons current-res current-env)
                     (cases return-value current-res
                       (returned-val (v) (cons current-res current-env))
                       (normal-val (old-v)
                                   (let ((res-pair (value-of-expression-wrapper (car lst) current-env)))
                                     (loop (cdr lst) (car res-pair) (cdr res-pair))))))))
    (if-exp (test-exp then-exp else-exp)
            (let ((val (unwrap-result (car (value-of-expression-wrapper test-exp env)))))
              (if (expval->bool val)
                  (value-of-expression-wrapper then-exp env)
                  (value-of-expression-wrapper else-exp env))))
    (switch-exp (ctrl-exp case-exps body-exps default-exp)
                (let ((ctrl-val (unwrap-result (car (value-of-expression-wrapper ctrl-exp env)))))
                  (let loop ((c-exps case-exps) (b-exps body-exps))
                    (cond
                      ((null? c-exps) (value-of-expression-wrapper default-exp env))
                      ((equal-expval? ctrl-val (unwrap-result (car (value-of-expression-wrapper (car c-exps) env))))
                       (value-of-expression-wrapper (car b-exps) env))
                      (else (loop (cdr c-exps) (cdr b-exps)))))))
    (while-exp (test-exp body-exp)
               (let loop ((loop-env env))
                 (let ((test-val (unwrap-result (car (value-of-expression-wrapper test-exp loop-env)))))
                   (if (expval->bool test-val)
                       (let ((body-res (value-of-expression-wrapper body-exp loop-env)))
                         (cases return-value (car body-res)
                           (returned-val (v) body-res)
                           (normal-val (v) (loop (cdr body-res)))))
                       (cons (normal-val (null-val)) loop-env)))))

  (for-exp (id list-exp body-exp)
             (let ((l-val (unwrap-result (car (value-of-expression-wrapper list-exp env)))))
               (let ((refs (expval->list-refs l-val)))
                 (let loop ((lst-refs refs) (loop-env env))
                   (if (null? lst-refs)
                       (cons (normal-val (null-val)) loop-env)
                       (let ((new-env (extend-env (list id) (list (car lst-refs)) loop-env)))
                         (let ((body-res (value-of-expression-wrapper body-exp new-env)))
                           (cases return-value (car body-res)
                             (returned-val (v) (cons (car body-res) loop-env))
                             (normal-val (v) (loop (cdr lst-refs) loop-env))))))))))

    ;; 10 & 14. GRAMÁTICA DE FUNCIONES: Se guardan directamente en el ambiente actual
    (func-exp (func-name vars exps ret-exp)
              (letrec ((proc (proc-val vars exps ret-exp (extend-env (list func-name) (list (newref (null-val))) env)))
                       (new-ref (newref proc))
                       (extended-env (extend-env (list func-name) (list new-ref) env)))
                (setref! new-ref proc)
                (cons (null-val) extended-env)))

    (app-exp (func-id arg-exps)
             (let ((proc (deref (apply-env env func-id))))
               (cases expval proc
                 (proc-val (vars body ret-exp saved-env)
                           (if (= (length vars) (length arg-exps))
                               (let ((brefs (map (lambda (arg-e var)
                                                   (cases expression arg-e
                                                     (var-exp (id)
                                                              (let ((current-ref (apply-env env id)))
                                                                (cases expval (deref current-ref)
                                                                  (list-val (l) current-ref)
                                                                  (dict-val (d) current-ref)
                                                                  (else (newref (unwrap-result (car (value-of-expression-wrapper arg-e env))))))))
                                                     (else (newref (unwrap-result (car (value-of-expression-wrapper arg-e env)))))))
                                                 arg-exps vars)))
                                 (let ((body-env (extend-env vars brefs saved-env)))
                                   (let loop ((lst-body body) (c-env body-env))
                                     (if (null? lst-body)
                                         (cons (car (value-of-expression-wrapper ret-exp c-env)) env)
                                         (let ((res-pair (value-of-expression-wrapper (car lst-body) c-env)))
                                           (cases return-value (car res-pair)
                                             (returned-val (v) (cons (car res-pair) env))
                                             (normal-val (v) (loop (cdr lst-body) (cdr res-pair)))))))))
                               (eopl:error 'app-exp "Argumentos erróneos")))
                 (else (eopl:error 'app-exp "Identificador no es invocable")))))

    (binary-op-exp (exp1 op exp2)
                   (let ((v1 (unwrap-result (car (value-of-expression-wrapper exp1 env))))
                         (v2 (unwrap-result (car (value-of-expression-wrapper exp2 env)))))
                     (cons (aplicar-operacion-binaria op v1 v2) env)))

    (unary-op-exp (op exp1)
                  (let ((val (unwrap-result (car (value-of-expression-wrapper exp1 env)))))
                    (cons
                     (cond
                       ((string=? op "not")  (bool-val (not (expval->bool val))))
                       ((string=? op "add1") (num-val (+ (expval->num val) 1)))
                       ((string=? op "sub1") (num-val (- (expval->num val) 1)))
                       ((string=? op "print") (begin (pretty-print-expval val) (newline) (null-val)))
                       ((string=? op "longitud") (num-val (length (expval->list-refs val))))
                       ((string=? op "cabeza") (let ((lst (expval->list-refs val)))
                                                 (if (null? lst) (eopl:error 'unary-op-exp "Error: Lista vacía en cabeza") (deref (car lst)))))
                       ((string=? op "cola") (let ((lst (expval->list-refs val)))
                                               (if (null? lst) (eopl:error 'unary-op-exp "Error: Lista vacía en cola") (list-val (cdr lst)))))
                       ((string=? op "vacio?") (bool-val (null? (expval->list-refs val))))
                       ((string=? op "lista?") (bool-val (cases expval val (list-val (l) #t) (else #f))))
                       ((string=? op "diccionario?") (bool-val (cases expval val (dict-val (d) #t) (else #f))))
                       ((string=? op "claves") (list-val (map (lambda (p) (newref (str-val (symbol->string (car p))))) (expval->dict-pairs val))))
                       ((string=? op "valores") (list-val (map cdr (expval->dict-pairs val))))
                       (else (eopl:error 'unary-op-exp "Operador no soportado")))
                     env)))

    (list-exp (exps) 
              (cons (list-val (map (lambda (e) (newref (unwrap-result (car (value-of-expression-wrapper e env))))) exps)) env))
    
    (dict-exp (keys exps) 
              (cons (dict-val (map cons keys (map (lambda (e) (newref (unwrap-result (car (value-of-expression-wrapper e env))))) exps))) env))
    
    (concat-exp (e1 e2) 
                (cons (list-val (append (expval->list-refs (unwrap-result (car (value-of-expression-wrapper e1 env)))) 
                                        (expval->list-refs (unwrap-result (car (value-of-expression-wrapper e2 env)))))) env))
    
    (append-exp (le ie) 
                (let ((lst (expval->list-refs (unwrap-result (car (value-of-expression-wrapper le env))))))
                  (cons (list-val (append lst (list (newref (unwrap-result (car (value-of-expression-wrapper ie env))))))) env)))
    
    (ref-list-exp (le idxe) 
                  (let ((refs (expval->list-refs (unwrap-result (car (value-of-expression-wrapper le env)))))
                        (idx (expval->num (unwrap-result (car (value-of-expression-wrapper idxe env))))))
                    (if (or (< idx 0) (>= idx (length refs)))
                        (eopl:error 'ref-list-exp "Error: Índice fuera de rango")
                        (cons (deref (list-ref refs idx)) env))))
    
    (set-list-exp (le idxe ve)
                  (let ((refs (expval->list-refs (unwrap-result (car (value-of-expression-wrapper le env)))))
                        (idx (expval->num (unwrap-result (car (value-of-expression-wrapper idxe env)))))
                        (v (unwrap-result (car (value-of-expression-wrapper ve env)))))
                    (if (or (< idx 0) (>= idx (length refs)))
                        (eopl:error 'set-list-exp "Error: Índice fuera de rango")
                        (begin (setref! (list-ref refs idx) v) (cons v env)))))
    
    (ref-dict-exp (de key) 
                  (cons (deref (cdr (assoc key (expval->dict-pairs (unwrap-result (car (value-of-expression-wrapper de env))))))) env))
    
    (set-dict-exp (de key ve)
                  (let ((p (assoc key (expval->dict-pairs (unwrap-result (car (value-of-expression-wrapper de env))))))
                        (v (unwrap-result (car (value-of-expression-wrapper ve env)))))
                    (setref! (cdr p) v) (cons v env)))

    (symbol-exp (id) (cons (symbol-val id) env))
    
    (simplificar-exp (exp1) 
                     (cons (simplificar-val (unwrap-result (car (value-of-expression-wrapper exp1 env)))) env))
    
    (evaluar-exp (exp1 ids exps)
                 (let ((target-val (unwrap-result (car (value-of-expression-wrapper exp1 env))))
                       (kv (map (lambda (e) (unwrap-result (car (value-of-expression-wrapper e env)))) exps)))
                   (cons (evaluar-simbolica target-val ids kv) env)))

    (else (eopl:error 'value-of-expression "Expresión no soportada"))))

; parser e interfaz de prueba
(define scan&parse (sllgen:make-string-parser lexical-spec grammar-spec))
(define just-scan (sllgen:make-string-scanner lexical-spec))
(define (interpretador texto)
  (value-of-program (scan&parse texto)))