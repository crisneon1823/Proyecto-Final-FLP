#lang eopl

(provide (all-defined-out))

; ==============================================================================
; ESPECIFICACIÓN LÉXICA
; ==============================================================================
(define lexical-spec
  '((white-sp (whitespace) skip)
    (comment ("#" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol) 
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (numero (digit (arbno digit) "." digit (arbno digit)) number)
    (numero ("-" digit (arbno digit) "." digit (arbno digit)) number)
    (string ("\"" (arbno (not #\")) "\"") string)))

; ==============================================================================
; GRAMÁTICA 100% LL(1) - ESTILO RECURSIVO SLLGEN
; ==============================================================================
(define grammar-spec
  '((program (expression) a-program)
    
    ; Literales y tipos primitivos
    (expression (numero) lit-num-exp)
    (expression (string) lit-str-exp)
    (expression ("true") true-exp)
    (expression ("false") false-exp)
    (expression ("null") null-exp)
    
    ; Variables y mutación (Prefijos estricto sin lookahead ambiguo)
    (expression (identifier) var-exp)
    (expression ("set" identifier "=" expression) assign-exp)
    (expression ("var" identifier "=" expression) var-decl-exp)
    (expression ("const" identifier "=" expression) const-decl-exp)

    ; Operadores Binarios (Formato prefijo cerrado)
    (expression ("op" "(" expression binary-op expression ")") binary-op-exp)
      (binary-op ("+") plus-op)
      (binary-op ("-") minus-op)
      (binary-op ("*") mult-op)
      (binary-op ("/") div-op)
      (binary-op ("%") mod-op)
      (binary-op ("<") lt-op)
      (binary-op (">") gt-op)
      (binary-op ("<=") le-op)
      (binary-op (">=") ge-op)
      (binary-op ("==") eq-op)
      (binary-op ("<>") ne-op)
      (binary-op ("and") and-op)
      (binary-op ("or") or-op)

    ; Operadores Unarios
    (expression (unary-op "(" expression ")") unary-op-exp)
      (unary-op ("not") not-op)
      (unary-op ("add1") add1-op)
      (unary-op ("sub1") sub1-op)
      (unary-op ("print") print-op)
      (unary-op ("longitud") longitud-op)
      (unary-op ("cabeza") cabeza-op)
      (unary-op ("cola") cola-op)
      (unary-op ("vacio?") vacio-op)
      (unary-op ("lista?") lista-op)
      (unary-op ("diccionario?") diccionario-op)
      (unary-op ("claves") claves-op)
      (unary-op ("valores") valores-op)

    ; Estructuras de Control Estables
    (expression ("begin" (separated-list expression ";") "end") begin-exp)
    (expression ("if" expression "then" expression "else" expression "end") if-exp)
    (expression ("while" expression "do" expression "done") while-exp)
    (expression ("for" identifier "in" expression "do" expression "done") for-exp)
    
    ; REFACTORIZACIÓN SWITCH: Lista recursiva pura en lugar de arbno
    (expression ("switch" expression "{" case-list "default" ":" expression "}") switch-exp)
    (case-list () empty-case-list)
    (case-list (case-block case-list) extended-case-list)
    (case-block ("case" expression ":" expression) a-case-block)
    
    ; REFACTORIZACIÓN FUNCIONES: Cuerpo fuertemente delimitado por llaves de control
    (expression ("func" identifier "(" (separated-list identifier ",") ")" "{" expression "}") func-exp)
    (expression ("call" identifier "(" (separated-list expression ",") ")") app-exp)

    ; Colecciones y Diccionarios (No-terminales limpios)
    (expression ("[" (separated-list expression ",") "]") list-exp)
    (expression ("{" (separated-list dict-item ",") "}") dict-exp)
    (dict-item (identifier ":" expression) a-dict-item)

    ; Álgebra Simbólica
    (expression ("symbol" identifier) symbol-exp)
    (expression ("simplificar" "(" expression ")") simplificar-exp)
    
    ; REFACTORIZACIÓN EVALUAR: Reutiliza dict-item para sanear el separated-list
    (expression ("evaluar" "(" expression "," eval-bindings ")") evaluar-exp)
    (eval-bindings ("bindings" "{" (separated-list dict-item ",") "}") a-binding-block)

    ; Primitivas sobre listas y diccionarios
    (expression ("concatenar" "(" expression "," expression ")") concat-exp)
    (expression ("append" "(" expression "," expression ")") append-exp)
    (expression ("ref-list" "(" expression "," expression ")") ref-list-exp)
    (expression ("set-list" "(" expression "," expression "," expression ")") set-list-exp)
    (expression ("ref-diccionario" "(" expression "," expression ")") ref-dict-exp)
    (expression ("set-diccionario" "(" expression "," expression "," expression ")") set-dict-exp)))

(sllgen:make-define-datatypes lexical-spec grammar-spec)

; ==============================================================================
; VALORES INTERNOS Y AMBIENTES
; ==============================================================================
(define-datatype expval expval?
  (num-val (num number?))
  (bool-val (bool boolean?))
  (str-val (str string?))
  (null-val)
  (list-val (lst list?))       
  (dict-val (dict list?))      
  (proc-val (vars (list-of symbol?))
            (body expression?) 
            (saved-env environment?))
  (symbol-val (sym symbol?)) 
  (sym-expr-val (op string?)   
                (left expval?) 
                (right expval?)))

(define-datatype environment environment?
  (empty-env)
  (extend-env (bvars (list-of symbol?)) (brefs (list-of integer?)) (saved-env environment?)) 
  (extend-env-const (bvars (list-of symbol?)) (brefs (list-of integer?)) (saved-env environment?)))

(define-datatype return-value return-value?
  (normal-val (value expval?))
  (returned-val (value expval?)))

(define (unwrap-result res)
  (cases return-value res
    (normal-val (v) v)
    (returned-val (v) v)))

; Almacén (Store)
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

; Gestión de Entornos
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
                      (let ((idx (location search-var bvars)))
                        (if idx #t (is-constant? saved-env search-var))))))

; Convertidores de tipos
(define (expval->num val) (cases expval val (num-val (num) num) (else (eopl:error 'expval->num "No es número: ~s" val))))
(define (expval->bool val)
  (cases expval val
    (bool-val (bool) bool) (num-val (num) (not (= num 0))) (str-val (str) (not (string=? str ""))) (null-val () #f) (else #t)))
(define (expval->str val) (cases expval val (str-val (str) str) (else (eopl:error 'expval->str "No es cadena: ~s" val))))
(define (expval->symbol val) (cases expval val (symbol-val (sym) sym) (str-val (str) (string->symbol str)) (else (eopl:error 'expval->symbol "No es símbolo: ~s" val))))
(define (expval->list-refs val) (cases expval val (list-val (lst) lst) (else (eopl:error 'expval->list-refs "No es lista: ~s" val))))
(define (expval->dict-pairs val) (cases expval val (dict-val (dict) dict) (else (eopl:error 'expval->dict-pairs "No es diccionario: ~s" val))))

(define (equal-expval? v1 v2)
  (cond
    ((and (cases expval v1 (num-val (n) #t) (else #f)) (cases expval v2 (num-val (n) #t) (else #f))) (= (expval->num v1) (expval->num v2)))
    ((and (cases expval v1 (bool-val (b) #t) (else #f)) (cases expval v2 (bool-val (b) #t) (else #f))) (eqv? (expval->bool v1) (expval->bool v2)))
    ((and (cases expval v1 (str-val (s) #t) (else #f)) (cases expval v2 (str-val (s) #t) (else #f))) (string=? (expval->str v1) (expval->str v2)))
    ((and (cases expval v1 (symbol-val (s) #t) (else #f)) (cases expval v2 (symbol-val (s) #t) (else #f))) (cases expval v1 (symbol-val (s1) (cases expval v2 (symbol-val (s2) (eqv? s1 s2)) (else #f))) (else #f)))
    ((and (cases expval v1 (null-val () #t) (else #f)) (cases expval v2 (null-val () #t) (else #f))) #t)
    (else #f)))

; Motores Binarios y de Álgebra Simbólica
(define (aplicar-operacion-binaria op val1 val2)
  (let ((is-sym1? (cases expval val1 (symbol-val (s) #t) (sym-expr-val (o l r) #t) (else #f)))
        (is-sym2? (cases expval val2 (symbol-val (s) #t) (sym-expr-val (o l r) #t) (else #f))))
    (if (or is-sym1? is-sym2?)
        (sym-expr-val op val1 val2)
        (cond
          ((string=? op "+")  (num-val (+ (expval->num val1) (expval->num val2))))
          ((string=? op "-")  (num-val (- (expval->num val1) (expval->num val2))))
          ((string=? op "*")  (num-val (* (expval->num val1) (expval->num val2))))
          ((string=? op "/")  (let ((d (expval->num val2))) (if (= d 0) (eopl:error 'div "División por cero") (num-val (/ (expval->num val1) d)))))
          ((string=? op "%")  (num-val (modulo (expval->num val1) (expval->num val2))))
          ((string=? op "<")  (bool-val (< (expval->num val1) (expval->num val2))))
          ((string=? op ">")  (bool-val (> (expval->num val1) (expval->num val2))))
          ((string=? op "<=") (bool-val (<= (expval->num val1) (expval->num val2))))
          ((string=? op ">=") (bool-val (>= (expval->num val1) (expval->num val2))))
          ((string=? op "==") (bool-val (equal-expval? val1 val2)))
          ((string=? op "<>") (bool-val (not (equal-expval? val1 val2))))
          ((string=? op "and") (bool-val (and (expval->bool val1) (expval->bool val2))))
          ((string=? op "or")  (bool-val (or (expval->bool val1) (expval->bool val2))))
          (else (eopl:error 'bin-op "Operador no reconocido"))))))

(define (simplificar-val val)
  (cases expval val
    (sym-expr-val (op left right)
                  (let ((l-sim (simplificar-val left)) (r-sim (simplificar-val right)))
                    (let ((l-num? (cases expval l-sim (num-val (n) #t) (else #f))) (r-num? (cases expval r-sim (num-val (n) #t) (else #f))))
                      (if (and l-num? r-num?)
                          (aplicar-operacion-binaria op l-sim r-sim)
                          (let ((n1 (if l-num? (expval->num l-sim) #f)) (n2 (if r-num? (expval->num r-sim) #f)))
                            (cond
                              ((and (string=? op "+") (equal? n1 0)) r-sim)
                              ((and (string=? op "+") (equal? n2 0)) l-sim)
                              ((and (string=? op "-") (equal? n2 0)) l-sim)
                              ((and (string=? op "*") (or (equal? n1 0) (equal? n2 0))) (num-val 0))
                              ((and (string=? op "*") (equal? n1 1)) r-sim)
                              ((and (string=? op "*") (equal? n2 1)) l-sim)
                              ((and (string=? op "/") (equal? n2 1)) l-sim)
                              (else (sym-expr-val op l-sim r-sim))))))))
    (else val)))

(define (evaluar-simbolica val targets keys-vals)
  (cases expval val
    (symbol-val (sym)
                (let loop ((t targets) (kv keys-vals))
                  (cond ((null? t) val) ((eqv? sym (car t)) (car kv)) (else (loop (cdr t) (cdr kv))))))
    (sym-expr-val (op left right)
                  (let ((l-ev (evaluar-simbolica left targets keys-vals)) (r-ev (evaluar-simbolica right targets keys-vals)))
                    (simplificar-val (sym-expr-val op l-ev r-ev))))
    (else val)))

(define (pretty-print-expval val)
  (cases expval val
    (num-val (num) (display num))
    (bool-val (bool) (if bool (display "true") (display "false")))
    (str-val (str) (display str))
    (null-val () (display "null"))
    (symbol-val (sym) (display sym))
    (sym-expr-val (op left right) (display "(") (pretty-print-expval left) (display " ") (display op) (display " ") (pretty-print-expval right) (display ")"))
    (list-val (lst) (display "[") (let loop ((l lst)) (unless (null? l) (pretty-print-expval (deref (car l))) (unless (null? (cdr l)) (display ", ")) (loop (cdr l)))) (display "]"))
    (dict-val (dict) (display "{") (let loop ((d dict)) (unless (null? d) (display (caar d)) (display ": ") (pretty-print-expval (deref (cdar d))) (unless (null? (cdr d)) (display ", ")) (loop (cdr d)))) (display "}"))
    (proc-val (v b e) (display "<function>"))))

; Auxiliar para aplanar la lista recursiva de casos del switch en un list nativo de Scheme
(define (case-list->list clst)
  (cases case-list clst
    (empty-case-list () '())
    (extended-case-list (c-block rest) (cons c-block (case-list->list rest)))))

; ==============================================================================
; EVALUACIÓN DEL INTÉRPRETE
; ==============================================================================
(define (value-of-program pgrm)
  (initialize-store!)
  (cases program pgrm (a-program (exp) (unwrap-result (car (value-of-expression-wrapper exp (empty-env)))))))

(define (value-of-expression-wrapper exp env)
  (let ((res-env-pair (value-of-expression exp env)))
    (if (return-value? (car res-env-pair)) res-env-pair (cons (normal-val (car res-env-pair)) (cdr res-env-pair)))))

(define (value-of-expression exp env)
  (cases expression exp
    (lit-num-exp (num) (cons (num-val num) env))
    (lit-str-exp (str) (cons (str-val str) env))
    (true-exp () (cons (bool-val #t) env))
    (false-exp () (cons (bool-val #f) env))
    (null-exp () (cons (null-val) env))
    (var-exp (id) (cons (deref (apply-env env id)) env))
    
    (assign-exp (id rhs-exp)
                (if (is-constant? env id)
                    (eopl:error 'assign-exp "Error: Reescribiendo constante ~s" id)
                    (let ((val (unwrap-result (car (value-of-expression-wrapper rhs-exp env)))))
                      (setref! (apply-env env id) val) (cons val env))))

    (var-decl-exp (id rhs-exp)
      (let* ((val (unwrap-result (car (value-of-expression-wrapper rhs-exp env))))
             (ref (newref val))
             (new-env (extend-env (list id) (list ref) env)))
        (cons (null-val) new-env)))

    (const-decl-exp (id rhs-exp)
      (let* ((val (unwrap-result (car (value-of-expression-wrapper rhs-exp env))))
             (ref (newref val))
             (new-env (extend-env-const (list id) (list ref) env)))
        (cons (null-val) new-env)))

    (binary-op-exp (exp1 op exp2)
      (let ((v1 (unwrap-result (car (value-of-expression-wrapper exp1 env))))
            (v2 (unwrap-result (car (value-of-expression-wrapper exp2 env)))))
        (let ((op-string
              (cases binary-op op
                (plus-op () "+") (minus-op () "-") (mult-op () "*") (div-op () "/") (mod-op () "%")
                (lt-op () "<") (gt-op () ">") (le-op () "<=") (ge-op () ">=") (eq-op () "==")
                (ne-op () "<>") (and-op () "and") (or-op () "or"))))
          (cons (aplicar-operacion-binaria op-string v1 v2) env))))

    (unary-op-exp (op exp1)
          (let ((val (unwrap-result (car (value-of-expression-wrapper exp1 env)))))
            (cons
              (cases unary-op op
                (not-op () (bool-val (not (expval->bool val))))
                (add1-op () (num-val (+ (expval->num val) 1)))
                (sub1-op () (num-val (- (expval->num val) 1)))
                (print-op () (begin (pretty-print-expval val) (newline) (null-val)))
                (longitud-op () (num-val (length (expval->list-refs val))))
                (cabeza-op () (let ((lst (expval->list-refs val))) (if (null? lst) (eopl:error 'unary "Lista vacía") (deref (car lst)))))
                (cola-op () (let ((lst (expval->list-refs val))) (if (null? lst) (eopl:error 'unary "Lista vacía") (list-val (cdr lst)))))
                (vacio-op () (bool-val (null? (expval->list-refs val))))
                (lista-op () (bool-val (cases expval val (list-val (l) #t) (else #f))))
                (diccionario-op () (bool-val (cases expval val (dict-val (d) #t) (else #f))))
                (claves-op () (list-val (map (lambda (p) (newref (str-val (symbol->string (car p))))) (expval->dict-pairs val))))
                (valores-op () (list-val (map cdr (expval->dict-pairs val)))))
              env)))

    (begin-exp (exps)
               (let loop ((lst exps) (current-res (normal-val (null-val))) (current-env env))
                 (if (null? lst) (cons current-res current-env)
                     (cases return-value current-res
                       (returned-val (v) (cons current-res current-env))
                       (normal-val (old-v) (let ((res-pair (value-of-expression-wrapper (car lst) current-env)))
                                             (loop (cdr lst) (car res-pair) (cdr res-pair))))))))

    (if-exp (test-exp then-exp else-exp)
            (let ((val (unwrap-result (car (value-of-expression-wrapper test-exp env)))))
              (if (expval->bool val) (value-of-expression-wrapper then-exp env) (value-of-expression-wrapper else-exp env))))

    ; PROCESAMIENTO ACTUALIZADO DEL SWITCH RECURSIVO
    (switch-exp (ctrl-exp case-list-ast default-exp)
      (let ((ctrl-val (unwrap-result (car (value-of-expression-wrapper ctrl-exp env))))
            (cases-flat (case-list->list case-list-ast)))
        (let loop ((lst cases-flat))
          (if (null? lst) (value-of-expression-wrapper default-exp env)
              (cases case-block (car lst)
                (a-case-block (case-exp body-exp)
                  (if (equal-expval? ctrl-val (unwrap-result (car (value-of-expression-wrapper case-exp env))))
                      (value-of-expression-wrapper body-exp env) (loop (cdr lst)))))))))

    (while-exp (test-exp body-exp)
               (let loop ((loop-env env))
                 (let ((test-val (unwrap-result (car (value-of-expression-wrapper test-exp loop-env)))))
                   (if (expval->bool test-val)
                       (let ((body-res (value-of-expression-wrapper body-exp loop-env)))
                         (cases return-value (car body-res) (returned-val (v) body-res) (normal-val (v) (loop (cdr body-res)))))
                       (cons (normal-val (null-val)) loop-env)))))

    (for-exp (id list-exp body-exp)
             (let ((l-val (unwrap-result (car (value-of-expression-wrapper list-exp env)))))
               (let loop ((lst-refs (expval->list-refs l-val)) (loop-env env))
                 (if (null? lst-refs) (cons (normal-val (null-val)) loop-env)
                     (let ((body-res (value-of-expression-wrapper body-exp (extend-env (list id) (list (car lst-refs)) loop-env))))
                       (cases return-value (car body-res)
                         (returned-val (v) (cons (car body-res) loop-env))
                         (normal-val (v) (loop (cdr lst-refs) loop-env))))))))

    (func-exp (func-name vars body-exp)
              (letrec ((proc (proc-val vars body-exp (extend-env (list func-name) (list (newref (null-val))) env)))
                       (new-ref (newref proc))
                       (extended-env (extend-env (list func-name) (list new-ref) env)))
                (setref! new-ref proc) (cons (null-val) extended-env)))

    (app-exp (func-id arg-exps)
             (let ((proc (deref (apply-env env func-id))))
               (cases expval proc
                 (proc-val (vars body-exp saved-env)
                           (if (= (length vars) (length arg-exps))
                               (let ((brefs (map (lambda (arg-e var)
                                                   (cases expression arg-e
                                                     (var-exp (id)
                                                              (let ((current-ref (apply-env env id)))
                                                                (cases expval (deref current-ref)
                                                                  (list-val (l) current-ref) (dict-val (d) current-ref)
                                                                  (else (newref (unwrap-result (car (value-of-expression-wrapper arg-e env))))))))
                                                     (else (newref (unwrap-result (car (value-of-expression-wrapper arg-e env)))))))
                                                 arg-exps vars)))
                                 (let ((res-pair (value-of-expression-wrapper body-exp (extend-env vars brefs saved-env))))
                                   (cons (unwrap-result (car res-pair)) env)))
                               (eopl:error 'app-exp "Error: Cantidad de argumentos inválida")))
                 (else (eopl:error 'app-exp "Identificador no ejecutable")))))

    (list-exp (exps) (cons (list-val (map (lambda (e) (newref (unwrap-result (car (value-of-expression-wrapper e env))))) exps)) env))
    
    (dict-exp (items) 
              (let* ((keys (map (lambda (item) (cases dict-item item (a-dict-item (id e) id))) items))
                     (exps (map (lambda (item) (cases dict-item item (a-dict-item (id e) e))) items)))
                (cons (dict-val (map cons keys (map (lambda (e) (newref (unwrap-result (car (value-of-expression-wrapper e env))))) exps))) env)))
    
    (concat-exp (e1 e2) (cons (list-val (append (expval->list-refs (unwrap-result (car (value-of-expression-wrapper e1 env)))) (expval->list-refs (unwrap-result (car (value-of-expression-wrapper e2 env)))))) env))
    (append-exp (le ie) (cons (list-val (append (expval->list-refs (unwrap-result (car (value-of-expression-wrapper le env)))) (list (newref (unwrap-result (car (value-of-expression-wrapper ie env))))))) env))
    
    (ref-list-exp (le idxe) 
                  (let ((refs (expval->list-refs (unwrap-result (car (value-of-expression-wrapper le env)))))
                        (idx (expval->num (unwrap-result (car (value-of-expression-wrapper idxe env))))))
                    (if (or (< idx 0) (>= idx (length refs))) (eopl:error 'ref "Índice fuera de rango") (cons (deref (list-ref refs idx)) env))))
    
    (set-list-exp (le idxe ve)
                  (let ((refs (expval->list-refs (unwrap-result (car (value-of-expression-wrapper le env)))))
                        (idx (expval->num (unwrap-result (car (value-of-expression-wrapper idxe env)))))
                        (v (unwrap-result (car (value-of-expression-wrapper ve env)))))
                    (if (or (< idx 0) (>= idx (length refs))) (eopl:error 'set "Índice fuera de rango") (begin (setref! (list-ref refs idx) v) (cons v env)))))
    
    (ref-dict-exp (de key) 
                  (let ((dict-v (unwrap-result (car (value-of-expression-wrapper de env))))
                        (key-v (expval->symbol (unwrap-result (car (value-of-expression-wrapper key env))))))
                    (let ((pair (assoc key-v (expval->dict-pairs dict-v)))) (if pair (cons (deref (cdr pair)) env) (eopl:error 'dict "Llave no encontrada")))))
    
    (set-dict-exp (de key ve)
                  (let ((dict-v (unwrap-result (car (value-of-expression-wrapper de env))))
                        (key-v (expval->symbol (unwrap-result (car (value-of-expression-wrapper key env))))))
                    (let ((pair (assoc key-v (expval->dict-pairs dict-v))) (v (unwrap-result (car (value-of-expression-wrapper ve env)))))
                      (if pair (begin (setref! (cdr pair) v) (cons v env)) (eopl:error 'dict "Llave no encontrada")))))

    (symbol-exp (id) (cons (symbol-val id) env))
    (simplificar-exp (exp1) (cons (simplificar-val (unwrap-result (car (value-of-expression-wrapper exp1 env)))) env))
    
    ; EXTRACCIÓN ADAPTADA DE BINDINGS DESDE DICT-ITEM
    (evaluar-exp (exp1 bindings)
      (cases eval-bindings bindings
        (a-binding-block (items)
          (let* ((ids (map (lambda (item) (cases dict-item item (a-dict-item (id e) id))) items))
                 (exps (map (lambda (item) (cases dict-item item (a-dict-item (id e) e))) items))
                 (target-val (unwrap-result (car (value-of-expression-wrapper exp1 env))))
                 (kv (map (lambda (e) (unwrap-result (car (value-of-expression-wrapper e env)))) exps)))
            (cons (evaluar-simbolica target-val ids kv) env)))))))

; Inicializadores globales corregidos e independientes
(define scan&parse (sllgen:make-string-parser lexical-spec grammar-spec))
(define just-scan (sllgen:make-string-parser lexical-spec grammar-spec)) ; Cambiado a firma válida del parser
(define (interpretador texto) (value-of-program (scan&parse texto)))