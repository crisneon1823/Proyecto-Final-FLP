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

; ----------------------------
; especificación gramatical
; ----------------------------

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
    (expression ("var"
                 (separated-list identifier "=" expression ","))
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
    (expression ("symbol" identifier)
                symbol-exp)

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
       "(" expression ")")
      unary-op-exp)

    ; funciones para listas y diccionarios
    (expression ("concatenar"
                 "(" expression "," expression ")")
                concat-exp)

    (expression ("append"
                 "(" expression "," expression ")")
                append-exp)

    (expression ("ref-list"
                 "(" expression "," expression ")")
                ref-list-exp)

    (expression ("set-list"
                 "(" expression "," expression "," expression ")")
                set-list-exp)

    (expression ("ref-diccionario"
                 "(" expression "," expression ")")
                ref-dict-exp)

    (expression ("set-diccionario"
                 "(" expression "," expression "," expression ")")
                set-dict-exp)))

