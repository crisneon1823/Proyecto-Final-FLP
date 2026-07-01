(provide (all-defined-out))

; ----------------------------
; especificación léxica
; ----------------------------

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