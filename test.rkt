#lang racket

;;; P → LD
;;; LD → D LD | ε
;;; D → DV | F | C | COM
;;; DV → ("let" | "const") I "=" E ";"
;;; E → N | I | "(" E ")" | O
;;; O → E OP E
;;; OP → "+" | "-" | "*" | "/" | "==" | "<" | ">" | "!="
;;; C → "for" "(" DV O ";" O ")" "{" LD "}"
;;; F → "function" I "(" LI ")" "{" LD "}"
;;; LI → I "," LI | ε
;;; COM → "//" I
;;; I → [a-zA-Z][a-zA-Z0-9]*
;;; N → [0-9]+

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; P = programa
;;; LD = lista de declaraciones

;;; D = declaracion
;;; DV = declaracion de variable
;;; F = funcion
;;; C = ciclo
;;; E = expresion
;;; O = operacion

;;; N = numero
;;; I = identificador
;;; COM = comentario{
;;; OP = Operador

;;; LI = lista de identificadores


(define COMMENT 'comment)
(define KEYWORD 'keyword)

(define FUNCTION_KEYWORD 'function_keyword)
(define VARIABLE_KEYWORD 'variable_keyword)
(define CICLE_KEYWORD 'cicle_keyword)

(define IDENTIFIER 'identifier)
(define OPERATOR 'operator)
(define NUMBER 'number)
(define NEWLINE 'newline)

(define SEPARATOR 'separator)
(define SEMICOLON 'semicolon)

(define LEFT_PARENTHESIS 'left_parenthesis)
(define RIGHT_PARENTHESIS 'right_parenthesis)

(define LEFT_BRACE 'left_brace)
(define RIGHT_BRACE 'right_brace)


; es_P: lista indice -> boolean
; Verifica si la lista de tokens forma un programa válido (P) según la gramática.
; Un P es un LD (lista de declaraciones).

;;; P → LD
(define (es_P tokens indice)
  (if (>= indice (length tokens)); Si el indice es mayor a la longitud de la lista devolvemos que es falso
     (list #f indice (list-ref tokens indice))
      (es_LD tokens indice) ; En caso de que siga estando en la longitud valida pasamos los tokens y el indice inicial
  )
)

; es_LD: lista indice -> numero / #f
; Verifica si la lista de tokens forma una lista de declaraciones válida (LD) según la gramática.
; Un LD puede ser un D (declaración) seguido de un LD, o puede ser vacío (ε).

; Esta función es la encargada de ir recorriendo las declaraciones e ir devolviendo el ultimo indice hasta el cual llego la ultima declaración

;;; LD → D LD | ε
(define (es_LD tokens indice)
  (cond 
      ; Si el indice es mayor que el numero de tokens, entonces hemos alcanzado el final y devolvemos el índice actual.
      [(>= indice (length tokens)) indice]
      ; Si no hemos alcanzado el final, intentamos analizar una declaración (D).
      [else 
        (let ((nuevo_indice (es_D tokens indice))) ; Definimos la variable nuevo_indice la cual toma el ultimo indice usado por es_D
              (if (number? nuevo_indice) ; Verificamos si el indice que me dio es_D es un número ya que si no lo es es porque devolvió falso
                  (es_LD tokens nuevo_indice) ; Si es_D fue exitoso, intentamos analizar el resto de los tokens como una lista de declaraciones (LD)
                 (list #f indice (list-ref tokens indice)) ; Si es_D no fue exitoso, devolvemos falso.
              )
            )
      ]
  )
)

; es_D: lista indice -> numero / #f
; Verifica si la lista de tokens forma una declaración válida (D) según la gramática.
; Un D puede ser un DV (declaración de variable), un F (función), un C (ciclo), o un COM (comentario).
; Devuelve el índice del token después de la D, o #f si los tokens no forman una D válida.

;;; D → DV | F | C | COM
(define (es_D tokens indice)
  (if (>= indice (length tokens)) ; Verificamos si la longitud es mayor que la de la lista
     (list #f indice (list-ref tokens indice))
      (let ((tmp (car (list-ref tokens indice))))
        (cond ((equal? tmp VARIABLE_KEYWORD) (es_DV tokens indice)) ; Verificamos si se trata de una declaración de una variable
              ((equal? tmp FUNCTION_KEYWORD) (es_F tokens indice)) ; Verificamos si se trata de una función
              ((equal? tmp CICLE_KEYWORD) (es_C tokens indice)) ; Verificamos si se trata de un ciclo
              ((equal? tmp COMMENT) (es_D tokens (+ indice 1))) ; Verificamos si se trata de un comentario
              ((equal? tmp NEWLINE) (es_D tokens (+ indice 1))) ; Si se trata de un espacio en blanco volvemos a llamar a es_D
              (else(list #f indice (list-ref tokens indice))))))) ; Si no se identifica ninguno de los casos devolvemos que es falso y no se encontró una declaración valida


; es_DV: vector indice -> numero / #f
; Verifica si el vector de tokens forma una declaración de variable válida (DV) según la gramática.
; Un DV es un KEYWORD seguido de un IDENTIFIER, un OPERATOR "=" y una E (expresión).
; Devuelve el índice del token después de la DV, o #f si los tokens no forman una DV válida.

;;; DV → ("let" | "const") I "=" E
(define (es_DV tokens indice)
  (if (>= indice (length tokens)) ; Verificamos si se sobrepasa la longitud
     (list #f indice (list-ref tokens indice))
      ; Si hay más tokens para analizar, extraemos el tipo y el valor del token en el índice actual.
      (let 
           (
            (token-type (car (list-ref tokens indice))) ; Guardamos el tipo de token
            (token-value (cdr (list-ref tokens indice))) ; Guardamos el valor del token
           )
        
        ; También verificamos si el siguiente token es un IDENTIFIER y si el token después de ese es un OPERATOR con valor "=".
        ; Si todas estas condiciones se cumplen, entonces tenemos el comienzo de una declaración de variable.
        (if (and 
                 (equal? token-type VARIABLE_KEYWORD) ; Primero verificamos que el token sea de tipo KEYWORD
                 (or (string=? token-value "let") (string=? token-value "const")) ; Luego verificamos si el valor del token es let o const
                 (equal? (car (list-ref tokens (+ indice 1))) IDENTIFIER)  ; Luego verificamos si hay un identificador
                 (equal? (car (list-ref tokens (+ indice 2))) OPERATOR) ; Posteriormente debe de haber un operador
                 (string=? (cdr (list-ref tokens (+ indice 2))) "=")) ; Y este operador debe ser el operador igual

            ; Si las condiciones se cumplen, llamamos a la función es_E para analizar la expresión que sigue.
            ; Pasamos el vector de tokens y el índice del primer token después de la asignación ("=") a es_E.
            (es_E tokens (+ indice 3))
            ; Si alguna de las condiciones no se cumple, los tokens no forman una declaración de variable válida,
            ; por lo que devolvemos #f.
           (list #f indice (list-ref tokens indice))
            )
      )
  )
)


; es_E: vector indice -> numero / #f
;;; E → N | I | "(" E ")"

(define (es_E tokens indice)
  (cond ; Ejecutamos el condicional para saber si se cumple alguno de los criterios
        ((>= indice (length tokens)) (list #f indice (list-ref tokens indice)))   ;; No hay más tokens por lo que se devuleve que es falso
        ((equal? (car (list-ref tokens indice)) NUMBER) (+ indice 1))  ; Puede ser un numero
        ((equal? (car (list-ref tokens indice)) IDENTIFIER) (+ indice 1))  ;; Puede ser un identificador
        ((and ; El utlimo caso es en el que este envuelto en parentesis

              (equal? (cdr (list-ref tokens indice)) "(")  ;; En caso de que no sea uno de los anteriores entonces puede ser una expresión en parentesis
              (es_E tokens (+ indice 1)) ;; 
              (equal? (cdr (list-ref tokens (+ indice 2))) ")"))
         (+ indice 3))
        (else(list #f indice (list-ref tokens indice)))))

; es_C: vector indice -> numero / #f
;;; C → "for" "(" DV O ";" O ")" "{" LD "}"

;; es_C: lista indice -> numero / #f
(define (es_C tokens indice)
  
      (if (and (equal? (cdr (list-ref tokens indice)) "for")
               (equal? (cdr (list-ref tokens (+ indice 1))) "("))
          (let* ((indice-1 (es_DV tokens (+ indice 2)))
                 (indice-2 (if (and (number? indice-1) (equal? (cdr (list-ref tokens indice-1)) ";"))
                               (es_O tokens (+ indice-1 1))
                               #f))
                 (indice-3 (if (and (number? indice-2) (equal? (cdr (list-ref tokens indice-2)) ";"))
                               (es_O tokens (+ indice-2 1))
                               #f))
                 (indice-4 (if (and (number? indice-3) (equal? (cdr (list-ref tokens indice-3)) ")"))
                            (if (equal? (cdr (list-ref tokens (+ indice-3 1))) "{")
                               (es_LD tokens (+ indice-3 2))
                               (list #f indice (list-ref tokens indice))
                               )
                               (list #f indice (list-ref tokens indice))
                               )))
            (if (and (number? indice-4) (equal? (cdr (list-ref tokens indice-4)) "}"))
                (+ indice-4 1)
                (list #f indice (list-ref tokens indice))))
          (list #f indice (list-ref tokens indice)))
      (list #f indice (list-ref tokens indice))
)


;; es_O: vector indice -> numero / #f
;;; O → E OP E | O OP E

(define (es_O tokens indice)
  (let ((nuevo_indice (es_E tokens indice)))
    (if (and (number? nuevo_indice)
             (>= nuevo_indice (- (length tokens) 1))
             (equal? (car (list-ref tokens nuevo_indice)) OPERATOR)
             (number? (es_E tokens (+ nuevo_indice 1))))
        (+ nuevo_indice 2)
       (list #f indice (list-ref tokens indice)))))

;; es_OP: vector indice -> numero / #f
;;; OP → "+" | "-" | "*" | "/" | "==" | "<" | ">" | "!="

(define (es_OP tokens indice)
  (if (and (>= indice (length tokens))
           (equal? (car (list-ref tokens indice)) OPERATOR))
      (+ indice 1)
     (list #f indice (list-ref tokens indice))))


;; es_F: vector indice -> numero / #f
;;; F → "function" I "(" LI ")" "{" LD "}"

(define (es_F tokens indice)
  (if (and (>= indice (- (length tokens) 7))  ;; Nos aseguramos de que hay suficientes tokens
           (equal? (cdr (list-ref tokens indice)) "function")
           (equal? (car (list-ref tokens (+ indice 1))) IDENTIFIER)
           (equal? (cdr (list-ref tokens (+ indice 2))) "("))
      (let* (
            (nuevo_indice (es_LI tokens (+ indice 3)))
            (nuevo_indice_2 (if (and 
                                    (number? nuevo_indice)
                                    (equal? (cdr (list-ref tokens nuevo_indice)) ")")
                                    (equal? (cdr (list-ref tokens (+ nuevo_indice 1))) "{")
                                  )
                                 (es_LD tokens (+ nuevo_indice 2))
                                (list #f indice (list-ref tokens indice)))
            )
            )
        (if (and (number? nuevo_indice_2)
                 (equal? (cdr (list-ref tokens nuevo_indice_2)) "}")
            )

            (+ nuevo_indice_2 1)
           (list #f indice (list-ref tokens indice))))

     (list #f indice (list-ref tokens indice))))

;; es_LI: vector indice -> numero / #f
;;; LI → I "," LI | ε

(define (es_LI tokens indice)
  (cond ((>= indice (length tokens))(list #f indice (list-ref tokens indice)))  ;; No hay más tokens
        ((equal? (car (list-ref tokens indice)) IDENTIFIER)  ;; Identificador
         (if (and (>= indice (- (length tokens) 1))  ;; Hay un token más
                  (equal? (cdr (list-ref tokens (+ indice 1))) ","))  ;; Y es una coma
              
             (es_LI tokens (+ indice 2))  ;; Continuamos con el siguiente identificador
             (+ indice 1)))  ;; No hay coma, terminamos aquí
        (else(list #f indice (list-ref tokens indice)))))


(define tokensEntrada  (list 

    (cons COMMENT "// Aqui hay un comentario") (cons NEWLINE "\n") (cons NEWLINE "\r\n")  
                                      
    (cons VARIABLE_KEYWORD "let") (cons IDENTIFIER "a") (cons OPERATOR "=") (cons NUMBER "5") (cons NEWLINE "\r\n") 

    (cons VARIABLE_KEYWORD "let") (cons IDENTIFIER "b") (cons OPERATOR "=") (cons NUMBER "10") (cons NEWLINE "\r\n") 
                                               
    ;(cons VARIABLE_KEYWORD "let") (cons IDENTIFIER "c") (cons OPERATOR "=") (cons NUMBER "30") (cons OPERATOR "+") (cons NUMBER "20") (cons NEWLINE "\r\n") 
    
    (cons NEWLINE "\r\n") 
    
    (cons CICLE_KEYWORD "for") (cons LEFT_PARENTHESIS "(") (cons VARIABLE_KEYWORD "let") (cons IDENTIFIER "i") (cons OPERATOR "=") (cons NUMBER "5") (cons SEMICOLON ";") 
    (cons IDENTIFIER "i") (cons OPERATOR "<") (cons NUMBER "5") (cons SEMICOLON ";") (cons IDENTIFIER "i") (cons OPERATOR "=") (cons IDENTIFIER "i") (cons OPERATOR "+") (cons NUMBER "1") (cons RIGHT_PARENTHESIS ")") (cons LEFT_BRACE "{") (cons NEWLINE "\r\n") 
    (cons COMMENT "// Este codigo es importante ") (cons NEWLINE "\n") 
    (cons VARIABLE_KEYWORD "let") (cons IDENTIFIER "c") (cons OPERATOR "=") (cons NUMBER "3") (cons NEWLINE "\r\n") 
    (cons IDENTIFIER "c") (cons OPERATOR "=") (cons IDENTIFIER "i") (cons OPERATOR "+") (cons NUMBER "3") (cons NEWLINE "\r\n") 
    (cons NEWLINE "\r\n") 
    (cons RIGHT_BRACE "}") 
    (cons NEWLINE "\r\n") 
    (cons NEWLINE "\r\n") 
    (cons FUNCTION_KEYWORD "function") (cons IDENTIFIER "prueba") (cons LEFT_PARENTHESIS "(") (cons IDENTIFIER "a") (cons SEPARATOR ",") (cons IDENTIFIER "b") (cons RIGHT_PARENTHESIS ")") (cons LEFT_BRACE "{") (cons NEWLINE "\r\n") 
    (cons KEYWORD "return") (cons IDENTIFIER "a") (cons OPERATOR "+") (cons IDENTIFIER "b") (cons NEWLINE "\r\n") 
    (cons RIGHT_BRACE "}") (cons NEWLINE "\r\n") 
    (cons NEWLINE "\r\n") 

    ;(cons CICLE_KEYWORD "while") (cons LEFT_PARENTHESIS "(") (cons IDENTIFIER "a") (cons OPERATOR ">") (cons NUMBER "2") (cons RIGHT_PARENTHESIS ")") (cons LEFT_BRACE "{") (cons NEWLINE "\r\n") 
    ;(cons VARIABLE_KEYWORD "let") (cons IDENTIFIER "z") (cons OPERATOR "=") (cons IDENTIFIER "z") (cons OPERATOR "+") (cons IDENTIFIER "a") (cons NEWLINE "\r\n") 
    ;(cons RIGHT_BRACE "}") 
    (cons NEWLINE "\r\n") 
    (cons NEWLINE "\r\n")
    ))

(print (list-ref tokensEntrada 22))

;;; (print (es_P 
;;;     tokensEntrada
;;;     0
;;;     ))


