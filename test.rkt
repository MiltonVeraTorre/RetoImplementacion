#lang racket

(define COMMENT 'comment)
(define KEYWORD 'keyword)

(define FUNCTION_KEYWORD 'function_keyword)
(define VARIABLE_KEYWORD 'variable_keyword)
(define FOR_KEYWORD 'for_keyword)
(define WHILE_KEYWORD 'while_keyword)
(define RETURN_KEYWORD 'return_keyword)

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

(define ERROR 'error)

; Manejo del entorno del interprete

; Función que agrega una variable al entorno
(define (add-to-env variable value)
  (set! ENV (cons (list variable value) ENV)))

; Función que busca una variable en el entorno
(define (lookup-in-env variable)
  (let ((entry (assoc variable ENV)))
    (if entry
        (cadr entry)
        (error "Variable no definida: " variable))))

; Función que actualiza el valor de una variable en el entorno
(define (update-in-env variable value)
  (let ((entry (assoc variable ENV)))
    (if entry
        (set-cdr! entry (list value))
        (error "Variable no definida: " variable))))





(define (es_LD_Eval tokens indice)
  (cond 
      ; Si el índice es mayor que el número de tokens, entonces hemos alcanzado el final y devolvemos el índice actual.
      [(>= indice (length tokens)) indice]
      ; Si no hemos alcanzado el final, intentamos analizar una declaración (D).
      [else 
        (let ((resultado (es_D_Eval tokens indice))) ; Definimos la variable resultado que almacenará el resultado de es_D
              (cond ((number? resultado) ; Verificamos si el resultado que me dio es_D es un número ya que si no lo es es porque devolvió una lista
                     (es_LD_Eval tokens resultado)) ; Si es_D fue exitoso, intentamos analizar el resto de los tokens como una lista de declaraciones (LD)
                    ((equal? (car resultado) #t) ; Verificamos si el primer elemento de la lista es #t
                     (cadr resultado)) ; Si el primer elemento de la lista es #t, devolvemos el segundo elemento de la lista que es el índice
                    (else resultado) ; Si el primer elemento de la lista es #f, devolvemos la lista tal cual
              )
            )
      ]
  )
)
; es_D: Verifica si una lista de tokens forma una declaración válida (D) según la gramática.
;;; D → DV | F | C | COM
(define (es_D_Eval tokens indice)
  (if (>= indice (length tokens)) ; Verificamos si la longitud es mayor que la de la lista
     indice ; Si es mayor devolvemos el indice

      (let ((tmp (car (list-ref tokens indice)))) 
        (cond ((equal? tmp VARIABLE_KEYWORD) (es_DV_Eval tokens indice)) ; Verificamos si se trata de una declaración de una variable
              ((equal? tmp IDENTIFIER) (es_AV_Eval tokens indice)) ; Verificamos si se trata de una asignación de una variable
              (else
                (es_D_Eval tokens (+ indice 1)) ; Si no se identifica ninguno de los casos seguimos buscando declaraciones de variables
              )
        )
      )
  )
) 


; es_DV: Verifica si el se forma una declaración de variable válida (DV) según la gramática.
;;; DV → ("let" | "const") I "=" E
(define (es_DV_Eval tokens indice)
  (if (>= indice (length tokens)) ; Verificamos si se sobrepasa la longitud
     (list #f indice (list-ref tokens indice))
      ; Si hay más tokens para analizar, extraemos el tipo y el valor del token en el índice actual.
      (let 
           (
            (token-type (car (list-ref tokens indice))) ; Guardamos el tipo de token
            (token-value (cdr (list-ref tokens indice))) ; Guardamos el valor del token
            (token-identifier (cdr (list-ref tokens (+ indice 1))))
           )
        
        ; También verificamos si el siguiente token es un IDENTIFIER y si el token después de ese es un OPERATOR con valor "=".
        ; Si todas estas condiciones se cumplen, entonces tenemos el comienzo de una declaración de variable.
        (if (and 
                 (equal? token-type VARIABLE_KEYWORD) ; Primero verificamos que el token sea de tipo VARIABLE_KEYWORD
                 (or (string=? token-value "let") (string=? token-value "const")) ; Luego verificamos si el valor del token es let o const
                 (equal? (car (list-ref tokens (+ indice 1))) IDENTIFIER)  ; Luego verificamos si hay un identificador
                 (equal? (car (list-ref tokens (+ indice 2))) OPERATOR) ; Posteriormente debe de haber un operador
                 (string=? (cdr (list-ref tokens (+ indice 2))) "=")) ; Y este operador debe ser el operador igual

            ; Si las condiciones se cumplen, llamamos a la función es_O para ver si es una operación válida.
            (let* ((result-o (es_O_Eval tokens (+ indice 3)))
                   (new-index (car result-o))
                   (operation (cdr result-o)))
              (list new-index (list token-identifier (car operation))))
            ; Si alguna de las condiciones no se cumple, los tokens no forman una declaración de variable válida,
            ; por lo que devolvemos #f.
           (list #f indice (list-ref tokens indice))
            )
      )
  )
)

; es_AV: Verifica si el se forma una asignación de variable válida (AV) según la gramática.
;;; DV → ("let" | "const") I "=" O
(define (es_AV_Eval tokens indice)
  (if (>= indice (length tokens)) ; Verificamos si se sobrepasa la longitud
     (list #f indice (list-ref tokens indice))
     ; Si hay más tokens para analizar, extraemos el tipo y el valor del token en el índice actual.
     (let ((token-identifier (cdr (list-ref tokens indice)))) ; Guardamos el identificador de la variable
       ; También verificamos si el siguiente token es un OPERATOR con valor "=".
       ; Si todas estas condiciones se cumplen, entonces tenemos el comienzo de una asignación de variable.
       (if (and 
            (equal? (car (list-ref tokens indice)) IDENTIFIER)  ; Verificamos si hay un identificador
            (equal? (car (list-ref tokens (+ indice 1))) OPERATOR) ; Debe de haber un operador
            (string=? (cdr (list-ref tokens (+ indice 1))) "=")) ; Y este operador debe ser el operador igual

           ; Si las condiciones se cumplen, llamamos a la función es_O para ver si es una operación válida.
           (let* ((result-o (es_O_Eval tokens (+ indice 2)))
                  (new-index (car result-o))
                  (operation (cdr result-o)))
             (list new-index (list token-identifier operation)))
           ; Si alguna de las condiciones no se cumple, los tokens no forman una asignación de variable válida,
           ; por lo que devolvemos #f.
          (list #f indice (list-ref tokens indice))
           )
     )
  )
)


;; es_O: Verifica si el se forma una operación válida (O) según la gramática.
;;; O → E (OP E)*
(define (es_O_Eval tokens indice)
  (let* ((resultado-e1 (es_E_Eval tokens indice))  ; Intenta obtener un nuevo índice y expresión de es_E_Eval
         (nuevo_indice (car resultado-e1)) ; Guardamos el valor del nuevo indice
         (valor-e1 (cadr resultado-e1))) ; Guardamos el valor de la expresión
    (if (and (number? nuevo_indice)  ; Si es_E_Eval fue exitoso
             (equal? (car (list-ref tokens nuevo_indice)) OPERATOR)) ; El siguiente token es un operador
        (let loop ((indice_actual nuevo_indice) ; Inicia la función de loop con el nuevo índice como el índice actual
                   (operaciones (list valor-e1))) ; Almacena la primera expresión en la lista de operaciones
          (if (and
               (equal? (car (list-ref tokens indice_actual)) OPERATOR) ; El token actual es un operador
               (number? (car (es_E_Eval tokens (+ indice_actual 1))))) ; Y si hay otra expresión después del operador

              (let* ((operador (cdr (list-ref tokens indice_actual))) ; Almacena el operador
                     (resultado-e2 (es_E_Eval tokens (+ indice_actual 1))) ; Obtiene el índice y la expresión de es_E_Eval
                     (siguiente_indice (car resultado-e2))
                     (valor-e2 (cadr resultado-e2))
                     (nuevas_operaciones (append operaciones (list operador valor-e2)))) ; Añade el operador y la expresión a la lista de operaciones

                (loop (+ siguiente_indice 1) nuevas_operaciones)) ; Continúa con el loop con el nuevo índice y las operaciones actualizadas

              (list indice_actual operaciones))) ; Devuelve el índice actual y las operaciones cuando no hay más operaciones posibles
        (list nuevo_indice (list valor-e1))) ; Si no hay operador, devuelve el índice y el valor de es_E_Eval
  )
)

; es_E: Verifica si el se forma una expresión válida (E) según la gramática.
;;; E → N | I 

(define (es_E_Eval tokens indice)
  (let ((token-actual (list-ref tokens indice))) ; Guardamos el token actual en una variable
    (cond ; Ejecutamos el condicional para saber si se cumple alguno de los criterios
      ((>= indice (length tokens)) (list #f indice token-actual))   ;; No hay más tokens por lo que se devuleve que es falso
      ((equal? (car token-actual) NUMBER) (list (+ indice 1) (cdr token-actual)))  ; Puede ser un numero
      ((equal? (car token-actual) IDENTIFIER) (list (+ indice 1) (cdr token-actual)))  ;; Puede ser un identificador
      (else (list #f indice token-actual))
    )
  )
)


(define tokensEntrada  (list 
                             
     (cons IDENTIFIER "a") (cons OPERATOR "=") (cons NUMBER "5") (cons OPERATOR "+") (cons NUMBER "10") (cons NEWLINE "\r\n") 

    (cons VARIABLE_KEYWORD "let") (cons IDENTIFIER "b") (cons OPERATOR "=") (cons NUMBER "10") (cons NEWLINE "\r\n") 
                                               
    ;(cons VARIABLE_KEYWORD "let") (cons IDENTIFIER "c") (cons OPERATOR "=") (cons NUMBER "30") (cons OPERATOR "+") (cons NUMBER "20") (cons NEWLINE "\r\n") 
    
    (cons NEWLINE "\r\n") 
    
    (cons FOR_KEYWORD "for") (cons LEFT_PARENTHESIS "(") (cons VARIABLE_KEYWORD "let") (cons IDENTIFIER "i") (cons OPERATOR "=") (cons NUMBER "5") (cons SEMICOLON ";") 
    (cons IDENTIFIER "i") (cons OPERATOR "<") (cons NUMBER "5") (cons SEMICOLON ";") (cons IDENTIFIER "i") (cons OPERATOR "=") (cons IDENTIFIER "i") (cons OPERATOR "+") (cons NUMBER "1") (cons RIGHT_PARENTHESIS ")") (cons LEFT_BRACE "{") (cons NEWLINE "\r\n") 
    (cons COMMENT "// Este codigo es importante ") (cons NEWLINE "\n") 
    (cons VARIABLE_KEYWORD "let") (cons IDENTIFIER "c") (cons OPERATOR "=") (cons NUMBER "3") (cons NEWLINE "\r\n") 
    (cons IDENTIFIER "c") (cons OPERATOR "=") (cons IDENTIFIER "i") (cons OPERATOR "+") (cons NUMBER "3") (cons NEWLINE "\r\n") 
    (cons NEWLINE "\r\n") 
    (cons RIGHT_BRACE "}") 
    (cons NEWLINE "\r\n") 
    (cons NEWLINE "\r\n") 
    (cons FUNCTION_KEYWORD "function") (cons IDENTIFIER "prueba") (cons LEFT_PARENTHESIS "(") (cons IDENTIFIER "a") (cons SEPARATOR ",") (cons IDENTIFIER "b") (cons RIGHT_PARENTHESIS ")") (cons LEFT_BRACE "{") (cons NEWLINE "\r\n") 
    (cons RETURN_KEYWORD "return") (cons IDENTIFIER "a") (cons OPERATOR "+") (cons IDENTIFIER "b") (cons NEWLINE "\r\n") 
    (cons RIGHT_BRACE "}") (cons NEWLINE "\r\n") 
    (cons NEWLINE "\r\n") 

    (cons WHILE_KEYWORD "while") (cons LEFT_PARENTHESIS "(") (cons IDENTIFIER "a") (cons OPERATOR ">") (cons NUMBER "2") (cons RIGHT_PARENTHESIS ")") (cons LEFT_BRACE "{") (cons NEWLINE "\r\n") 
    (cons VARIABLE_KEYWORD "let") (cons IDENTIFIER "z") (cons OPERATOR "=") (cons IDENTIFIER "z") (cons OPERATOR "+") (cons IDENTIFIER "a") (cons NEWLINE "\r\n") 
    (cons RIGHT_BRACE "}") 
    (cons NEWLINE "\r\n") 
    (cons NEWLINE "\r\n")
    ))

(print (list-ref tokensEntrada 6))

(print (es_DV_Eval 
    tokensEntrada
    0
    ))


