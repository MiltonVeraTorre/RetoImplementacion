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
(define ENV '())
; Función que agrega una variable al entorno
(define (add-to-env variable value)
  (set! ENV (cons (list variable value) ENV)))
; define-operation: Añade una nueva variable y su operación asociada al entorno
(define (define-operation identifier operation)
  (set! ENV (cons (list identifier operation) ENV)))

; set-operation: Cambia la operación asociada con una variable existente en el entorno
(define (set-operation identifier operation)
  (let ((found (assoc identifier ENV)))
    (if found
        (begin
          (set! ENV (remove found ENV))
          (add-to-env identifier operation))
        (error "Variable not defined: " identifier))))





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

      (let ((result (cond ((equal? (car (list-ref tokens indice)) VARIABLE_KEYWORD) (es_DV_Eval tokens indice)) ; Verificamos si se trata de una declaración de una variable
                           ((equal? (car (list-ref tokens indice)) IDENTIFIER) (es_AV_Eval tokens indice)) ; Verificamos si se trata de una asignación de una variable
                           (else
                             (list #f indice (list-ref tokens indice)) ; Si no se identifica ninguno de los casos seguimos buscando declaraciones de variables
                           ))))
        (if (car result)
            (es_D_Eval tokens (car result))
            (es_D_Eval tokens (+ indice 1)))) ; Si no se identifica ninguno de los casos seguimos buscando declaraciones de variables
      )
)



(define (es_DV_Eval tokens indice)
  (if (>= indice (length tokens)) 
     (list #f indice (list-ref tokens indice))
     (let ((token-type (car (list-ref tokens indice)))
           (token-value (cdr (list-ref tokens indice)))
           (token-identifier (cdr (list-ref tokens (+ indice 1)))))
       (if (and 
            (equal? token-type VARIABLE_KEYWORD) 
            (or (string=? token-value "let") (string=? token-value "const")) 
            (equal? (car (list-ref tokens (+ indice 1))) IDENTIFIER)
            (equal? (car (list-ref tokens (+ indice 2))) OPERATOR)
            (string=? (cdr (list-ref tokens (+ indice 2))) "="))
           (let* ((result-o (es_O_Eval tokens (+ indice 3)))
                  (new-index (car result-o))
                  (operation (cdr result-o)))
             (define-operation token-identifier (car operation))
             (list new-index (list token-identifier (car operation))))
           (list #f indice (list-ref tokens indice))
            )
      )
  )
)

(define (es_AV_Eval tokens indice)
  (if (>= indice (length tokens)) 
     (list #f indice (list-ref tokens indice))
     (let ((token-identifier (cdr (list-ref tokens indice))))
       (if (and 
            (equal? (car (list-ref tokens indice)) IDENTIFIER)
            (equal? (car (list-ref tokens (+ indice 1))) OPERATOR) 
            (string=? (cdr (list-ref tokens (+ indice 1))) "="))
           (let* ((result-o (es_O_Eval tokens (+ indice 2)))
                  (new-index (car result-o))
                  (operation (cdr result-o)))
             (set-operation token-identifier (car operation))
             (list new-index (list token-identifier (car operation))))
          (list #f indice (list-ref tokens indice))
           )
     )
  )
)


;; es_O: Verifica si el se forma una operación válida (O) según la gramática.
;;; O → E (OP E)*
(define (es_O_Eval tokens indice)
  (let* (
         (resultado-e1 (es_E_Eval tokens indice)) ; Obtenemos el indice y la expresión evaluada
         (nuevo_indice (car resultado-e1)) ; Guardamos el nuevo indice
         (valor-e1 (cadr resultado-e1)) ; Guardamos el valor de la expresión evaluada
        )
    (if (and 
             (number? nuevo_indice) ; Verificamos que el nuevo indice sea un número
             (equal? (car (list-ref tokens nuevo_indice)) OPERATOR) ; Verificamos que el siguiente token sea un operador
        )
        (let loop (
                   (indice_actual nuevo_indice) ; Definimos el indice actual
                   (operaciones (list valor-e1)) ; Definimos la lista de empezando por el valor que dio es_E_Eval
                  )
          (if (and
                (equal? (car (list-ref tokens indice_actual)) OPERATOR) ; Verificamos que el siguiente token sea un operador
                (number? (car (es_E_Eval tokens (+ indice_actual 1)))) ; Verificamos si devolvió un token valido
               )
              (let* (
                     (operador (cdr (list-ref tokens indice_actual))) ; Guardamos el operador
                     (resultado-e2 (es_E_Eval tokens (+ indice_actual 1))) ; Obtenemos el indice y la expresión evaluada
                     (siguiente_indice (car resultado-e2))  ; Guardamos el nuevo indice
                     (valor-e2 (cadr resultado-e2))        ; Guardamos el valor de la expresión evaluada
                     (nuevas_operaciones (append operaciones (list operador valor-e2))) ; agregamos el operador y el valor a la lista de operaciones
                    )
                (loop siguiente_indice nuevas_operaciones) ; Volvemos a llamar a loop con el nuevo indice y la nueva lista de operaciones
              )
              (list indice_actual operaciones) ; Si no se cumple la condición, devolvemos el indice actual y la lista de operaciones
          )
        )
        (list nuevo_indice (list valor-e1))) ; Si no hay operador, devuelve el índice y el valor de es_E_Eval
  )
)


; es_E: Verifica si el se forma una expresión válida (E) según la gramática.
;;; E → N | I 

(define (es_E_Eval tokens indice)
  (let ((token-actual (list-ref tokens indice))) ; Guardamos el token actual en una variable
    (cond ; Ejecutamos el condicional para saber si se cumple alguno de los criterios
      ((>= indice (length tokens)) (list #f indice token-actual))   ;; No hay más tokens por lo que se devuleve que es falso
      ((equal? (car token-actual) NUMBER) (list (+ indice 1) (string->number (cdr token-actual))))  ; Puede ser un numero
      ((equal? (car token-actual) IDENTIFIER) (list (+ indice 1) (cdr token-actual)))  ;; Puede ser un identificador
      (else (list #f indice token-actual))
    )
  )
)


(define tokensEntrada  (list 
                             
     (cons VARIABLE_KEYWORD "let") (cons IDENTIFIER "a") (cons OPERATOR "=") (cons NUMBER "5") (cons OPERATOR "+") (cons NUMBER "10") (cons OPERATOR "-") (cons NUMBER "2") (cons NEWLINE "\r\n") 

    (cons VARIABLE_KEYWORD "let") (cons IDENTIFIER "b") (cons OPERATOR "=") (cons NUMBER "10")  (cons NEWLINE "\r\n") 
    
    ;(cons VARIABLE_KEYWORD "let") (cons IDENTIFIER "c") (cons OPERATOR "=") (cons IDENTIFIER "a") (cons OPERATOR "+") (cons IDENTIFIER "b") (cons NEWLINE "\r\n") 
                                               
    ;(cons VARIABLE_KEYWORD "let") (cons IDENTIFIER "c") (cons OPERATOR "=") (cons NUMBER "30") (cons OPERATOR "+") (cons NUMBER "20") (cons NEWLINE "\r\n") 
    
    (cons NEWLINE "\r\n") 
    
    (cons FOR_KEYWORD "for") (cons LEFT_PARENTHESIS "(") (cons VARIABLE_KEYWORD "let") (cons IDENTIFIER "i") (cons OPERATOR "=") (cons NUMBER "5") (cons SEMICOLON ";") 
    (cons IDENTIFIER "i") (cons OPERATOR "<") (cons NUMBER "5") (cons SEMICOLON ";") (cons IDENTIFIER "i") (cons OPERATOR "=") (cons IDENTIFIER "i") (cons OPERATOR "+") (cons NUMBER "1") (cons RIGHT_PARENTHESIS ")") (cons LEFT_BRACE "{") (cons NEWLINE "\r\n") 
    (cons COMMENT "// Este codigo es importante ") (cons NEWLINE "\n") 
    (cons VARIABLE_KEYWORD "let") (cons IDENTIFIER "d") (cons OPERATOR "=") (cons NUMBER "3") (cons NEWLINE "\r\n") 
    (cons IDENTIFIER "d") (cons OPERATOR "=") (cons IDENTIFIER "i") (cons OPERATOR "+") (cons NUMBER "3") (cons NEWLINE "\r\n") 
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



; Ejecutamos nuestro agrupador de variables de entorno
(print (es_LD_Eval 
    tokensEntrada
    0
    ))
; En este punto nuestra lista de entorno tiene las variables agrupadas

; En este punto borraremos las variables que se refieren a si mismas ya que no soportamos esta funcionalidad
; Igualmente borramos las variables que se refieren a otras variables que se refieren a si mismas
(define (remove-self-and-mutual-references env)
  (let* ((self-references (filter (lambda (pair)
                                    (member (car pair) (flatten (cdr pair))))
                                  env))
         (self-refs-names (map car self-references)))
    (filter (lambda (pair)
              (not (ormap (lambda (ref)
                            (member ref (flatten (cdr pair))))
                          self-refs-names)))
            env)))

(set! ENV (remove-self-and-mutual-references ENV))


; Funciones encargadas de ejecutar las operaciones

; Aplica una operación a dos operandos
(define (apply-operator operator operand1 operand2)
  (case operator
    [("+") (+ operand1 operand2)]
    [("-") (- operand1 operand2)]
    [("*") (* operand1 operand2)]
    [("/") (/ operand1 operand2)]))

; Evalúa una lista de operaciones y operandos
(define (eval-operations operations)
  (let loop ((operands (filter number? operations)) ; Extraer operandos
             (operators (filter string? operations)) ; Extraer operadores
             (result (car operations)))
    (if (null? operators)
        result
        (loop (cdr operands)
              (cdr operators)
              (apply-operator (car operators) result (cadr operands))))))

; Evalúa una lista de expresiones
(define (eval-expr-list expr-list)
  (map (lambda (expr)
         (let ((var (car expr))
               (operations (cadr expr)))
           (cons var (eval-operations operations))))
       expr-list))

(display "Variables de entorno sin operaciones: ")
(newline)
(display ENV)
(newline)
(display "Variables de entorno con operaciones: ")
(print (eval-expr-list ENV))
;(print ENV)



