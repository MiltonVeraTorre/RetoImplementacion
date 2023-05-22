#lang racket


;;;;;;;;;;;;;;;;;; DEFINICION DE LOS TOKENS ;;;;;;;;;;;;;;;;;;;;;;;;

; Define los tipos de tokens que vamos a manejar
; Cada uno de estos representa un tipo diferente de token en el lenguaje que estamos analizando.
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

;;;;;;;;;;;;;;;;;; TOKENIZADOR ;;;;;;;;;;;;;;;;;;;;;;;;

; Documentación general de la funcion de tokenize(flujo de la funcion)

;;; 1) Se define una función auxiliar recursiva inner que toma un string y una lista de tokens acumulados como argumentos.

;;; 2) inner examina el principio del string y verifica una serie de condiciones para determinar qué tipo de token (si corresponde) comienza el string. Cada condición corresponde a un tipo diferente de token que podría aparecer al principio del string. Las condiciones se verifican en un orden específico, por lo que si varias condiciones podrían ser verdaderas, se elige la primera.

;;; 3) Se examina cada condición y en caso de ser verdadera se genera el token y se llama inner con un substring que ya no incluya el token que ya se reconoció

;;; 4) Finalmente, se llama a inner con el string de entrada completo y una lista vacía de tokens acumulados.



; Definimos la funcion tokenize que tokeniza un string
(define (tokenize str)
  ; Declarando la función interna 'inner' que se llama a sí misma recursivamente
  ; Letrec permite definir variables que se pueden llamar entre sí en este caso es para que inner se pueda llamar a sí misma sin estar completamente definida
  (letrec [(inner ; inner toma un string y una lista de tokens acumulados
      (lambda (str acc) 
                    (cond ; Comenzamos con una condición

                          ; Si el string es vacío, terminamos la recursión y devolvemos los tokens acumulados en orden inverso
                          ; Se devuelven en orden inverso ya que se van agregando al inicio de la lista y para obtenerlo en su orden original hay que invertirla
                          [(string=? str "") (reverse acc)] 

                          ; Si el string comienza con un salto de línea "\n", generamos un token NEWLINE y llamamos a inner de nuevo
                          [(string-prefix? str "\n") 
                           (inner (substring str 1) (cons (cons NEWLINE "\n") acc))]
                           
                          ; Si el string comienza con un salto de línea "\r\n", generamos un token NEWLINE y llamamos a inner de nuevo
                          [(string-prefix? str "\r\n") 
                           (inner (substring str 2) (cons (cons NEWLINE "\r\n") acc))]

                          ; Si el string comienza con un espacio " ", lo ignoramos y llamamos a inner de nuevo
                          [(string-prefix? str " ") 
                           (inner (substring str 1) acc)]
                          
                          ; Si el string comienza con un comentario "//", generamos un token COMMENT y llamamos a inner de nuevo
                          [(string-prefix? str "//") 

                          ; Usamos let* para definir múltiples variables que pueden referirse entre sí
                           (let* 
                                ; Definimos la variable comment-end que representa el final del comentario en el string
                                [(comment-end ; Buscamos el final del comentario
                                      ; Usamos el operador 'or' para devolver el primer valor que no sea falso
                                      (or 
                                            (and 
                                               ; 'regexp-match-positions' busca en el string (str) una coincidencia con la expresión regular
                                               ; En este caso, la expresión regular busca los caracteres de fin de línea (\r o \n)
                                              (regexp-match-positions #rx"[\r\n]" str) 
                                              ; Si hay una coincidencia, 'caar' extrae la posición del primer carácter de la primera coincidencia 
                                              (caar (regexp-match-positions #rx"[\r\n]" str))
                                              )
                                      ; Si no hay coincidencia, devuelve la longitud del string
                                      ; Esto significa que el comentario llega hasta el final del string
                                      (string-length str)
                                      )
                                      ) 
                                 ; Definimos la variable comment que representa el comentario extraído del string
                                 (comment 
                                      (substring str 0 comment-end)) ; Extraemos el comentario del string
                                      ] 
                            ; Llamamos a la función inner de nuevo, pero esta vez el string que le pasamos comienza después del comentario que acabamos de procesar
                            ; Además, añadimos el comentario que acabamos de procesar a la lista de tokens acumulados (acc)
                             (inner 
                                  (substring str (+ 1 comment-end)) 
                                  (cons (cons COMMENT comment) acc)
                             )
                             )
                             ] 


                          [(regexp-match? #rx"^[0-9]" str) 
                            (let* 
                              [ ; Comienza la lista de declaraciones de variables
                                (number-end ; Declaramos la variable 'number-end'
                                  (or ; Usamos el operador 'or' para asignar el primer valor que no sea falso
                                    (and ; Busca la primera ocurrencia de un espacio en blanco en la cadena 'str'.
                                      (regexp-match-positions #rx"[^0-9.]" str) ; Devuelve una lista de posiciones donde se encuentra el patrón. En este caso, estamos buscando cualquier carácter que no sea un dígito o un punto.
                                      (caar (regexp-match-positions #rx"[^0-9.]" str)) ; Extraemos la primera posición de la primera coincidencia. 'caar' se usa para obtener el primer elemento de la primera sublista.
                                    )
                                    (string-length str) ; Si no se encuentra un espacio en blanco, tomamos la longitud total de la cadena 'str'
                                  )
                                )
                                (number (substring str 0 number-end)) ; Extraemos el número de la cadena 'str' tomando una subcadena desde el inicio hasta 'number-end'  
                              ]
                              (inner (substring str number-end) (cons (cons NUMBER number) acc)) ; Llamamos a la función 'inner' de forma recursiva, proporcionando una subcadena de 'str' que comienza en 'number-end' y agregando el número a la lista de tokens 'acc'.
                            )
                          ]

                          ; Si el string comienza con una palabra clave "let", "const", "function", "for", generamos un token KEYWORD y llamamos a inner de nuevo
                          [(string-prefix? str "let ") ; string-prefix? devuelve #t si el string comienza con el substring que le pasamos
                           (inner 
                                  (substring str 4)  ; Esto extrae una subcadena de 'str' empezando desde el cuarto índice (0-based), esencialmente eliminando "let " del inicio del string
                                  (cons (cons VARIABLE_KEYWORD "let") acc)  ; Crea una nueva pareja donde el primer elemento es otra pareja (KEYWORD "let") y el segundo elemento es 'acc', que es el acumulador que almacena los tokens generados hasta ahora.
                            )
                          ] 

                          [(string-prefix? str "const ") 
                           (inner (substring str 6) (cons (cons VARIABLE_KEYWORD "const") acc))]
                          [(string-prefix? str "function ") 
                           (inner (substring str 9) (cons (cons FUNCTION_KEYWORD "function") acc))]
                          [(string-prefix? str "for") 
                           (inner (substring str 3) (cons (cons FOR_KEYWORD "for") acc))]
                          [(string-prefix? str "while") 
                           (inner (substring str 5) (cons (cons WHILE_KEYWORD "while") acc))]
                          [(string-prefix? str "return") 
                           (inner (substring str 6) (cons (cons RETURN_KEYWORD "return") acc))]

                          ; Si el string comienza con un operador "=", "+", "-", "*", "/", "==", "<", ">", generamos un token OPERATOR y llamamos a inner de nuevo
                          [(string-prefix? str "= ") 
                           (inner (substring str 2) (cons (cons OPERATOR "=") acc))]
                          [(string-prefix? str "+ ") 
                           (inner (substring str 2) (cons (cons OPERATOR "+") acc))]
                          [(string-prefix? str "- ") 
                           (inner (substring str 2) (cons (cons OPERATOR "-") acc))]
                          [(string-prefix? str "* ") 
                           (inner (substring str 2) (cons (cons OPERATOR "*") acc))]
                          [(string-prefix? str "/ ") 
                           (inner (substring str 2) (cons (cons OPERATOR "/") acc))]
                          [(string-prefix? str "== ") 
                           (inner (substring str 3) (cons (cons OPERATOR "==") acc))]
                          [(string-prefix? str "< ") 
                           (inner (substring str 2) (cons (cons OPERATOR "<") acc))]
                          [(string-prefix? str "> ") 
                           (inner (substring str 2) (cons (cons OPERATOR ">") acc))]

                           ; Si el string comienza con un paréntesis o llave, generamos un token PARENTHESIS o BRACE y llamamos a inner de nuevo
                          [(string-prefix? str "(") 
                           (inner (substring str 1) (cons (cons LEFT_PARENTHESIS "(") acc))]
                          [(string-prefix? str "( ") 
                           (inner (substring str 2) (cons (cons LEFT_PARENTHESIS "(") acc))]

                          [(string-prefix? str ")") 
                           (inner (substring str 1) (cons (cons RIGHT_PARENTHESIS ")") acc))]
                          [(string-prefix? str ") ") 
                           (inner (substring str 2) (cons (cons RIGHT_PARENTHESIS ")") acc))]

                          [(string-prefix? str "{") 
                           (inner (substring str 1) (cons (cons LEFT_BRACE "{") acc))]
                          [(string-prefix? str "{ ") 
                           (inner (substring str 2) (cons (cons LEFT_BRACE "{") acc))]


                          [(string-prefix? str "}") 
                           (inner (substring str 1) (cons (cons RIGHT_BRACE "}") acc))]
                          [(string-prefix? str "} ") 
                           (inner (substring str 2) (cons (cons RIGHT_BRACE "}") acc))]

                          [(string-prefix? str "; ") 
                           (inner (substring str 2) (cons (cons SEMICOLON ";") acc))]
                          [(string-prefix? str ";") 
                           (inner (substring str 1) (cons (cons SEMICOLON ";") acc))]

                          [(string-prefix? str ", ") 
                           (inner (substring str 2) (cons (cons SEPARATOR ",") acc))]
                          [(string-prefix? str ",") 
                           (inner (substring str 1) (cons (cons SEPARATOR ",") acc))]


                          [(string-prefix? str "!= ") 
                           (inner 
                                (substring str 3) 
                                (cons (cons OPERATOR "!=") acc)
                           )
                          ]
                          ; Si ninguna de las condiciones anteriores es cierta, asumimos que el string comienza con un identificador
                          [else 
                           (let* ; Declaramos variables locales en el bloque
                            [ ; Comienza la lista de declaraciones de variables
                                  (identifier-end ; Declaramos la variable 'identifier-end'
                                          (or ; Usamos el operador 'or' para asignar el primer valor que no sea falso
                                              (and ; Busca la primera ocurrencia de un espacio en blanco en la cadena 'str'.
                                                  (regexp-match-positions #rx"[ ]" str) ; Devuelve una lista de posiciones donde se encuentra el patrón. En este caso, estamos buscando espacios en blanco.
                                                  (caar (regexp-match-positions #rx"[ ]" str))  ; Extraemos la primera posición de la primera coincidencia. 'caar' se usa para obtener el primer elemento de la primera sublista.
                                              )
                                              (string-length str) ; Si no se encuentra un espacio en blanco, tomamos la longitud total de la cadena 'str'
                                            )
                                  )
                                  (identifier (substring str 0 identifier-end)) ; Extraemos el identificador de la cadena 'str' tomando una subcadena desde el inicio hasta 'identifier-end'  
                              ]
                              (inner (substring str identifier-end) (cons (cons IDENTIFIER identifier) acc)) ; Llamamos a la función 'inner' de forma recursiva, proporcionando una subcadena de 'str' que comienza en 'identifier-end' y agregando el identificador a la lista de tokens 'acc'.
                            )
                            ]
                             )))]
                              (inner str '())))

;;;;;;;;;;;;;;;;; ANALIZADOR DE LA GRAMATICA ;;;;;;;;;;;;;;;;;;;;;;;;


;;; P → LD

;;; LD → D LD | ε

;;; D → DV | F | C | COM

;;; DV → ("let" | "const") I "=" E 
;;; AV →  I "=" E 

;;; E → N | I | "(" E ")" | O

;;; O → E (OP E)*

;;; OP → "+" | "-" | "*" | "/" | "==" | "<" | ">" | "!="

;;; FOR → "for" "(" DV ";" O ";" AV ")" "{" LD "}"
;; WHILE → "while" "(" O ")" "{" LD "}"

;;; F → "function" I "(" LI ")" "{" LD "}"

;;; RD → "return" O

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
;;; RD = retorno de funcion
;;; FOR = ciclo for
;;; WHILE = ciclo while
;;; E = expresion
;;; O = operacion

;;; N = numero
;;; I = identificador
;;; COM = comentario{
;;; OP = Operador

;;; LI = lista de identificadores




; es_P:  Verifica si un lista de tokens forma un programa válido (P) según la gramática.
;;; P → LD
(define (es_P tokens indice)

  (es_LD tokens indice) ; Se llama la función es_LD
)

; es_LD: Verifica si una lista de declaraciones (LD) es válida según la gramática.
; Esta función es la encargada de ir recorriendo las declaraciones e ir devolviendo el ultimo indice hasta el cual llego la ultima declaración
;;; LD → D LD | ε
(define (es_LD tokens indice)
  (cond 
      ; Si el índice es mayor que el número de tokens, entonces hemos alcanzado el final y devolvemos el índice actual.
      [(>= indice (length tokens)) indice]
      ; Si no hemos alcanzado el final, intentamos analizar una declaración (D).
      [else 
        (let ((resultado (es_D tokens indice))) ; Definimos la variable resultado que almacenará el resultado de es_D
              (cond ((number? resultado) ; Verificamos si el resultado que me dio es_D es un número ya que si no lo es es porque devolvió una lista
                     (es_LD tokens resultado)) ; Si es_D fue exitoso, intentamos analizar el resto de los tokens como una lista de declaraciones (LD)
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
(define (es_D tokens indice)
  (if (>= indice (length tokens)) ; Verificamos si la longitud es mayor que la de la lista
     indice ; Si es mayor devolvemos el indice

      (let ((tmp (car (list-ref tokens indice)))) 
        (cond ((equal? tmp VARIABLE_KEYWORD) (es_DV tokens indice)) ; Verificamos si se trata de una declaración de una variable
              ((equal? tmp IDENTIFIER) (es_AV tokens indice)) ; Verificamos si se trata de una asignación de una variable
              ((equal? tmp FUNCTION_KEYWORD) (es_F tokens indice)) ; Verificamos si se trata de una función
              ((equal? tmp FOR_KEYWORD) (es_FOR tokens indice)) ; Verificamos si se trata de un ciclo for
              ((equal? tmp WHILE_KEYWORD) (es_WHILE tokens indice)) ; Verificamos si se trata de un ciclo while
              ((equal? tmp COMMENT) (es_D tokens (+ indice 1))) ; Verificamos si se trata de un comentario
              ((equal? tmp NEWLINE) (es_D tokens (+ indice 1))) ; Si se trata de un espacio en blanco volvemos a llamar a es_D
              ((equal? tmp RIGHT_BRACE) (list #t indice)) ; Si se trata de un una llave de cierre dejamos de bsucar declaraciones
              ((equal? tmp RETURN_KEYWORD) (es_D tokens (es_RD tokens indice))) ; Si es el return de una función detectamos su longitud y volvemos a llamar a es_D
              (else(list #f indice (list-ref tokens indice))))))) ; Si no se identifica ninguno de los casos devolvemos que es falso y no se encontró una declaración valida


; es_DV: Verifica si el se forma una declaración de variable válida (DV) según la gramática.
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
                 (equal? token-type VARIABLE_KEYWORD) ; Primero verificamos que el token sea de tipo VARIABLE_KEYWORD
                 (or (string=? token-value "let") (string=? token-value "const")) ; Luego verificamos si el valor del token es let o const
                 (equal? (car (list-ref tokens (+ indice 1))) IDENTIFIER)  ; Luego verificamos si hay un identificador
                 (equal? (car (list-ref tokens (+ indice 2))) OPERATOR) ; Posteriormente debe de haber un operador
                 (string=? (cdr (list-ref tokens (+ indice 2))) "=")) ; Y este operador debe ser el operador igual

            ; Si las condiciones se cumplen, llamamos a la función es_O para ver si es una operación válida.
            (es_O tokens (+ indice 3))
            ; Si alguna de las condiciones no se cumple, los tokens no forman una declaración de variable válida,
            ; por lo que devolvemos #f.
           (list #f indice (list-ref tokens indice))
            )
      )
  )
)
; es_AV: Verifica si el se forma una asignación de variable válida (AV) según la gramática.
;;; DV → ("let" | "const") I "=" O
(define (es_AV tokens indice)
  (if (>= indice (length tokens)) ; Verificamos si se sobrepasa la longitud
     (list #f indice (list-ref tokens indice))

        ; También verificamos si el siguiente token es un IDENTIFIER y si el token después de ese es un OPERATOR con valor "=".
        ; Si todas estas condiciones se cumplen, entonces tenemos el comienzo de una declaración de variable.
        (if (and 
                 
                 (equal? (car (list-ref tokens indice)) IDENTIFIER)  ; Luego verificamos si hay un identificador
                 (equal? (car (list-ref tokens (+ indice 1))) OPERATOR) ; Posteriormente debe de haber un operador
                 (string=? (cdr (list-ref tokens (+ indice 1))) "=")) ; Y este operador debe ser el operador igual

            ; Si las condiciones se cumplen, llamamos a la función es_O para ver si es una operación válida.
            (es_O tokens (+ indice 2))
            ; Si alguna de las condiciones no se cumple, los tokens no forman una declaración de variable válida,
            ; por lo que devolvemos #f.
           (list #f indice (list-ref tokens indice))
            )
      
  )
)

; es_RD: Verifica si el se forma un retorno de función válido (RD) según la gramática.
;;; RD → "return" O
(define (es_RD tokens indice)
  (if (>= indice (length tokens)) ; Verificamos si se sobrepasa la longitud
     (list #f indice (list-ref tokens indice))        
        (if (equal? (car (list-ref tokens indice)) RETURN_KEYWORD)  ; Luego verificamos si es el identificador return
            
            ; Si las condiciones se cumplen, llamamos a la función es_O para analizar la expresión que sigue.
            (es_O tokens (+ indice 1))
            ; Si alguna de las condiciones no se cumple, los tokens no forman una declaración de variable válida,
            ; por lo que devolvemos #f.
           (list #f indice (list-ref tokens indice))
        )
      
  )
)


; es_E: Verifica si el se forma una expresión válida (E) según la gramática.
;;; E → N | I | "(" E ")"

(define (es_E tokens indice)
  (cond ; Ejecutamos el condicional para saber si se cumple alguno de los criterios
        ((>= indice (length tokens)) (list #f indice (list-ref tokens indice)))   ;; No hay más tokens por lo que se devuleve que es falso
        ((equal? (car (list-ref tokens indice)) NUMBER) (+ indice 1))  ; Puede ser un numero
        ((equal? (car (list-ref tokens indice)) IDENTIFIER) (+ indice 1))  ;; Puede ser un identificador
        ((and ; El ultimo caso es en el que este envuelto en parentesis
              (equal? (cdr (list-ref tokens indice)) "(")  ;; En caso de que no sea uno de los anteriores entonces puede ser una expresión en parentesis
              (es_E tokens (+ indice 1)) ;; 
              (equal? (cdr (list-ref tokens (+ indice 2))) ")"))
         (+ indice 3))
        (else(list #f indice (list-ref tokens indice)))))


;; es_O: Verifica si el se forma una operación válida (O) según la gramática.
;;; O → E (OP E)*
(define (es_O tokens indice)
  (let ((nuevo_indice (es_E tokens indice))) ; Intenta obtener un nuevo índice de es_E
    (if (number? nuevo_indice) ; Si es_E fue exitoso
        (if (and 
                 (equal? (car (list-ref tokens nuevo_indice)) OPERATOR) ; El siguiente token es un operador
                 (number? (es_E tokens (+ nuevo_indice 1))) ; Y si hay otra expresión después del operador
            ) 
            (let loop ((indice_actual nuevo_indice)) ; inicializa la función de loop con el nuevo índice como el índice actual
              (if (and
                   (equal? (car (list-ref tokens indice_actual)) OPERATOR) ; El token actual es un operador
                   (number? (es_E tokens (+ indice_actual 1))) ; Y si hay otra expresión después del operador
                  ) 

                    (loop (+ indice_actual 2)) ; Sigue haciendo operaciones si se cumplieron las condiciones

                  indice_actual ; Devuelve el índice actual cuando no hay más operaciones posibles
              )
            ) 
            nuevo_indice) ; Si no hay operador, devuelve el índice de es_E
       (list #f indice (list-ref tokens indice)) ; Si es_E no fue exitoso, devuelve #f
    )
  )
)



; es_FOR: Verifica si el se forma un ciclo for válido (FOR) según la gramática.
;;; FOR → "for" "(" DV O ";" AV ")" "{" LD "}"

(define (es_FOR tokens indice)
  (if (and (equal? (cdr (list-ref tokens indice)) "for")      ; Verificamos si el token actual es un for
           (equal? (cdr (list-ref tokens (+ indice 1))) "(")  ; Verificamos si el siguiente token es un parentesis izquierdo
      )
      (let* (
              (indice-1 (es_DV tokens (+ indice 2)))          ; Llamamos a es_DV para verificar si es una declaración de variable
              (indice-2 (if (and                             
                                (number? indice-1)            ; Verificamos si el resultado de es_DV es un número
                                (equal? (cdr (list-ref tokens indice-1)) ";")   ; Verificamos si el siguiente token es un punto y coma
                            )
                            (es_O tokens (+ indice-1 1))        ; Llamamos a es_O para verificar si es una operación
                            (list #f indice (list-ref tokens indice))    ; Si no es un punto y coma devolvemos que es falso
                          )
              )
              (indice-3 (if (and 
                                (number? indice-2)               ; Verificamos si el resultado de es_O es un número
                                (equal? (cdr (list-ref tokens indice-2)) ";")  ; Verificamos si el siguiente token es un punto y coma
                            )
                            (es_AV tokens (+ indice-2 1))        ; Llamamos a es_AV para verificar si es una asignación de variable
                            (list #f indice (list-ref tokens indice))  ; Si no es un punto y coma devolvemos que es falso
                        )
              )
              (indice-4 (if (and 
                                (number? indice-3)               ; Verificamos si el resultado de es_AV es un número
                                (equal? (cdr (list-ref tokens indice-3)) ")")     ; Verificamos si el siguiente token es un parentesis derecho
                            )
                            (if (equal? (cdr (list-ref tokens (+ indice-3 1))) "{")   ; Verificamos si el siguiente token es una llave de apertura
                                (es_LD tokens (+ indice-3 2))         ; Llamamos a es_LD para verificar si es una lista de declaraciones
                                (list #f indice (list-ref tokens indice))    ; Si no es una llave de apertura devolvemos que es falso
                            )
                            (list #f indice (list-ref tokens indice))

                          )
              )
             )
        (if (and 
                (number? indice-4)   ; Verificamos si el resultado de es_LD es un número
                (equal? (cdr (list-ref tokens indice-4)) "}")    ; Verificamos si el siguiente token es una llave de cierre
            )

            (+ indice-4 1)       ; Si es así devolvemos el índice actual más uno

            (list #f indice (list-ref tokens indice)))   ; Si no es una llave de cierre devolvemos que es falso
      )

      (list #f indice (list-ref tokens indice))   ; Si no es un parentesis derecho devolvemos que es falso
      
  )
)


; es_WHILE: Verifica si el se forma un ciclo while válido (WHILE) según la gramática.
;;; WHILE → "while" "(" O ")" "{" LD "}"

(define (es_WHILE tokens indice)

    (if (and 
            (equal? (car (list-ref tokens indice)) WHILE_KEYWORD) ; Verificamos si el token actual es un while
            (equal? (cdr (list-ref tokens (+ indice 1))) "(")     ; Verificamos si el siguiente token es un parentesis izquierdo
        )    
        (let* (
                (indice-1 (es_O tokens (+ indice 2)))             ; Llamamos a es_O para verificar si es una operación
                (indice-2 (if (and 
                                  (number? indice-1) 
                                  (equal? (cdr (list-ref tokens indice-1)) ")"); Verificamos si después hay un parentes´si de cierre
                              ) 
                                (if (equal? (cdr (list-ref tokens (+ indice-1 1))) "{") ; Verificamos si después hay una llave de apertura
                                  (es_LD tokens (+ indice-1 2)) ; Llamamos a es_LD para verificar si es una lista de declaraciones
                                  (list #f indice (list-ref tokens indice)) ; Si no es una llave de apertura devolvemos que es falso
                                )

                                (list #f indice (list-ref tokens indice)) ; Si no es un parentesis de cierre devolvemos que es falso
                          )
                )
              )

            (if (and 
                    (number? indice-2) 
                    (equal? (cdr (list-ref tokens indice-2)) "}") ; Verificamos si después hay una llave de cierre
                )

                (+ indice-2 1) ; Si es así devolvemos el índice actual más uno

                (list #f indice (list-ref tokens indice)) ; Si no es una llave de cierre devolvemos que es falso
            )
        ) 
        (list #f indice (list-ref tokens indice)))) ; Si no es un parentesis izquierdo devolvemos que es falso



;; es_OP: Verifica si el se forma un operador válido (OP) según la gramática.
;;; OP → "+" | "-" | "*" | "/" | "==" | "<" | ">" | "!="

(define (es_OP tokens indice)
  (if (and 
           (>= indice (length tokens)) ; Verificamos si se sobrepasa la longitud
           (equal? (car (list-ref tokens indice)) OPERATOR) ; Verificamos si el token actual es un operador
      ) 
        (+ indice 1)
        (list #f indice (list-ref tokens indice))
  )
)


;; es_F: Verifica si el se forma una función válida (F) según la gramática.
;;; F → "function" I "(" LI ")" "{" LD "}"

(define (es_F tokens indice)
  (if (and 
           (equal? (cdr (list-ref tokens indice)) "function")  ; Verificamos si el token actual es un function
           (equal? (car (list-ref tokens (+ indice 1))) IDENTIFIER)  ; Verificamos si el siguiente token es un identificador
           (equal? (cdr (list-ref tokens (+ indice 2))) "("))        ; Verificamos si el siguiente token es un parentesis izquierdo
      (let* (
            (nuevo_indice (es_LI tokens (+ indice 3)))               ; Llamamos a es_LI para verificar si es una lista de identificadores
            (nuevo_indice_2 (if (and 
                                    (number? nuevo_indice)           ; Verificamos si el resultado de es_LI es un número
                                    (equal? (cdr (list-ref tokens nuevo_indice)) ")")   ; Verificamos si el siguiente token es un parentesis derecho
                                    (equal? (cdr (list-ref tokens (+ nuevo_indice 1))) "{")  ; Verificamos si el siguiente token es una llave de apertura
                                )
                                 (es_LD tokens (+ nuevo_indice 2))                           ; Llamamos a es_LD para verificar si es una lista de declaraciones
                                 (list #f indice (list-ref tokens indice))                   ; Si no es una llave de apertura devolvemos que es falso
                            )
            )
            )
        (if (and 
                 (number? nuevo_indice_2)   ; Verificamos si el resultado de es_LD es un número
                 (equal? (cdr (list-ref tokens nuevo_indice_2)) "}")  ; Verificamos si el siguiente token es una llave de cierre
            )

              (+ nuevo_indice_2 1)  ; Si es así devolvemos el índice actual más uno
              (list #f indice (list-ref tokens indice))     ; Si no es una llave de cierre devolvemos que es falso
        )
      )

     (list #f indice (list-ref tokens indice))))

;; es_LI: vector indice -> numero / #f
;;; LI → I ("," LI)* | ε

(define (es_LI tokens indice)
  (if (and (>= indice (length tokens))
           (not (equal? (car (list-ref tokens indice)) IDENTIFIER)))
      (list #f indice (list-ref tokens indice)) ; Si no hay más tokens o el token actual no es un identificador, fallamos

      (let loop ((indice_actual indice)) ; Comenzamos un bucle con el índice actual
        (if (and 
                 (equal? (car (list-ref tokens indice_actual)) IDENTIFIER) ; El token actual es un identificador
                 (equal? (cdr (list-ref tokens (+ indice_actual 1))) ",")) ; El siguiente token es una coma

            (loop (+ indice_actual 2)) ; Continuamos con el siguiente identificador

            (if (equal? (car (list-ref tokens indice_actual)) IDENTIFIER) ; Si el token actual es un identificador
                (+ indice_actual 1) ; terminamos aquí, incrementando el índice en uno
                (list #f indice (list-ref tokens indice)) ; Si el token actual no es un identificador, fallamos
            )
        )
      )
  )
) 
;;;;;;;;;;;;;;;;;; INTERPRETE ;;;;;;;;;;;;;;;;;;;;;;;;

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

; Aplica un operador a dos operandos
(define (apply-operator operator operand1 operand2)
  (case operator
    [("+") (+ operand1 operand2)]
    [("-") (- operand1 operand2)]
    [("*") (* operand1 operand2)]
    [("/") (/ operand1 operand2)]))

; Evalúa una lista de operaciones y operandos
(define (eval-operations operations)
  ;; Primero, maneja las multiplicaciones y divisiones
  (let* ((intermediate-ops (handle-mult-div operations '()))
         ;; Luego, maneja las sumas y restas
         (final-result (handle-add-sub intermediate-ops '())))
    final-result))

; Maneja las multiplicaciones y divisiones
(define (handle-mult-div operations result)
  (if (null? operations)
      (reverse result)
      (let ((first (car operations))
            (rest (cdr operations)))
        (if (and (string? first) (or (string=? first "*") (string=? first "/")))
            (let ((new-result (apply-operator first (car result) (car rest))))
              (handle-mult-div (cdr rest) (cons new-result (cdr result))))
            (handle-mult-div rest (cons first result))))))

; Maneja las sumas y restas
(define (handle-add-sub operations result)
  (if (null? operations)
      (car result)
      (let ((first (car operations))
            (rest (cdr operations)))
        (if (and (string? first) (or (string=? first "+") (string=? first "-")))
            (let ((new-result (apply-operator first (car result) (car rest))))
              (handle-add-sub (cdr rest) (cons new-result (cdr result))))
            (handle-add-sub rest (cons first result))))))

; Evalúa una lista de expresiones
(define (eval-expr-list expr-list)
  (map (lambda (expr)
         (let ((var (car expr))
               (operations (cadr expr)))
           (cons var (eval-operations operations))))
       expr-list))







; Obtiene el valor de una variable en el entorno
(define (get-env-variable env var)
  (let ((pair (assoc var env))) 
    (if pair 
        (cdr pair) 
        (error "Variable not found in environment: " var))))





;;;;;;;;;;;;;;;;;; GENERADOR DE HTML USANDO LOS TOKENS ;;;;;;;;;;;;;;;;;;;;;;;;

; Función para generar HTML a partir de tokens
; Esta función toma una lista de tokens y la convierte en un string de HTML.
(define (generate-html tokens)
  (string-append "<!DOCTYPE html>\n<html>\n<head>\n<link rel=\"stylesheet\" href=\"estilos.css\">\n</head>\n<body>\n"
                 (string-join (map (lambda (token)
                                     (if (eq? (car token) NEWLINE) ; Si el token es una nueva línea, generamos un <br>.
                                         "<br>\n"
                                         (string-append "<text class=\"" (symbol->string (car token)) "\">" ; Si el token es cualquier otra cosa, generamos un <text> con una clase
                                                        (cdr token) 
                                                        "</text>"))) tokens)
                              "\n")
                 "</body>\n</html>"))


; Leer el archivo
(define entrada (file->string "entrada.txt"))
; Guardar resultado de tokenizacion en una variable
(define tokens (tokenize entrada))
; Guardar el resultado del analisis de gramatica de la tokenizacion
(define resultado (es_P tokens 0))
; Analizar si el resultado del analisis de gramatica es una lista o un numero
    ; Si es un numero desplegar en consola Gramatica correcta
    ; Si es una lista remplazar el token del par por un token de error

; Ejecutamos el interprete para que coloque las variables en el entorno
(es_LD_Eval tokens 0)
; Eliminamos las autoreferencias
(set! ENV (remove-self-and-mutual-references ENV))
; Ejecutamos las operaciones y mostramos el entorno
(display "Variables de entorno sin operaciones: ")
(newline)
(display ENV)
(newline)
(display "Variables de entorno con operaciones: ")
(newline)
(print (eval-expr-list ENV))
(newline)
(display "Obtención de variable a: ")
(newline)
(print (get-env-variable (eval-expr-list ENV) "a"))
(newline)



(if (number? resultado)
    (print "Gramatica correcta")
    (if (and (list? resultado) (>= (length resultado) 3)) 
        (begin
            (set! tokens (list-set tokens (cadr resultado) (cons ERROR (cdr (caddr resultado)))))
            (print "Gramatica incorrecta")
        )
        (print "Resultado desconocido")
    ))

; Desplegar el resultado con la funcion del html

; Función principal
 (define (main)
       (let [(output (generate-html tokens))] ; Genera HTML a partir de los tokens.
         (call-with-output-file "output.html" ; Abre el archivo de salida.
           (lambda (out) (display output out))))
 ) ; Escribe el HTML en el archivo de salida.

(main) ; Ejecuta la función principal.
