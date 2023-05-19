#lang racket

;;;;;;; DEFINICIÓN DE LA GRAMATICA


;; Gramatica de Javascript

;;; P → LD
;;; LD → D LD | ε
;;; D → DV | F | C | COM
;;; DV → ("let" | "const") I "=" E
;;; E → N | I | "(" E ")"
;;; O → E OP E | O OP E
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


;;;;;;;;;;;;;;;;;; DEFINICION DE LOS TOKENS ;;;;;;;;;;;;;;;;;;;;;;;;

; Define los tipos de tokens que vamos a manejar
; Cada uno de estos representa un tipo diferente de token en el lenguaje que estamos analizando.
(define COMMENT 'comment)
(define KEYWORD 'keyword)
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


;;;;;;;;;;;;;;;;;; TOKENIZADOR ;;;;;;;;;;;;;;;;;;;;;;;;

; Documentación general de la funcion de tokenize(flujo de la funcion)

;;; 1) Se define una función auxiliar recursiva inner que toma un string y una lista de tokens acumulados como argumentos.

;;; 2) inner examina el principio del string y verifica una serie de condiciones para determinar qué tipo de token (si corresponde) comienza el string. Cada condición corresponde a un tipo diferente de token que podría aparecer al principio del string. Las condiciones se verifican en un orden específico, por lo que si varias condiciones podrían ser verdaderas, se elige la primera.

;;; 3) Se examina cada condición y en caso de ser verdadera se genera el token y se llama inner con un substring que ya no incluya el token que ya se reconoció

;;; 4) Finalmente, se llama a inner con el string de entrada completo y una lista vacía de tokens acumulados.



(define ContadorParentesis 0)
(define ContadorBrackets 0)


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
        (inner (substring str 1) (cons (cons NEWLINE "\n") acc))
      ]  

      ; Si el string comienza con un salto de línea "\r\n", generamos un token NEWLINE y llamamos a inner de nuevo
      [(string-prefix? str "\r\n") 
        (inner (substring str 2) (cons (cons NEWLINE "\r\n") acc))
      ]

      ; Si el string comienza con un espacio " ", lo ignoramos y llamamos a inner de nuevo
      [(string-prefix? str " ") 
        (inner (substring str 1) acc)
      ]
                          
      ; Si el string comienza con un comentario "//", generamos un token COMMENT y llamamos a inner de nuevo
      [(string-prefix? str "//") 

        ; Usamos let* para definir múltiples variables que pueden referirse entre sí
        (let* 
        ; Definimos la variable comment-end que representa el final del comentario en el string
        [
          (comment-end ; Buscamos el final del comentario
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
            (substring str 0 comment-end) ; Extraemos el comentario del string
          ) 
        ] 
          ; Llamamos a la función inner de nuevo, pero esta vez el string que le pasamos comienza después del comentario que acabamos de procesar
          ; Además, añadimos el comentario que acabamos de procesar a la lista de tokens acumulados (acc)
          (inner 
            (substring str (+ 1 comment-end)) 
            (cons (cons COMMENT comment) acc)
          )
        )
      ] 

      ; Si el string comienza con una palabra clave "let", "const", "function", "for", generamos un token KEYWORD y llamamos a inner de nuevo
      [(string-prefix? str "let ") ; string-prefix? devuelve #t si el string comienza con el substring que le pasamos
        (inner 
          (substring str 4)  ; Esto extrae una subcadena de 'str' empezando desde el cuarto índice (0-based), esencialmente eliminando "let " del inicio del string
          (cons (cons KEYWORD "let") acc)  ; Crea una nueva pareja donde el primer elemento es otra pareja (KEYWORD "let") y el segundo elemento es 'acc', que es el acumulador que almacena los tokens generados hasta ahora.
        )
      ] 

      [(regexp-match? #rx"^[0-9]" str) 
        (let* 
          ; Comienza la lista de declaraciones de variables
          [(number-end ; Declaramos la variable 'number-end'
            (or ; Usamos el operador 'or' para asignar el primer valor que no sea falso
              (and ; Busca la primera ocurrencia de un espacio en blanco en la cadena 'str'.
                (regexp-match-positions #rx"[^0-9.]" str) ; Devuelve una lista de posiciones donde se encuentra el patrón. En este caso, estamos buscando cualquier carácter que no sea un dígito o un punto.
                  (caar (regexp-match-positions #rx"[^0-9.]" str)) ; Extraemos la primera posición de la primera coincidencia. 'caar' se usa para obtener el primer elemento de la primera sublista.
              )
              (string-length str) ; Si no se encuentra un espacio en blanco, tomamos la longitud total de la cadena 'str'
            )
          )
          (number (substring str 0 number-end))] ; Extraemos el número de la cadena 'str' tomando una subcadena desde el inicio hasta 'number-end'  
          
          (inner (substring str number-end) (cons (cons NUMBER number) acc)) ; Llamamos a la función 'inner' de forma recursiva, proporcionando una subcadena de 'str' que comienza en 'number-end' y agregando el número a la lista de tokens 'acc'.
        )
      ]


      [(string-prefix? str "const ") 
        (inner (substring str 6) (cons (cons KEYWORD "const") acc))]
      [(string-prefix? str "function ") 
        (inner (substring str 9) (cons (cons KEYWORD "function") acc))]
      [(string-prefix? str "for") 
        (inner (substring str 3) (cons (cons KEYWORD "for") acc))]
      [(string-prefix? str "while") 
        (inner (substring str 5) (cons (cons KEYWORD "while") acc))]
      [(string-prefix? str "return") 
        (inner (substring str 6) (cons (cons KEYWORD "return") acc))]

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
        (inner (substring str 3) (cons (cons OPERATOR "!=") acc))
        ]
      ; Si ninguna de las condiciones anteriores es cierta, asumimos que el string comienza con un identificador
      [else 
        (let* ; Declaramos variables locales en el bloque
          ; Comienza la lista de declaraciones de variables
          [
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
      )
    )
  )]
    (inner str '())
  )
)

;;;;;;;;;;;;;;;;; ANALIZADOR DE LA GRAMATICA ;;;;;;;;;;;;;;;;;;;;;;;;


; es_P: lista indice -> boolean
; Verifica si la lista de tokens forma un programa válido (P) según la gramática.
; Un P es un LD (lista de declaraciones).

;;; P → LD
(define (es_P tokens indice)
  (if (>= indice (length tokens)); Si el indice es mayor a la longitud de la lista devolvemos que es falso
      #f
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
                  #f ; Si es_D no fue exitoso, devolvemos falso.
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
    #f
    (case ((list-ref tokens indice) 'type) ; Verificamos si se cumple alguno de los casos definidos en la gramatica
      [(KEYWORD) (es_DV tokens (+ indice 1))] ;Verificamos si de trata de una declaración de una variable
      [(IDENTIFIER) (es_F tokens (+ indice 1))] ; Verificamos si se trata de una función
      [(NUMBER) (es_C tokens (+ indice 1))] ; Verificamos si se trata de un ciclo
      [(COMMENT) (es_COM tokens (+ indice 1))] ; Verificamos si se trata de un comentario
      [else #f]
    )
  )
) ; Si no se identifica ninguno de los casos devolvemos que es falso y no se encontró una declaración valida


; es_DV: vector indice -> numero / #f
; Verifica si el vector de tokens forma una declaración de variable válida (DV) según la gramática.
; Un DV es un KEYWORD seguido de un IDENTIFIER, un OPERATOR "=" y una E (expresión).
; Devuelve el índice del token después de la DV, o #f si los tokens no forman una DV válida.

;;; DV → ("let" | "const") I "=" E
(define (es_DV tokens indice)
  (if (>= indice (length tokens)) ; Verificamos si se sobrepasa la longitud
    #f
    ; Si hay más tokens para analizar, extraemos el tipo y el valor del token en el índice actual.
    (let 
      (
        (token-type (car (list-ref tokens indice))) ; Guardamos el tipo de token
        (token-value (cdr (list-ref tokens indice))) ; Guardamos el valor del token
      )
        
      ; También verificamos si el siguiente token es un IDENTIFIER y si el token después de ese es un OPERATOR con valor "=".
      ; Si todas estas condiciones se cumplen, entonces tenemos el comienzo de una declaración de variable.
      (if (and 
        (eq? token-type KEYWORD) ; Primero verificamos que el token sea de tipo KEYWORD
        (or (string=? token-value "let") (string=? token-value "const")) ; Luego verificamos si el valor del token es let o const
        (eq? (vector-ref tokens (+ indice 1) 'type) IDENTIFIER) ; Luego verificamos si hay un identificador
        (eq? (vector-ref tokens (+ indice 2) 'type) OPERATOR) ; Posteriormente debe de haber un operador
        (string=? (vector-ref tokens (+ indice 2) 'value) "=")
        ) ; Y este operador debe ser el operador igual

        ; Si las condiciones se cumplen, llamamos a la función es_E para analizar la expresión que sigue.
        ; Pasamos el vector de tokens y el índice del primer token después de la asignación ("=") a es_E.
        (es_E tokens (+ indice 3))
        ; Si alguna de las condiciones no se cumple, los tokens no forman una declaración de variable válida,
        ; por lo que devolvemos #f.
        #f
      )
    )
  )
)


; es_E: vector indice -> numero / #f
;;; E → N | I | "(" E ")"

(define (es_E tokens indice)
  (cond ; Ejecutamos el condicional para saber si se cumple alguno de los criterios
    ((>= indice (length tokens)) #f)   ;; No hay más tokens por lo que se devuleve que es falso
    ((equal? (car (list-ref tokens indice)) NUMBER) (+ indice 1))  ; Puede ser un numero
    ((equal? (car (list-ref tokens indice)) IDENTIFIER) (+ indice 1))  ;; Puede ser un identificador
    ((and ; El utlimo caso es en el que este envuelto en parentesis
      (equal? (cdr (list-ref tokens indice)) "(")  ;; En caso de que no sea uno de los anteriores entonces puede ser una expresión en parentesis
      (es_E tokens (+ indice 1)) ;; 
      (equal? (cdr (list-ref tokens (+ indice 2))) ")"))
      
      (+ indice 3)
    )
    (else #f)
  )
)

; es_C: vector indice -> numero / #f
;;; C → "for" "(" DV O ";" O ")" "{" LD "}"

;; es_C: lista indice -> numero / #f
(define (es_C tokens indice)
  (if (and (>= indice (- (length tokens) 8))  ;; Nos aseguramos de que hay suficientes tokens
           (equal? (cdr (list-ref tokens indice)) "for")
           (equal? (cdr (list-ref tokens (+ indice 1))) "(")
           (number? (es_DV tokens (+ indice 2)))
           (equal? (cdr (list-ref tokens (+ indice 3))) ";")
           (number? (es_O tokens (+ indice 4)))
           (equal? (cdr (list-ref tokens (+ indice 5))) ";")
           (number? (es_O tokens (+ indice 6)))
           (equal? (cdr (list-ref tokens (+ indice 7))) ")")
           (equal? (cdr (list-ref tokens (+ indice 8))) "{")
           (number? (es_LD tokens (+ indice 9)))
           (equal? (cdr (list-ref tokens (+ indice 10))) "}"))
      (+ indice 11)
      #f))

; es_COM : vector indice -> numero / #f
;;; COM → "//" I

(define (es_COM tokens indice)
  (if (and (>= indice (- (length tokens) 1))
           (equal? (cdr (list-ref tokens indice)) "//")
           (equal? (car (list-ref tokens (+ indice 1))) IDENTIFIER))
      (+ indice 2)
      #f))

;; es_O: vector indice -> numero / #f
;;; O → E OP E | O OP E

(define (es_O tokens indice)
  (let ((nuevo_indice (es_E tokens indice)))
    (if (and (number? nuevo_indice)
             (>= nuevo_indice (- (length tokens) 1))
             (equal? (car (list-ref tokens nuevo_indice)) OPERATOR)
             (number? (es_E tokens (+ nuevo_indice 1))))
        (+ nuevo_indice 2)
        #f)))

;; es_OP: vector indice -> numero / #f
;;; OP → "+" | "-" | "*" | "/" | "==" | "<" | ">" | "!="

(define (es_OP tokens indice)
  (if 
    (and 
      (>= indice (length tokens))
      (equal? (car (list-ref tokens indice)) OPERATOR)
    )
    (+ indice 1)
    #f
  )
)


;; es_F: vector indice -> numero / #f
;;; F → "function" I "(" LI ")" "{" LD "}"

(define (es_F tokens indice)
  (if 
    (and 
      (>= indice (- (length tokens) 7))  ;; Nos aseguramos de que hay suficientes tokens
      (equal? (cdr (list-ref tokens indice)) "function")
      (equal? (car (list-ref tokens (+ indice 1))) IDENTIFIER)
      (equal? (cdr (list-ref tokens (+ indice 2))) "(")
    )
    (let* (
      (nuevo_indice (es_LI tokens (+ indice 3)))
      (nuevo_indice_2 
        (if 
          (and 
          (number? nuevo_indice)
          (equal? (cdr (list-ref tokens nuevo_indice)) ")")
          (equal? (cdr (list-ref tokens (+ nuevo_indice 1))) "{")
          )
          (es_LD tokens (+ nuevo_indice 2))
          #f
        )
      ))
      (if (and (number? nuevo_indice_2)
        (equal? (cdr (list-ref tokens nuevo_indice_2)) "}")
        )
        (+ nuevo_indice_2 1)
        #f
      )
    )
  #f
  )
)

;; es_LI: vector indice -> numero / #f
;;; LI → I "," LI | ε

(define (es_LI tokens indice)
  (cond 
    ((>= indice (length tokens)) #f)  ;; No hay más tokens
    ((equal? (car (list-ref tokens indice)) IDENTIFIER)  ;; Identificador
      (if 
        (and 
          (>= indice (- (length tokens) 1))  ;; Hay un token más
          (equal? (cdr (list-ref tokens (+ indice 1))) ",")
        )  ;; Y es una coma
              
        (es_LI tokens (+ indice 2))  ;; Continuamos con el siguiente identificador
        (+ indice 1)
      )
    )  ;; No hay coma, terminamos aquí
    (else #f)
  )
)




;;;;;;;;;;;;;;;;;; GENERADOR DE HTML USANDO LOS TOKENS ;;;;;;;;;;;;;;;;;;;;;;;;

; Función para generar HTML a partir de tokens
; Esta función toma una lista de tokens y la convierte en un string de HTML.
(define (generate-html tokens)
  (string-append "<!DOCTYPE html>\n<html>\n<head>\n<link rel=\"stylesheet\" href=\"estilos.css\">\n</head>\n<body>\n"
    (string-join 
      (map (lambda (token)
        (if (eq? (car token) NEWLINE) ; Si el token es una nueva línea, generamos un <br>.
        "<br>\n"
        (string-append "<text class=\"" (symbol->string (car token)) "\">" ; Si el token es cualquier otra cosa, generamos un <text> con una clase
          (cdr token) 
          "</text>"))) 
        tokens
      )
      "\n"
    )
    "</body>\n</html>"
  )
)



; Función para imprimir tokens
(define (print-tokens tokens)
  (for-each (lambda (token)
    (printf "~a ~a~n" (car token) (cdr token)))
    tokens
  )
)

; Función principal
(define (main)
  (let* 
    [(input (file->string "entrada.txt"))] ; Lee el archivo de entrada en un string.
    ;(printf "Texto de entrada:\n~a\n" input) ; Imprime el texto de entrada.
    (let [(tokens (tokenize input))] ; Tokeniza el string de entrada.
      (print-tokens tokens) ; Imprime los tokens.
      ()
      (let [(output (generate-html tokens))] ; Genera HTML a partir de los tokens.
        (call-with-output-file "output.html" ; Abre el archivo de salida.
        (lambda (out) (display output out)))
      )
    )
  )
) ; Escribe el HTML en el archivo de salida.

(main) ; Ejecuta la función principal.
