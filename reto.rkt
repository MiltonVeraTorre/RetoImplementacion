#lang racket

; Define los tipos de tokens que vamos a manejar
; Cada uno de estos representa un tipo diferente de token en el lenguaje que estamos analizando.
(define COMMENT 'comment)
(define KEYWORD 'keyword)
(define IDENTIFIER 'identifier)
(define OPERATOR 'operator)
(define NUMBER 'number)
(define NEWLINE 'newline)
(define DELIMITER 'delimiter)

(define PARENTHESIS 'parenthesis)
(define BRACE 'brace)

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

                          ; Si el string comienza con una palabra clave "let", "const", "function", "for", generamos un token KEYWORD y llamamos a inner de nuevo
                          [(string-prefix? str "let ") ; string-prefix? devuelve #t si el string comienza con el substring que le pasamos
                           (inner 
                                  (substring str 4)  ; Esto extrae una subcadena de 'str' empezando desde el cuarto índice (0-based), esencialmente eliminando "let " del inicio del string
                                  (cons (cons KEYWORD "let") acc)  ; Crea una nueva pareja donde el primer elemento es otra pareja (KEYWORD "let") y el segundo elemento es 'acc', que es el acumulador que almacena los tokens generados hasta ahora.
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
                           (inner (substring str 1) (cons (cons PARENTHESIS "(") acc))]
                          [(string-prefix? str "( ") 
                           (inner (substring str 2) (cons (cons PARENTHESIS "(") acc))]

                          [(string-prefix? str ")") 
                           (inner (substring str 1) (cons (cons PARENTHESIS ")") acc))]
                          [(string-prefix? str ") ") 
                           (inner (substring str 2) (cons (cons PARENTHESIS ")") acc))]

                          [(string-prefix? str "{") 
                           (inner (substring str 1) (cons (cons BRACE "{") acc))]
                          [(string-prefix? str "{ ") 
                           (inner (substring str 2) (cons (cons BRACE "{") acc))]


                          [(string-prefix? str "}") 
                           (inner (substring str 1) (cons (cons BRACE "}") acc))]
                          [(string-prefix? str "} ") 
                           (inner (substring str 2) (cons (cons BRACE "}") acc))]

                          [(string-prefix? str "; ") 
                           (inner (substring str 2) (cons (cons DELIMITER ";") acc))]
                          [(string-prefix? str ";") 
                           (inner (substring str 1) (cons (cons DELIMITER ";") acc))]

                          [(string-prefix? str ", ") 
                           (inner (substring str 2) (cons (cons DELIMITER ",") acc))]
                          [(string-prefix? str ",") 
                           (inner (substring str 1) (cons (cons DELIMITER ",") acc))]


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



; Función para imprimir tokens
(define (print-tokens tokens)
  (for-each (lambda (token)
              (printf "~a ~a~n" (car token) (cdr token)))
            tokens))

; Función principal
(define (main)
  (let* [(input (file->string "entrada.txt"))] ; Lee el archivo de entrada en un string.
    (printf "Texto de entrada:\n~a\n" input) ; Imprime el texto de entrada.
    (let [(tokens (tokenize input))] ; Tokeniza el string de entrada.
      (print-tokens tokens) ; Imprime los tokens.
      (let [(output (generate-html tokens))] ; Genera HTML a partir de los tokens.
        (call-with-output-file "output.html" ; Abre el archivo de salida.
          (lambda (out) (display output out))))))) ; Escribe el HTML en el archivo de salida.

(main) ; Ejecuta la función principal.