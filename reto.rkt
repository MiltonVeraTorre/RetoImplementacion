#lang racket

; Define los tipos de tokens que vamos a manejar
; Cada uno de estos representa un tipo diferente de token en el lenguaje que estamos analizando.
(define COMMENT 'comment)
(define KEYWORD 'keyword)
(define IDENTIFIER 'identifier)
(define OPERATOR 'operator)
(define NUMBER 'number)
(define NEWLINE 'newline)

; Función para analizar la entrada en tokens
; Esta función toma un string y lo divide en tokens, que son pares de símbolos y strings.
(define (tokenize str)
  ; Declarando la función interna 'inner' que se llama a sí misma recursivamente
  (letrec [(inner 
      (lambda (str acc) ; 'str' es la cadena de entrada, 'acc' es la lista acumulativa de tokens
                    ; Se evalúa la condición para decidir qué acción tomar basándose en la cadena de entrada
                    (cond 
                       ; Si la cadena está vacía, se devuelve la lista de tokens en orden inverso
                      [(string=? str "") (reverse acc)] 
                          ; Si el primer carácter es una nueva línea, se crea un token NEWLINE y se continua con el resto de la cadena
                          [(char=? (string-ref str 0) #\newline) 
                           (inner (substring str 1) (cons (cons NEWLINE "\n") acc))]
                           
                          [(char=? (string-ref str 0) #\return) 
                           (inner (substring str 1) (cons (cons NEWLINE "\r") acc))]

                          ; Si el primer carácter es un espacio, se ignora y se continua con el resto de la cadena
                          [(char=? (string-ref str 0) #\space) 
                           (inner (substring str 1) acc)]

                          ; Si la cadena comienza con "//", se crea un token COMMENT con todo el texto hasta el final de la línea
                          [(string-prefix? str "//") 
                           (let* ; Con let se definen las variables commend-end y comment 
                                [(comment-end ; Almacena la posicion del final del comentario
                                                  (or (and 
                                                        (regexp-match-positions #rx"\n" str)  ; Busca la primera aparición de un cambio de linea
                                                       (caar (regexp-match-positions #rx"\n" str))); Si se encuentra la aprición del cambio de linea se obtiene la posicion
                                                  (string-length str))) ; Si no se encuentra el cambio de linea, se obtiene la longitud de la cadena
                                 (comment ; Almacena el contenido del comentario
                                      (substring str 0 comment-end) ; substr usa como argumento la cadena, la posicion inicial y la posicion final
                                      
                                      )
                                      ] 
                             (inner (substring str (+ 1 comment-end)) (cons (cons COMMENT comment) acc)))] ; Ya con el comentario extraido se vuelve a llamar inner con el resto de la cadena

                          ; Si la cadena comienza con "let ", "const ", "function " o "for ", se crea un token KEYWORD y se continua con el resto de la cadena   
                          [(string-prefix? str "let ") ; con string-prefix? se verifica si la cadena comienza con let
                           (inner (substring str 4) (cons (cons KEYWORD "let") acc))] ; En caso de que si comience con let, se llama a inner con el resto de la cadena y se agrega un token KEYWORD con el valor "let" a la lista acc 
                          [(string-prefix? str "const ") 
                           (inner (substring str 6) (cons (cons KEYWORD "const") acc))]
                          [(string-prefix? str "function ") 
                           (inner (substring str 9) (cons (cons KEYWORD "function") acc))]
                          [(string-prefix? str "for ") 
                           (inner (substring str 4) (cons (cons KEYWORD "for") acc))]

                          ; Si la cadena comienza con uno de los operadores, se crea un token OPERATOR y se continua con el resto de la cadena
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
                          [(string-prefix? str "!= ") 
                           (inner (substring str 3) (cons (cons OPERATOR "!=") acc))]
                          [else 
                           (let* [(identifier-end (or (and (regexp-match-positions #rx" " str) 
                                                          (caar (regexp-match-positions #rx" " str)))
                                                     (string-length str)))
                                 (identifier (substring str 0 identifier-end))]
                             (inner (substring str identifier-end) (cons (cons IDENTIFIER identifier) acc)))]
                             
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