#lang racket

; Define los tipos de tokens que vamos a manejar
; Cada uno de estos representa un tipo diferente de token en el lenguaje que estamos analizando.
(define COMMENT 'comment)
(define KEYWORD 'keyword)
(define IDENTIFIER 'identifier)
(define OPERATOR 'operator)
(define NUMBER 'number)
(define NEWLINE 'newline)

(define PARENTHESIS 'parenthesis)
(define BRACE 'brace)

; Definimos la funcion tokenize que tokeniza un string
(define (tokenize str)
  ; Declarando la función interna 'inner' que se llama a sí misma recursivamente
  (letrec [(inner 
      (lambda (str acc) 
                    (cond 
                          [(string=? str "") (reverse acc)] 
                          [(string-prefix? str "\n") 
                           (inner (substring str 1) (cons (cons NEWLINE "\n") acc))]
                           
                          [(string-prefix? str "\r\n") 
                           (inner (substring str 2) (cons (cons NEWLINE "\r\n") acc))]

                          [(string-prefix? str " ") 
                           (inner (substring str 1) acc)]
                          
                          [(string-prefix? str "//") 
                           (let* 
                                [(comment-end 
                                                  (or (and 
                                                        (regexp-match-positions #rx"[\r\n]" str)  
                                                       (caar (regexp-match-positions #rx"[\r\n]" str)))
                                                  (string-length str))) 
                                 (comment 
                                      (substring str 0 comment-end))]
                             (inner (substring str (+ 1 comment-end)) (cons (cons COMMENT comment) acc)))] 

                          [(string-prefix? str "let ") 
                           (inner (substring str 4) (cons (cons KEYWORD "let") acc))] 
                          [(string-prefix? str "const ") 
                           (inner (substring str 6) (cons (cons KEYWORD "const") acc))]
                          [(string-prefix? str "function ") 
                           (inner (substring str 9) (cons (cons KEYWORD "function") acc))]
                          [(string-prefix? str "for ") 
                           (inner (substring str 4) (cons (cons KEYWORD "for") acc))]

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

                          [(string-prefix? str "( ") 
                           (inner (substring str 2) (cons (cons PARENTHESIS "(") acc))]
                          [(string-prefix? str ") ") 
                           (inner (substring str 2) (cons (cons PARENTHESIS ")") acc))]

                          [(string-prefix? str "{ ") 
                           (inner (substring str 2) (cons (cons BRACE "{") acc))]
                          [(string-prefix? str "} ") 
                           (inner (substring str 2) (cons (cons BRACE "}") acc))]


                          [(string-prefix? str "!= ") 
                           (inner (substring str 3) (cons (cons OPERATOR "!=") acc))]


                          [else 
                           (let* [(identifier-end (or (and (regexp-match-positions #rx"[ ]" str) 
                                                          (caar (regexp-match-positions #rx"[ ]" str)))
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