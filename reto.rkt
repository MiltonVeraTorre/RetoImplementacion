#lang racket

; Define los tipos de tokens que vamos a manejar
(define COMMENT 'comment)
(define KEYWORD 'keyword)
(define IDENTIFIER 'identifier)
(define OPERATOR 'operator)
(define NUMBER 'number)

; Función para analizar la entrada en tokens
(define (tokenize str)
  (cond [(string-prefix? str "//") 
         (let* [(comment-end (and (regexp-match-positions #rx"\n" str) 
                                 (caar (regexp-match-positions #rx"\n" str))))
                (comment (if comment-end (substring str 0 comment-end) str))]
           (cons (cons COMMENT comment) 
                 (if comment-end (tokenize (substring str (+ comment-end 1) (string-length str))) '())))]
        [(string-prefix? str "let ") 
         (cons (cons KEYWORD "let") (tokenize (substring str 4 (string-length str))))]
        [(string-prefix? str "= ") 
         (cons (cons OPERATOR "=") (tokenize (substring str 2 (string-length str))))]
        [(string-prefix? str " ") 
         (tokenize (substring str 1 (string-length str)))]
        [(string-prefix? str "\n") 
         (tokenize (substring str 1 (string-length str)))]
        [else
         (let* [(match (regexp-match #px"^[a-zA-Z_][a-zA-Z0-9_]*" str))
                (id (if match (car match) ""))]
           (if (> (string-length id) 0)
               (cons (cons IDENTIFIER id) (tokenize (substring str (string-length id) (string-length str))))
               (let* [(match (regexp-match #px"^[0-9]+" str))
                      (num (if match (car match) ""))]
                 (if (> (string-length num) 0)
                     (cons (cons NUMBER num) (tokenize (substring str (string-length num) (string-length str))))
                     '()))))]))



; Función para generar HTML a partir de tokens
(define (generate-html tokens)
  (string-append "<!DOCTYPE html>\n<html>\n<head>\n<link rel=\"stylesheet\" href=\"estilos.css\">\n</head>\n<body>\n"
                 (string-join (map (lambda (token)
                                     (string-append "<text class=\"" (symbol->string (car token)) "\">" (cdr token) "</text><br>\n")) tokens)
                              "")
                 "</body>\n</html>"))

; Función principal
(define (main)
  (let* [(input (file->string "entrada.txt"))
         (tokens (tokenize input))
         (output (generate-html tokens))]
    (call-with-output-file "output.html"
      (lambda (out) (display output out)))))

(main)
