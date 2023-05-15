#lang racket

; Define los tipos de tokens que vamos a manejar
(define COMMENT 'comment)
(define KEYWORD 'keyword)
(define IDENTIFIER 'identifier)
(define OPERATOR 'operator)
(define NUMBER 'number)
(define NEWLINE 'newline)

; Función para analizar la entrada en tokens
(define (tokenize str)
  (letrec [(inner (lambda (str acc)
                    (cond [(string=? str "") (reverse acc)]
                          [(char=? (string-ref str 0) #\newline)
                           (inner (substring str 1) (cons (cons NEWLINE "\n") acc))]
                          [(char=? (string-ref str 0) #\space)
                           (inner (substring str 1) acc)]
                          [(string-prefix? str "//")
                           (let* [(comment-end (or (and (regexp-match-positions #rx"\n" str) 
                                                       (caar (regexp-match-positions #rx"\n" str)))
                                                  (string-length str)))
                                 (comment (substring str 0 comment-end))]
                             (inner (substring str (+ 1 comment-end)) (cons (cons COMMENT comment) acc)))]
                          [(string-prefix? str "let ")
                           (inner (substring str 4) (cons (cons KEYWORD "let") acc))]
                          [(string-prefix? str "= ")
                           (inner (substring str 2) (cons (cons OPERATOR "=") acc))]
                          [else
                           (let* [(identifier-end (or (and (regexp-match-positions #rx" " str) 
                                                          (caar (regexp-match-positions #rx" " str)))
                                                     (string-length str)))
                                 (identifier (substring str 0 identifier-end))]
                             (inner (substring str identifier-end) (cons (cons IDENTIFIER identifier) acc)))])))]
    (inner str '())))




; Función para generar HTML a partir de tokens
(define (generate-html tokens)
  (string-append "<!DOCTYPE html>\n<html>\n<head>\n<link rel=\"stylesheet\" href=\"estilos.css\">\n</head>\n<body>\n"
                 (string-join (map (lambda (token)
                                     (if (eq? (car token) NEWLINE)
                                         "<br>\n"
                                         (string-append "<text class=\"" (symbol->string (car token)) "\">" 
                                                        (cdr token) 
                                                        "</text>"))) tokens)
                              "\n")
                 "</body>\n</html>"))



; Función principal
(define (main)
  (let* [(input (file->string "entrada.txt"))
         (tokens (tokenize input))
         (output (generate-html tokens))]
    (call-with-output-file "output.html"
      (lambda (out) (display output out)))))

(main)
