#lang racket

;;; P → LD
;;; LD → D LD | ε
;;; D → DV | F | C | COM
;;; DV → ("let" | "const") I "=" E ";"
;;; E → N | I | "(" E ")"
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


(define KEYWORD 'keyword)
(define IDENTIFIER 'indentifier)
(define OPERATOR 'operator)
(define NUMBER 'number)
(define COMMENT 'comment)
(define PARENTHESIS 'parenthesis)
(define BRACKET 'bracket)
(define SEMICOLON 'semicolon)
(define COMMA 'comma)
(define COLOR 'colon)
