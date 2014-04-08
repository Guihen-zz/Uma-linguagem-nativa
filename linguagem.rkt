#lang plai-typed

; Definição dos símbolos da linguagem.
(define-type Símbolo-aritmético
  [número (n : number)]
  [adição (x : Símbolo-aritmético) (y : Símbolo-aritmético)]
  [multiplicação (x : Símbolo-aritmético) (y : Símbolo-aritmético)])

(define-type Símbolo-aritmético-extendido
  [número-extendido (n : number)]
  [adição-extendida (x : Símbolo-aritmético-extendido) (y : Símbolo-aritmético-extendido)]
  [multiplicação-extendida (x : Símbolo-aritmético-extendido) (y : Símbolo-aritmético-extendido)]
  [subtração (x : Símbolo-aritmético-extendido) (y : Símbolo-aritmético-extendido)])

; Definição das ferramentas da linguagem.
(define (traduz [extensão-da-linguagem : Símbolo-aritmético-extendido]) : Símbolo-aritmético
  (type-case Símbolo-aritmético-extendido extensão-da-linguagem
    [número-extendido (n) (número n)]
    [adição-extendida (x y) (adição (traduz x) (traduz y))]
    [multiplicação-extendida (x y) (multiplicação (traduz x) (traduz y))]
    [subtração (x y) (adição (traduz x) (multiplicação (número -1) (traduz y)))]))

(define (analisa [entrada : s-expression]) : Símbolo-aritmético-extendido
  (cond
    [(s-exp-number? entrada) (número-extendido (s-exp->number entrada))]
    [(s-exp-list? entrada)
     (let ([lista (s-exp->list entrada)])
       (case (s-exp->symbol (first lista))
         [(+) (adição-extendida (analisa (second lista)) (analisa (third lista)))]
         [(*) (multiplicação-extendida (analisa (second lista)) (analisa (third lista)))]
         [(-) (subtração (analisa (second lista)) (analisa (third lista)))]
         [else (error 'analisa "A lista dada possui entradas inválida")]))]
     [else (error 'analisa "Entrada inválida")]))

(define (interpreta [expressão : Símbolo-aritmético]) : number
  (type-case Símbolo-aritmético expressão
    [número (n) n]
    [adição (x y) (+ (interpreta x) (interpreta y))]
    [multiplicação (x y) (* (interpreta x) (interpreta y))]))

(define (console) : number
  (interpreta (traduz (analisa (read)))))

(define (executa [expressão : s-expression]) : number
  (interpreta (traduz (analisa expressão))))