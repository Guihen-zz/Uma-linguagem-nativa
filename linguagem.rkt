#lang plai-typed

; Definição dos símbolos da linguagem.
(define-type Expressão-aritmética
  [número (n : number)]
  [adição (x : Expressão-aritmética) (y : Expressão-aritmética)]
  [multiplicação (x : Expressão-aritmética) (y : Expressão-aritmética)])

(define-type Expressão-aritmética-extendida
  [número-extendido (n : number)]
  [adição-extendida (x : Expressão-aritmética-extendida) (y : Expressão-aritmética-extendida)]
  [multiplicação-extendida (x : Expressão-aritmética-extendida) (y : Expressão-aritmética-extendida)]
  [subtração (x : Expressão-aritmética-extendida) (y : Expressão-aritmética-extendida)]
  [oposto-de (x : Expressão-aritmética-extendida)])

(define-type Função
  [função-unária (nome : symbol) (parâmetro : symbol) (corpo : Expressão-aritmética)])

; Definição das ferramentas da linguagem.
(define (traduz [extensão-da-linguagem : Expressão-aritmética-extendida]) : Expressão-aritmética
  (type-case Expressão-aritmética-extendida extensão-da-linguagem
    [número-extendido (n) (número n)]
    [adição-extendida (x y) (adição (traduz x) (traduz y))]
    [multiplicação-extendida (x y) (multiplicação (traduz x) (traduz y))]
    [subtração (x y) (adição (traduz x) (multiplicação (número -1) (traduz y)))]
    [oposto-de (x) (multiplicação (número -1) (traduz x))]))

(define (analisa [entrada : s-expression]) : Expressão-aritmética-extendida
  (cond
    [(s-exp-number? entrada) (número-extendido (s-exp->number entrada))]
    [(s-exp-list? entrada)
     (let ([lista (s-exp->list entrada)])
       (case (s-exp->symbol (first lista))
         [(+) (adição-extendida (analisa (second lista)) (analisa (third lista)))]
         [(*) (multiplicação-extendida (analisa (second lista)) (analisa (third lista)))]
         [(-) (subtração (analisa (second lista)) (analisa (third lista)))]
         [(!) (oposto-de (analisa (second lista)))]
         [else (error 'analisa "A lista dada possui entradas inválida")]))]
     [else (error 'analisa "Entrada inválida")]))

(define (interpreta [expressão : Expressão-aritmética]) : number
  (type-case Expressão-aritmética expressão
    [número (n) n]
    [adição (x y) (+ (interpreta x) (interpreta y))]
    [multiplicação (x y) (* (interpreta x) (interpreta y))]))

(define (console) : number
  (interpreta (traduz (analisa (read)))))

(define (executa [expressão : s-expression]) : number
  (interpreta (traduz (analisa expressão))))

; Testes da linguagem
(test (executa '(+ 1 5)) 6)
(test (executa '(* 2 5)) 10)
(test (executa '(- 1 5)) -4)
(test (executa '(- (+ 1 (* 2 5)) (+ 1 2))) 8) ; interação soma, subtração e multiplicação
(test (executa '(! 1)) -1)