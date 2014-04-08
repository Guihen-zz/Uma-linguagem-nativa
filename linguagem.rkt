#lang plai-typed

(define-type Símbolo-aritmético
  [número (n : number)]
  [soma (x : Símbolo-aritmético) (y : Símbolo-aritmético)]
  [multiplicação (x : Símbolo-aritmético) (y : Símbolo-aritmético)])

(define (analisa [entrada : s-expression]) : Símbolo-aritmético
  (cond
    [(s-exp-number? entrada) (número (s-exp->number entrada))]
    [(s-exp-list? entrada)
     (let ([lista (s-exp->list entrada)])
       (case (s-exp->symbol (first lista))
         [(+) (soma (analisa (second lista)) (analisa (third lista)))]
         [(*) (multiplicação (analisa (second lista)) (analisa (third lista)))]
         [else (error 'analisa "A lista dada possui entradas inválida")]))]
     [else (error 'analisa "Entrada inválida")]))
