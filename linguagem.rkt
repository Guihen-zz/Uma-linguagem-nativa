#lang plai-typed

(define-type Símbolo-aritmético
  [número (n : number)]
  [soma (x : SímboloAritmético) (y : SímboloAritmético)]
  [multiplicação (x : SímboloAritmético) (y : SímboloAritmético)])

(define (analisa [entrada : s-expression]) : Símbolo-aritmético
  (cond
    [(s-exp-number? entrada) (número (s-exp->number entrada))]
    [(s-exp-list? entrada)
     (let ([lista (s-exp->list s)])
       (case (s-exp->symbol (first lista))
         [(+) (soma (analisa (second lista) (analisa (third lista))))]
         [(*) (multiplicação (analisa (second lista) (analisa (third lista))))]
         [else (error 'analisa "A lista dada possui entradas inválida")]))]
     [else (error 'analisa "Entrada inválida")]))
         