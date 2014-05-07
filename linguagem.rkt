#lang plai-typed

;========================================================================;
; Definição dos símbolos da linguagem.
(define-type Expressão-aritmética
  [número (n : number)]
  [adição (x : Expressão-aritmética) (y : Expressão-aritmética)]
  [multiplicação (x : Expressão-aritmética) (y : Expressão-aritmética)]
  [igual-a-zero? (x : Expressão-aritmética) (faça : Expressão-aritmética) (senão : Expressão-aritmética)]
  [nome (n : symbol)]
  [aplicação (função : symbol) (argumento : Expressão-aritmética)])

; Definição das extensões da linguagem.
(define-type Expressão-aritmética-extendida
  [número-extendido (n : number)]
  [adição-extendida (x : Expressão-aritmética-extendida) (y : Expressão-aritmética-extendida)]
  [multiplicação-extendida (x : Expressão-aritmética-extendida) (y : Expressão-aritmética-extendida)]
  [subtração (x : Expressão-aritmética-extendida) (y : Expressão-aritmética-extendida)]
  [oposto-de (x : Expressão-aritmética-extendida)]
  [igual-a-zero?-extendido (x : Expressão-aritmética-extendida) (faça : Expressão-aritmética-extendida) (senão : Expressão-aritmética-extendida)]
  [nome-extendido (n : symbol)]
  [aplicação-extendida (função : symbol) (argumento : Expressão-aritmética-extendida)])

;========================================================================;
; Abstração para a permissão de funções na linguagem.
(define-type Função
  [função-unária (nome : symbol) (parâmetro : symbol) (corpo : Expressão-aritmética)])

; Procedimento para a utilização do argumento em uma dada função.
(define (instancia-função [função : Expressão-aritmética] [nome-do-parâmetro : symbol] 
    [argumento : Expressão-aritmética]) : Expressão-aritmética
  (type-case Expressão-aritmética função
    [número (n) função]
    [nome (n) (cond 
            [(symbol=? n nome-do-parâmetro) argumento]
            [else função])]
    [aplicação (f a) (aplicação f (instancia-função a nome-do-parâmetro argumento))]
    [adição (x y) (adição (instancia-função x nome-do-parâmetro argumento) 
                (instancia-função y nome-do-parâmetro argumento))]
    [multiplicação (x y) (multiplicação (instancia-função x nome-do-parâmetro argumento) 
                (instancia-função y nome-do-parâmetro argumento))]
    [igual-a-zero? (x faça senão) (igual-a-zero? (instancia-função x nome-do-parâmetro argumento) 
                (instancia-função faça nome-do-parâmetro argumento)
                (instancia-função senão nome-do-parâmetro argumento))]))

(define (busca-função [nome : symbol] [conjunto-de-funções : (listof Função)]) : Função
  (cond 
    [(empty? conjunto-de-funções) (error 'busca-função "Função chamada não definida")]
    [(cons? conjunto-de-funções) (cond
        [(equal? nome (função-unária-nome (first conjunto-de-funções))) (first conjunto-de-funções)]
        [else (busca-função nome (rest conjunto-de-funções))])]))

;========================================================================;
; Implementação do Ambiente da linguagem.
(define-type Associação
  [associação (rótulo : symbol) (valor : number)])

(define-type-alias Ambiente (listof Associação))
(define ambiente-vazio empty) ; empty := lista vazia
(define estende-ambiente cons) ; cons recebe dois argumentos e cria uma lista
(define (busca-associação [rótulo : symbol] [ambiente : Ambiente]) : number
  (cond 
    [(empty? ambiente) (error 'busca-associação "Associação não encontrada!")]
    [else (cond
            [(symbol=? rótulo (associação-rótulo (first ambiente))) (associação-valor (first ambiente))]
            [else (busca-associação rótulo (rest ambiente))])]))

;========================================================================;
; Definição das ferramentas da linguagem.
(define (traduz [extensão-da-linguagem : Expressão-aritmética-extendida]) : Expressão-aritmética
  (type-case Expressão-aritmética-extendida extensão-da-linguagem
    [número-extendido (n) (número n)]
    [adição-extendida (x y) (adição (traduz x) (traduz y))]
    [multiplicação-extendida (x y) (multiplicação (traduz x) (traduz y))]
    [subtração (x y) (adição (traduz x) (multiplicação (número -1) (traduz y)))]
    [oposto-de (x) (multiplicação (número -1) (traduz x))]
    [igual-a-zero?-extendido (expressão faça senão) (igual-a-zero? (traduz expressão) (traduz faça) (traduz senão))]
    [nome-extendido (n) (nome n)]
    [aplicação-extendida (funcao argumento) (aplicação funcao (traduz argumento))]))

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
         [(igual-a-zero?) (igual-a-zero?-extendido (analisa (second lista)) (analisa (third lista)) (analisa (fourth lista)))]
         [(aplica:) (aplicação-extendida (s-exp->symbol (second lista)) (analisa (third lista)))]
         [else (error 'analisa "A lista dada possui entradas inválida")]))]
     [else (error 'analisa "Entrada inválida")]))

(define (interpreta [expressão : Expressão-aritmética] [ambiente : Ambiente] [conjunto-de-funções : (listof Função)]) : number
  (type-case Expressão-aritmética expressão
    [número (n) n]
    [adição (x y) (+ (interpreta x ambiente conjunto-de-funções) (interpreta y ambiente conjunto-de-funções))]
    [multiplicação (x y) (* (interpreta x ambiente conjunto-de-funções) (interpreta y ambiente conjunto-de-funções))]
    [igual-a-zero? (expressão faça senão) (if (zero? (interpreta expressão ambiente conjunto-de-funções)) 
        (interpreta faça ambiente conjunto-de-funções) (interpreta senão ambiente conjunto-de-funções))]
    [nome (n) (busca-associação n ambiente)]
    [aplicação (função argumento) 
          (local ([define f (busca-função função conjunto-de-funções)])
            (interpreta (função-unária-corpo f)
                  (estende-ambiente (associação (função-unária-parâmetro f) 
                             (interpreta argumento ambiente conjunto-de-funções)) 
                            ambiente) conjunto-de-funções))]))
;========================================================================;
; Interface da linguagem
(define biblioteca-nativa
  (list [função-unária 'dobro 'x (adição (nome 'x) (nome 'x))]))

(define (console) : number
  (interpreta (traduz (analisa (read))) ambiente-vazio biblioteca-nativa))

(define (executa [expressão : s-expression]) : number
  (interpreta (traduz (analisa expressão)) ambiente-vazio biblioteca-nativa))

;========================================================================;
; Testes da linguagem
(test (executa '(+ 1 5)) 6)
(test (executa '(* 2 5)) 10)
(test (executa '(- 1 5)) -4)
(test (executa '(- (+ 1 (* 2 5)) (+ 1 2))) 8) ; interação soma, subtração e multiplicação
(test (executa '(! 1)) -1)
(test (executa '(igual-a-zero? (+ 4 (! 4)) (* 2 2) (- 2 3))) 4) ; caso positivo
(test (executa '(igual-a-zero? (- 4 (! 4)) (* 2 2) (- 2 3))) -1) ; caso negativo
(test (executa '(aplica: dobro (aplica: dobro 5))) 20) ; testando biblioteca de funções nativas