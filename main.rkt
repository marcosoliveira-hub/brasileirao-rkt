#lang racket
(require examples) ;; https://github.com/malbarbo/racket-test-examples

;; 6902 - Paradigma de Programação Lógica e Funcional
;; Acadêmico: Marcos Vinicius de Oliveira
;; RA: 124408
;; Prof. Marco A. L. Barbosa
;; Tabela do Brasileirão

;; Desempenho -> (time: string, pontos: int, vitorias: int, saldo_gols: int)

(struct desempenho (time pontos vitorias saldo-gols) #:transparent)

;; Resultado -> (time1: string, gols1: int, time2: string, gols2: int)

(struct resultado (time1 gols1 time2 gols2) #:transparent)

;; Transforma uma string de um resultado em uma struct do tipo resultado
;; String -> Resultado
(define (string->resultado s)
  (define string-separada (string-split s))
  (resultado (first string-separada)
             (string->number (second string-separada))
             (first (rest (rest string-separada)))
             (string->number (second (rest (rest string-separada))))))

(examples
 (check-equal? (string->resultado "Sao-Paulo 1 Atletico-MG 2")
               (resultado "Sao-Paulo" 1 "Atletico-MG" 2)))

;; Verifica se uma string está contida em uma lista de strings
;; String lista-String -> Boolean
(define (nao-contem? s lst)
  (cond
    [(empty? lst) #t]
    [(equal? s (first lst)) #f]
    [else (nao-contem? s (rest lst))]))

;; Calcula os pontos de um time em um jogo dado o resultado
;; String Resultado -> Int
;; Se o time vence, recebe 3 pontos, se houver empate recebe 1 ponto, se perder não recebe pontos
(define (calcula-pontos-jogo time resultado)
  (cond
    [(equal? (resultado-time1 resultado) time)
     (if (= (resultado-gols1 resultado) (resultado-gols2 resultado)) 1
         (if (> (resultado-gols1 resultado) (resultado-gols2 resultado)) 3 0))]
    [(equal? (resultado-time2 resultado) time)
     (if (= (resultado-gols1 resultado) (resultado-gols2 resultado)) 1
         (if (> (resultado-gols2 resultado) (resultado-gols1 resultado)) 3 0))]
    [else 0]))

(examples
 (check-equal? (calcula-pontos-jogo "Sao-Paulo" (resultado "Sao-Paulo" 1 "Atletico-MG" 2)) 0)
 (check-equal? (calcula-pontos-jogo "Sao-Paulo" (resultado "Sao-Paulo" 1 "Atletico-MG" 1)) 1)
 (check-equal? (calcula-pontos-jogo "Sao-Paulo" (resultado "Sao-Paulo" 2 "Atletico-MG" 1)) 3)
 (check-equal? (calcula-pontos-jogo "Atletico-MG" (resultado "Sao-Paulo" 2 "Atletico-MG" 1)) 0)
 (check-equal? (calcula-pontos-jogo "Atletico-MG" (resultado "Sao-Paulo" 1 "Atletico-MG" 1)) 1)
 (check-equal? (calcula-pontos-jogo "Atletico-MG" (resultado "Sao-Paulo" 1 "Atletico-MG" 2)) 3))

;; Calcula as vitorias de um time em um jogo dado o resultado
;; String Resultado -> Int
;; Se o time vencer recebe 1 vitoria, caso contrario recebe 0 vitorias
(define (calcula-vitorias-jogo time resultado)
  (cond
    [(equal? (resultado-time1 resultado) time)
     (if (> (resultado-gols1 resultado) (resultado-gols2 resultado)) 1 0)]
    [(equal? (resultado-time2 resultado) time)
     (if (< (resultado-gols1 resultado) (resultado-gols2 resultado)) 1 0)]
    [else 0]))

(examples
 (check-equal? (calcula-vitorias-jogo "Sao-Paulo" (resultado "Sao-Paulo" 1 "Atletico-MG" 2)) 0)
 (check-equal? (calcula-vitorias-jogo "Sao-Paulo" (resultado "Sao-Paulo" 1 "Atletico-MG" 1)) 0)
 (check-equal? (calcula-vitorias-jogo "Sao-Paulo" (resultado "Sao-Paulo" 2 "Atletico-MG" 1)) 1)
 (check-equal? (calcula-vitorias-jogo "Atletico-MG" (resultado "Sao-Paulo" 2 "Atletico-MG" 1)) 0)
 (check-equal? (calcula-vitorias-jogo "Atletico-MG" (resultado "Sao-Paulo" 1 "Atletico-MG" 1)) 0)
 (check-equal? (calcula-vitorias-jogo "Atletico-MG" (resultado "Sao-Paulo" 1 "Atletico-MG" 2)) 1))

;; Calcula o saldo de gols de um time em um jogo dado o resultado
;; String Resultado -> Int
;; Saldo dado pela diferença de gols do time analisado e do time adversario
(define (calcula-saldo-jogo time resultado)
  (cond
    [(equal? (resultado-time1 resultado) time)
     (- (resultado-gols1 resultado) (resultado-gols2 resultado))]
    [(equal? (resultado-time2 resultado) time)
     (- (resultado-gols2 resultado) (resultado-gols1 resultado))]
    [else 0]))

(examples
 (check-equal? (calcula-saldo-jogo "Sao-Paulo" (resultado "Sao-Paulo" 1 "Atletico-MG" 2)) -1)
 (check-equal? (calcula-saldo-jogo "Sao-Paulo" (resultado "Sao-Paulo" 1 "Atletico-MG" 1)) 0)
 (check-equal? (calcula-saldo-jogo "Sao-Paulo" (resultado "Sao-Paulo" 2 "Atletico-MG" 1)) 1)
 (check-equal? (calcula-saldo-jogo "Atletico-MG" (resultado "Sao-Paulo" 2 "Atletico-MG" 1)) -1)
 (check-equal? (calcula-saldo-jogo "Atletico-MG" (resultado "Sao-Paulo" 1 "Atletico-MG" 1)) 0)
 (check-equal? (calcula-saldo-jogo "Atletico-MG" (resultado "Sao-Paulo" 1 "Atletico-MG" 2)) 1))

;; Procura o desemenho de um time em uma lista de desempenhos, se não encontrar retorna #f, caso contrario retorna o desempenho
;; String ListaDesempenho -> Desempenho
(define (encontra-desempenho time desempenhos)
  (define desempenho (filter (lambda (time-encontrado) (equal? time (desempenho-time time-encontrado))) desempenhos))
  (if (empty? desempenho)
      #f ; O time não está na lista
      (first desempenho))) ; Retorna o desempenho encontrado

(examples
 (check-equal? (encontra-desempenho "Sao-Paulo" (list (desempenho "Sao-Paulo" 1 0 0)
                                                      (desempenho "Atletico-MG" 3 1 0)
                                                      (desempenho "Palmeiras" 1 0 -1)
                                                      (desempenho "Flamengo" 1 0 -1)))
               (desempenho "Sao-Paulo" 1 0 0))
 (check-equal? (encontra-desempenho "Botafogo" (list (desempenho "Sao-Paulo" 1 0 0)
                                                     (desempenho "Atletico-MG" 3 1 0)
                                                     (desempenho "Palmeiras" 1 0 -1)
                                                     (desempenho "Flamengo" 1 0 -1)))
               #f))

;; Atualiza o desempenho de ambos os times dado um resultado e uma lista de desempenhos
;; Se o time não estiver na lista de desempenhos, cria um novo desempenho para ele
;; resultado lista-desempenho -> lista-desempenho
(define (atualiza-desempenho resultado desempenhos)
  (define time1 (resultado-time1 resultado))
  (define time2 (resultado-time2 resultado))
  (define desempenho1 (encontra-desempenho time1 desempenhos)) ;; Se houver um desempenho, utiliza-lo como base
  (define desempenho2 (encontra-desempenho time2 desempenhos)) ;; Se houver um desempenho, utiliza-lo como base

  (define pontos-jogo1 (calcula-pontos-jogo time1 resultado))
  (define vitorias-jogo1 (calcula-vitorias-jogo time1 resultado))
  (define saldo-jogo1 (calcula-saldo-jogo time1 resultado))

  (define pontos-jogo2 (calcula-pontos-jogo time2 resultado))
  (define vitorias-jogo2 (calcula-vitorias-jogo time2 resultado))
  (define saldo-jogo2 (calcula-saldo-jogo time2 resultado))

  (define desempenho-atual1
    (or desempenho1
        (desempenho time1 0 0 0))) ; Se o time ainda não estiver na lista, cria um desempenho novo vazio

  (define desempenho-atual2
    (or desempenho2
        (desempenho time2 0 0 0))) ; Se o time ainda não estiver na lista, cria um desempenho novo vazio

  ;; Crie uma lista onde o desempenho atualizado de ambos os times são inseridos primeiro e o restante dos desempenhos são inseridos
  ;; ignorando os desempenhos desatualizados de ambos os times no resultado
  (cons (desempenho time1 (+ (desempenho-pontos desempenho-atual1) pontos-jogo1)
                    (+ (desempenho-vitorias desempenho-atual1) vitorias-jogo1)
                    (+ (desempenho-saldo-gols desempenho-atual1) saldo-jogo1))
        (cons (desempenho time2 (+ (desempenho-pontos desempenho-atual2) pontos-jogo2)
                          (+ (desempenho-vitorias desempenho-atual2) vitorias-jogo2)
                          (+ (desempenho-saldo-gols desempenho-atual2) saldo-jogo2))
              (filter (lambda (d) (not (or (equal? time1 (desempenho-time d)) ;; Filtragem dos desempenhos desatualizados
                                           (equal? time2 (desempenho-time d)))))
                      desempenhos))))

(examples
 (check-equal?
  (atualiza-desempenho (resultado "Sao-Paulo" 1 "Atletico-MG" 2)
                       (list (desempenho "Sao-Paulo" 1 0 0)
                             (desempenho "Atletico-MG" 3 1 0)
                             (desempenho "Palmeiras" 1 0 -1)
                             (desempenho "Flamengo" 1 0 -1)))
  (list (desempenho "Sao-Paulo" 1 0 -1)
        (desempenho "Atletico-MG" 6 2 1)
        (desempenho "Palmeiras" 1 0 -1)
        (desempenho "Flamengo" 1 0 -1)))

 (check-equal?
  (atualiza-desempenho (resultado "Sao-Paulo" 1 "Botafogo" 1)
                       (list (desempenho "Sao-Paulo" 1 0 0)
                             (desempenho "Atletico-MG" 3 1 0)
                             (desempenho "Palmeiras" 1 0 -1)
                             (desempenho "Flamengo" 1 0 -1)))

  (list (desempenho "Sao-Paulo" 2 0 0)
        (desempenho "Botafogo" 1 0 0)
        (desempenho "Atletico-MG" 3 1 0)
        (desempenho "Palmeiras" 1 0 -1)
        (desempenho "Flamengo" 1 0 -1))))

;; Atualiza os desempenhos de todos os times dado uma lista de resultados e uma lista de desempenhos
;; lista-resultado lista-desempenho -> lista-desempenho
(define (atualiza-desempenhos resultados desempenhos-base)
  (define (atualiza resultado desempenhos-base)
    (atualiza-desempenho resultado desempenhos-base))
  (foldr atualiza desempenhos-base resultados))

(examples
 (check-equal?
  (atualiza-desempenhos (list (resultado "Sao-Paulo" 1 "Atletico-MG" 2)
                              (resultado "Flamengo" 2 "Palmeiras" 1)
                              (resultado "Palmeiras" 0 "Sao-Paulo" 0)
                              (resultado "Atletico-MG" 1 "Flamengo" 2))
                        empty)
  (list (desempenho "Sao-Paulo" 1 0 -1)
        (desempenho "Atletico-MG" 3 1 0)
        (desempenho "Flamengo" 6 2 2)
        (desempenho "Palmeiras" 1 0 -1))))

(define (calcula-desempenhos resultados)
  (atualiza-desempenhos resultados empty))

;; Verifica se o desempenho d1 de um time é melhor que o desempenho d2 de outro time.
;; O primeiro criterio a ser analisado é o numero de pontos, em caso de empate,
;; o segundo criterio é o numero de vitorias, em caso de empate, o terceiro é o
;; saldo de gols e por fim, em caso de empate em todos os parametros anteriores,
;; o criterio final de desempate é a ordem alfabetica do nome do time
;; Desempenho Desempenho -> Boolean
(define (comparar-desempenho d1 d2)
  (cond
    [(> (desempenho-pontos d1) (desempenho-pontos d2))
     #t]
    [(< (desempenho-pontos d1) (desempenho-pontos d2))
     #f]
    [(> (desempenho-vitorias d1) (desempenho-vitorias d2))
     #t]
    [(< (desempenho-vitorias d1) (desempenho-vitorias d2))
     #f]
    [(> (desempenho-saldo-gols d1) (desempenho-saldo-gols d2))
     #t]
    [(< (desempenho-saldo-gols d1) (desempenho-saldo-gols d2))
     #f]
    [else (if (string<? (desempenho-time d1) (desempenho-time d2))
              #t  ;; Caso time de desempenho d1 venha antes na ordem alfabética
              #f) ;; Caso contrário
          ]))

(examples
 (check-equal? (comparar-desempenho (desempenho "Sao-Paulo" 1 0 -1)
                                    (desempenho "Atletico-MG" 6 2 1))
               #f)
 (check-equal? (comparar-desempenho (desempenho "Sao-Paulo" 4 0 -1)
                                    (desempenho "Atletico-MG" 1 2 1))
               #t)
 (check-equal? (comparar-desempenho (desempenho "Sao-Paulo" 3 0 -1)
                                    (desempenho "Atletico-MG" 3 2 1))
               #f)
 (check-equal? (comparar-desempenho (desempenho "Sao-Paulo" 3 1 1)
                                    (desempenho "Atletico-MG" 3 0 2))
               #t)
 (check-equal? (comparar-desempenho (desempenho "Sao-Paulo" 3 0 2)
                                    (desempenho "Atletico-MG" 3 1 1))
               #f)
 (check-equal? (comparar-desempenho (desempenho "Sao-Paulo" 3 0 2)
                                    (desempenho "Atletico-MG" 3 0 1))
               #t)
 (check-equal? (comparar-desempenho (desempenho "Sao-Paulo" 3 0 2)
                                    (desempenho "Atletico-MG" 3 0 2))
               #f)
 (check-equal? (comparar-desempenho (desempenho "Atletico-MG" 3 0 2)
                                    (desempenho "Sao-Paulo" 3 0 2)) #t))

;; Retorna o melhor desempenho dentre todos em um conjunto de desempenhos
;; lista-desempenho -> desempenho
(define (melhor-desempenho desempenhos)
  (if (empty? desempenhos)
      empty
      (foldr (lambda (d1 d2)
               (if (comparar-desempenho d1 d2) d1 d2))
             (first desempenhos)
             (rest desempenhos))))

(examples
 (check-equal? (melhor-desempenho (list (desempenho "Sao-Paulo" 1 0 -1)
                                        (desempenho "Atletico-MG" 6 2 1)
                                        (desempenho "Palmeiras" 1 0 -1)
                                        (desempenho "Flamengo" 6 2 1)))
               (desempenho "Atletico-MG" 6 2 1))
 (check-equal? (melhor-desempenho (list (desempenho "Sao-Paulo" 1 0 -1)
                                        (desempenho "Atletico-MG" 1 2 3)
                                        (desempenho "Palmeiras" 1 0 -1)
                                        (desempenho "Flamengo" 1 2 1))) (desempenho "Atletico-MG" 1 2 3))
 (check-equal? (melhor-desempenho (list (desempenho "Sao-Paulo" 1 0 3)
                                        (desempenho "Botafogo" 1 2 3)
                                        (desempenho "Palmeiras" 1 0 3)
                                        (desempenho "Flamengo" 1 2 3))) (desempenho "Botafogo" 1 2 3)))

;; Ordena uma lista de desempenhos pelo desempenho geral de cada time
;; lista-desempenho -> lista-desempenho
(define (ordenar-por-desempenho-geral desempenhos)
  (define maior (melhor-desempenho desempenhos))
  (if (empty? desempenhos)
      empty
      (cons maior (ordenar-por-desempenho-geral (filter (lambda (d) (not (equal? maior d))) desempenhos)))))

(examples
 (check-equal? (ordenar-por-desempenho-geral (list (desempenho "Sao-Paulo" 1 0 -1)
                                                   (desempenho "Atletico-MG" 6 2 1)
                                                   (desempenho "Palmeiras" 1 0 -1)
                                                   (desempenho "Flamengo" 6 2 1)))
               (list (desempenho "Atletico-MG" 6 2 1)
                     (desempenho "Flamengo" 6 2 1)
                     (desempenho "Palmeiras" 1 0 -1)
                     (desempenho "Sao-Paulo" 1 0 -1))))

;; Transforma um desempenho em uma string
;; Desempenho -> String
(define (desempenho->string desempenho)
  (string-append (desempenho-time desempenho) " "
                 (number->string (desempenho-pontos desempenho)) " "
                 (number->string (desempenho-vitorias desempenho)) " "
                 (number->string (desempenho-saldo-gols desempenho))))

(examples
 (check-equal? (desempenho->string (desempenho "Sao-Paulo" 1 0 -1)) "Sao-Paulo 1 0 -1"))

;; Classifica os times de acordo com os resultados dos jogos
;; ListaString -> ListaString
(define (classifica-times sresultados)

  ;; Transforma a lista de strings da entrada em uma lista de resultados
  (define resultados (map string->resultado sresultados))

  ;; Calcula o desempenho de cada time
  ;; ListaResultado -> ListaDesempenho
  (define desempenhos (calcula-desempenhos resultados))

  ;; Faz a classificao dos times pelo desempenho
  ;; ListaDesempenho -> ListaDesempenho
  (define classificacao (ordenar-por-desempenho-geral desempenhos))

  ;; Transforma classificação (lista de desempenhos) em uma lista de strings
  (map desempenho->string classificacao))

(display-lines (classifica-times (port->lines)))

(examples
 (check-equal? (classifica-times (list "Sao-Paulo 1 Atletico-MG 2"
                                       "Flamengo 2 Palmeiras 1"
                                       "Palmeiras 0 Sao-Paulo 0"
                                       "Atletico-MG 1 Flamengo 2"))
               (list "Flamengo 6 2 2"
                     "Atletico-MG 3 1 0"
                     "Palmeiras 1 0 -1"
                     "Sao-Paulo 1 0 -1"))
 (check-equal? (classifica-times (list "Sao-Paulo 1 Atletico-MG 1"
                                       "Flamengo 1 Palmeiras 1"
                                       "Palmeiras 1 Sao-Paulo 1"
                                       "Atletico-MG 1 Flamengo 1"))
               (list "Atletico-MG 2 0 0"
                     "Flamengo 2 0 0"
                     "Palmeiras 2 0 0"
                     "Sao-Paulo 2 0 0"))
 (check-equal? (classifica-times (list "Sao-Paulo 5 Atletico-MG 0"
                                       "Flamengo 3 Palmeiras 0"
                                       "Palmeiras 1 Sao-Paulo 0"
                                       "Atletico-MG 2 Flamengo 0"))
               (list "Sao-Paulo 3 1 4"
                     "Flamengo 3 1 1"
                     "Palmeiras 3 1 -2"
                     "Atletico-MG 3 1 -3")))
