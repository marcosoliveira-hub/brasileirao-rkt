#lang racket
(require examples)

;; Desempenho -> (time: string, pontos: int, vitorias: int, saldo_gols: int)
;; Resultado -> (time1: string, gols1: int, time2: string, gols2: int)

(struct desempenho (time pontos vitorias saldo-gols) #:transparent)

(struct resultado (time1 gols1 time2 gols2) #:transparent)

#| (examples
 (check-equal? (desempenho-time ((list "Sao-Paulo 1 Atletico-MG 2"
                                       "Flamengo 2 Palmeiras 1"
                                       "Palmeiras 0 Sao-Paulo 0"
                                       "Atletico-MG 1 Flamengo 2")))
               (list "Flamengo 6 2 2"
                     "Atletico-MG 3 1 0"
                     "Palmeiras 1 0 -1"
                     "Sao-Paulo 1 0 -1"))) |#


(define (string->resultado s)
  (define string-separada (string-split s))
  (resultado (first string-separada)
             (string->number (second string-separada))
             (first (rest (rest string-separada)))
             (string->number (second (rest (rest string-separada))))))

(define (nao-contem? s lst)
  (cond
    [(empty? lst) #t]
    [(equal? s (first lst)) #f]
    [else (nao-contem? s (rest lst))]))


;; Transforma a lista de strings da entrada em uma lista de resultados
(define resultados (map string->resultado (list "Sao-Paulo 1 Atletico-MG 2"
                                                "Flamengo 2 Palmeiras 1"
                                                "Palmeiras 0 Sao-Paulo 0"
                                                "Atletico-MG 1 Flamengo 2")))

;; (lista resultado) -> (lista string)
(define (encontra-times resultados lst-times)
  (define time1 (resultado-time1 (first resultados)))
  (define time2 (resultado-time2 (first resultados)))
  (cond
    [(and (nao-contem? time1 lst-times) (nao-contem? time2 lst-times) lst-times)
     (encontra-times (rest resultados) (append (list time1 time2) lst-times))]
    [(nao-contem? time1 lst-times) (encontra-times (rest resultados) (cons time1 lst-times))]
    [(nao-contem? time2 lst-times) (encontra-times (rest resultados) (cons time2 lst-times))]
    [else (if (empty? (rest resultados)) lst-times (encontra-times (rest resultados) lst-times))]))

(define times (encontra-times resultados empty))


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

;; Se o time vencer recebe 1 vitoria, caso contrario recebe 0 vitorias
(define (calcula-vitorias-jogo time resultado)
  (cond
    [(equal? (resultado-time1 resultado) time)
     (if (> (resultado-gols1 resultado) (resultado-gols2 resultado)) 1 0)]
    [(equal? (resultado-time2 resultado) time)
     (if (< (resultado-gols1 resultado) (resultado-gols2 resultado)) 1 0)]
    [else 0]))

;; Saldo dado pela diferença de gols do time analisado e do time adversario
(define (calcula-saldo-jogo time resultado)
  (cond
    [(equal? (resultado-time1 resultado) time)
     (- (resultado-gols1 resultado) (resultado-gols2 resultado))]
    [(equal? (resultado-time2 resultado) time)
     (- (resultado-gols2 resultado) (resultado-gols1 resultado))]
    [else 0]))


(define (encontra-desempenho time desempenhos)
  (define desempenho (filter (lambda (time-encontrado) (equal? time (desempenho-time time-encontrado))) desempenhos))
  (if (empty? desempenho)
      #f ; O time não está na lista
      (first desempenho))) ; Retorna o desempenho encontrado


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

(define (atualiza-desempenhos resultados desempenhos-base)
  (define (atualiza resultado desempenhos-base)
    (atualiza-desempenho resultado desempenhos-base))
  (foldl atualiza desempenhos-base resultados))

(define (calcula-desempenhos resultados)
  (atualiza-desempenhos resultados empty))

;; (lista string) (lista resultado) -> (lista desempenho)
(define desempenhos (calcula-desempenhos resultados))

;; (lista desemepenho) -> (lista desempenho)
