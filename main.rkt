#lang racket
(require examples)

;; Desempenho -> (time: string, pontos: int, vitorias: int, saldo_gols: int)
;; Resultado -> (time1: string, gols1: int, time2: string, gols2: int)

(struct desempenho (time pontos vitorias saldo_gols) #:transparent)

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
             (second string-separada)
             (first (rest (rest string-separada)))
             (second (rest (rest string-separada)))))

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

#|(define calcula-pontos-jogo (time resultado)
  (cond
    [(equal? (resultado-time1 resultado) time)
     (if (= (resultado-gols1) (resultado-gols2)) 1 (- resultado-gols2 (* 3 (resultado-gols1))))]
    [(equal? (resultado-time2 resultado) time)
     (if (= (resultado-gols1) (resultado-gols2)) 1 (- resultado-gols1 (* 3 (resultado-gols2))))]
    [else 0]))

(define calcula-vitorias-jogo (time resultado)
  (cond
    [(equal? (resultado-time1 resultado) time)
     (if (> (resultado-gols1) (resultado-gols2)) 1 0]
    [(equal? (resultado-time2 resultado) time)
     (if (< (resultado-gols1) (resultado-gols2)) 1 0]
    [else 0]))

(define calcula-saldo-jogo (time resultado)
  (cond
    [(equal? (resultado-time1 resultado) time)
     (- (resultado-gols1) (resultado-gols2))]
    [(equal? (resultado-time1 resultado) time)
     (- (resultado-gols2) (resultado-gols1))]
    [else 0]))
|#
;(define (calcula-desempenhos times resultados))

;(define (desempenhos (calcula-desempenhos times resultados)))