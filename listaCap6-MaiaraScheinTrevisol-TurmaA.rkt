;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname listaCap6-MaiaraScheinTrevisol-TurmaA) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;QUESTAO 1)

(define-struct aluno (nome num curso inst))
;Um elemento do conjunto Aluno tem o formato
;        (make-aluno nome num curso inst)
;
;onde:
;nome: String, representa o nome do aluno
;num: Número, representa o número de matrícula do aluno
;curso: String, representa o curso do aluno
;inst: String, representa a instituição de ensino do aluno


(define-struct instEnsino (nomeInst anoFundacao))
;Um elemento do conjunto InstEnsino tem o formato:
;     (make-instEnsino nome anoFund)
;onde:
;nome: String, representa o nome da instituicao de ensino
;anoFund: Número, representa o ano de fundacao da instituicao de ensino



;QUESTAO 2)

;contrato:
;mesmaInstituicao? : Aluno Aluno Instituicao -> Booleano
;
;Objetivo: dado dois elementos da estrtutura Aluno e um da estrutura InstEnsino, esta função
;devolve verdadeiro se eles possuírem o mesmo valor para nome da
;instituição de ensino.

;Exemplos:
;(mesmaInstituicao? aluno1 aluno2 instituicao) = true
;(mesmaInstituicao? aluno1 aluno3 instituicao) = false

(define aluno1 (make-aluno "maria" 286509 "ECP" "UFRGS"))
(define aluno2 (make-aluno "pedro" 202843 "CIC" "UFRGS"))
(define aluno3 (make-aluno "joana" 342434 "CIC" "PUC"))
(define instituicao (make-instEnsino "UFRGS" 1903))

(define (mesmaInstituicao? aluno1 aluno2 instituicao)
  (cond
    [(string=? (aluno-inst aluno1)(aluno-inst aluno2)(instEnsino-nomeInst instituicao)) true]
    [else false]
  )
 )

;TESTES:
(check-expect (mesmaInstituicao? aluno1 aluno2 instituicao) #true)
(check-expect (mesmaInstituicao? aluno1 aluno3 instituicao) #false)



;QUESTAO 3)
;NOME: IDADE DA INSTITUIÇÃO - Aluno Aluno InstEnsino -> Número
;OBJETIVO: Dado dois alunos e uma instituição, caso os alunos estiverem matriculados nessa mesma instituição
;          o programa retorna a idade da instituição em 2020, caso contrário ele retorna -1;
;EXEMPLOS:
;(idade aluno1 aluno2 instituicao) = 117
;(idade aluno1 aluno3 instituicao) = -1

(define (idade aluno1 aluno2 instituicao)
  (cond
    [(mesmaInstituicao? aluno1 aluno2 instituicao)
     (- 2020 (instEnsino-anoFundacao instituicao))]
    [else -1]
    )
  )

;TESTES:
(check-expect (idade aluno1 aluno2 instituicao) 117)
(check-expect (idade aluno1 aluno3 instituicao) -1)



;QUESTAO 4)
;LETRA a)

(define-struct carro(ano modelo valor ar direcao vidros))
;Um elemento do conjunto Carro tem o formato
; (make-carro ano modelo valor ar direcao vidros)
;onde:
;ano: Integer, é o ano de fabricação do carro
;modelo: String, é o modelo do carro
;valor: Integer, é o valor de mercado do carro
;ar: Booleano, caso seja verdadeiro, indica que o carro possui ar-condicionado
;direcao: Booleano, caso verdadeiro, indica que o carro possui direção hidráulica
;vidros: Booleano, caso verdadeiro, indica que o carro possui vidros elétricos

(define-struct moto(ano modelo valor))
;Um elemento do conjunto Moto tem o formato
; (make-moto ano modelo valor)
;onde:
;ano: Integer, é o ano de fabricação da moto
;modelo: String, é o modelo da moto
;valor: Integer, é o valor de mercado da moto


(define carro1 (make-carro 2000 "Corsa" 15000 true false false))
(define carro2 (make-carro 2015 "Palio" 30000 true true false))
(define moto1 (make-moto 2007 "Honda" 6000))
(define moto2 (make-moto 2017 "Honda" 10000))

;Letra b)
;FUNÇÃO: cdc - calcula diária de locação de um carro: Carro -> Float
;Objetivos: Dado um elemento de estrututa Carro, calcula o preço de diária
;           com base nas especificações do mesmo              
;Exemplos:
;(cdc carro1) = 67.5
;(cdc carro2 = 105)
(define (cdc um-carro)
 (+ (* 0.0025 (carro-valor um-carro))
     (cond
        [(carro-ar um-carro) 30]
        [(carro-direcao um-carro) 30]
        [(carro-vidros um-carro) 30]
        )
    )
  )
;TESTES:
(check-expect (cdc carro1) 67.5)
(check-expect (cdc carro2) 105)

;Letra c)
;FUNÇÃO: cdm- calcula diária de locação de uma moto: Moto -> Float
;Objetivos: Dado um elemento de estrututa Moto, calcula o preço de diária
;           com base nas especificações da mesma  
;Exemplos
;(cdm moto1) = 85
;(cdm moto2) = 115

(define (cdm uma-moto)
  (+ (* 0.0025 (moto-valor uma-moto))
     (cond
       [(< (moto-ano uma-moto)  2011) 70]
       [else 90]
       )
  )
)

;TESTES:
(check-expect (cdm moto1) 85)
(check-expect (cdm moto2) 115)



;QUESTÃO 5)
;
(define centroCirculo(make-posn 0 0))
(define circunferencia1(make-posn 0 3))
(define circunferencia2(make-posn 0 5))
;um elemento do conjunto Posn tem o formato:
;       (make-posn valor-x valor-)
;Onde:
;valor-x: Integer, valor inteiro correspondente à coordenada x do Posn
;valor-y: Integer, valor inteiro correspondente à coordenada y do Posn


;FUNÇÃO: Calcula área do circulo: Posn Posn -> Float
;Objetivo: dado dois elementos do tipo Posn, um representando o centro do círculo
;          e outro representando um ponto em sua circunferencia, a função
;          deve retornar a área do círculo
;Exemplos:
;(area centroCirculo circunferencia1) = 28.2735
;(area centroCirculo circunferencia2) = 78.5375

(define (area centroCirculo circunferencia)
  (* 3.1415
     (+
      (sqr (- (posn-x circunferencia)(posn-x centroCirculo)))
      (sqr (- (posn-y circunferencia)(posn-y centroCirculo)))
      )
     )
  )

;TESTES:
(check-expect (area centroCirculo circunferencia1) 28.2735)
(check-expect (area centroCirculo circunferencia2) 78.5375)






