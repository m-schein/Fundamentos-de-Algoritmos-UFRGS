;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname listaCap9-MaiaraScheinTrevisol-TurmaA) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;QUESTÃO 1)

(define-struct carro(ano modelo valor ar direcao vidros))
;Um elemento do conjunto Carro tem o formato
; (make-carro ano modelo valor ar direcao vidros)
;onde:
;ano: Integer, é o ano de fabricação do carro
;modelo: String, é o modelo do carro
;valor: Integer, é o valor de mercado do carro
;ar: Booleano, caso seja verdadeiro, indica que o carro possui ar-condicionado
;direcao: Integer, caso seja 1, indica que o carro possui direção hidráulica, caso seja zero, indica que o carro não possui direção hidráulica
;vidros: Integer, caso seja 1, indica que o carro possui vidros elétricos, caso seja zero, indica que o carro não possui direção hidráulica

(define carro1 (make-carro 2000 "Corsa" 16000 false false true))
(define carro2 (make-carro 2015 "Palio" 30000 true true true))
(define carro3 (make-carro 1996 "Toyota" 5000 false false false))
(define carro4 (make-carro 2020 "Jeep" 80000 true true true))
(define carro5 (make-carro 2012 "Celta" 20000 false true false))

(define carro6 (make-carro 2000 "Corsa" 12000 false false true))
(define carro7 (make-carro 2015 "Palio" 90000 false true true))
(define carro8 (make-carro 1996 "Toyota" 3000 false false false))
(define carro9 (make-carro 2020 "Jeep" 200000 false true true))
(define carro10 (make-carro 2012 "Celta" 15000 false true true))
(define listaDeCarros2 (cons carro6(cons carro7(cons carro8(cons carro9(cons carro10 empty))))))

;QUESTÃO 2)
;Uma listaDeCarros é a lista
(define listaDeCarros (cons carro5(cons carro4(cons carro3(cons carro2(cons carro1 empty))))))
;Onde:
;carro1, carro2, carro3, carro4 e carro5: são estruturas do tipo Carro.

;FUNÇÃO:   temAC  listaDeCarros -> Integer
;Objetivo: Dada uma lista de structs do tipo Carro, a função verifica se algum dos carros
;          vêm equipados com ar condicionado.
;Exemplos:
;(temAC listaDeCarros) = #true
;(temAC listaDeCarros2) = #false

(define (temAC listaDeCarros)
 (cond
   [(empty? listaDeCarros) false]
    [else
     (cond
       [(carro-ar (first listaDeCarros)) true]
       [else (temAC(rest listaDeCarros))]
       )]
 )
)
;TESTES:
(check-expect (temAC listaDeCarros) #true)
(check-expect (temAC listaDeCarros2) #false)



;QUESTÃO 3)
;FUNÇÃO: quantosVE  listaDeCarros -> Integer
;Objetivo: Dada uma lista de structs do tipo Carro, a função calcula quantos dos carros
;          vêm equipados com vidros elétricos.
;Exemplos:
;(valor listaDeCarros) = 3
;(valor listaDeCarros2) = 4

(define (quantosVE listaDeCarros)
  (cond
    [(empty? listaDeCarros) 0]
    [(carro-vidros (first listaDeCarros))(+ 1 (quantosVE(rest listaDeCarros)))]
    [else (quantosVE(rest listaDeCarros))]
  )
 )
;Testes:
(check-expect(quantosVE listaDeCarros) 3)
(check-expect(quantosVE listaDeCarros2) 4)




;QUESTÃO 4)
;FUNÇÃO: valor  listaDeCarros -> Integer
;Objetivo: Dada uma lista de structs do tipo Carro, a função calcula a soma dos
;          valores de mercado de todos os carros do cadastro.
;Exemplos:
;(valor listaDeCarros) = 151000
;(valor listaDeCarros2) = 320000

(define (valor listaDeCarros)
  (cond
    [(empty? listaDeCarros) 0]
    [else (+(carro-valor (first listaDeCarros)) (valor(rest listaDeCarros)))]
  )
 )
;Testes:
(check-expect(valor listaDeCarros)151000)
(check-expect(valor listaDeCarros2)320000)




;QUESTÃO 5)
;FUNÇÃO: desconto  Carro -> Carro
;Objetivo: Dada uma struct do tipo Carro, a função calcula um desconto de 10% no
;          valore de mercado do carro informado e retorna o carro com o novo valor.
;Exemplos:
;(desconto carro1) = (make-carro 2000 "Corsa" 14400 #false #false #true)
;(desconto carro2) = (make-carro 2015 "Palio" 27000 #true #true #true)


(define (desconto carroQualquer)
  (make-carro (carro-ano carroQualquer)
              (carro-modelo carroQualquer)
              (* 0.9 (carro-valor carroQualquer))
              (carro-ar carroQualquer)
              (carro-direcao carroQualquer)
              (carro-vidros carroQualquer))
    )

;Testes:
(check-expect (desconto carro1) (make-carro 2000 "Corsa" 14400 #false #false #true))
(check-expect (desconto carro2) (make-carro 2015 "Palio" 27000 #true #true #true))

;FUNÇÃO: decValor  listaDeCarros ano -> Carro
;Objetivo: Dada uma lista de structs do tipo Carro (Cadastro) e um ano, a função verifica se há algum carro
;          com o ano de fabricação correspondente ao informado e calcula um desconto de 10% no valor de 
;          mercado do carro escolhido. Caso não haja carros desse ano, ela retorna "Não há carros no cadastro".
;Exemplos:
;(decValor listaDeCarros 1996) = (make-carro 2012 "Celta" 18000 #false #true #false)
;(decValor listaDeCarros 2012) = (make-carro 1996 "Toyota" 4500 #false #false #false)

(define (decValor listaDeCarros ano)
  (cond
    [(empty? listaDeCarros) "Não há carros no cadastro"]
    [(eqv?(carro-ano(first listaDeCarros))ano) (desconto (first listaDeCarros)) ]
    [else (decValor (rest listaDeCarros) ano)]
    )
  )

;Testes:
(check-expect(decValor listaDeCarros 2012) (make-carro 2012 "Celta" 18000 #false #true #false))
(check-expect(decValor listaDeCarros 1996) (make-carro 1996 "Toyota" 4500 #false #false #false))
(check-expect(decValor listaDeCarros 1990) "Não há carros no cadastro")

