-- Nome: Gabriel Prost Gomes Pereira

module Main where

main :: IO()
main = do
  putStr("Func. 1; entrada: 8; resultado: " ++ show (soma1 8) ++ "\n")
  putStr("Func. 1; entrada: -5; resultado: " ++ show (soma1 (-5)) ++ "\n")
  putStr("Func. 2; entrada: 10; resultado: " ++ show (sempre 10) ++ "\n")
  putStr("Func. 2; entrada: \"oi\"; resultado: " ++ show (sempre "oi") ++ "\n")
  putStr("Func. 3; entradas: 2.0 3.0 4.0; resultado: " ++ show (treco 2 3 4) ++ "\n")
  putStr("Func. 3; entradas: -3.0 1.0 7.0; resultado: " ++ show (treco (-3) 1 7) ++ "\n")
  putStr("Func. 4; entradas: 30 7; resultado: " ++ show (resto 30 7) ++ "\n")
  putStr("Func. 4; entradas: -7 10; resultado: " ++ show (resto (-7) 10) ++ "\n")
  putStr("Func. 5; entradas: 1 2 3 4; resultado: " ++ show (precoMaior 1 2 3 4) ++ "\n")
  putStr("Func. 5; entradas: -1 -2 -3 -4; resultado: " ++ show (precoMaior (-1) (-2) (-3) (-4)) ++ "\n")
  putStr("Func. 6; entradas: 5 7; resultado: " ++ show (impar 5 7) ++ "\n")
  putStr("Func. 6; entradas: -5 7; resultado: " ++ show (impar (-5) 7) ++ "\n")
  putStr("Func. 6; entradas: 5 8; resultado: " ++ show (impar 5 8) ++ "\n")
  putStr("Func. 6; entradas: -5 -9; resultado: " ++ show (impar (-5) (-9)) ++ "\n")
  putStr("Func. ?; entrada: (5, 76); resultado: " ++ show (somaPar (5, 76)) ++ "\n")
  putStr("Func. ?; entrada: (-6, -10); resultado: " ++ show (somaPar (-6, -10)) ++ "\n")
  putStr("Func. 7; entradas: 5 7 2; resultado: " ++ show (equacao 5 7 2) ++ "\n")
  putStr("Func. 7; entradas: -3 -8 14; resultado: " ++ show (equacao (-3) (-8) 14) ++ "\n")
  putStr("Func. 7; entradas: -3 -8 14; resultado: " ++ show (equacao (-3) (-8) 14) ++ "\n")
  putStr("Func. 8; entradas: 60 1.80; resultado: " ++ show (diagnostico 60 1.80) ++ "\n")
  putStr("Func. 8; entradas: 60 2.02; resultado: " ++ show (diagnostico 60 2.02) ++ "\n")
  putStr("Func. 8; entradas: -60 2.02; resultado: " ++ show (diagnostico (-60) 2.02) ++ "\n")
  putStr("Func. 9; entrada: 1997; resultado: " ++ show (bissexto 1997)  ++ "\n")
  putStr("Func. 9; entrada: 1900; resultado: " ++ show (bissexto 1900)  ++ "\n")
  putStr("Func. 9; entrada: 2000; resultado: " ++ show (bissexto 2000)  ++ "\n")

{-
1. Escreva uma fun????o chamada soma1 que recebe um inteiro como argumento e retorna um inteiro uma unidade maior que a entrada.
-}
soma1 :: Integer -> Integer
soma1 = (+ 1)

{-
2. Escreva  uma  fun????o  chamada  sempre  que,  n??o  importando  o  valor  de  entrada, devolva sempre zero. Observe que neste caso a entrada pode ser de qualquer tipo. 
-}
sempre :: _ -> Int
sempre x = 0

{-
3. Escreva  uma  fun????o  chamada  treco  que  receba  tr??s  valores  em  ponto  flutuantes com precis??o dupla e retorne o resultado da soma dos dois primeiros multiplicado pelo terceiro. 
-}
treco :: Double -> Double -> Double -> Double
treco x y = (*) (x + y)

{-
4. Escreva uma fun????o chamada resto que devolva o resto de uma divis??o entre dois n??meros inteiros.
-}
resto :: Integer -> Integer -> Integer
resto = mod -- "mod" eh equivalente ao operador '%' em C

{-
5. Escreva uma fun????o chamada precoMaior que devolva o maior valor entre quatro valores monet??rios. 
-}
precoMaior :: Integer -> Integer -> Integer -> Integer -> Integer
precoMaior w x y z = max (max w x) (max y z) 

{-
6. Escreva uma fun????o chamada impar que devolva True, sempre que o resultado do produto de dois n??meros inteiros for ??mpar.  
-}
divisibleBy :: Integer -> Integer -> Bool
divisibleBy x y = (resto x y) == 0

impar :: Integer -> Integer -> Bool
impar x y = not (divisibleBy (x * y) 2)

{-
Em Haskell existe o tipo par cuja assinatura tem a seguinte forma: ???????????????(????????????,????????????). Escreva uma fun????o em Haskell que devolva a soma dos componentes de um par de inteiros. 
-}
somaPar :: (Int, Int) -> Int
somaPar (a, b) = a + b 

{-
7. Escreva uma fun????o em Haskell que receba n??meros reais (double) e devolva o resultado da equa????o ????^2 +????/2 +????.
-}
equacao :: Double -> Double -> Double -> Double
equacao x y = (+)(x^2 + y / 2)

{-
8. Escreva uma fun????o em Haskell chamada diagnostico que receba o peso do aluno e imprima um  diagn??stico  de  obesidade,  segundo  a  tabela  que  pode  ser  encontrada  no  link: Sobrepeso,  obesidade  e  obesidade  m??rbida:  entenda  a  diferen??a  entre  os  tr??s  termos (cuidadospelavida.com.br).  Observe  que  este  diagn??stico  ??  meramente  estat??stico  e  n??o tem nenhum valor real, est?? sendo usado nesta quest??o apenas para a defini????o das faixas. Todo e qualquer diagn??stico deve ser feito por um profissional m??dico. 
-}
imcDiagnostico :: Double -> [Char]
imcDiagnostico imc
  | imc < 0 = "ERRO"
  | imc <= 17 = "Muito abaixo do peso"
  | imc <= 18.49 = "Abaixo do peso"
  | imc <= 24.99 = "Peso Normal"
  | imc <= 29.99 = "Sobrepeso"
  | imc <= 34.99 = "Obesidade leve"
  | imc <= 39.99 = "Obesidade severa"
  | otherwise = "Obesidade m??rbida"

diagnostico :: Double -> Double -> [Char]
diagnostico peso altura = imcDiagnostico (peso / altura^2)

{-
9. Escreva uma fun????o em Haskell chamada bissexto que receba um ano e devolva True se o 
ano for bisexto sabendo que anos bissextos obedecem a seguinte regra:  
???????????????????? ???????? ???????????????? ???????????? ???????????????????? ?????????????????????????????????????? ???????????? 4 
      ???????????????????????? ???????? ???????????????? ???????????? ?????????? ?????????????????????????????????? ???????? 100 
            ???????????????????????? ???????? ???????????????? ???????????? ?????????? ?????????????????????????????????? ???????? 400 
1997 n??o ?? bissexto, 1900 n??o ?? bissexto e 2000 ?? bissexto. 
-}
bissexto :: Integer -> Bool
bissexto x
  | divisibleBy x 400 = True
  | divisibleBy x 100 = False
  | divisibleBy x 4 = True
  | otherwise = False