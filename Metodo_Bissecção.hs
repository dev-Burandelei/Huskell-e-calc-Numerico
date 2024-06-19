module Main where

import Control.Monad (mapM_)

-- Definindo o tipo de dados para as informações de cada iteração
data IteracaoInfo = IteracaoInfo
    { iteracao :: Int
    , pontoMedio :: Double
    , extremidadeInf :: Double
    , extremidadeSup :: Double
    , valorF :: Double
    } deriving Show

-- Definindo o limite inferior do intervalo
extremidade_inf :: Double
extremidade_inf = 1.0

-- Definindo o limite superior do intervalo
extremidade_sup :: Double
extremidade_sup = 20.0

-- Variáveis adicionais
tol :: Double
tol = 1e-6

max_iteracoes :: Int
max_iteracoes = 100 

-- Função que define a função f(x) = x^3 - x - 2
f :: Double -> Double
--f x = x ^ 3 - 9*x + 3
--f x = 2*(x^3) - 20*x - 7
--f x = x * (logBase 10 x) - 1
f x = 10* exp (-x)
-- Função que verifica se o ponto médio é a raiz da função ou se a tolerância foi atingida
zero_Func :: Double -> Bool
zero_Func ponto_medio = abs (f ponto_medio) < tol

-- Função recursiva que implementa o método da bisseção e retorna uma lista de informações de iteração
metodo_Bisseccao :: Int -> Double -> Double -> Double -> [IteracaoInfo]
metodo_Bisseccao num_interacoes extremidade_inf extremidade_sup f_ponto_medio
    -- Verifica se o número máximo de iterações foi excedido
    | num_interacoes >= max_iteracoes = []
    -- Calcula o ponto médio do intervalo atual
    | otherwise = IteracaoInfo num_interacoes ponto_medio extremidade_inf extremidade_sup f_ponto_medio : proximaIteracao
    where
        ponto_medio = (extremidade_inf + extremidade_sup) / 2
        f_ponto_medio = f ponto_medio
        -- Verifica se o ponto médio é a raiz da função
        convergiu = zero_Func ponto_medio
        -- Calcula a próxima iteração
        proximaIteracao =
            if convergiu
                then []
                else if (f ponto_medio) * (f extremidade_inf) < 0 -- Se f(ponto_medio) e f(Extremidade_inf) possuem sinais opostos
                    then metodo_Bisseccao (num_interacoes + 1) extremidade_inf ponto_medio f_ponto_medio
                    else metodo_Bisseccao (num_interacoes + 1) ponto_medio extremidade_sup f_ponto_medio

-- Função principal do programa
main :: IO ()
main = do
    -- Chama a função de bisseção e obtém a lista de informações de iteração
    let iteracoes = metodo_Bisseccao 0 extremidade_inf extremidade_sup (f ((extremidade_inf + extremidade_sup) / 2))
    -- Imprime as informações de cada iteração
    mapM_ print iteracoes
    -- Extrai a raiz da última iteração (ponto médio do último intervalo)
    let raiz = pontoMedio (last iteracoes)
    putStrLn $ "Raiz encontrada: " ++ show raiz
