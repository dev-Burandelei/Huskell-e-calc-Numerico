import System.IO (hFlush, stdout)

-- Define a função f(x)
f :: Double -> Double
--f x = 10 * exp (-x)
--f x = (2*x ^ 3) - 20*x - 7
--f x = x ^ 2 + x - 6
--f x = x ^ 3 - 9*x + 3
--f x = 2*(x ^ 3) - 20*x - 7
f x = 10* exp (-x)


-- Define a função g(x) para o método do ponto fixo, tal que g(x) = ln(10/x)
g :: Double -> Double
--g x = log (10 / x)
--g x = (20 * x + 7) ** (1 / 3) / sqrt 2
--g x = 6 - x ^2
--g x = sqrt(6 -x)
--g x = ((x ^ 3)/9) + (1/3)
--g x = ((20*x + 7) / 2) ** (1/3)
g x = log (10/x)
-- Define a tolerância e o número máximo de iterações
tol :: Double
tol = 1e-5

max_iteracoes :: Int
max_iteracoes = 1000

-- Método do ponto fixo
metodoPontoFixo :: (Double -> Double) -> Double -> Double -> Int -> (Maybe Double, Int)
metodoPontoFixo g x0 tol maxIter = iterar x0 0
  where
    iterar x iter
      | abs (g x - x) < tol = (Just x, iter)
      | iter >= maxIter = (Nothing, iter)
      | otherwise = iterar (g x) (iter + 1)

-- Função para iniciar o método do ponto fixo
pontoFixo :: (Double -> Double) -> Double -> Double -> Int -> IO ()
pontoFixo g x0 tol maxIter = do
    let (resultado, iteracoes) = metodoPontoFixo g x0 tol maxIter
    case resultado of
        Nothing -> putStrLn $ "O método não convergiu após " ++ show iteracoes ++ " iterações."
        Just x -> putStrLn $ "Aproximação da raiz: " ++ show x ++ " encontrada em " ++ show iteracoes ++ " iterações."

main :: IO ()
main = do
    -- Solicitar ao usuário a adivinhação inicial
    putStrLn "Digite a adivinhação inicial (x0):"
    hFlush stdout
    x0Str <- getLine
    let x0 = read x0Str :: Double
    
    -- Iniciar o método do ponto fixo
    pontoFixo g x0 tol max_iteracoes
