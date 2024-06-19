import System.IO (hFlush, stdout)

-- Variáveis adicionais
tol :: Double
tol = 1e-4 

max_iteracoes :: Int
max_iteracoes = 100 

-- Definindo o tipo de dados para expressões matemáticas
data Expr
    = Const Double
    | Var String
    | Add Expr Expr
    | Mul Expr Expr
    | Pow Expr Double
    | Exp Expr
    deriving (Show)

-- Função que avalia uma expressão substituindo as variáveis pelos seus valores numéricos
eval :: Expr -> [(String, Double)] -> Double
eval (Const c) _ = c
eval (Var x) env = case lookup x env of
    Just v  -> v
    Nothing -> error $ "Variable " ++ x ++ " not found"
eval (Add e1 e2) env = eval e1 env + eval e2 env
eval (Mul e1 e2) env = eval e1 env * eval e2 env
eval (Pow e n) env = eval e env ** n
eval (Exp e) env = exp (eval e env)


-- Função que verifica se um valor está dentro da tolerância
zero_Func :: Double -> Bool
zero_Func x = abs x < tol

-- Método da Secante
secante :: Expr -> Double -> Double -> Int -> [Double]
secante f x0 x1 maxIter = secante' x0 x1 maxIter 0 []
  where
    secante' :: Double -> Double -> Int -> Int -> [Double] -> [Double]
    secante' x0 x1 maxIter iter history
        | iter >= maxIter = history
        | zero_Func fx1 = x1 : history
        | otherwise = secante' x1 xNext maxIter (iter + 1) (x1 : history)
      where
        fx0 = eval f [("x", x0)]
        fx1 = eval f [("x", x1)]
        xNext = x1 - fx1 * (x1 - x0) / (fx1 - fx0)

-- Exemplo de expressão: f x = 10 * e⁻x
expr1 :: Expr
expr1 = Mul (Const 10) (Exp (Mul (Const (-1)) (Var "x")))

-- Função principal
main :: IO ()
main = do
    -- Solicitar ao usuário as aproximações iniciais
    putStrLn "Digite a primeira aproximação (x0):"
    hFlush stdout
    x0Str <- getLine
    let x0 = read x0Str :: Double

    putStrLn "Digite a segunda aproximação (x1):"
    hFlush stdout
    x1Str <- getLine
    let x1 = read x1Str :: Double
    
    -- Executar o método da Secante
    let result = secante expr1 x0 x1 max_iteracoes
    putStrLn "Histórico das iterações:"
    mapM_ print (reverse result)
