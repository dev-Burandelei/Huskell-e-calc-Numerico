import System.IO (hFlush, stdout)

-- Variáveis adicionais
tol :: Double
tol = 1e-6

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
-- Função que calcula a derivada de uma expressão em relação a uma variável
deriv :: Expr -> String -> Expr
-- Derivada de uma constante
deriv (Const _) _ = Const 0
-- Derivada de uma variável
deriv (Var x) v
    | x == v = Const 1
    | otherwise = Const 0
-- Derivada de uma soma
deriv (Add e1 e2) v = Add (deriv e1 v) (deriv e2 v)
-- Derivada de um produto
deriv (Mul e1 e2) v = Add (Mul (deriv e1 v) e2) (Mul e1 (deriv e2 v))
-- Derivada de uma potência
deriv (Pow e n) v = Mul (Mul (Const n) (Pow e (n - 1))) (deriv e v)
-- Derivada para euler
deriv (Exp e) v = Mul (Exp e) (deriv e v)


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

-- Método de Newton-Raphson
newtonRaphson :: Expr -> Expr -> Double -> Double -> Int -> [Double]
newtonRaphson f f' x0 tol maxIter = newtonRaphson' x0 tol maxIter 0 []
  where
    newtonRaphson' :: Double -> Double -> Int -> Int -> [Double] -> [Double]
    newtonRaphson' x tol maxIter iter history
        | iter >= maxIter = history
        | zero_Func fx = x : history
        | otherwise = newtonRaphson' xNext tol maxIter (iter + 1) (x : history)
      where
        fx = eval f [("x", x)]
        fpx = eval f' [("x", x)]
        xNext = x - fx / fpx

-- Exemplo de expressão: f x = 10 * e⁻x
expr1 :: Expr
expr1 = Mul (Const 10) (Exp (Mul (Const (-1)) (Var "x")))

-- Derivada da expressão expr1 em relação a 'x'
derivExpr1 :: Expr
derivExpr1 = deriv expr1 "x"

-- Função principal
main :: IO ()
main = do
    -- Solicitar ao usuário a adivinhação inicial
    putStrLn "Digite a adivinhação inicial (x0):"
    hFlush stdout
    x0Str <- getLine
    let x0 = read x0Str :: Double
    
    -- Executar o método de Newton-Raphson
    let result = newtonRaphson expr1 derivExpr1 x0 tol max_iteracoes
    putStrLn "Histórico das iterações:"
    mapM_ print (reverse result)



