module APL.Eval
  ( Val (..),
    Env,
    envEmpty,
    eval,
    printExp
  )
where

import APL.AST (Exp (..), VName)

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  deriving (Eq, Show)

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env

type Error = String

evalIntBinOp :: (Integer -> Integer -> Either Error Integer) -> Env -> Exp -> Exp -> Either Error Val
evalIntBinOp f env e1 e2 =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> case f x y of
      Left err -> Left err
      Right z -> Right $ ValInt z
    (Right _, Right _) -> Left "Non-integer operand"

evalIntBinOp' :: (Integer -> Integer -> Integer) -> Env -> Exp -> Exp -> Either Error Val
evalIntBinOp' f env e1 e2 =
  evalIntBinOp f' env e1 e2
  where
    f' x y = Right $ f x y

eval :: Env -> Exp -> Either Error Val
eval _env (CstInt x) = Right $ ValInt x
eval _env (CstBool b) = Right $ ValBool b
eval env (Var v) = case envLookup v env of
  Just x -> Right x
  Nothing -> Left $ "Unknown variable: " ++ v
eval env (Add e1 e2) = evalIntBinOp' (+) env e1 e2
eval env (Sub e1 e2) = evalIntBinOp' (-) env e1 e2
eval env (Mul e1 e2) = evalIntBinOp' (*) env e1 e2
eval env (Div e1 e2) = evalIntBinOp checkedDiv env e1 e2
  where
    checkedDiv _ 0 = Left "Division by zero"
    checkedDiv x y = Right $ x `div` y
eval env (Pow e1 e2) = evalIntBinOp checkedPow env e1 e2
  where
    checkedPow x y =
      if y < 0
        then Left "Negative exponent"
        else Right $ x ^ y
eval env (Eql e1 e2) =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValBool $ x == y
    (Right (ValBool x), Right (ValBool y)) -> Right $ ValBool $ x == y
    (Right _, Right _) -> Left "Invalid operands to equality"
eval env (If cond e1 e2) =
  case eval env cond of
    Left err -> Left err
    Right (ValBool True) -> eval env e1
    Right (ValBool False) -> eval env e2
    Right _ -> Left "Non-boolean conditional."
eval env (Let var e1 e2) =
  case eval env e1 of
    Left err -> Left err
    Right v -> eval (envExtend var v env) e2

-- TODO: Add cases after extending Exp.
-- New: Evaluate Lambda expressions
evaluation env (Lambda param body) = Right $ ValFun env param body

-- New: Evaluate function application (Apply)
evaluation env (Apply func arg) =
  case eval env func of
    Left err -> Left err
    Right (ValFun closureEnv param body) ->
      case eval env arg of
        Left err -> Left err
        Right argVal -> eval (envExtend param argVal closureEnv) body
    Right _ -> Left "Trying to apply a non-function value."

-- New: Handle TryCatch expressions
evaluation env (TryCatch tryExp catchExp) =
  case eval env tryExp of
    Right val -> Right val
    Left _ -> eval env catchExp

    -- Pretty-printer for expressions
printExp :: Exp -> String
-- Constants
printExp (CstInt n) = show n
printExp (CstBool b) = if b then "True" else "False"

-- Variables
printExp (Var x) = x

-- Binary operators (use infix notation)
printExp (Add e1 e2) = "(" ++ printExp e1 ++ " + " ++ printExp e2 ++ ")"
printExp (Sub e1 e2) = "(" ++ printExp e1 ++ " - " ++ printExp e2 ++ ")"
printExp (Mul e1 e2) = "(" ++ printExp e1 ++ " * " ++ printExp e2 ++ ")"
printExp (Div e1 e2) = "(" ++ printExp e1 ++ " / " ++ printExp e2 ++ ")"
printExp (Pow e1 e2) = "(" ++ printExp e1 ++ " ** " ++ printExp e2 ++ ")"
printExp (Eql e1 e2) = "(" ++ printExp e1 ++ " == " ++ printExp e2 ++ ")"

-- If expression
printExp (If cond e1 e2) =
  "if " ++ printExp cond ++ " then " ++ printExp e1 ++ " else " ++ printExp e2

-- Let expression
printExp (Let var e1 e2) =
  "let " ++ var ++ " = " ++ printExp e1 ++ " in " ++ printExp e2

-- Lambda expression
printExp (Lambda param body) =
  "\\" ++ param ++ " -> " ++ printExp body

-- TryCatch expression
printExp (TryCatch tryExp catchExp) =
  "try " ++ printExp tryExp ++ " catch " ++ printExp catchExp

-- Helper for printing arguments
-- Add parentheses if the argument is not a constant or variable
printExpArg :: Exp -> String
printExpArg e@(CstInt _) = printExp e
printExpArg e@(CstBool _) = printExp e
printExpArg e@(Var _) = printExp e
printExpArg e = "(" ++ printExp e ++ ")"