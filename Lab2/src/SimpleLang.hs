module SimpleLang where
-- Язык Simple -- очень простой императивный язык.
-- В нём только один тип данных: целые числа.

data Expression =
    Var String                   -- Переменные
  | Val Int                      -- Целые константы
  | Op Expression Bop Expression -- Бинарные операции
  deriving (Show, Eq)

data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt       -- >
  | Ge       -- >=
  | Lt       -- <
  | Le       -- <=
  | Eql      -- ==
  deriving (Show, Eq)

data Statement =
    -- присвоить переменной значение выражения
    Assign   String     Expression
    -- увеличить переменную на единицу
  | Incr     String
    -- ненулевые значения работают как истина в if, while и for
  | If       Expression Statement  Statement
  | While    Expression Statement
  | For      Statement  Expression Statement Statement
    -- как { ... } в C-подобных языках
  | Block [Statement]
    -- пустая инструкция
  | Skip
  deriving (Show, Eq)

-- примеры программ на этом языке в конце модуля

-- по состоянию можно получить значение каждой переменной
-- (в реальной программе скорее использовалось бы Data.Map.Map String Int)
type State = String -> Int

-- Задание 1 -----------------------------------------

-- в начальном состоянии все переменные имеют значение 0
empty :: State
empty = const 0

-- возвращает состояние, в котором переменная var имеет значение newVal, 
-- все остальные -- то же, что в state
extend :: State -> String -> Int -> State
extend state var newVal inp = if inp /= var then state inp else newVal

-- Задание 2 -----------------------------------------

-- возвращает значение выражения expr при значениях переменных из state.
eval :: State -> Expression -> Int
eval state (Var a) = state a
eval _ (Val a) = a
eval state (Op a oper b) = case oper of
        Plus -> (eval state a) + (eval state b)
        Minus -> (eval state a) - (eval state b)
        Times -> (eval state a) * (eval state b)
        Divide -> (eval state a) `div` (eval state b)
        Gt -> fromEnum ((eval state a) > (eval state b))
        Ge -> fromEnum ((eval state a) >= (eval state b))
        Lt -> fromEnum ((eval state a) < (eval state b))
        Le -> fromEnum ((eval state a) <= (eval state b))
        Eql -> fromEnum ((eval state a) == (eval state b))

-- Задание 3 -----------------------------------------

-- Можно выразить Incr через Assign, For через While, Block через 
-- последовательное выполнение двух инструкций (; в C).
-- Следующий тип задаёт упрощённый набор инструкций (промежуточный язык Simpler).
data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

-- упрощает программу Simple
desugar :: Statement -> DietStatement
desugar (Assign a exp) = DAssign a exp
desugar (Incr a) = DAssign a (Op (Var a) Plus (Val 1))
desugar (If exp state1 state2) = DIf exp (desugar state1) (desugar state2)
desugar (While exp state1) = DWhile exp (desugar state1)
desugar Skip = DSkip
desugar (Block []) = DSkip
desugar (Block (state1 : states)) = DSequence (desugar state1) (desugar (Block states))
desugar (For state exp state1 state2) = DSequence (desugar state) (DWhile exp (DSequence (desugar state2) (desugar state1)))

-- Задание 4 -----------------------------------------

-- принимает начальное состояние и программу Simpler
-- и возвращает состояние после работы программы
runSimpler :: State -> DietStatement -> State
runSimpler state (DAssign a exp) = extend state a (eval state exp)
runSimpler state (DIf exp state1 state2) = if eval state exp == 1 then runSimpler state state1 else runSimpler state state2
runSimpler state DSkip = state
runSimpler state (DWhile exp state1) = if eval state exp == 1 then runSimpler (runSimpler state state1) (DWhile exp state1) else state
runSimpler state (DSequence state1 state2) = runSimpler (runSimpler state state1) state2

-- 
-- in s "A" ~?= 10

-- принимает начальное состояние и программу Simple
-- и возвращает состояние после работы программы
run :: State -> Statement -> State
run state state1 = runSimpler state (desugar state1)

-- Программы -------------------------------------------

{- Вычисление факториала

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Вычисление целой части квадратного корня

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = Block [Assign "B" (Val 0),
                    While (Op (Var "A") Ge (Op (Var "B") Times (Var "B"))) (Assign "B" (Op (Var "B") Plus (Val 1))),
                    Assign "B" (Op (Var "B") Minus (Val 1))]

{- Вычисление числа Фибоначчи

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = Block [Assign "F0" (Val 1),
                   Assign "F1" (Val 1),
                   If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2)) (Op (Var "C") Le (Var "In"))
                                (Assign "C" (Op (Var "C") Plus (Val 1)))
                                (Block [Assign "T" (Op (Var "F0") Plus (Var "F1")),
                                        Assign "F0" (Var "F1"),
                                        Assign "F1" (Var "T"),
                                        Assign "Out" (Var "T")])))]
