data BinaryOperator = PlusOperator 
			| MinusOperator 
			| MultOperator deriving(Show,Eq)

data UnaryOperator = UnaryMinusOerator deriving(Show,Eq)

data Term = IntConstant{ intValue :: Int }    
            | Variable{ varName :: String }    
            | BinaryTerm{ binaryOp :: BinaryOperator, lhv :: Term, rhv :: Term } 
            | UnaryTerm{ unaryOp :: UnaryOperator, hv :: Term } deriving(Show,Eq)

-- Binary plus
infixl 7 <+>
(<+>) (IntConstant a) (IntConstant b) = IntConstant (a + b)
(<+>) a b = BinaryTerm PlusOperator a b

-- Binary minus
infixl 7 <->
(<->) (IntConstant a)  (IntConstant b) = IntConstant (a - b)
(<->) a b = BinaryTerm MinusOperator a b

-- Binary mult
infixl 8 <*>
(<*>) (IntConstant a) (IntConstant b) = IntConstant (a * b)
(<*>) a b = BinaryTerm MultOperator a b

-- Unary minus
infixl 9 <-->
(<-->) (IntConstant a) = IntConstant (-a)
(<-->) a = UnaryTerm UnaryMinusOerator a

replaceVar :: Term -> String -> Term -> Term
replaceVar (IntConstant const) _ _ = IntConstant const
replaceVar (Variable var) nameString newTerm = if var == nameString then newTerm else Variable var
replaceVar (BinaryTerm op l r) nameString newTerm = BinaryTerm op (replaceVar l nameString newTerm) (replaceVar r nameString newTerm)
replaceVar (UnaryTerm op v) nameString newTerm = UnaryTerm op (replaceVar v nameString newTerm)