data FunMonad a = FunMonad{ fun :: () -> a }

instance Functor FunMonad where
    fmap f (FunMonad x) = FunMonad (\() -> f (x ()) )

instance Applicative FunMonad where
    pure x = FunMonad $ \() -> x
    FunMonad a <*> FunMonad b = FunMonad $ \() -> (a ()) (b ())

instance Monad FunMonad where
    return a  =   FunMonad ( \() -> a )
    m >>= k   =   k (fun m ())  
    fail      =   error



-- ����� ���� � �������

--
-- �������������, ��� ��������
--
myMonad  = FunMonad $ \() -> 3
myMonad' = fmap (*2) myMonad

fun myMonad ()      -- 3
fun myMonad' ()     -- 6

--
-- �������������, ��� �������������� ��������
--
myMonad   = FunMonad $ \() -> 3
myMonad'  = FunMonad $ \() -> (^2)
myMonad'' = myMonad' <*> myMonad

fun myMonad'' ()    -- 9

--
-- �������������, ��� ������
--
myMonad  = FunMonad $ \() -> 3
myMonad' = myMonad >>= (\x -> FunMonad $ \() -> x^2)

fun myMonad' ()     -- 9