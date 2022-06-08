module Ch5 where

-- import Data.Function (applyFlipped)
import Data.List (List(..), (:))
import Data.Maybe 
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, show, (+), (-), negate, (<))

const :: ∀ a b. a -> b -> a
const a _ = a

flip:: ∀ a b c. (a -> b -> c) -> b -> a -> c
flip f a b = f b a


apply :: ∀ a b. (a -> b) -> a -> b
apply f x  = f x
infixr 0 apply as $ 

applyFlipped :: ∀ a b. a -> (a -> b) -> b
applyFlipped = flip apply


addOne :: Int -> Int -> Int
addOne a b = a + b


singleton :: ∀ a. a -> List a
singleton a = Cons a Nil 


null :: ∀ a. List a -> Boolean
null Nil = true
null _  = false

snoc :: ∀ a. List a -> a -> List a
snoc Nil a = singleton a
snoc (h : t) a = (h : snoc t a)

length :: ∀ a. List a -> Int
length Nil = 0
length (_ : t) = 1 + (length t)


head :: ∀ a. List a -> Maybe a
head Nil = Nothing
head ( h: _) = Just h

tail :: ∀ a. List a -> Maybe (List a)
tail Nil = Nothing
tail (_:xs) = Just xs

last :: ∀ a. List a -> Maybe a
last Nil = Nothing
last (x:Nil) = Just x 
last (_:xs) = last xs




init :: ∀ a. List a -> Maybe(List a)
init Nil = Nothing
init l = Just $ go l where
  go Nil = Nil
  go (_:Nil) = Nil
  go (x:xs) = x:go xs


index :: ∀ a. List a -> Int -> Maybe a
index (l:_)  0 = Just l 
index (_:_)  i | i < 0 = Nothing
index (_:Nil) _ = Nothing
index (_:lx) a = index lx (a - 1)
index (Nil) _ = Nothing

test:: Effect Unit 
test = do
  -- log $ show $ singleton 1
  -- log $ show $ null Nil
  -- log $ show $ snoc (1 : 2 : Nil) 3
  -- log $ show $ snoc (1 : 2 : Nil) 3
  -- log $ show $ (last Nil :: Maybe Unit)
  log $ show $ index (1:2:3:Nil) (1)

