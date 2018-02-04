import Control.Monad
import Data.Monoid
import Data.Char(isDigit, isLower)
{-
编辑--空白字符操作--Tab转字符
-}
newtype Parser a = Parser (String -> [(a, String)])

apply :: Parser a -> String -> [(a, String)]
apply (Parser p) s = p s

parse :: Parser a -> String -> a
parse p = fst.head.apply p

instance Applicative Parser where
    pure = return
    (<*>) = ap
    
instance Functor Parser where
    fmap  = liftM
    
instance Monad Parser where
    return x = Parser (\s -> [(x, s)])
    p >>= q = Parser (\s -> [ (y, s'') | (x,s') <- apply p s, (y,s'') <- apply (q x) s'])
    
getc :: Parser Char
getc = Parser f where
                    f [] = []
                    f (x:xs) = [(x,xs)]
{-
*Main> apply getc "what"
[('w',"hat")]
-}	
sat :: (Char -> Bool) -> Parser Char
sat p = do{
			c <- getc;
			if p c then return c
			else (Parser (\s -> []))

}
sat' p = do {c <- getc ; guard' (p c); return c}
guard' :: Bool -> Parser ()
guard' True = return ()
guard' False = Parser (\s -> [])
{-
*Main> apply (sat (=='a')) "abcde"
[('a',"bcde")]
*Main> apply (sat (=='a')) "wabcde"
[]
-}
char :: Char -> Parser()
char x = do {c <- sat (==x);return ()}
{-
*Main> apply ( char 'w' ) "what"
[((),"hat")]
-}

string :: String -> Parser ()
string [] = return ()
string (x:xs) = do {char x; string xs; return ()}


lower :: Parser Char
lower = sat isLower

digit :: Parser Int
digit = do {d <- sat isDigit; return (cvt d)}
           where cvt d = fromEnum d - fromEnum '0'
{-
*Main> apply digit "123"
[(1,"23")]
-}	

(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = Parser f where f s = let ps = apply p s in
									if null ps then apply q s
									else ps	   


lowers :: Parser String
lowers = do {c <- lower; cs <- lowers; return (c:cs)}
			<|> return ""
{-
*Main> apply lowers "abcDeFG"
[("abc","DeFG")]
*Main> 
-}
------------------------------------------------------
------------------------------------------------------下面是一个例子
--例子： 识别一个a+b结构，其中a和b都是单个数字
best = digit >>= rest
rest m = do {char '+'; n <- digit; return (m+n)}
		<|> return m
{-
*Main> apply best "1+3hello"
[(4,"hello")]
-}		
-------------------------------------------------------
-------------------------------------------------------
many :: Parser a -> Parser [a]
many p = do {
	x <- p;
	xs <- many p;
	return (x:xs)} <|> none
	where none = return [] --none和上面的fail是有区别的
{-
*Main> apply (many digit) "1234vb"
[([1,2,3,4],"vb")]
*Main> 
-}	

lowers = many lower
{-
*Main> apply lowers "abcDefg"
[("abc","Defg")]
*Main> 
-}













--参考下面这个实现： Just x >>= f = f x
foo :: Maybe String
foo = Just 3 >>= (\x ->
		Just "!" >>= (\y ->
			Just (show x ++ y)))
			
foo2 :: Maybe String
foo2 = do
	x <- Just 3
	y <- Just "!"
	Just (show x ++ y)
				