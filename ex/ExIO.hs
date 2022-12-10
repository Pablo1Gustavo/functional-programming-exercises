module ExIO where

import Prelude hiding
    ( putStr
    , putStrLn
    , getLine
    , interact
    , (>>)
    , (>>=)
    )

-- read through the whole module first, to get an idea
-- of what's required and to decide where to start

isEOL :: Char -> Bool
isEOL = (==) '\n'

getLine :: IO String
getLine = do 
    c <- getChar
    if isEOL c then
        return ""
    else do 
        s <- getLine
        return $ c:s

getInt :: IO Int
getInt = do
    n <- getLine
    return $ read n

getSafeInt :: IO (Maybe Int)
getSafeInt =  do
    s <- getLine
    let cs = reads s :: [(Int,String)]
    case cs of
        [(n,_)] -> return $ Just n
        _       -> return Nothing

infixl 1 >>

(>>) :: IO a -> IO b -> IO b
(>>) ax ay = do
    ax; ay

-- pauses till the user presses any normal key
pause :: IO ()
pause = do
    getChar
    return ()

skip :: IO ()
skip = return ()

newline :: IO ()
newline = putChar '\n'

putStr :: String -> IO ()
putStr = foldr ((>>) . putChar) skip

-- transform f into one "just like f" except that it prints a newline
-- after any side-effects f may had
lnize :: (a -> IO b) -> a -> IO b
lnize f x = do
    ax <- f x
    newline
    return ax

putStrLn :: String -> IO ()
putStrLn = lnize putStr

putCharLn :: Char -> IO ()
putCharLn = lnize putChar

-- reads the entire user input as a single string, transforms it, and prints it
interact :: (String -> String) -> IO ()
interact f = do
    s <- getLine
    putStr (f s)

perlineize :: (String -> String) -> (String -> String)
perlineize f = unlines . map f . lines

interactPerLine :: (String -> String) -> IO ()
interactPerLine = interact . perlineize

when :: Bool -> IO () -> IO ()
when p a
    | p = a
    | otherwise = skip

unless :: Bool -> IO () -> IO ()
unless = undefined

guard :: Bool -> IO ()
guard = undefined

forever :: IO a -> IO b
forever a = a >> forever a

-- transforms the action given to an equivalent one that has no result
void :: IO a -> IO ()
void = (>> skip)

-- Kleisli compositions
infixr 1 >=>, <=<

-- diagrammatic order
(>=>) :: (a -> IO b) -> (b -> IO c) -> (a -> IO c)
(>=>) f g x = do
    ax <- f x
    g ax 

-- traditional order
-- comparison of types:
-- (.)   :: (b ->    c) -> (a ->    b) -> a ->    c
-- (<=<) :: (b -> IO c) -> (a -> IO b) -> a -> IO c
(<=<) :: (b -> IO c) -> (a -> IO b) -> (a -> IO c)
(<=<) = flip (>=>)


-- Bind
infixl 1 >>=

(>>=) :: IO a -> (a -> IO b) -> IO b
(>>=) ax f = do
    x <- ax
    y <- f x
    return y


infixl 4 $>, <$

-- make an action that has the side effects of the action on the left
-- but with result the value on the right
($>) :: IO a -> b -> IO b
($>) a x = do
    a
    return x

-- vice-versa
(<$) :: a -> IO b -> IO a
(<$) = flip ($>)

ap :: IO (a -> b) -> IO a -> IO b
af `ap` ax = undefined

filterIO :: (a -> IO Bool) -> [a] -> IO [a]
filterIO = undefined

iomap :: (a -> b) -> IO a -> IO b
iomap f ax = do
    x <- ax
    return $ f x

mapIO :: (a -> IO b) -> [a] -> IO [b]
mapIO f = sequenceIO . map f

zipWithIO :: (a -> b -> IO c) -> [a] -> [b] -> IO [c]
zipWithIO f xs = sequenceIO . zipWith f xs

zipWithIO_ :: (a -> b -> IO c) -> [a] -> [b] -> IO ()
zipWithIO_ f xs = void . zipWithIO f xs

sequenceIO :: [IO a] -> IO [a]
sequenceIO [] = return []
sequenceIO (ax:axs) = do
    x <- ax
    xs <- sequenceIO axs
    return $ x:xs

sequenceIO_ :: [IO a] -> IO ()
sequenceIO_ = void . sequenceIO

replicateIO :: Integral i => i -> IO a -> IO [a]
replicateIO 0 _ = return []
replicateIO n ax = do
    x <- ax
    ax' <- replicateIO (n - 1) ax
    return $ x : ax'

replicateIO_ :: Integral i => i -> IO a -> IO ()
replicateIO_ n = void . replicateIO n

forIO :: [a] -> (a -> IO b) -> IO [b]
forIO = undefined

forIO_ :: [a] -> (a -> IO b) -> IO ()
forIO_ = undefined

joinIO :: IO (IO a) -> IO a
joinIO = undefined

foldlIO :: (b -> a -> IO b) -> b -> [a] -> IO b
foldlIO = undefined

foldlIO_ :: (b -> a -> IO b) -> b -> [a] -> IO ()
foldlIO_ = undefined
