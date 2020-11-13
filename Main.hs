module Main where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Error
import Text.Megaparsec.Char
import Data.Void

import Data.Traversable
import Data.List
import Data.Monoid
import Control.Monad


forIntersperse items delimiter f = sequence $ intersperse delimiter $ map f items

add = tell . pure


data Term = Variable String | Application Term Term | Lambda String Term deriving Show

type Parser = Parsec Void String

term :: Parser Term
term = fmap (foldl1 Application) <$> some $ named <|> parens

parens :: Parser Term
parens = do
 string "(" <* space
 item <- term <* space
 string ")" <* space
 return item

named :: Parser Term
named = do
 name <- some alphaNumChar <* space
 lambda name <|> return (Variable name)

lambda :: String -> Parser Term
lambda name = do
 string "=>" <* space
 Lambda name <$> term


countUses :: String -> Term -> Int
countUses v (Variable name) | v == name = 1
countUses v (Variable name) = 0
countUses v (Application f x) = countUses v f + countUses v x
countUses v (Lambda name term) | v == name = 0
countUses v (Lambda name term) = countUses v term

linearCheck :: Term -> WriterT All IO ()
linearCheck (Variable name) = return ()
linearCheck (Application f x) = linearCheck f >> linearCheck x
linearCheck (Lambda name term) = do 
 if countUses name term == 1 then
  pure ()
 else do
  lift $ putStrLn $ show (countUses name term)  ++ " uses of " ++ name
  tell $ All False
 linearCheck term

data Symbol = Symbol { symbolName :: String, symbolText :: Term } deriving Show

symbols :: Parser [Symbol]
symbols = many $ do
 name <- some alphaNumChar <* space
 string "=" <* space
 item <- term <* space
 string ";" <* space
 return $ Symbol name item

data CType = CPointer CType | CVoid | CStructRef String | CFPtr CType [(String, CType)]

data CSymbol = CGlobal String CType CExpression | CFunction String CType [(String, CType)] [CStatement] | CStruct String [(String, CType)] |
 CInclude String | CMacro String CExpression

data CStatement = CScope [CStatement] | CEffect CExpression | CReturn CExpression | CDeclare CType String CExpression

data CExpression = CVariable String | CInitializer [CExpression] | CCall CExpression [CExpression] | CArrow CExpression String | CAssign CExpression CExpression |
 CSizeOf CType


voidp :: CType
voidp = CPointer CVoid

thunk :: CType
thunk = CStructRef "thunk"

thunkp :: CType
thunkp = CPointer thunk


hoistType :: CType
hoistType = CFPtr voidp [("self", voidp), ("argument", voidp)]

allocateBlock :: String -> [String] -> CType -> [CStatement]
allocateBlock hoist capture structure = execWriter $ do
 let this = "_this"
 add $ CDeclare structure this (CCall (CVariable "malloc") [CSizeOf structure])
 add $ CEffect $ CAssign (CArrow (CVariable this) "_hoist") (CVariable hoist)
 for capture $ \field -> do
  add $ CEffect $ CAssign (CArrow (CVariable this) field) (CVariable field)
 add $ CReturn $ CVariable this
 
evaluateBlock :: String -> Int -> [String] -> CType -> String -> Term -> Writer [CSymbol] [CStatement]
evaluateBlock base n capture structure argument term = do
  result <- compileTerm base (n + 1) (capture ++ [argument]) term
  return $ pretext ++ [CReturn result] where
   pretext = execWriter $ do
    add $ CDeclare structure "_this" (CVariable "_this0")
    for capture $ \field -> do
     add $ CDeclare voidp field (CArrow (CVariable "_this") field)
    add $ CEffect $ CCall (CVariable "free") [CVariable "_this0"]
 
compileTerm :: String -> Int -> [String] -> Term -> Writer [CSymbol] CExpression
compileTerm base n capture (Lambda argument term) = do
 let structureName = base ++ "_thunk" ++ show n
 let state = map (\x -> (x,voidp)) capture
 add $ CStruct structureName (("_hoist", hoistType) : state)
 
 let structure = (CPointer $ CStructRef structureName)

 let hoist = base ++ "_hoist" ++ show n
 hoistBlock <- evaluateBlock base n capture structure argument term
 add $ CFunction hoist voidp [("_this0", voidp), (argument, voidp)] hoistBlock

 let create = base ++ "_create" ++ show n
 add $ CFunction create thunkp state (allocateBlock hoist capture structure)
 
 return $ CCall (CVariable create) (map CVariable capture) 

compileTerm base n capture (Application f x) = do
 f' <- compileTerm base n capture f
 x' <- compileTerm base n capture x
 return $ CCall (CVariable "call") [f', x']

compileTerm base n capture (Variable name) = pure (CVariable name)

compileSymbol :: Symbol -> Writer [CSymbol] ()
compileSymbol (Symbol name term) = do
 result <- compileTerm name 0 [] term
 add $ CMacro name result

compile :: [Symbol] -> [CSymbol]
compile symbols = [CInclude "thunk.h"] ++ (execWriter $ traverse compileSymbol symbols)

class PrettyPrint a where
 prettyPrint :: a -> WriterT String (State Int) ()

indent :: WriterT String (State Int) ()
indent = do
 spaces <- lift get
 replicateM (spaces * 4) (tell " ")
 pure ()

line :: WriterT String (State Int) ()
line = do
 tell "\n"
 indent

increase :: WriterT String (State Int) ()
increase = do
 tell "\n"
 lift $ modify (+1)
 indent

decrease :: WriterT String (State Int) ()
decrease = do
 tell "\n"
 lift $ modify (subtract 1)
 indent

bracketStart :: WriterT String (State Int) ()
bracketStart = do
 line
 tell "{"
 increase

bracketEnd :: WriterT String (State Int) ()
bracketEnd = do
 decrease
 tell "}"
 line

bracketEndSemi :: WriterT String (State Int) ()
bracketEndSemi = do
 decrease
 tell "};"
 line


prettyPrintType :: CType -> WriterT String (State Int) () -> WriterT String (State Int) ()
prettyPrintType (CPointer x) k = prettyPrintType x (pure ()) >> tell "*" >> k 
prettyPrintType CVoid k = tell "void" >> k
prettyPrintType (CStructRef name) k = tell "struct " >> tell name >> k
-- this very hacky and likely doesn't cover all cases
prettyPrintType (CFPtr returnType arguments) k = do
 prettyPrintType returnType (tell "(*" >> k >> tell ")")
 tell "(" >> (forIntersperse arguments (tell ", ") $ \(name, ctype) -> prettyPrintTypeNamed ctype name) >> tell ")"
 

prettyPrintTypeNamed :: CType -> String -> WriterT String (State Int) ()
prettyPrintTypeNamed t name = prettyPrintType t (tell $ " " ++ name) 

instance PrettyPrint CType where
 prettyPrint t = prettyPrintType t (pure ())
  
instance PrettyPrint CSymbol where
 prettyPrint (CGlobal name ctype expression) = do
  prettyPrintTypeNamed ctype name
  tell " = "
  prettyPrint expression
  tell ";"
 prettyPrint (CFunction name returnType arguments statements) = do
  prettyPrintTypeNamed returnType name
  tell "("
  forIntersperse arguments (tell ", ") $ \(name, ctype) -> do
    prettyPrintTypeNamed ctype name
  tell ")"
  bracketStart
  forIntersperse statements line prettyPrint
  bracketEnd
 prettyPrint (CStruct name fields) = do
  tell "struct "
  tell name
  bracketStart
  forIntersperse fields line $ \(name, ctype) -> do
   prettyPrintTypeNamed ctype name
   tell ";"
  bracketEndSemi
 prettyPrint (CInclude file) = do
  tell "#include \"" >> tell file >> tell "\""
  line
 prettyPrint (CMacro name token) = do
  tell "#define " >> tell name >> tell " "  
  prettyPrint token
  line
 
instance PrettyPrint CStatement where
 prettyPrint (CScope statements) = do
  bracketStart
  forIntersperse statements line prettyPrint
  bracketEnd
 prettyPrint (CEffect expression) = prettyPrint expression >> tell ";"
 prettyPrint (CReturn expression) = tell "return " >> prettyPrint expression >> tell ";"
 prettyPrint (CDeclare ctype name expression) = do
  prettyPrintTypeNamed ctype name
  tell " = "
  prettyPrint expression
  tell ";"

instance PrettyPrint CExpression where
 prettyPrint (CVariable variable) = tell variable
 prettyPrint (CInitializer values) = do
  tell "{" 
  forIntersperse values (tell ", ") prettyPrint
  tell "}"
 prettyPrint (CCall function arguments) = do 
  prettyPrint function
  tell "("
  forIntersperse arguments (tell ", ") prettyPrint
  tell ")"
 prettyPrint (CArrow value name) =  prettyPrint value >> tell "->" >> tell name
 prettyPrint (CAssign lvalue value) = prettyPrint lvalue >> tell " = " >> prettyPrint value  
 prettyPrint (CSizeOf ctype) = tell "sizeof(" >> prettyPrint ctype >> tell ")"


pretty :: PrettyPrint a => a -> IO ()
pretty x = putStrLn $ snd $ evalState (runWriterT $ prettyPrint x) 0

main :: IO ()
main = do
 stdin <- getContents
 case parse symbols "stdin" stdin of
  Left error -> putStrLn $ errorBundlePretty error 
  Right program -> do 
   ((), All valid) <- runWriterT $ traverse (linearCheck . symbolText) program >> pure ()
   case valid of
    True -> putStrLn $ evalState (execWriterT $ traverse prettyPrint globals) 0 where
     globals = compile program
    False -> pure ()
