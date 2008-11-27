-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Packed.Static.Syntax
-- Copyright   :  (c) Reiner Pope 2008
-- License     :  GPL-style
--
-- Maintainer  :  Reiner Pope <reiner.pope@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- QuasiQuoting for matrices and vectors.
--
-----------------------------------------------------------------------------

module Data.Packed.Static.Syntax(mat,vec,vecD,vecC,viewVec) where

import Data.Complex

import Control.Monad

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Types.Data.Num.Decimal.Literals.TH
import Data.Packed.Static.Shapes
import Data.Packed.Static.Vector
import Data.Packed.Static.Matrix

import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String(Parser)
import qualified Text.Parsec.Token as T


import Foreign.Storable

import qualified Language.Haskell.Meta.Parse as MP


mat :: QuasiQuoter
mat = QuasiQuoter parseExpM (error "No pattern qausiquoter for mat: use ... instead")

parseExpM s = parsecToQ (sepBy (sepBy expr comma) semi) s >>= mkQExpM where

mkQExpM :: [[Exp]] -> ExpQ
mkQExpM xs = do
  let cols = length $ head xs
  when (not $ all ((==cols) . length) xs) $ fail "Inconsistent row lengths in [$mat|...|]"
  [| ( $(decLiteralV (fromIntegral $ length xs)) >< $(decLiteralV (fromIntegral cols)) )
                $(return $ ListE (concat xs)) |]


vec :: QuasiQuoter
vec = QuasiQuoter parseExp (error "No pattern quasiquoter for vec: use vecD, vecC, or viewList instead")

-- | Pattern quasiquoter for vectors of Double.
vecD :: QuasiQuoter
vecD = QuasiQuoter (error "No expression quasiquoter for vecD: use vec instead") parsePatD
-- | Pattern quasiquoter for vectors of Complex Double.
vecC :: QuasiQuoter
vecC = QuasiQuoter (error "No expression quasiquoter for vecC: use vec instead") parsePatC

newtype VecView n t = VecView [t]

-- | Views a vector with @VecView@; used for the pattern quasiquoters:
-- 
-- @ foo :: Vector D2 Double -> Double
-- foo (viewVec -> [$vecD|x,y|]) = x + y@
viewVec :: (Storable t) => Vector n t -> VecView n t
viewVec = VecView . toList

--- Vec pattern parser
parsePatD s = mkQPatD =<< parsecToQ (sepBy identifier comma) s

mkQPatD :: [String] -> PatQ
mkQPatD xs = sigP (return $ ConP 'VecView [ListP (map (VarP . mkName) xs)]) 
                 ((conT ''VecView) `appT` (decLiteralT $ fromIntegral $ length xs) `appT` (conT ''Double))

parsePatC s = mkQPatC =<< parsecToQ (sepBy identifier comma) s

mkQPatC :: [String] -> PatQ
mkQPatC xs = sigP (return $ ConP 'VecView [ListP (map (VarP . mkName) xs)]) 
                  ((conT ''VecView) `appT` (decLiteralT $ fromIntegral $ length xs) `appT` (conT ''Complex `appT` conT ''Double))


--- Vec expression parser
parseExp s = parsecToQ (sepBy expr comma) s >>= mkQExp where

mkQExp :: [Exp] -> ExpQ
mkQExp xs = [| unsafeReshape (fromListsU $(return $ ListE xs)) `atShape` $(decLiteralV (fromIntegral $ length xs)) |]

----- Haskell parsing
-- | Does a simplistic parse using Parsec, which just counts brackets and escapes comments and string literals.
-- This parse accumulates text, which is then parsed properly using haskell-src-exts.
expr = do
  s <- outerCode
  case MP.parseExp s of
    Left err -> fail err
    Right exp -> return exp

infixr >>+
p1 >>+ p2 = do
  x1 <- p1
  x2 <- p2
  return (x1 ++ x2)

p >/> q = do
  p' <- p
  notFollowedBy q
  return p'

codeChar = noneOf "{}()[]-,;\"\'" <|> (try (char '-' >/> char '-'))
innerCodeChar = codeChar <|> char ','

outerCode = fmap concat $ many (fmap return codeChar <|> codeChoices)
innerCode = fmap concat $ many (fmap return innerCodeChar <|> codeChoices)

codeChoices = (nestedCommentCode <|> singleLineCommentCode <|>
                  stringLit <|> charLit <|> bracesCode <|> parensCode <|> bracketsCode)

bracesCode = string "{" >>+ innerCode >>+ string "}"
parensCode = string "(" >>+ innerCode >>+ string ")"
bracketsCode = string "[" >>+ innerCode >>+ string "]"
nestedCommentCode = try (string "{-") >>+ insideNestedCode >>+ string "-}"
insideNestedCode = fmap concat $ many (nestedCommentCode <|> fmap return (noneOf "-") <|> try (string "-" >/> char '}'))
singleLineCommentCode = (try $ string "--") >>+ manyTill anyChar newline
stringLit = fmap show $ stringLiteral 
charLit = fmap show $ charLiteral

identifier = T.identifier haskell
comma = T.comma haskell <?> "comma"
stringLiteral = T.stringLiteral haskell
charLiteral = T.charLiteral haskell
semi = T.semi haskell <?> "semicolon"

---- to be later moved to another library
parsecToQ :: Parser a -> String -> Q a
parsecToQ p s = do
    loc <- location
    let file       = loc_filename loc
        (line,col) = loc_start loc
        p' = do  pos <- getPosition
                 setPosition $
                      (flip setSourceName) file $
                      (flip setSourceLine) line $
                      (flip setSourceColumn) col $  pos
                 v <- p
                 eof
                 return v
    e <- case runParser p' () "" s of
      Left err -> fail $ show err
      Right e  -> return e
    return e

