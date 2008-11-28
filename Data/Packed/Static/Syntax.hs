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
-- BIG WARNING: the expression quasiquoters for matrices and vectors
-- are broken for infix expressions. All operators will be assumed to
-- be left infix with infix level 9. To avoid unexpected parses, fully
-- parenthesise all infix expressions.
-----------------------------------------------------------------------------

module Data.Packed.Static.Syntax(
    mat,
    vec,
    -- * Matrix views
    MatView,
    viewMat,
    -- * Vector views
    VecView,
    viewVec,
 ) where

import Data.Complex

import Control.Monad

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Types.Data.Num.Decimal.Literals.TH
import Data.Packed.Static.Imports
import Data.Packed.Static.Shapes
import Data.Packed.Static.Vector
import Data.Packed.Static.Matrix

import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String(Parser)
import qualified Text.Parsec.Token as T


import Foreign.Storable

import qualified Language.Haskell.Meta.Parse as MP


----- mat parser
-- | Required for the 'mat' pattern quasiquoter. See 'mat'.
data MatView n t = n :>< [[t]]
-- | Required for the 'mat' pattern quasiquoter. See 'mat'.
viewMat :: Element t => Matrix (m, n) t -> MatView (m, n) t
viewMat m = shapeOf m :>< toLists m

-- | The matrix quasiquoter for expressions and patterns. 
-- 
-- * Elements on the same row are separated by commas; rows 
--   themselves are separated by semicolons. All whitespace is optional
-- 
-- * The expression quasiquoter allows arbitrary Haskell 
--   expressions as its elements; the pattern quasiquoter
--   requires that each element is a variable.
-- 
-- * Using the quasiquoter for patterns requires that you
--   use the 'viewMat' view pattern first (this is a
--    workaround since Template Haskell doesn't yet support
--    view patterns).
-- 
-- For example,
-- 
-- @ example1 :: (Element t) => Matrix (D2,D3) t -> Matrix (D2,D2) t
-- example1 (viewMat -> [$mat|a, b, c;
--                            d, e, f|]) = [$mat|a+b,   b+c;
--                                               sin c, f  |]@
mat :: QuasiQuoter
mat = QuasiQuoter parseMatExp parseMatPat

parseMat p s = do
  xs <- parsecToQ (sepBy (sepBy p comma) semi) s
  let rows = length xs
      cols = length $ head xs
  when (not $ all ((==cols) . length) xs) $ fail "Inconsistent row lengths in [$mat|...|]"
  return (xs,rows,cols)

parseMatExp s = do
  (xs,rows,cols) <- parseMat expr s
  [| ( $(decLiteralV $ fromIntegral rows) >< $(decLiteralV $ fromIntegral cols) )
                $(return $ ListE (concat xs)) |]

parseMatPat s = do
  (xs,rows,cols) <- parseMat identifier s
  conP '(:><) [ sigP wildP (tupleT 2 `appT` (decLiteralT $ fromIntegral rows) `appT` (decLiteralT $ fromIntegral cols))
              , listP (map (listP . map (varP . mkName)) xs) ]

------- vec parser
-- | Required for the 'vec' quasiquoter. See 'vec'.
data VecView n t = n :|> [t]

-- | Required for the 'vec' quasiquoter. See 'vec'.
viewVec :: (Storable t) => Vector n t -> VecView n t
viewVec v = shapeOf v :|> toList v

-- | The vector quasiquoter for expressions and patterns. This is
-- very similar to the 'mat' quasiquoter.
-- 
--  * Elements are separated by commas; whitespace is ignored.
-- 
--  * The expression quasiquoter allows arbitrary Haskell expressions for
--    each element; the pattern quasiquoter requires that each element is
--    a variable pattern.
-- 
--  * The pattern quasiquoter must be preceeded by a the 'viewVec' view pattern.
-- 
-- For example,
-- 
-- @ example2 :: (Storable t, Num t) => Vector D2 t -> Vector D3 t
-- example2 (viewVec -> [$vec|a, b|]) = [$vec|a*b, 5, 7|]@
vec :: QuasiQuoter
vec = QuasiQuoter parseVecExp parseVecPat

--- Vec pattern parser
parseVec p s = parsecToQ (sepBy p comma) s

parseVecPat s = do
  xs <- parseVec identifier s
  conP '(:|>) [ sigP wildP (decLiteralT $ fromIntegral $ length xs)
                         , return $ ListP (map (VarP . mkName) xs)  ]

parseVecExp s = do
  xs <- parseVec expr s
  [| unsafeReshape (fromListU $(return $ ListE xs)) `atShape` $(decLiteralV (fromIntegral $ length xs)) |]


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

