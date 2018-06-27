{-# LANGUAGE TemplateHaskell #-}
module Test.Hspec.Subject (
  subject
, describe_
, it_
) where

import           Test.Hspec

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Language.Haskell.TH.Quote (QuasiQuoter(..))
import           Language.Haskell.Meta.Parse (parseExp)

subject :: a -> SpecWith a -> Spec
subject = before . return

describe_ :: QuasiQuoter
describe_ = qq $ \ e -> [|describe $(liftString e) . subject $(reifyExpression e)|]

it_ :: QuasiQuoter
it_ = qq $ \ e -> [|it $(liftString e) $(reifyExpression ("(" ++ e ++ ")"))|]

qq :: (String -> Q Exp) -> QuasiQuoter
qq e = QuasiQuoter {
    quoteExp = e
  , quotePat = err "pattern"
  , quoteType = err "type"
  , quoteDec = err "declaration"
  }
  where
    err name = fail ("This QuasiQuoter can not be used as a " ++ name ++ "!")

reifyExpression :: String -> Q Exp
reifyExpression s = case parseExp s of
  Left _ -> fail "Parse error in expression!"
  Right e -> return e
