Given the following boilerplate

```haskell
{-# LANGUAGE QuasiQuotes #-}
module Main where
import           Test.Hspec
import           Test.Hspec.Subject

main :: IO ()
main = hspec spec
```

this can be used like so

```haskell
spec :: Spec
spec = do
  [describe_|23 + 42|] $ do
    [it_|`shouldBe` 65|]
```

which is equivalent to

```haskell
spec :: Spec
spec = do
  describe "23 + 42" $ do
    subject (23 + 42) $ do
      it "`shouldBe` 65" (`shouldBe` 65)
```

(note that `subject` is just an alias for `before . return`)
