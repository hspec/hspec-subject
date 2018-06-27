This can be used like so

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
    before (return $ 23 + 42) $ do
      it "`shouldBe` 65" (`shouldBe` 65)
```
