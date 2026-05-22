# Inline Function plugin for the [Haskell Language Server](https://github.com/haskell/haskell-language-server#readme)

The Inline Function plugin allows you to refactor a top-level function by inlining its definition in all its call sites and then removing it.

```haskell
addThree = (3+)

main = do
  print (addThree 4)
  print (addThree 5)
```

After inlining `addThree`:

```haskell
main = do
  print (3+4)
  print (3+5)
```

