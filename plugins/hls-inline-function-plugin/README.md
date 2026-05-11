# Inline Function plugin for the [Haskell Language Server](https://github.com/haskell/haskell-language-server#readme)

Things to mention:
* What is inlining (replace a callsite with the body of the function being called and substituting the parameters with the arguments)
* What happens if there is a variable capture (the plugin should rename the variables in the inlined body to avoid capture)
* What happens if the function being inlined is recursive (the plugin should not inline recursive functions)
* GIF of the plugin in action
* Supported bindings

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

