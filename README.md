[Lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus#Definition) parser/unparser and reducer.

Demo:

```
iex()> import Lambda
iex()> expr = "(λx.Ly.x (y z)) w" # "L" can be used in place of "λ" for ease of typing
iex()> expr |> tokenize
  ["(", "λ", "x", ".", "λ", "y", ".", "x", " ", "(", "y", " ", "z", ")", ")", " ", "w"]
iex()> expr |> tokenize |> to_ast
  {:A, {:L, "x", {:L, "y", {:A, "x", {:A, "y", "z"}}}}, "w"}
iex()> expr |> tokenize |> to_ast == expr |> parse
  true
iex()> expr |> parse |> as_string
  "(λx.λy.x (y z)) w"
iex()> expr |> parse |> reduce_once
  {:L, "y", {:A, "w", {:A, "y", "z"}}}
iex()> expr |> parse |> reduce_once |> as_string
  "λy.w (y z)"

iex()> "((λx.λy.x (y z)) a) b" |> parse |> reduce |> as_string
  "a (b z)"

iex()> y_combinator = "λg.(λx.g (x x)) (Lx.g (x x))"
iex()> y_combinator |> parse |> reduce_once |> as_string
  "λg.g ((λx.g (x x)) (λx.g (x x)))"
iex()> y_combinator |> parse |> reduce(2) |> as_string
  "λg.g (g ((λx.g (x x)) (λx.g (x x))))"
iex()> y_combinator |> parse |> reduce(4) |> as_string
  "λg.g (g (g (g ((λx.g (x x)) (λx.g (x x))))))"

iex()> "(λx.x x) (λx.x x)" |> parse |> reduce(100) |> as_string
  "(λx.x x) (λx.x x)"
```
