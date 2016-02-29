Lambda calculus parser/unparser and reducer.

Demo:

```
iex(8)> import Lambda
iex(9)> expr = "(Lx.Ly.x (y z)) w"
iex(10)> expr |> tokenize
["(", "L", "x", ".", "L", "y", ".", "x", " ", "(", "y", " ", "z", ")", ")", " ", "w"]
iex(11)> expr |> tokenize |> to_ast
{:A, {:L, "x", {:L, "y", {:A, "x", {:A, "y", "z"}}}}, "w"}
iex(12)> parse(expr) == to_ast(tokenize(expr))
true
iex(13)> expr |> parse |> as_string
"(Lx.Ly.x (y z)) w"
iex(14)> expr |> parse |> reduce_once
{:L, "y", {:A, "w", {:A, "y", "z"}}}
iex(15)> expr |> parse |> reduce_once |> as_string
"Ly.w (y z)"

iex(22)> "((Lx.Ly.x (y z)) a) b" |> parse |> reduce |> as_string
"a (b z)"

iex(16)> y_combinator = "Lg.(Lx.g (x x)) (Lx.g (x x))"
"Lg.(Lx.g (x x)) (Lx.g (x x))"
iex(17)> y_combinator |> parse |> reduce_once |> as_string
"Lg.g ((Lx.g (x x)) (Lx.g (x x)))"
iex(18)> y_combinator |> parse |> reduce(2) |> as_string
"Lg.g (g ((Lx.g (x x)) (Lx.g (x x))))"
iex(20)> y_combinator |> parse |> reduce(4) |> as_string
"Lg.g (g (g (g ((Lx.g (x x)) (Lx.g (x x))))))"

iex(21)> "(Lx.x x) (Lx.x x)" |> parse |> reduce(100) |> as_string
"(Lx.x x) (Lx.x x)"
```
