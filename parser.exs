ExUnit.start

defmodule Lambda do
  def tokenize(string_expr), do: string_expr |> String.replace("L", "λ") |> String.codepoints

  defmacrop is_var(x), do: quote(do: unquote(x) >= "a" and unquote(x) <= "z")

  defp finish([t, x, :L]      ), do: {:L, finish(x), finish(t)}
  defp finish([t2, t1, :A]    ), do: {:A, finish(t1), finish(t2)}
  defp finish(x) when is_var(x), do: x

  defp pop([acc, h | t]), do: [[acc|h] | t]
  defp push(stack, x), do: [x | stack]

  def to_ast(tokens), do: to_ast(tokens, [])
  def to_ast([]      , [acc]                ), do: acc |> finish
  def to_ast([]      , stack                ), do: to_ast([],        stack |> pop)
  def to_ast([")" |t], [[x, :"("] | stack]  ), do: to_ast(t,         [x | stack])
  def to_ast([")" |t], stack                ), do: to_ast([")" | t], stack |> pop)
  def to_ast(["." |t], stack                ), do: to_ast(t,         stack |> pop)
  def to_ast(["(" |t], stack                ), do: to_ast(t,         stack |> push( [:"("]    ))
  def to_ast(["λ" |t], stack                ), do: to_ast(t,         stack |> push( [:L]      ))
  def to_ast([" " |t], [_, [_,:A] | _]=stack), do: to_ast([" "|t],   stack |> pop)
  def to_ast([" " |t], [acc | stack]        ), do: to_ast(t,         stack |> push( [acc, :A] ))
  def to_ast([x   |t], stack) when is_var(x) , do: to_ast(t,         stack |> push( x         ))

  defp has(op, {op, _, _}),       do: true
  defp has(op, {_, t1, t2}),      do: has(op, t1) or has(op, t2)
  defp has(_op, x) when is_var(x), do: false

  def as_string(x) when is_var(x), do: x
  def as_string({op, t1, t2}) do
    t1_s = as_string t1
    t2_s = as_string t2
    if op == :A do
      if has(:L, t1), do: t1_s = "(#{t1_s})"
      if has(:A, t2), do: t2_s = "(#{t2_s})"
      "#{t1_s} #{t2_s}"
    else
      "λ#{t1_s}.#{t2_s}"
    end
  end

  def reduce_once({:A, {:L, old, term}, new}), do: sub(old, new, term)
  def reduce_once({op, t1, t2}), do: {op, reduce_once(t1), reduce_once(t2)}
  def reduce_once(x) when is_var(x), do: x

  def reduce(term), do: reduce(term, 100)
  def reduce(term, 0), do: term
  def reduce(term, count) do
    case reduce_once(term) do
      ^term -> term
      new_term -> reduce(new_term, count - 1)
    end
  end

  defp sub(old, new, {:A, t1, t2}), do: {:A, sub(old, new, t1), sub(old, new, t2)}
  defp sub(old, new, {:L, v,   t}), do: {:L, v, (if old == v, do: t, else: sub(old, new, t))}
  defp sub(old, new, var) when is_var(var), do: if var == old, do: new, else: var

  def parse(string_expr), do: string_expr |> tokenize |> to_ast

end

defmodule Lambda.Test do
  use ExUnit.Case, async: true
  import Lambda

  defmacrop reduce_test(fun, pre, post) do
    quote do
      assert apply(Lambda, unquote(fun), [parse(unquote(pre))]) == parse(unquote(post))
    end
  end
  defmacrop reduce_tests fun do
    quote do
      reduce_test unquote(fun), "(λx.x) y",   "y"
      reduce_test unquote(fun), "(λx.z) y",   "z"
      reduce_test unquote(fun), "(λx.x z) y", "y z"
      reduce_test unquote(fun), "(λx.x x) y", "y y"
      reduce_test unquote(fun), "(λx.z z) y", "z z"
      reduce_test unquote(fun), "(λx.λz.x z) y", "λz.y z"
      reduce_test unquote(fun), "(λx.x x)", "(λx.x x)"
      reduce_test unquote(fun), "x y", "x y"
    end
  end
  test "reduce" do
    reduce_tests :reduce
    reduce_test :reduce, "(λx.λz.x z) (λx.x)", "λz.z"
    inner = "((λx.g (x x)) (λx.g (x x)))"
    # Derivations per wikipedia article on Lambda calculus
    y = "(λg.#{inner})"
    y_g_reduced_once = reduce(parse("#{y} g"), 1)
    y_g_reduced_twice = reduce(parse("#{y} g"), 2)
    assert y_g_reduced_once == parse(inner)
    assert y_g_reduced_twice == parse("g #{inner}")
    # Y g = g (Y g)
    assert y_g_reduced_twice == {:A, "g", y_g_reduced_once}
  end
  test "reduce_once" do
    reduce_tests :reduce_once
    reduce_test :reduce_once, "(λx.λz.x z) (λx.x)", "λz.(λx.x) z"
  end

  test "tokenize" do
    assert tokenize("λx.x") == ["λ", "x", ".", "x"]
    assert tokenize("λx.x y") == ["λ", "x", ".", "x", " ", "y"]
  end

  test "to_ast" do
    assert to_ast(tokenize("a b c")) == to_ast(tokenize("((a b) c)"))
    assert to_ast(tokenize("x (λy.y) z")) == to_ast(tokenize("(x λy.y) z"))
    assert to_ast(tokenize("x λy.y z")) == to_ast(tokenize("x (λy.y z)"))
    thing = {:A, {:L, "x", {:L, "y", "z"}}, {:L, "x", "x"}}
    assert to_ast(tokenize("((λx.((λy.((z))))) ((λx.(x))))")) == thing
    assert to_ast(tokenize("(((λx.((λy.((z))))) ((λx.(x)))))")) == thing
    assert to_ast(tokenize("(λx.λy.z) (λx.x)")) == thing
    assert to_ast(tokenize("(λx.x) (λx.x)")) == {:A, {:L, "x", "x"}, {:L, "x", "x"}}
    assert to_ast(tokenize("x y")) == {:A, "x", "y"}
    assert to_ast(tokenize("λx.x")) == {:L, "x", "x"}
    assert to_ast(tokenize("(λx.x)")) == {:L, "x", "x"}
  end

  test "parse" do
    assert parse("λx.x") == tokenize("λx.x") |> to_ast
  end

  defmacrop test_is_normalized(expr) do
    quote do: assert (unquote(expr) |> parse |> as_string) == unquote(expr)
  end
  test "as_string" do
    # Parse and unparse the given normalized strings.  Because as_string is
    # normalizing, we should get back the same string from |> parse |> as_string.
    test_is_normalized "a b"
    test_is_normalized "a b c"
    test_is_normalized "λx.x"
    test_is_normalized "x λx.x"
    test_is_normalized "(λx.x) y"
    test_is_normalized "λx.x y"
    test_is_normalized "(x λx.x) λy.y"
    test_is_normalized        "λg.(λx.g (x x)) (λx.g (x x))"
    test_is_normalized "(λz.λw.λg.(λx.g (x x)) (λx.g (x x))) λx.x"
  end
end
