ExUnit.start

defmodule Lambda do
  def tokenize(string_expr), do: string_expr |> String.codepoints

  defmacro is_var(x), do: quote(do: unquote(x) >= "a" and unquote(x) <= "z")

  def finish([t, x, :L]      ), do: {:L, finish(x), finish(t)}
  def finish([t2, t1, :A]    ), do: {:A, finish(t1), finish(t2)}
  def finish(x) when is_var(x), do: x

  def pop([acc, h | t]), do: [[acc|h] | t]
  def push(stack, x), do: [x | stack]

  def to_ast(tokens), do: to_ast(tokens, [])
  def to_ast([]      , [acc]                ), do: acc |> finish
  def to_ast([]      , stack                ), do: to_ast([],        stack |> pop)
  def to_ast([")" |t], [[x, :"("] | stack]  ), do: to_ast(t,         [x | stack])
  def to_ast([")" |t], stack                ), do: to_ast([")" | t], stack |> pop)
  def to_ast(["." |t], stack                ), do: to_ast(t,         stack |> pop)
  def to_ast(["(" |t], stack                ), do: to_ast(t,         stack |> push( [:"("]    ))
  def to_ast(["L" |t], stack                ), do: to_ast(t,         stack |> push( [:L]      ))
  def to_ast([" " |t], [_, [_,:A] | _]=stack), do: to_ast([" "|t],   stack |> pop)
  def to_ast([" " |t], [acc | stack]        ), do: to_ast(t,         stack |> push( [acc, :A] ))
  def to_ast([x   |t], stack) when is_var(x) , do: to_ast(t,         stack |> push( x         ))

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

  def sub(old, new, {:A, t1, t2}), do: {:A, sub(old, new, t1), sub(old, new, t2)}
  def sub(old, new, {:L, v,   t}), do: {:L, v, (if old == v, do: t, else: sub(old, new, t))}
  def sub(old, new, var) when is_var(var), do: if var == old, do: new, else: var

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
      reduce_test unquote(fun), "(Lx.x) y",   "y"
      reduce_test unquote(fun), "(Lx.z) y",   "z"
      reduce_test unquote(fun), "(Lx.x z) y", "y z"
      reduce_test unquote(fun), "(Lx.x x) y", "y y"
      reduce_test unquote(fun), "(Lx.z z) y", "z z"
      reduce_test unquote(fun), "(Lx.Lz.x z) y", "Lz.y z"
      reduce_test unquote(fun), "(Lx.x x)", "(Lx.x x)"
      reduce_test unquote(fun), "x y", "x y"
    end
  end

  test "reduce" do
    reduce_tests :reduce
    reduce_test :reduce, "(Lx.Lz.x z) (Lx.x)", "Lz.z"
    inner = "((Lx.g (x x)) (Lx.g (x x)))"
    # Derivations per wikipedia article on Lambda calculus
    y = "(Lg.#{inner})"
    y_g_reduced_once = reduce(parse("#{y} g"), 1)
    y_g_reduced_twice = reduce(parse("#{y} g"), 2)
    assert y_g_reduced_once == parse(inner)
    assert y_g_reduced_twice == parse("g #{inner}")
    # Y g = g (Y g)
    assert y_g_reduced_twice == {:A, "g", y_g_reduced_once}
  end
  test "reduce_once" do
    reduce_tests :reduce_once
    reduce_test :reduce_once, "(Lx.Lz.x z) (Lx.x)", "Lz.(Lx.x) z"
  end

  test "tokenize" do
    assert tokenize("Lx.x") == ["L", "x", ".", "x"]
    assert tokenize("Lx.x y") == ["L", "x", ".", "x", " ", "y"]
  end

  test "to_ast" do
    thing = {:A, {:L, "x", {:L, "y", "z"}}, {:L, "x", "x"}}
    assert to_ast(tokenize("a b c")) == to_ast(tokenize("((a b) c)"))
    assert to_ast(tokenize("((Lx.((Ly.((z))))) ((Lx.(x))))")) == thing
    assert to_ast(tokenize("(((Lx.((Ly.((z))))) ((Lx.(x)))))")) == thing
    assert to_ast(tokenize("(Lx.Ly.z) (Lx.x)")) == thing
    assert to_ast(tokenize("(Lx.x) (Lx.x)")) == {:A, {:L, "x", "x"}, {:L, "x", "x"}}
    assert to_ast(tokenize("x y")) == {:A, "x", "y"}
    assert to_ast(tokenize("Lx.x")) == {:L, "x", "x"}
    assert to_ast(tokenize("(Lx.x)")) == {:L, "x", "x"}
  end

  test "parse" do
    assert parse("Lx.x") == tokenize("Lx.x") |> to_ast
  end
end
