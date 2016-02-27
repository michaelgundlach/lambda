ExUnit.start

defmodule Assert do
  defmacro assert {:==, _md, [a, b]}=expr do
    quote do
      text = unquote(Macro.to_string(expr))
      if not (unquote(a) == unquote(b)) do
        IO.puts "FALSE: '#{unquote(a)}' == #{unquote(Macro.to_string(b))}"
      else
        IO.puts "TRUE:  #{unquote(Macro.to_string(b))}"
      end
    end
  end
end

defmodule Lambda do
  use ExUnit.Case, async: true

  def tokenize(term), do: term |> String.codepoints

  defmacro is_var(x), do: quote(do: unquote(x) >= "a" and unquote(x) <= "z")

  def finish([t, x, :L]      ), do: {:L, finish(x), finish(t)}
  def finish([t2, t1, :A]    ), do: {:A, finish(t1), finish(t2)}
  def finish(x) when is_var(x), do: x

  def pop([acc, h | t]), do: [[acc|h] | t]
  def push(stack, x), do: [x | stack]

  def to_ast(tokens), do: to_ast(tokens, [])
  def to_ast([]       , [acc]               ), do: acc |> finish
  def to_ast([]       , stack               ), do: to_ast([],        stack |> pop)
  def to_ast([")" |t], [[x, :"("] | stack]  ), do: to_ast(t,         [x | stack])
  def to_ast([")" |t], stack                ), do: to_ast([")" | t], stack |> pop)
  def to_ast(["." |t], stack                ), do: to_ast(t,         stack |> pop)
  def to_ast(["(" |t], stack                ), do: to_ast(t,         stack |> push( [:"("]    ))
  def to_ast(["L" |t], stack                ), do: to_ast(t,         stack |> push( [:L]      ))
  def to_ast([" " |t], [acc | stack]        ), do: to_ast(t,         stack |> push( [acc, :A] ))
  def to_ast([x   |t], stack) when is_var(x) , do: to_ast(t,         stack |> push( x         ))

  def reduce(["L", var, "." | t]) do
    "TODO"
  end

  def parse(term), do: term |> tokenize |> to_ast

  test "tokenize" do
    assert Lambda.tokenize("Lx.x") == ["L", "x", ".", "x"]
    assert Lambda.tokenize("Lx.x y") == ["L", "x", ".", "x", " ", "y"]
  end

  # test "reduce" do
  #   assert Lambda.reduce(Lambda.tokenize("Lx.x y")) == "y"
  # end

  test "to_ast" do
    thing = {:A, {:L, "x", {:L, "y", "z"}}, {:L, "x", "x"}}
    assert Lambda.to_ast(Lambda.tokenize("((Lx.((Ly.((z))))) ((Lx.(x))))")) == thing
    assert Lambda.to_ast(Lambda.tokenize("(((Lx.((Ly.((z))))) ((Lx.(x)))))")) == thing
    assert Lambda.to_ast(Lambda.tokenize("(Lx.Ly.z) (Lx.x)")) == thing
    assert Lambda.to_ast(Lambda.tokenize("(Lx.x) (Lx.x)")) == {:A, {:L, "x", "x"}, {:L, "x", "x"}}
    assert Lambda.to_ast(Lambda.tokenize("x y")) == {:A, "x", "y"}
    assert Lambda.to_ast(Lambda.tokenize("Lx.x")) == {:L, "x", "x"}
    assert Lambda.to_ast(Lambda.tokenize("(Lx.x)")) == {:L, "x", "x"}
  end

  # test "parse" do
  #   assert Lambda.parse("Lx.x") == Lambda.tokenize("Lx.x") |> Lambda.to_ast
  # end
end
