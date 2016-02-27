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

  # Group tokens by parentheses groups.
  def group(tokens), do: group(tokens, [])

  def group([]      , acc), do: acc |> :lists.reverse
  def group(["("| t], acc)  do
                                {the_group, unparsed_tokens} = group(t, [])
                                group(unparsed_tokens, [the_group | acc])
  end
  def group([")"| t], acc), do: {{:group, group([], acc)}, t}
  def group([h  | t], acc), do: group(t, [h | acc])

  # def group(["L", var, "." | t], acc) do
  #   {the_term, unparsed_tokens} = group(t, [])
  #   # group(t, [{:L, var, 
  # end

  def group2(tokens), do: group2(tokens, [ [] ])

  def terminate(acc) do
    case acc do
      [:group | t] -> {:group, t}
      [:L, var | t] -> {:L, var, t}
      [x] when is_tuple(x) -> x
      x -> x
    end
  end
    
  def emit(token, [h|t]=_stack), do: [ [token | h] | t]

  def group2([], [acc]),                 do: terminate(acc)
  def group2(["("| t], stack),           do: group2(t,  [[:group]  | stack])
  def group2(["L", var, "." |t], stack), do: group2(t,  [[var, :L] | stack])
  def group2([")"| t], [acc| stack]),    do: group2(t,  emit(terminate(acc |> :lists.reverse), stack))
  def group2([],       [acc| stack]),    do: group2([], emit(terminate(acc), stack))
  def group2([x  | t], stack),           do: group2(t,  emit(x             , stack))

  def inject(acc, [h|t]), do: [ [finish(acc) | h] | t ]

  defmacro is_var(x), do: quote(do: unquote(x) >= "a" and unquote(x) <= "z")
  def finish([t, x, :L]), do: {:L, finish(x), finish(t)}
  def finish([t2, t1, :S]), do: {:S, finish(t1), finish(t2)}
  def finish([t, :"("]), do: finish(t)
  def finish(x) when is_var(x), do: x

  def pop([acc,h|t]), do: [[acc|h] | t]
  def pop([acc]), do: [acc]
  def push(stack, x), do: [x | stack]

  def group3(tokens), do: group3(tokens, []) |> finish
  def group3([]       , [acc]               ),   do: acc
  def group3([]       , stack               ),   do: group3([],        stack |> pop)
  def group3([")" | t], [[_, :"("] | _]=stack),  do: group3(t,         stack |> pop)
  def group3([")" | t], stack               ),   do: group3([")" | t], stack |> pop)
  def group3(["." | t], stack               ),   do: group3(t,         stack |> pop)
  def group3(["(" | t], stack               ),   do: group3(t,         stack |> push( [:"("]    ))
  def group3(["L" | t], stack               ),   do: group3(t,         stack |> push( [:L]      ))
  def group3([" " | t], [acc | stack]       ),   do: group3(t,         stack |> push( [acc, :S] ))
  def group3([x   | t], stack) when is_var(x),   do: group3(t,         stack |> push( x         ))

  # Lx.x y z
  # [ [] ]
  # [ [L x], [] ]
  # [ [L x x], [] ]
  # [ [S x y], [L x], [] ]
  # [ [S y z], [S x], [L x], [] ]   # now we hit the end.
  # [ [S x {:S, y, z}], [L x], [] ] # we terminated and emitted. Like ).
  # [ [L x {:S, x, {:S, y, z}}], [] ] # again
  # [ {:L, x, {:S, x, {:S, y, z}}} ] # again


  



  def ast(["L", var, "." | t]), do: [:L, var, ast(t)]
  def ast(tokens) do
    
  end

  def reduce(["L", var, "." | t]) do
    "TODO"
  end

  def parse(term), do: term |> tokenize |> group2 |> ast

  test "tokenize" do
    assert Lambda.tokenize("Lx.x") == ["L", "x", ".", "x"]
    assert Lambda.tokenize("Lx.x y") == ["L", "x", ".", "x", " ", "y"]
  end

  # test "reduce" do
  #   assert Lambda.reduce(Lambda.tokenize("Lx.x y")) == "y"
  # end

  test "group3" do
    assert Lambda.group3(Lambda.tokenize("((((Lx.((Ly.((z))))) ((Lx.(x))))))")) == []
    assert Lambda.group3(Lambda.tokenize("(Lx.Ly.z) (Lx.x)")) == []
    assert Lambda.group3(Lambda.tokenize("(Lx.x) (Lx.x)")) == []
    assert Lambda.group3(Lambda.tokenize("x y")) == []
    assert Lambda.group3(Lambda.tokenize("Lx.x")) == []
    assert Lambda.group3(Lambda.tokenize("(Lx.x)")) == []
  end
  # test "group2" do
  #   assert Lambda.group2(Lambda.tokenize("abc")) == ["a", "b", "c"]
  #   assert Lambda.group2(Lambda.tokenize("abc(de(fg)h)")) == [
  #     "a", "b", "c", {:group, [
  #         "d", "e", {:group, [
  #             "f", "g"]},
  #         "h"]}]
  # end

  # test "parse" do
  #   assert Lambda.parse("Lx.x") == Lambda.tokenize("Lx.x") |> Lambda.group2
  # end
end
