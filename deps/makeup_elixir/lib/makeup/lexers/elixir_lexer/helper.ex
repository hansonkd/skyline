defmodule Makeup.Lexers.ElixirLexer.Helper do
  @moduledoc false
  import NimbleParsec
  alias Makeup.Lexer.Combinators

  def with_optional_separator(combinator, separator) when is_binary(separator) do
    combinator |> repeat(string(separator) |> concat(combinator))
  end

  # Allows escaping of the first character of a right delimiter.
  # This is used in sigils that don't support interpolation or character escapes but
  # must support escaping of the right delimiter.
  def escape_delim(rdelim) do
    rdelim_first_char = String.slice(rdelim, 0..0)
    string("\\" <> rdelim_first_char)
  end

  def sigil(ldelim, rdelim, ranges, middle, ttype, attrs \\ %{}) do
    left = string("~") |> utf8_string(ranges, 1) |> string(ldelim)
    right = string(rdelim)

    choices = middle ++ [utf8_char([])]

    left
    |> repeat_until(choice(choices), [right])
    |> concat(right)
    |> optional(utf8_string([?a..?z, ?A..?Z], min: 1))
    |> traverse({Combinators, :collect_raw_chars_and_binaries, [ttype, attrs]})
  end

  def escaped(literal) when is_binary(literal) do
    string("\\" <> literal)
  end

  def keyword_matcher(kind, fun_name, words) do
    heads = for {ttype, words} <- words do
      for word <- words do
        case kind do
          :defp ->
            quote do
              defp unquote(fun_name)([{:name, attrs, unquote(ttype)} | tokens]) do
                [{unquote(ttype), attrs, unquote(word)} | unquote(fun_name)(tokens)]
              end
            end |> IO.inspect
          :def ->
            quote do
              def unquote(fun_name)([{:name, attrs, unquote(ttype)} | tokens]) do
                [{unquote(ttype), attrs, unquote(word)} | unquote(fun_name)(tokens)]
              end
            end
        end
      end
    end

    quote do
      unquote_splicing(heads)
    end
  end
end