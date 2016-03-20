defmodule Skyline.Topic.Scope do
  alias Skyline.Topic.Scope
  @moduledoc false

  @stack :phoenix_router_scopes
  @pipes :phoenix_pipeline_scopes

  defstruct path: nil, alias: nil, pipes: [], private: %{}, assigns: %{}

  @doc """
  Initializes the scope.
  """
  def init(module) do
    Module.put_attribute(module, @stack, [%Scope{}])
    Module.put_attribute(module, @pipes, MapSet.new)
  end

  @doc """
  Builds a route based on the top of the stack.
  """
  def route(module, verb, path, pipe, pipe_opts, opts) do
    IO.inspect({"Pipe", pipe})
    private = Keyword.get(opts, :private, %{})
    assigns = Keyword.get(opts, :assigns, %{})

    {path, alias, pipes, private, assigns} =
      join(module, path, pipe, private, assigns)
    Skyline.Topic.Router.Route.build(verb, path, alias, pipe_opts, pipes, private, assigns)
  end

  @doc """
  Defines the given pipeline.
  """
  def pipeline(module, pipe) when is_atom(pipe) do
    update_pipes module, &MapSet.put(&1, pipe)
  end

  @doc """
  Appends the given pipes to the current scope pipe through.
  """
  def pipe_through(module, pipes) do
    pipes = List.wrap(pipes)

    update_stack(module, fn [scope|stack] ->
      scope = put_in scope.pipes, scope.pipes ++ pipes
      [scope|stack]
    end)
  end

  @doc """
  Pushes a scope into the module stack.
  """
  def push(module, path) when is_binary(path) do
    push(module, path: path)
  end

  def push(module, opts) when is_list(opts) do
    path = Keyword.get(opts, :path)
    path = path && Skyline.Topic.Pipe.Utils.split(path)

    alias = Keyword.get(opts, :alias)
    alias = alias && Atom.to_string(alias)

    scope = %Scope{path: path,
                   alias: alias,
                   pipes: [],
                   private: Keyword.get(opts, :private, %{}),
                   assigns: Keyword.get(opts, :assigns, %{})}

    update_stack(module, fn stack -> [scope|stack] end)
  end

  @doc """
  Pops a scope from the module stack.
  """
  def pop(module) do
    update_stack(module, fn [_|stack] -> stack end)
  end

  @doc """
  Returns true if the module's definition is currently within a scope block
  """
  def inside_scope?(module), do: length(get_stack(module)) > 1

  defp join(module, path, alias, private, assigns) do
    stack = get_stack(module)
    {join_path(stack, path), join_alias(stack, alias),
     join_pipe_through(stack), join_private(stack, private),
     join_assigns(stack, assigns)}
  end

  defp join_path(stack, path) do
    "/" <>
      ([Skyline.Topic.Pipe.Utils.split(path)|extract(stack, :path)]
       |> Enum.reverse()
       |> Enum.concat()
       |> Enum.join("/"))
  end

  defp join_alias(stack, alias) when is_atom(alias) do
    [alias|extract(stack, :alias)]
    |> Enum.reverse()
    |> Module.concat()
  end

  defp join_as(_stack, nil), do: nil
  defp join_as(stack, as) when is_atom(as) or is_binary(as) do
    [as|extract(stack, :as)]
    |> Enum.reverse()
    |> Enum.join("_")
  end

  defp join_private(stack, private) do
    Enum.reduce stack, private, &Map.merge(&1.private, &2)
  end

  defp join_assigns(stack, assigns) do
    Enum.reduce stack, assigns, &Map.merge(&1.assigns, &2)
  end

  defp join_pipe_through(stack) do
    for scope <- Enum.reverse(stack),
        item <- scope.pipes,
        do: item
  end

  defp extract(stack, attr) do
    for scope <- stack,
        item = Map.fetch!(scope, attr),
        do: item
  end

  defp get_stack(module) do
    get_attribute(module, @stack)
  end

  defp update_stack(module, fun) do
    update_attribute(module, @stack, fun)
  end

  defp update_pipes(module, fun) do
    update_attribute(module, @pipes, fun)
  end

  defp get_attribute(module, attr) do
    Module.get_attribute(module, attr) ||
      raise "Phoenix router scope was not initialized"
  end

  defp update_attribute(module, attr, fun) do
    Module.put_attribute(module, attr, fun.(get_attribute(module, attr)))
  end
end
