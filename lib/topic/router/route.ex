defmodule Skyline.Topic.Router.Route do
  # This module defines the Route struct that is used
  # throughout Phoenix's router. This struct is private
  # as it contains internal routing information.
  @moduledoc false

  alias Skyline.Topic.Router.Route
  import Skyline.Topic.Conn

  @doc """
  The `Skyline.Topic.Router.Route` struct. It stores:
    * :verb - the HTTP verb as an upcased string
    * :path - the normalized path as string
    * :pipe - the pipe module
    * :opts - the pipe options
    * :private - the private route info
    * :assigns - the route info
    * :pipe_through - the pipeline names as a list of atoms
  """

  defstruct [:verb, :path, :pipe, :opts,
             :private, :pipe_through, :assigns]

  @type t :: %Route{}

  @doc """
  Receives the verb, path, pipe, options and helper
  and returns a `Skyline.Topic.Router.Route` struct.
  """
  @spec build(String.t, String.t, atom, atom, atom, %{}, %{}) :: t
  def build(verb, path, pipe, opts, pipe_through, private, assigns)
      when is_atom(verb) and
           is_atom(pipe) and
           is_list(pipe_through) and is_map(private and is_map(assigns))
           do

    %Route{verb: verb, path: path, private: private,
           pipe: pipe, opts: opts,
           pipe_through: pipe_through, assigns: assigns}
  end

  @doc """
  Builds the expressions used by the route.
  """
  def exprs(route) do
    {path, binding} = build_path_and_binding(route)

    %{path: path,
      verb_match: verb_match(route.verb),
      binding: binding,
      dispatch: build_dispatch(route, binding)}
  end

  defp verb_match(:*), do: Macro.var(:_verb, nil)
  defp verb_match(verb), do: verb

  defp build_path_and_binding(%Route{path: path} = route) do
    {params, segments} = Skyline.Topic.Pipe.Utils.build_path_match(path)

    binding = for var <- params do
      {Atom.to_string(var), Macro.var(var, nil)}
    end

    {segments, binding}
  end

  defp build_dispatch(route, binding) do
    exprs =
      [maybe_binding(binding),
       maybe_merge(:private, route.private),
       maybe_merge(:assigns, route.assigns),
       build_pipes(route)]

    {:__block__, [], Enum.filter(exprs, & &1 != nil)}
  end

  defp maybe_merge(key, data) do
    if map_size(data) > 0 do
      quote do
        var!(conn) =
          update_in var!(conn).unquote(key), &Map.merge(&1, unquote(Macro.escape(data)))
      end
    end
  end

  defp maybe_binding([]), do: nil
  defp maybe_binding(binding) do
    quote do
      var!(conn) =
        update_in var!(conn).params, &Map.merge(&1, unquote({:%{}, [], binding}))
    end
  end

  defp build_pipes(route) do
    IO.inspect({:route, route})
    quote do
      var!(conn)
      |> Skyline.Topic.Conn.put_private(:phoenix_pipelines, unquote(route.pipe_through))
      |> Skyline.Topic.Conn.put_private(:phoenix_route, fn conn ->
        # We need to store this in a variable so the compiler
        # does not see a call and then suddenly start tracking
        # changes in the controller.
        pipe = unquote(route.pipe)
        opts = pipe.init(unquote(route.opts))
        pipe.call(conn, opts)
      end)
    end |> pipe_through(route)
  end

  defp pipe_through(initial, route) do
    pipes = route.pipe_through |> Enum.reverse |> Enum.map(&{&1, [], true})
    {conn, body} = Skyline.Topic.Pipe.compile(__ENV__, pipes, [])
    quote do
      unquote(conn) = unquote(initial)
      unquote(body)
    end
  end

end
