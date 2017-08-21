defmodule Skyline.Topic.Dispatcher do
    @moduledoc false

    # Router
    #
    # A message dispatch scheme inspired by emqttd.
    #
    # This tree differs from emqttd by acting as a registry for the pids themselves
    # instead of acting as a registry for active topics, which are then dispatched seperately.
    #
    # It also differs by each key being immutable and tagged by pid of the subscriber process.
    # This makes write locks unecassary and lets you remove subscriptions and prune leaves without
    # walking the tree.
    # In emqttd trees, the nodes count the leaves by the value on the edge which gets mutated.
    # 
    # No benchmarks have been done to see if one is more efficient then the other.

    use Skyline.Amnesia.Dispatch.TreeDatabase
    require Exquisite
    require Amnesia
    require Amnesia.Helper

    @doc "Register a subcsriber pid with a topic"
    @spec add_topic_subscription(String.t, pid) :: :ok
    def add_topic_subscription(topic, pid) do
      add_words(edges(topic), pid)
      :ok
    end

    @doc "Cast a message to all pid's registered with the topic"
    @spec broadcast_msg(String.t, Skyline.skyline_msg) :: :ok
    def broadcast_msg(topic, msg) do
      Enum.each(collect_pids(topic), fn(pid) -> GenServer.cast(pid, msg) end)
      :ok
    end

    @doc "Remove all edges associated with pid"
    @spec prune_tree(pid) :: :ok
    def prune_tree(pid) do
      Amnesia.transaction do
        Enum.each(TreeEdge.where(subscriber_pid == pid) |> Amnesia.Selection.values, &TreeEdge.delete/1)
      end
      :ok
    end

    @spec collect_pids(String.t) :: [pid]
    defp collect_pids(topic) do
      collect_level(split(topic), ".", [])
    end

    @spec collect_level([String.t], String.t, [pid]) :: [pid]
    defp collect_level([], parent, acc) do
      acc
    end
    defp collect_level([w | tail], parent, acc) do
      List.foldl(
        [w, "+"],
        acc,
        fn(chunk, acc2) ->
          case edge_exists(parent, chunk) do
            nil -> acc2
            _ ->
              case tail do
                 [] -> select_pids(parent, chunk) ++ acc2
                 _ -> collect_level(tail, join([parent, chunk]), acc2)
              end
          end
        end
      ) ++ select_pids(parent, "#")
    end

    @spec edge_exists(String.t, String.t) :: boolean
    defp edge_exists(path, word) do
      pos = TreeEdge.where!(
        node_path == path and child_word == word,
        select: subscriber_pid,
        limit: 1
      )
      case pos do
        nil -> false
        _ -> true
      end
    end

    @spec select_pids(String.t, String.t) :: [pid]
    defp select_pids(path, word) do
      TreeEdge.where!(
        node_path == path and child_word == word and terminal == true,
        select: subscriber_pid
      ) |> Amnesia.Selection.values
    end

    @spec add_words([{String.t, String.t}], pid) :: :ok
    defp add_words([], _pid) do
      :ok
    end

    defp add_words([{location, w} | tail], pid) do
      %TreeEdge{node_path: location, child_word: w, subscriber_pid: pid, terminal: tail == []} |> TreeEdge.write!
      add_words(tail, pid)
    end

    @spec split(String.t) :: [String.t]
    defp split(topic) do
      String.split(topic, "/")
    end

    @spec join([String.t]) :: String.t
    defp join(chunks) do
      Enum.join(chunks, "/")
    end

    @spec edges(String.t) :: [{String.t, String.t}]
    defp edges(topic) do
      edges(split(topic), ".", [])
    end

    @spec edges([String.t], String.t, [{String.t, String.t}]) :: [{String.t, String.t}]
    defp edges([], _parent, acc) do
      Enum.reverse(acc)
    end

    defp edges([w|tail], parent, acc) do
      child = Enum.join([parent, w], "/")
      edges(tail, child, [{parent, w}|acc])
    end

end
