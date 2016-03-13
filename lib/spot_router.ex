defmodule Spotmq.Router do

    use Spotmq.Amnesia.Tree.Database
    require Exquisite

    def add_topic_subscription(topic, pid) do
      add_words(edges(topic), pid)
    end

    def broadcast_msg(topic, msg) do
      Enum.each(collect_pids(topic), fn(pid) -> GenServer.cast(pid, msg) end)
    end

    def prune_tree(pid) do
      Enum.each(TreeEdge.where(subscriber_pid == pid) |> Amnesia.Selection.values, &TreeEdge.delete/1)
    end

    def collect_pids(topic) do
      collect_level(split(topic), ".", [])
    end

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

    defp edge_exists(path, word) do
      pos = TreeEdge.where(
        node_path == path and child_word == word,
        select: subscriber_pid,
        limit: 1
      )
      case pos do
        nil -> false
        _ -> true
      end
    end

    def select_pids(path, word) do
      TreeEdge.where(
        node_path == path and child_word == word and terminal == true,
        select: subscriber_pid
      ) |> Amnesia.Selection.values
    end

    defp add_words([], _pid) do
    end

    defp add_words([{location, w} | tail], pid) do
      %TreeEdge{node_path: location, child_word: w, subscriber_pid: pid, terminal: tail == []} |> TreeEdge.write
      add_words(tail, pid)
    end

    defp split(topic) do
      String.split(topic, "/")
    end

    defp join(chunks) do
      Enum.join(chunks, "/")
    end

    defp edges(topic) do
      edges(split(topic), ".", [])
    end

    defp edges([], _parent, acc) do
      Enum.reverse(acc)
    end

    defp edges([w|tail], parent, acc) do
      child = Enum.join([parent, w], "/")
      edges(tail, child, [{parent, w}|acc])
    end

end
