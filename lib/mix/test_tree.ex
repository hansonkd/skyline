defmodule Mix.Tasks.TestTree do
  use Mix.Task
  use Spotmq.Amnesia.Tree.Database
  require Exquisite

  def run(_) do
    # This creates the mnesia schema, this has to be done on every node before
    # starting mnesia itself, the schema gets stored on disk based on the
    # `-mnesia` config, so you don't really need to create it every time.
    Amnesia.Schema.create

    # Once the schema has been created, you can start mnesia.
    Amnesia.start

    # When you call create/1 on the database, it creates a metadata table about
    # the database for various things, then iterates over the tables and creates
    # each one of them with the passed copying behaviour
    #
    # In this case it will keep a ram and disk copy on the current node.
    Database.create()

    # This waits for the database to be fully created.
    Database.wait

    Amnesia.transaction do
      # ... initial data creation

      # %TreeNode{node_path: "A/B", subscriber_pid: "1"} |> TreeNode.write
      # %TreeEdge{node_path: "A/B", child_word: "1"} |> TreeEdge.write

      IO.inspect TreeNode.read("A/B")
      IO.inspect TreeEdge.read("A/B")
      IO.inspect ((TreeEdge.where node_path == "A/B", select: child_word) |> Amnesia.Selection.values)

      #add_words(edges("A/B/C"))

      IO.inspect edges("A")

      IO.inspect TreeEdge.read("A")
      IO.inspect TreeEdge.read("A/B")
      IO.inspect TreeEdge.read("A/B/C")

      IO.inspect "PIDS"
      add_topic_subscription("1/+/3", 1)
      add_topic_subscription("+/+/3", 2)
      add_topic_subscription("1/#", "all")
      add_topic_subscription("1/2/3", 9)
      prune_tree(9)
      IO.inspect TreeEdge.read("./1/2")
      IO.inspect TreeEdge.read("./1")

      IO.inspect collect_pids("1/2/3")
    end

    # Stop mnesia so it can flush everything and keep the data sane.
    Amnesia.stop
  end

  def add_topic_subscription(topic, pid) do
    add_words(edges(topic), pid)
  end

  def prune_tree(pid) do
    Enum.each(TreeEdge.where(subscriber_pid == pid) |> Amnesia.Selection.values, &TreeEdge.delete/1)
  end

  def collect_pids(topic) do
    collect_level(split(topic), ".", [])
  end

  def broadcast_msg(topic, msg) do
    Enum.each(collect_pids(topic), fn(pid) -> GenServer.cast(pid, msg) end)
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
