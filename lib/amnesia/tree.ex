defmodule Spotmq.Amnesia.Tree do
  use Amnesia

  # defines a database called Database, it's basically a defmodule with
  # some additional magic
  defdatabase Database do

    # this defines a table with an user_id key and a content attribute, and
    # makes the table a bag; tables are basically records with a bunch of helpers
    deftable StoredTopic, [:topic_id, :message], type: :set do
      @type t :: %StoredTopic{topic_id: String.t, message: String.t}
    end

    deftable TreeNode, [:node_path, :subscriber_pid], type: :bag do
      @type t :: %TreeNode{node_path: String.t, subscriber_pid: Pid}
    end

    deftable TreeEdge, [:node_path, :child_word, :subscriber_pid, :terminal], type: :bag do
      @type t :: %TreeEdge{node_path: String.t, child_word: String.t, subscriber_pid: Pid, terminal: boolean}
    end
  end
end
