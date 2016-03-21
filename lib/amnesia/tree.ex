defmodule Skyline.Amnesia.Dispatch do
  @moduledoc false
  use Amnesia

  # defines a database called Database, it's basically a defmodule with
  # some additional magic
  defdatabase TreeDatabase do
    @moduledoc false

    deftable TreeEdge, [:node_path, :child_word, :subscriber_pid, :terminal], type: :bag do
      @moduledoc false
      
      @type t :: %TreeEdge{node_path: String.t, child_word: String.t, subscriber_pid: Pid, terminal: boolean}
    end
  end
end
