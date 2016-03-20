defmodule Mix.Tasks.DestroyDb do
  use Mix.Task
  use Skyline.Amnesia.Topic.TopicDatabase

  def run(_) do
    # Start mnesia, or we can't do much.
    Amnesia.start

    # Destroy the database.
    TopicDatabase.destroy

    # Stop mnesia, so it flushes everything.
    Amnesia.stop

    # Destroy the schema for the node.
    Amnesia.Schema.destroy
  end
end
