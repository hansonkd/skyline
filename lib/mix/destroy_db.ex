defmodule Mix.Tasks.DestroyDb do
  use Mix.Task
  use Skyline.Amnesia.Topic.Database

  def run(_) do
    # Start mnesia, or we can't do much.
    Amnesia.start

    # Destroy the database.
    Database.destroy

    # Stop mnesia, so it flushes everything.
    Amnesia.stop

    # Destroy the schema for the node.
    Amnesia.Schema.destroy
  end
end
