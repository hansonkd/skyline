defmodule Mix.Tasks.CreateDb do
  use Mix.Task
  use Spotmq.Persist.Topic.Database

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
    Database.create(disk: [node])

    # This waits for the database to be fully created.
    Database.wait

    Amnesia.transaction do
      # ... initial data creation
    end

    # Stop mnesia so it can flush everything and keep the data sane.
    Amnesia.stop
  end
end
