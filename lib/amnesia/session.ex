defmodule Skyline.Amnesia.Session do
  @moduledoc false
  use Amnesia

  # defines a database called Database, it's basically a defmodule with
  # some additional magic
  defdatabase SessionDatabase do
    @moduledoc false

    # this defines a table with an user_id key and a content attribute, and
    # makes the table a bag; tables are basically records with a bunch of helpers
    deftable StoredSession, [:client_id,
                             :topics,
                             :last_will_topic,
                             :last_will_message,
                             :last_will_qos], type: :set do
      @moduledoc false

      # this isn't required, but it's always nice to spec things
      @type t :: %StoredSession{client_id: String.t, topics: [String.t]}
    end

  end
end
