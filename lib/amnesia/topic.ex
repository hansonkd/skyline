defmodule Skyline.Amnesia.Topic do
  @moduledoc false
  use Amnesia

  # defines a database called Database, it's basically a defmodule with
  # some additional magic
  defdatabase TopicDatabase do
    @moduledoc false

    # this defines a table with an user_id key and a content attribute, and
    # makes the table a bag; tables are basically records with a bunch of helpers
    deftable StoredTopic, [:topic_id, :message], type: :set do
      @moduledoc false

      # this isn't required, but it's always nice to spec things
      @type t :: %StoredTopic{topic_id: String.t, message: String.t}
    end

    # deftable SubscriptionQueue, [:client_id, :message], type: :set do
    #   @moduledoc false
    # 
    #   # this isn't required, but it's always nice to spec things
    #   @type t :: %StoredTopic{topic_id: String.t, message: String.t}
    # end

  end
end
