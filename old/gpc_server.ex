defmodule SpotGproc do
  use GenServer

  def init(args) do
    :gproc.reg({:p, :l, :spotmq}, [], [])
    {:ok, args[:attrs]}
  end

  def handle_cast(msg, state) do
    #IO.inspect "Got #{inspect msg} in process #{inspect self()}"
    #IO.inspect "Got #{inspect state}"
    {:noreply, state}
  end

end
