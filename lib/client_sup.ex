defmodule Skyline.ClientSupervisor do
  use Supervisor

  def start_link(socket, app_config) do
    Supervisor.start_link(__MODULE__, {socket, app_config})
  end

  def init({socket, app_config}) do
    children = [
        #worker(Skyline.Client, [socket, app_config, self], restart: :transient)
    ]
    supervise(children, strategy: :one_for_one)
  end
end