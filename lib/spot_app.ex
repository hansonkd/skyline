defmodule SpotApp do
  use Application

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
      worker(SpotApp.Worker, [8000])
    ]

    opts = [strategy: :one_for_one, name: SpotApp.Supervisor]
    Supervisor.start_link(children, opts)

  end

end
