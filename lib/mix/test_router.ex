defmodule MyPlug do
  use Skyline.Topic.Controller

  pipe :awesome

  def awesome(conn, opts) do
    IO.puts("AWESOME!!!!!")
    conn
  end

  def init(opts) do
    opts
  end
  def publish(conn, _opts) do
    IO.inspect {:woooooo, conn}
    conn
  end
end

defmodule Inline do

  def init(opts) do
    IO.puts("Called once")
    opts
  end
  def call(conn, _opts) do
    IO.inspect {:inline, conn}
    conn
  end
end

defmodule Mix.Tasks.TestRouter do
  use Skyline.Topic.Router

  pipeline :mypipeline do
    pipe Inline, []
  end

  def what(msg, opts) do
    IO.inspect {:hi, msg}
    msg
  end

  def whati(msg, opts) do
    IO.inspect {:argooooooooooooo, msg}
    {:ok, msg}
  end

  def run(_) do
    IO.inspect mypipeline(%Skyline.Msg.PublishReq{}, [])
  end

  scope "/" do
    pipe_through :mypipeline
    publish("*path", MyPlug)
    match(:subscribe, "oh/*catchall", MyPlug, [])
  end
end
