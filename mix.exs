defmodule Skyline.Mixfile do
  use Mix.Project

  def project do
    [app: :skyline,
     version: "0.0.1",
     elixir: "~> 1.7",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps()]
  end

  def application do
    [applications: [:logger, :socket, :amnesia],
     mod: {Skyline, []}]
  end

  defp deps do
    [{:socket, "~> 0.3"},
     {:amnesia, "~> 0.2"},
     {:dialyxir, "~> 0.5", only: [:dev]},
     {:ex_doc, "~> 0.14", only: :dev},
     {:earmark, "~> 1.2", only: :dev}]
  end
end
