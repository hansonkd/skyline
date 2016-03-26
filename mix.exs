defmodule Skyline.Mixfile do
  use Mix.Project

  def project do
    [app: :skyline,
     version: "0.0.1",
     elixir: "~> 1.2",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps]
  end

  # Configuration for the OTP application
  #
  # Type "mix help compile.app" for more information
  def application do
    [applications: [:logger, :socket, :amnesia, :con_cache],
     mod: {Skyline, []}]
  end

  # Dependencies can be Hex packages:
  #
  #   {:mydep, "~> 0.3.0"}
  #
  # Or git/path repositories:
  #
  #   {:mydep, git: "https://github.com/elixir-lang/mydep.git", tag: "0.1.0"}
  #
  # Type "mix help deps" for more examples and options
  defp deps do
    [{:socket, "~> 0.3"},
     {:amnesia, github: "meh/amnesia", tag: :master},
     {:epocxy, github: "duomark/epocxy", tag: :master},
     {:con_cache, "~> 0.11.0"},
     {:dialyxir, "~> 0.3", only: [:dev]},
     {:ex_doc, "~> 0.11", only: :dev},
     {:earmark, ">= 0.0.0", only: :dev}]
  end
end
