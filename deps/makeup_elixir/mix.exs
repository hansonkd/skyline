defmodule MakeupElixir.Mixfile do
  use Mix.Project

  @version "0.8.0"

  def project do
    [
      app: :makeup_elixir,
      version: @version,
      elixir: "~> 1.4",
      start_permanent: Mix.env == :prod,
      deps: deps(),
      # Package
      package: package(),
      description: description(),
      docs: [
        main: "readme",
        assets: "assets",
        extras: [
          "README.md"
        ]
      ]
    ]
  end

  defp description do
    """
    Elixir lexer for the Makeup syntax highlighter.
    """
  end

  defp package do
    [
      name: :makeup_elixir,
      licenses: ["BSD"],
      maintainers: ["Tiago Barroso <tmbb@campus.ul.pt>"],
      links: %{"GitHub" => "https://github.com/tmbb/makeup_elixir"}
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:nimble_parsec, "~> 0.2.2"},
      {:makeup, "~> 0.5.0"},
      {:ex_doc, "~> 0.18.3", only: [:dev]},
      {:benchee, "~> 0.13", only: [:dev, :test]}
    ]
  end
end
