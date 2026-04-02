defmodule SerialDistExample.MixProject do
  use Mix.Project

  # Note: do NOT define BEAM_INTERFACE in erlc_options. This project
  # includes AtomVM's dist_util which expects binary packets from recv.
  # The iolist_to_binary call in serial_dist_controller handles BEAM's
  # iovec output from dist_ctrl_get_data.

  def project do
    [
      app: :serial_dist_example,
      version: "0.1.0",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger, :crypto]
    ]
  end

  defp deps do
    [
      {:circuits_uart, "~> 1.5"}
    ]
  end
end
