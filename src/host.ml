external snapshot_signals : unit -> unit = "probe_snapshot_signals"
external signals_unchanged : unit -> bool = "probe_signals_unchanged"
external notify : Unix.file_descr -> bool = "probe_notify"
