traces_in_dir <- function(dir) {
  file_list <- list.files("../../data/GPC/hw31-PDMAEMA-kinetik/ascii/", full.names = T)
  traces <- purrr::map(file_list, read_gpc_elu)
  return(traces)
}

plot_traces <- function(traces, durations, xlim = c(10,45)) {
  trace_cols <- rainbow(length(traces))
  plot(traces[[1]]$Volume, traces[[1]]$RI, xlim = xlim, type = "l", col = trace_cols[1], lwd = 1)
  for (trace_nr in 2:length(traces)) {
    print(trace_nr)
    trace <- traces[[trace_nr]]
    points(trace$Volume, trace$RI, type = "l", col = trace_cols[trace_nr], lwd = 1)
  }
  legend("topleft", legend = durations,
    col = trace_cols, lty = 1)
}

