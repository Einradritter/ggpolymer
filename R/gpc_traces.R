#' Read all gpc ascii files in a directory
#'
#' @param dir A string.
#' @return A list of gpc data tibbles as elements.
traces_in_dir <- function(dir) { # an example should be added to documentation
  file_list <- list.files(dir, full.names = T)
  traces <- purrr::map(file_list, read_gpc_elu)
  return(traces)
}

#' Plot refractive index vs elution volume from a list of gpc data tibbles
#'
#' @param traces A list of gpc data tibbles (best used with traces_in_dir(),
#' @seealso \code{\link{traces_in_dir}})
#' @param durations a vector (numeric or string) of reaction durations at sample
#'   time.
plot_traces <- function(traces, durations, xlim = c(10,45)) { # example should be added to documentation
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



