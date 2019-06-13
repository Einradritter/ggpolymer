#' Read all gpc ascii files in a directory
#'
#' @param dir A string.
#' @return A list of gpc data tibbles as elements.
#' @export
traces_in_dir <- function(dir, data_type = "elu") { # an example should be added to documentation
  file_list <- list.files(dir, full.names = T)
  if (data_type == "elu") {
    traces <- purrr::map(file_list, read_gpc_elu)
  }
  else if (data_type == "raw") {
    traces <- purrr::map(file_list, read_gpc_raw)
  }
  else if (data_type == "mwd") {
    traces <- purrr::map(file_list, read_gpc_mwd)
  }
  else {print("error: data_type must be \"elu\", \"raw\" or \"mwd\"")}
  return(traces)
}



#' Plot refractive index vs elution volume from a list of gpc data tibbles
#'
#' @param traces A list of gpc data tibbles (best used with traces_in_dir(),
#' @seealso \code{\link{traces_in_dir}})
#' @param durations a vector (numeric or string) of reaction durations at sample
#'   time.
#' @export
plot_traces <- function(traces, durations = FALSE, xlim = c(10,45), ylim = c(0,100), normalized_RI = FALSE) { # example should be added to documentation
  trace_cols <- rainbow(length(traces))
  if (normalized_RI == FALSE) {
    plot(traces[[1]]$Volume, traces[[1]]$RI, xlim = xlim, ylim = ylim, type = "l", col = trace_cols[1], lwd = 1)
    for (trace_nr in 2:length(traces)) {
      print(trace_nr)
      trace <- traces[[trace_nr]]
      points(trace$Volume, trace$RI, type = "l", col = trace_cols[trace_nr], lwd = 1)
    }
  }
  else if (normalized_RI == TRUE) {
    plot(traces[[1]]$Volume, traces[[1]]$RI_norm, xlim = xlim, ylim = ylim, type = "l", col = trace_cols[1], lwd = 1)
    for (trace_nr in 2:length(traces)) {
      print(trace_nr)
      trace <- traces[[trace_nr]]
      points(trace$Volume, trace$RI_norm, type = "l", col = trace_cols[trace_nr], lwd = 1)
    }
  }
  else {print("error: normalized_RI needs to be TRUE or FALSE")}
  if (durations != FALSE) {
   legend("topleft", legend = durations, col = trace_cols, lty = 1)
  }
}



