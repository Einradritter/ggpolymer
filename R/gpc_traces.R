#' Read all gpc ascii files in a directory
#'
#' @param dir A string.
#' @param data_type A String. "elu", "raw" or "mwd"
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

## helper funtion for plot_traces to deal with mwd data
## todo: roxygen infos should be added
plot_traces_mwd <- function(traces, ...){
  plot(traces[[1]]$molar_mass, traces[[1]]$RI_mwd, type = "l", ...)
  for (trace_nr in 2:length(traces)) {
    points(traces[[trace_nr]]$molar_mass, traces[[trace_nr]]$RI_mwd, type = "l", col = trace_nr+1, ...)
  }
}

## helper funtion for plot_traces to deal with mwd data
## todo: roxygen infos should be added
## needs some work
plot_traces_mwd_norm <- function(traces, ...){
  plot(traces[[1]]$molar_mass, traces[[1]]$RI_mwd_norm, type = "l", ...)
  for (trace_nr in 2:length(traces)) {
    points(traces[[trace_nr]]$molar_mass, traces[[trace_nr]]$RI_mwd_norm, type = "l", col = trace_nr+1, ...)
  }
}


## Helper function for plot_traces to deal with raw data
## todo: roxygen data should be added
plot_traces_raw <- function(traces, ...) {
  plot(traces[[1]]$time, traces[[1]]$RI_raw, type = "l", ...)
  for (trace_nr in 2:length(traces)) {
    points(traces[[trace_nr]]$time, traces[[trace_nr]]$RI_raw, type = "l", col = trace_nr+1, ...)
  }
}


## Helper function for plot_traces, to deal with elu data
## todo: roxygen data should be added
plot_traces_elu <- function(traces, ...) {
  plot(traces[[1]]$Volume, traces[[1]]$RI_elu, type = "l", ...)
  for (trace_nr in 2:length(traces)) {
    points(traces[[trace_nr]]$Volume, traces[[trace_nr]]$RI_elu, type = "l", col = trace_nr+1, ...)
  }
}

## Helper function for plot_traces, to deal with normalized elu data
## todo: roxygen data should be added
plot_traces_elu_norm <- function(traces, ...) {
  plot(traces[[1]]$Volume, traces[[1]]$RI_elu_norm, type = "l", ...)
  for (trace_nr in 2:length(traces)) {
    points(traces[[trace_nr]]$Volume, traces[[trace_nr]]$RI_elu_norm, type = "l", col = trace_nr+1, ...)
  }
}


## todo: this could be done more elegant using a class system i guess
## todo: plot elu should be a helper function on its own, the code looks spagetti
#' plotting function which is able to plot different GPC spectra depending on the chosen data_type
#'
#' @param traces A list of gpc data tibbles (best used with traces_in_dir(),
#' @seealso \code{\link{traces_in_dir}})
#' @param durations A vector (numeric or string) of reaction durations at sample
#'   time.
#' @param data_type A string. /"raw/", "/elu/" or /"mwd/".
#' @export
plot_traces <- function( traces, data_type = "elu", normalized_RI = FALSE, durations = FALSE, ... ) { # example should be added to documentation
  if (normalized_RI == FALSE) {
    if ( data_type == "mwd" ) {
      plot_traces_mwd( traces, ... )
    }
    else if ( data_type == "raw" ){
      plot_traces_raw( traces, ... )
    }
    else if ( data_type == "elu" ){
      plot_traces_elu( traces, ... )
    }
    else {
      print( "error: data_type must be \"raw\", \"mwd\" or \"elu\"." )
    }
  }
  else if ( normalized_RI == TRUE ) {
    if ( data_type == "mwd" ) {
      plot_traces_mwd_norm( traces, ... )
    }
    else if ( data_type == "raw" ){
      plot_traces_raw( traces, ... )
    }
    else if ( data_type == "elu" ){
      plot_traces_elu_norm( traces, ... )
    }
    else {
      print( "error: data_type must be \"raw\", \"mwd\" or \"elu\"." )
    }
  }
}

#' Function to plot all the gpc plots of interest in a dirctory
#'
#' @importFrom magrittr %>%
#' @param dir A string. The dir.
#' @param data_type A string. "elu", "raw", or "mwd".
#' @param normalized_RI TRUE or FALSE. Use normalized RI data or not.
#' @export
plot_traces_in_dir <- function(dir, data_type = "elu", normalized_RI = FALSE, ...) {
  traces_in_dir(dir, data_type) %>% plot_traces(data_type, normalized_RI, ...)
}

