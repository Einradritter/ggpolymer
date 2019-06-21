###

#' Function to import gpc mwd data as a tidy tibble, mostly used for pplot_gpc_mwd
#'
#' @importFrom magrittr %>%
#' @param dir A string. The dir.
#' @param curve_names A vector of strings. The names of the GPC curves".
#' @export
mwd_pp <- function(dir, curve_names) {
  mwd <- traces_in_dir(dir, data_type = "mwd")
  if (length(curve_names) == length(mwd)){
    mwd_d <- mwd[[1]]
    for (i in 2:length(mwd)) {
      mwd_d <- dplyr::full_join(mwd_d, mwd[[i]], by = "molar_mass")
    }
    mwd <- dplyr::select(mwd_d,molar_mass, dplyr::starts_with("RI_mwd_norm"))
    names(mwd)[2:length(mwd)] <- curve_names
    mwd <- mwd %>% tidyr::gather(names(mwd)[-1], key = "sample", value = "RI")
    mwd$sample <- as.factor(mwd$sample) # change sample var into factor to not run into problems with legend
    mwd$sample <- factor(mwd$sample, levels = curve_names) # order factor levels to get right
    return(mwd)
  }
  else {
    print("error: length of curve_names and number of curves dont match.")
  }
}


#' Function to pretty plot pc mwd curves based tidy gpc mwd tibble.
#'
#' @importFrom magrittr %>%
#' @param mwd A tibble. Usually created by the mwd_pp function.
#' @export
pplot_gpc_mwd <- function(mwd, curve_names) {
  br <- c(10^3, 3*10^3, 10^4, 3* 10^4)
  gg_obj <- mwd %>% ggplot2::ggplot() +
    ggplot2::geom_path(ggplot2::aes(molar_mass, RI, col = sample)) +
    ggplot2::scale_x_log10("molar mass [g/mol]", breaks = br) +
    ggplot2::scale_y_continuous("RI detector signal [a.u.]") +
    ggplot2::scale_fill_discrete(breaks=curve_names) +
    ggplot2::annotation_logticks(sides = "b") +
    ggplot2::theme_bw()
  return(gg_obj)
}

#' Function to pretty plot all gpc mwd curves in a dirctory
#'
#' @importFrom magrittr %>%
#' @param dir A string. The dir.
#' @param curve_names A vector of strings. The names of the GPC curves".
#' @export
pplot_gpc_mwd_in_dir <- function(dir, curve_names) {
  gg_obj<- mwd_pp(dir, curve_names) %>%
    pplot_gpc_mwd(curve_names)
  return(gg_obj)
}

