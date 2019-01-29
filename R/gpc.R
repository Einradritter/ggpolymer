plot_gpc_kinetic <- function(db, id) {
  entry <- dplyr::filter(db, reaction_id == id)
  entry %>% ggplot2::ggplot(ggplot2::aes(time, mn)) +
    ggplot2::geom_point()
}
