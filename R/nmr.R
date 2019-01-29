### atrp data analysis

## what i need is a simple function to plot:
## a) ln ( c_0(M) / c(M) ) vs time from pgc data.

## i need an nmr_db to track the values as well


nmr_kinetic <- function(db, id) {

  db_entry <- db %>%
    dplyr::filter(reaction_id == id)

  time <- db_entry %>%
    dplyr::select("time")

  int <- db_entry %>%
    dplyr::select("integral")
  if (0 %in% int) {print("warning: nmr integrale is zero, cant devide!")}

  if(db_entry$sample_nr[1] == 0) {
    int_0 <- as.numeric(int[1,1])} else {
      print("error: no zero sample")}

  return(dplyr::bind_cols(time,log(int_0/int),(1-(int/int_0))*100) %>%
           dplyr::rename("log[int_0/int]" = integral, "conversation" = integral1))
}

plot_nmr_kinetic <- function(kinetic_data) {
  kinetic_data %>%
    ggplot2::ggplot(ggplot2::aes(time, `log[int_0/int]`)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = lm) +
    ggplot2::labs(x = "time [min]", y = "log(int_0/int)")
}
## the labels could be improved with the package latx2exp
## with the command TeX("$\\log(\\frac{int_{0}}{int})$")


plot_mn_vs_con <- function(nmr_db, gpc_db, r_id) {
  convers <- nmr_db %>% nmr_kinetic(id = r_id)
  mn <- gpc_db %>% dplyr::filter(reaction_id == r_id)
  join <- dplyr::full_join(mn, convers, "time") %>%
    na.omit()
  join %>%
    ggplot2::ggplot(ggplot2::aes(conversation, mn)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = lm)
}

k_app <- function(kinetic_data) {
  lin_mod <- kinetic_data %>% lm(formula = `log[int_0/int]` ~ time , data = .)
  k_app <- unname(lin_mod$coefficients[2])
  return(k_app)
}

