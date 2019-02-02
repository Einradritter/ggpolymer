file_list <- list.files("../../data/GPC/hw31-PDMAEMA-kinetik/ascii/", full.names = T)

traces <- purrr::map(file_list, read_gpc_elu)

duration <- c("10", "20", "30", "40", "50", "60")
for (trace_nr in 1:length(traces)) {
  print(trace_nr)
  trace <- traces[[trace_nr]]
  traces[[trace_nr]]<- trace %>% tibble::add_column(duration = rep(duration[trace_nr], dim(trace)[1]))
}

traces[[6]]


elu_data <- traces[[1]] %>% dplyr::select("Volume")

for (trace in traces) {
  elu_data <- dplyr::full_join(elu_data, trace, by = "Volume")
}

elu_data <- elu_data %>%
  tidyr::gather(dplyr::starts_with("RI"), key = "sample_RI", value = `RI Signal`, na.rm = T) %>%
  tidyr::gather(dplyr::starts_with("molar"), key = "sample_mm", value = `molar mass`, na.rm = T) %>%
  tidyr::gather(dplyr::starts_with("integral"), key = "sample_int", value = `integral`, na.rm = T) %>%
  tidyr::gather(dplyr::starts_with("duration"), key = "sample_dur", value = `duration`, na.rm = T) %>%
  dplyr::select(-sample_RI, -sample_mm, -sample_int, -sample_dur)
#  dplyr::arrange(desc(Volume, duration))

ggplot2::ggplot(elu_data) +
  ggplot2::geom_point(ggplot2::aes(Volume, `RI Signal`, color = duration), na.rm = T) +
  ggplot2::theme_bw()



# data<- data %>% mutate(Reaktionszeit = factor(mesurement)) %>%
#   mutate(Reaktionszeit = forcats::fct_recode(Reaktionszeit,"3 Stunden"="RI Signal.x",
#     "5 Stunden"="RI Signal.y",
#     "22 Stunden 50 Minuten"="RI Signal.x.x",
#     "1 Stunde"="RI Signal.y.y")) %>%
#   mutate(Reaktionszeit = factor(Reaktionszeit, levels = c("1 Stunde",
#     "3 Stunden",
#     "5 Stunden",
#     "22 Stunden 50 Minuten"))) %>%
#   select(-mesurement)
#
