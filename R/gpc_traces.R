# ### todo: a function to build a tidy dataset from a list of gpc ascii reports
#
# ##source("function_definitions/functions_data_import_gpc.R")
# ##source("function_definitions/functions_project.R")
#
# #fetch_dirs_in_folder <- function (dir) {
# #return(all dirs in folder)
# #}
#
# ### get mit list.files(dir), besser wäre aber eine lesemethode, die nur bestimmte datentypen akzeptiert?
#
#
# #combine_gpc_datasets <- function( list(dir1, dir2, dir3 ...)) {
# #return(a tibble, ready to plot)
# #}
#
# #add_reaction_durations <- function (a tibble, list(duration1, duration2, ...)) {
# #return (a tibble with added duration times)
# #}
#
# # plot_gpc_traces <- function (a tibble from combin_gpc_datasets with or without added reaction durations) {
# #return( a simple ggplot)
# #}
#
# ####funktioniert garnicht! bisher
#
#
# file_list <- list.files("../../data/GPC/hw31-PDMAEMA-kinetik/ascii/", full.names = T)
#
# traces <- purrr::map(file_list, read_gpc_elu)
#
# duration <- c(1, 2, 3, 4, 5, 6)
# for (trace_nr in 1:length(traces)) {
#    print(trace_nr)
#    trace <- traces[[trace_nr]]
#    traces[[trace_nr]]<- trace %>% tibble::add_column(duration = rep(duration[trace_nr], dim(trace)[1]))
# }
#
# traces[[6]]
#
#
# test <- traces %>% purrr::reduce(rbind)
#
# test %>% dplyr::distinct()
# ####
# ####
# test2 <- test %>% tidyr::unnest()
# ####
# ####
# plot(test$Volume, test$`RI Signal`, type = "l")
#
#
# test %>% dplyr::arrange(desc("duration"))
#
# test %>% ggplot2::ggplot() +
#   ggplot2::geom_line(ggplot2::aes(Volume, `RI Signal`, color = duration))
#
# # for (trace_nr in 0:length(traces)) {
# #   print(trace_nr)
# #   traces[[trace_nr]] %>% tibble::add_column()
# # }
# # time = rep(times[trace_nr]), dim(traces[[trace_nr]][1])
# #
# # #  traces[[trace_nr]] %>% tibble::add_column(time = rep(times[trace_nr]), dim(traces[[trace_nr]][1]))}
# #
# # #traces[[1]] %>% tibble::add_column(time = rep("30 min", dim(traces[[1]])[1]))
# #
#
# elu_data <- traces[[1]] #%>% dplyr::select("volume")
#
#
# for (trace in traces) {
#   elu_data <- dplyr::full_join(elu_data, trace, by = "Volume")
# }
#
# elu_data <- elu_data %>%
#   tidyr::gather(dplyr::starts_with("RI"), key = "sample", value = `RI Signal`, na.rm = T) %>%
#   tidyr::gather(dplyr::starts_with("molar"), key = "sample_mm", value = `molar mass`, na.rm = T) %>%
#   tidyr::gather(dplyr::starts_with("integral"), key = "sample_int", value = `integral`, na.rm = T) %>%
#   dplyr::select(-sample_mm, -sample_int)
#
# ggplot2::ggplot(elu_data) +
#   ggplot2::geom_path(ggplot2::aes(Volume, `RI Signal`, color = sample)) +
#   theme_bw()
#
# # # join all data tables
# # gpc_data_list <- list(HW07B,HW07C,HW07D,HW07E)
# #joined_data <- gpc_data_list[[1]] %>% select("Volume") # initialize Volume
# #
# # for (dataset in gpc_data_list) {
# #   joined_data <- dataset %>% dplyr::full_join (joined_data, by = "Volume")
# # }
# #
# # # tidy up!
# # data<- joined_data %>%
# #   tidyr::gather(dplyr::starts_with("RI"), key = "mesurement", value = `RI Signal`, na.rm = T) %>%
# #   tidyr::gather(dplyr::starts_with("molar"), key = "mesurement2", value = `molar mass`, na.rm = T) %>%
# #   select(-mesurement2)
# #
# # # turn mesurement into ordered factor
# # data<- data %>% mutate(Reaktionszeit = factor(mesurement)) %>%
# #   mutate(Reaktionszeit = forcats::fct_recode(Reaktionszeit,"3 Stunden"="RI Signal.x",
# #     "5 Stunden"="RI Signal.y",
# #     "22 Stunden 50 Minuten"="RI Signal.x.x",
# #     "1 Stunde"="RI Signal.y.y")) %>%
# #   mutate(Reaktionszeit = factor(Reaktionszeit, levels = c("1 Stunde",
# #     "3 Stunden",
# #     "5 Stunden",
# #     "22 Stunden 50 Minuten"))) %>%
# #   select(-mesurement)
# #
# # #plot
# # gpc_traces<- ggplot(data) +
# #   geom_point(mapping = aes(x = `Volume`, y = `RI Signal`, color = `Reaktionszeit`)) +
# #   labs(x="Retentionszeit [min]", color = "Polymerisationszeit") +
# #   theme_bw()
# #
# # molar_mass_vs_volume<- ggplot(data) +
# #   geom_point(mapping = aes(x = `Volume`, y = `molar mass`, color = `Reaktionszeit`)) +
# #   labs(x="Retentionszeit [min]", color = "Molare Masse [g/mol]") +
# #   theme_bw()
# #
# #
# # ### export plot
# # setwd("C:/Users/h/Uni/Analytik-Praktikum/Praktikumsprojekt")
# # #export_project_plot(filename = "gpc_traces_HW07.jpeg", plot = gpc_traces)
# #
# # ### find 2 maxima with interactivity
# # library(plotly)
# # ggplotly(gpc_traces)
# # ggplotly(molar_mass_vs_volume)
