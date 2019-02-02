# read_gpc <- function(dir) {
#   readr::read_table2("C:/Users/hkw/uni/Masterarbeit/data/GPC/hw31-PDMAEMA-kinetik/hw31-0.TXT", col_names = FALSE)
# }

read_gpc_raw <- function(file_dir) {
  file<- readr::read_file(file_dir)
  rawstart<- stringr::str_locate(file, "RAWstart")[2]+5
  rawstop<- stringr::str_locate(file, "RAWstop")[1]-3
  data <- file %>% stringr::str_sub(rawstart, rawstop) %>%
    readr::read_delim(delim = "\t", skip = 1, trim_ws = T,
      col_names = c("A","B","C","D"),
      col_types = readr::cols( A = readr::col_double(),
        B = readr::col_double(),
        C = readr::col_double(),
        D = readr::col_double())) %>%
    dplyr::select(-D) %>% tidyr::separate(C, into = c("time", "ms"), sep = 8) %>% dplyr::select(-"ms") %>%
    readr::type_convert(col_types = readr::cols(time = readr::col_time(format = "%H:%M:%S")))
  return(data)
}

read_gpc_elu <- function(file_dir) {
  file<- readr::read_file(file_dir)
  elu_start<- stringr::str_locate(file, "ELUstart")[2]+5
  elu_stop<- stringr::str_locate(file, "ELUstop")[1]-3
  data <- file %>% stringr::str_sub(elu_start, elu_stop) %>%
    readr::read_delim(delim = "\t", skip = 1, trim_ws = T,
      col_names = c( "Volume", "molar mass","RI Signal", "integral","empty"),
      col_types = readr::cols( "Volume" = readr::col_double(),
        "molar mass" = readr::col_double(),
        "RI Signal" = readr::col_double(),
        "integral" = readr::col_double(),
        "empty" = readr::col_double())) %>%
    dplyr::select(-"empty")
  return(data)
}


read_gpc_mwd <- function(file_dir) {
  file<- readr::read_file(file_dir)
  elu_start<- stringr::str_locate(file, "MWDstart")[2]+5
  elu_stop<- stringr::str_locate(file, "MWDstop")[1]-3
  data <- file %>% stringr::str_sub(elu_start, elu_stop) %>%
    readr::read_delim(delim = "\t", skip = 1, trim_ws = T,
      col_names = c( "molar mass", "RI Signal","Integral", "empty"),
      col_types = readr::cols( "molar mass" = readr::col_double(),
        "RI Signal" = readr::col_double(),
        "Integral" = readr::col_double(),
        "empty" = readr::col_character())) %>%
    dplyr::select(-"empty")
  return(data)
}

