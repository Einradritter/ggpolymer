## database imports

db_substances <- readxl::read_xlsx("data/db_substance.xlsx")
#db_atrp <- readxl::read_xlsx("/data/db_atrp.xlsx")
#db_gpc <- readxl::read_xlsx("/data/gpc_db.xlsx")
#db_nmr <- readxl::read_xlsx("/data/nmr_db.xlsx")
#db_results <- readxl::read_xlsx("/data/db_analytic_results.xlsx")

#temp1 <- db_atrp %>% dplyr::left_join(db_gpc, by = "reaction_id")
#temp2 <- db_atrp %>% dplyr::left_join(db_nmr, by = "reaction_id")
#db_complete <- temp2 %>% dplyr::full_join(temp1)

#rm(temp1)
#rm(temp2)


#nmr_db <-readxl::read_Xlsx("../../data/nmr_db.xlsx")

#file.exists("C:/Users/hkw/.ssh/id_rsa")
