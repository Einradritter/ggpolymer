## the goal here was to reduce duplicate code by vectorizing the calculations... but to much effort for now.

# calc_weights_2 <- function(db, mass_product, extend = 1,
#                            monomere = "MMA", initiator = "EBriB", catalyst = "CuBr", ligand = "PMDETA",
#                            ratio_m_i = 200, ratio_cat_i = 1.5, ratio_lig_i = 3.0) {
#
#   subs <- c(initiator, monomere, catalyst, ligand)
#   M <- purrr::map(subs, get, db = db, property = "molar_mass")
#
#   # calculate amount of substance in mole
#   M_i <- M[[1]]
#   M_m <- M[[2]]
#   M_poly <- ratio_m_i * M_m * extend + M_i
#   n_i <- mass_product/M_poly
#   n_m <- ratio_m_i * n_i
#   n_cat <- ratio_cat_i * n_i
#   n_lig <- ratio_lig_i * n_i
#   n <- c(n_i, n_m, n_cat, n_lig)
#
#   return(tidyr::unnest(tibble::tibble(subs, M))+tibble::add_column(n))
# }
