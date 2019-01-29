# extraction functions

fetch <- function(db, property, chemical) {
  db %>%
    dplyr::filter(substance == chemical) %>%
    dplyr::select(property)
}

# calculating the volume of a substance

calc_V <- function(db, chemical, m) {
  if (db %>% fetch("ag_state", chemical) == "liquid") {
    dens <- db %>% fetch("density", chemical)
    return(as.numeric(m / dens))
  } else {
    return(NA)
  }
}

# calculating the weights of an atrp reaction

calc_weights <- function(db, mass_product = 1, extend = 1,
                         monomere = "MMA", initiator = "EBriB", catalyst = "CuBr", ligand = "PMDETA",
                         ratio_m_i = 200, ratio_cat_i = 1.5, ratio_lig_i = 3.0) {

  # fetch molar masses in g/mol from db
  M_i <- fetch(db, "molar_mass", initiator) %>% as.numeric()
  M_m <- fetch(db, "molar_mass", monomere) %>% as.numeric()
  M_lig <- fetch(db, "molar_mass", ligand) %>% as.numeric()
  M_cat <- fetch(db, "molar_mass", catalyst) %>% as.numeric()

  # calculate amount of substance in mole
  M_poly <- ratio_m_i * M_m * extend + M_i
  n_i <- mass_product/M_poly
  n_m <- ratio_m_i * n_i
  n_cat <- ratio_cat_i * n_i
  n_lig <- ratio_lig_i * n_i
  n_return <- c(n_i, n_m, n_cat, n_lig)

  # calculate masses in g
  m_i <- n_i * M_i
  m_m <- n_m * M_m
  m_cat <- n_cat * M_cat
  m_lig <- n_lig * M_lig
  m_return <- c(m_i, m_m, m_cat, m_lig)

  # calculate Volumes in mL
  v_i <- db %>% calc_V(chemical = initiator, m_i)
  v_m <- db %>% calc_V(chemical = monomere, m_m)
  v_cat <- db %>% calc_V(chemical = catalyst, m_cat)
  v_lig <- db %>% calc_V(chemical = ligand, m_lig)
  v_return <- c(v_i, v_m, v_cat, v_lig)

  substance_return <- c(initiator, monomere, catalyst, ligand)
  return(tibble::tibble(substance_return, n_return, m_return, v_return))
}

clean_up_db <- function(db) {
  index <- purrr::map_lgl(db, ~ all(is.na(.)))
  return(db[, !index])
}

calc_reaction_time <- function(M_poly, M_mono, M_ini, k_app, M_I) {
  t <- -1 / k_app * log( ( -1 * ( (M_poly - M_ini) / M_mono ) /M_I ) + 1)
  return(t)
}
