
#' Calculate the degree of polymerisation of the blocks of a DBCP-surfactant
#'
#' @param M_tot A numeric. The total molecular weigth of the surfactant in
#'   g/mol.
#' @param hlb A numeric between 0 and 1. The hydrophilic balance, or more exact
#'   the proportion of the molecular weight of the water soluble block on the
#'   total molecular weight of the surfactant.
#' @param M_sol_mono A numeric. The molecular weight of the monomer forming the
#'   water soluble block in g/mol.
#' @param M_insol_mono A numeric. The molecular weight of the monomer forming
#'   the water insuble block in g/mol.
#' @export
DBCP_sur_DPs <- function(M_tot, hlb, M_sol_mono, M_insol_mono) {
  n <- M_tot*hlb/M_sol_mono
  m <- (M_tot - n * M_sol_mono) / M_insol_mono
  return(c(n, m))
}

#' Calculate the weights of an ATRP reaction
#' @param db A tibble. A chemical database with e.g. molecular weights. Usually  ---- insert DB ---.
#' @param mass_product A numeric. The desired mass in g.
#' @param conversation A numeric.
#' @param monomere A string.
#' @param initiator A string.
#' @param catalyst A string.
#' @param ligand A string.
#' @param ratio_m_i A numeric.
#' @param ratio_cat_i A numeric.
#' @param ratio_lig_i A numeric.
#' @export
calc_weights <- function(db, mass_product = 1, conversation = 1,
  monomere = "MMA", initiator = "EBriB", catalyst = "CuBr", ligand = "PMDETA",
  ratio_m_i = 200, ratio_cat_i = 1.5, ratio_lig_i = 3.0) {

  # fetch molar masses in g/mol from db
  M_i <- fetch(db, "molar_mass", initiator) %>% as.numeric()
  M_m <- fetch(db, "molar_mass", monomere) %>% as.numeric()
  M_lig <- fetch(db, "molar_mass", ligand) %>% as.numeric()
  M_cat <- fetch(db, "molar_mass", catalyst) %>% as.numeric()

  # calculate amount of substance in mole
  M_poly <- ratio_m_i * M_m * conversation + M_i
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



clean_up_db <- function(db) {
  index <- purrr::map_lgl(db, ~ all(is.na(.)))
  return(db[, !index])
}

calc_reaction_time <- function(M_poly, M_mono, M_ini, k_app, M_I) {
  t <- -1 / k_app * log( ( -1 * ( (M_poly - M_ini) / M_mono ) /M_I ) + 1)
  return(t)
}
