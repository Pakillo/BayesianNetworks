#' Make Stan code
#'
#' @returns A string containing a Stan model.
#' @keywords internal
#' @export
#'
#' @details
#' This function is designed to be used internally. See `fit_model` for documentation
#' of arguments.
#'
stancode <- function(plant_effort = c("param", "data"),
                     animal_pref = c("fixed", "varying"),
                     plant_abun = c("estimate", "relative", "absolute"),
                     animal_abun = c("estimate", "relative", "absolute")) {
  # validate arguments
  plant_effort <- match.arg(plant_effort)
  animal_pref <- match.arg(animal_pref)
  plant_abun <- match.arg(plant_abun)
  animal_abun <- match.arg(animal_abun)

  # generate data block
  scode_data <- paste0(
    "data {\n",
    "  // Dimensions of the data matrix, and matrix itself.\n",
    "  int<lower=1> n_p;\n",
    "  int<lower=1> n_a;\n",
    "  array[n_p, n_a] int<lower=0> M;\n",
    switch(plant_effort,
           param = "",
           data = "  vector<lower=0> [n_p] C;\n"),
    "  real<lower=0> beta;\n",
    switch(plant_abun,
           estimate = "  real<lower=0> alpha_sigma;\n",
           relative = "  simplex[n_p] sigma;\n",
           absolute = "  vector<lower=0> sigma;\n"),
    switch(animal_abun,
           estimate = "  real<lower=0> alpha_tau;\n",
           relative = "  simplex[n_a] tau;\n",
           absolute = "  vector<lower=0> tau;\n"),
    "}"
  )

  # generate transformed data block
  scode_tdata <- paste0(
    "transformed data {\n",
    ifelse(plant_abun == "estimate" | animal_abun == "estimate",
           "// Prior parameters for abundances\n", ""),
    switch(plant_abun,
           estimate = "  vector<lower=0>[n_p] a_sigma = rep_vector(alpha_sigma, n_p);\n",
           relative = "",
           absolute = ""),
    switch(animal_abun,
           estimate = "  vector<lower=0>[n_a] a_tau = rep_vector(alpha_tau, n_a);\n",
           relative = "",
           absolute = ""),
    "// Pre-compute the marginals of M to save computation in the model loop.\n",
    "  array[n_p] int M_rows = rep_array(0, n_p);\n",
    "  array[n_a] int M_cols = rep_array(0, n_a);\n",
    "  int M_tot = 0;\n",
    "  for (i in 1:n_p) {\n",
    "    for (j in 1:n_a) {\n",
    "      M_rows[i] += M[i, j];\n",
    "      M_cols[j] += M[i, j];\n",
    "      M_tot += M[i, j];\n",
    "    }\n",
    "  }\n",
    "}"
  )

  # generate parameter block
  scode_param <- paste0(
    "parameters {\n",
    switch(plant_effort,
           param = "  real<lower=0> C;\n",
           data = ""),
    switch(animal_pref,
           fixed = "  real<lower=0> r;\n",
           varying = "  vector<lower=0> [n_a] r;\n"),
    switch(plant_abun,
           estimate = "  simplex[n_p] sigma;\n",
           relative = "",
           absolute = ""),
    switch(animal_abun,
           estimate = "  simplex[n_a] tau;\n",
           relative = "",
           absolute = ""),
    "  real<lower=0, upper=1> rho;\n",
    "}"
  )

  # generate model block
  scode_model <- paste0(
    "model {\n",
    "  // Priors\n",
    switch(animal_pref,
           fixed = "  r ~ exponential(beta);\n",
           varying = paste0("  for (j in 1:n_a) {\n",
                            "    r[j] ~ exponential(beta);\n",
                            "  }\n")),
    switch(plant_abun,
           estimate = "  sigma ~ dirichlet(a_sigma);\n",
           relative = "",
           absolute = ""),
    switch(animal_abun,
           estimate = "  tau ~ dirichlet(a_tau);\n",
           relative = "",
           absolute = ""),
    "  // Global sums and parameters\n",
    "  target += M_tot * log(C) - C;\n",
    "  // Weighted marginals of the data matrix\n",
    paste0("  for (i in 1:n_p) {\n",
           "    target += M_rows[i] * log(sigma[i]);\n",
           "  }\n",
           "  for (j in 1:n_a) {\n",
           "    target += M_cols[j] * log(tau[j]);\n",
           "  }\n"),
    "  // Pairwise loop\n",
    paste0("  for (i in 1:n_p) {\n",
           "    for (j in 1:n_a) {\n",
           "      real nu_ij_0 = log(1 - rho);\n"),
    paste0("      real nu_ij_1 = log(rho) + M[i,j] * log(1 + ",
           switch(animal_pref,
                  fixed = "r",
                  varying = "r[j]"),
           ") - ",
           switch(plant_effort,
                  param = "C",
                  data = "C[i]"),
           " * ",
           switch(animal_pref,
                  fixed = "r",
                  varying = "r[j]"),
           " * ",
           "sigma[i] * tau[j];\n"),
    "      if (nu_ij_0 > nu_ij_1)\n",
    "        target += nu_ij_0 + log1p_exp(nu_ij_1 - nu_ij_0);\n",
    "      else\n",
    "        target += nu_ij_1 + log1p_exp(nu_ij_0 - nu_ij_1);\n",
    "    }\n",
    "  }\n",
    "}"
  )

  # generate generated quantities block
  scode_gquant <- paste0(
    "generated quantities {\n",
    "  // Posterior edge probability matrix\n",
    "  array[n_p, n_a] real<lower=0> Q;\n",
    "  for (i in 1:n_p) {\n",
    "    for (j in 1:n_a) {\n",
    "      real nu_ij_0 = log(1 - rho);\n",
    paste0("      real nu_ij_1 = log(rho) + M[i,j] * log(1 + ",
           switch(animal_pref,
                  fixed = "r",
                  varying = "r[j]"),
           ") - ",
           switch(plant_effort,
                  param = "C",
                  data = "C[i]"),
           " * ",
           switch(animal_pref,
                  fixed = "r",
                  varying = "r[j]"),
           " * sigma[i] * tau[j];\n"),
    "      if (nu_ij_1 > 0)\n",
    "        Q[i, j] = 1 / (1 + exp(nu_ij_0 - nu_ij_1));\n",
    "      else\n",
    "        Q[i, j] = exp(nu_ij_1) / (exp(nu_ij_0) + exp(nu_ij_1));\n",
    "    }\n",
    "  }\n",
    "}"
  )

  # make code
  scode <- paste(scode_data, scode_tdata, scode_param, scode_model, scode_gquant,
                 sep = "\n")
  return(scode)
}
