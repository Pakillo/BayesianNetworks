# Generate all combinations of model constructions
g <- expand.grid(plant_effort = c("param", "data"),
                 animal_pref = c("fixed", "varying"),
                 plant_abun = c("param", "data"),
                 animal_abun = c("param", "data"), stringsAsFactors = FALSE)

mod_names <- apply(g, 1, function(x) {
  paste0(substr(as.character(x), 1, 1), collapse = "")
})
for (i in 1:nrow(g)) {
  sc <- BayesianWebs:::stancode(plant_effort = g[i, 1],
                                animal_pref = g[i, 2],
                                plant_abun = g[i, 3],
                                animal_abun = g[i, 4])
  cmdstanr::write_stan_file(sc, dir = tempdir(), basename = mod_names[i])
}
spaths <- paste0(tempdir(), "/", mod_names, ".stan") |>
  `names<-`(mod_names)
smods <- lapply(spaths, cmdstanr::cmdstan_model, compile = FALSE,
                USE.NAMES = TRUE)
synt <- sapply(smods,
               FUN = function(x) utils::capture.output(x$check_syntax(), type = "message"))
