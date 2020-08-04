get_total_catch<- function(datfile, yrs = 101:120) {
   dat <- r4ss::SS_readdat(datfile, verbose = FALSE)
   catch_sum <- sum(dat$catch[dat$catch$year %in% yrs, "catch"])
}

get_avg_catch<- function(datfile, yrs = 101:120) {
  dat <- r4ss::SS_readdat(datfile, verbose = FALSE)
  catch_avg <- mean(dat$catch[dat$catch$year %in% yrs, "catch"])
}

get_catch_sd <- function(datfile, yrs = 101:120) {
  dat <- r4ss::SS_readdat(datfile, verbose = FALSE)
  catch_var <- sd(dat$catch[dat$catch$year %in% yrs, "catch"])
}
get_SSB_yr <- function(summary, yr) {
  SSB_yr <- summary$ts %>% 
          filter(year == yr) %>% 
          filter(model_run == "cod_OM") %>% 
          select(iteration, SpawnBio)
  SSB_yr
}

check_convergence <- function(summary, min_yr = 101, max_yr = 120, n_EMs = 5 ) {
  require(dplyr) # note: not the best way to do this
  if(any(!is.na(summary$scalar$params_on_bound))) warning("Params on bounds")
  calc_SSB <- summary$ts %>%
    filter(year > 100 & year <= 120) %>% 
    select(iteration, scenario, year, model_run, SpawnBio) %>% 
    tidyr::spread(key = model_run, value = SpawnBio) %>% 
    tidyr::gather(key = "model_run_EM", value = "SSB_EM",
                  seq(4, length.out = n_EMs)) %>% 
    mutate(SSB_ratio = SSB_EM/cod_OM)
  filter_SSB <- calc_SSB %>% 
    filter(SSB_ratio > 2 | SSB_ratio < 0.5)
  if(nrow(filter_SSB) > 0 ) warning("Some large/small SSBs relative to OM")
  return_val <- calc_SSB
}
