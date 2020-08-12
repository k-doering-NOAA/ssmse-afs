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
get_SSB_avg <- function(summary, min_yr = 175, max_yr = 200) {
  OM_vals <- unique(summary$ts$model_run)
  OM_vals <- grep("_OM$", OM_vals, value = TRUE)
  SSB_yr <- summary$ts %>% 
          filter(year >= min_yr & year <= max_yr) %>% 
          filter(model_run %in% OM_vals) %>% 
          select(iteration, scenario, year, SpawnBio) %>% 
          group_by(iteration, scenario) %>% 
          summarize(avg_SSB = mean(SpawnBio), .groups = "keep") %>% 
          ungroup()
  SSB_yr
}


# need to make general
check_convergence <- function(summary, min_yr = 101, max_yr = 120, n_EMs = 5) {
  require(dplyr) # note: not the best way to do this
  if(any(!is.na(summary$scalar$params_on_bound))) {
    warning("Params on bounds")
  } else {
    message("No params on bounds")
  }
  summary$ts$model_type <- ifelse(grepl("_EM_", summary$ts$model_run), "EM", "OM")
  calc_SSB <- summary$ts %>%
    filter(year >= min_yr & year <= max_yr) %>% 
    select(iteration, scenario, year, model_run, model_type, SpawnBio)
  OM_vals <- calc_SSB %>% 
              filter(model_type == "OM") %>% 
              rename(SpawnBio_OM = SpawnBio ) %>% 
              select(iteration, scenario, year, SpawnBio_OM)
  EM_vals <- calc_SSB %>% 
               filter(model_type == "EM") %>% 
               rename(SpawnBio_EM = SpawnBio) %>% 
               select(iteration, scenario, year, model_run, SpawnBio_EM)
  bind_vals <- full_join(EM_vals, OM_vals, by = c("iteration", "scenario", "year")) %>% 
                  mutate(SSB_ratio = SpawnBio_EM/SpawnBio_OM)
  filter_SSB <- bind_vals %>% 
    filter(SSB_ratio > 2 | SSB_ratio < 0.5)
  if(nrow(filter_SSB) > 0 ) {
    warning("Some large/small SSBs relative to OM")
  } else {
    message("All SSBs in EM are no greater than double and no less than half SSB vals in the OM")
  }
  return_val <- bind_vals
}
