# load pkgs set options ----
#devtools::install_github("r4ss/r4ss", ref = "15444de")
# devtools::install_github("nmfs-fish-tools/SSMSE", ref = "404023f")
library(SSMSE)
library(r4ss)
library(furrr) # to run in parallel
source("code/get_metrics.R") # not used yet

# specify locations, create folders ----
cod_mod_path <- system.file("extdata", "models", "cod", package = "SSMSE")
datfile_path <- file.path(cod_mod_path, "ss3.dat")
fig_path <- "figures"
runs_path <- "model_runs"
dir.create(fig_path)
dir.create(runs_path)

# get the sampling scheme ----
sample_struct <- SSMSE::create_sample_struct(dat = datfile_path, nyrs = 20)
# modify
sample_struct$lencomp <- data.frame(Yr = seq(105, 120, by = 5), 
                                    Seas = sample_struct$lencomp$Seas,
                                    FltSvy = sample_struct$lencomp$FltSvy, 
                                    Sex = sample_struct$lencomp$Sex, 
                                    Part = sample_struct$lencomp$Part,
                                    Nsamp = sample_struct$lencomp$Nsamp)
# outline scenarios to run ---
scenarios <- data.frame(
  scen_name = paste0("rand-recdevs-", 1:5),
  rec_devs = "rand"
)

# function to call SSSMSE
call_SSMSE <- function(scen_name, rec_dev, sample_struct) {
  out <- SSMSE::run_SSMSE(scen_name_vec = scen_name, # name of the scenario
           out_dir_scen_vec = "model_runs", # directory in which to run the scenario
           iter_vec = 20, # run with 5 iterations each
           OM_name_vec = "cod",
           EM_name_vec = "cod",
           MS_vec = "EM",       # The management strategy is specified in the EM
           use_SS_boot_vec = TRUE, # use the SS bootstrap module for sampling
           nyrs_vec = 20,        # Years to project OM forward
           nyrs_assess_vec = 5, # Years between assessments
           rec_dev_pattern = rec_dev, # Don't use recruitment deviations
           impl_error_pattern = c("none"), # Don't use implementation error
           sample_struct_list = list(sample_struct), # How to sample data for running the EM.
           seed = 12345) #Set a fixed integer seed that allows replication 
  out
}
future::plan(multiprocess) # run in serial b/c just 1 scenario
# Run SSMSE useing the variables in the data fram.e
output <- furrr::future_pmap(scenarios, ~call_SSMSE(scen_name = .x, rec_dev = .y, sample_struct = sample_struct))

summary <- SSMSE::SSMSE_summary_all(dir = "model_runs", scenarios = paste0("rand-recdevs-", 1:5))
SSB_df <- check_convergence(summary)

summary$ts <- tidyr::separate(summary$ts,
                              col = model_run,
                              into = c(NA, "model_type"),
                              remove = FALSE,
                              sep = "_",
                              extra = "drop")
ggplot2::ggplot(data = subset(summary$ts, model_run %in% c("cod_OM", "cod_EM_120")),
                ggplot2::aes(x = year, y = SpawnBio)) +
  ggplot2::geom_vline(xintercept = 100, color = "gray") +
  ggplot2::geom_line(ggplot2::aes(linetype = as.character(iteration), color = model_type))+
  ggplot2::scale_color_manual(values = c("#D65F00", "black")) +
  ggplot2::scale_linetype_manual(values = rep("solid", 100)) +
  ggplot2::guides(linetype = FALSE) +
  #ggplot2::facet_wrap(. ~ scenario) +
  ggplot2::theme_classic()

# calculate performance metrics ----
# look at catch in OM from yrs 101:120.
# Keep it simple with total catch, avgcatch, sdcatch, and SSB in yr 120
OM_dat <- file.path("model_runs", "self-test",as.character(1:100), "cod_OM", "ss3.dat")
tot_catch <- lapply(OM_dat, function(x) get_total_catch(x))
avg_catch <- lapply(OM_dat, function(x) get_avg_catch(x))
catch_sd <- lapply(OM_dat, function(x) get_catch_sd(x))
SSB_120 <- get_SSB_yr(summary, yr = 120)


