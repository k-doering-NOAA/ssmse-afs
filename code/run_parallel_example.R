# load pkgs set options ----
devtools::install_github("r4ss/r4ss", ref = "15444de")
devtools::install_github("nmfs-fish-tools/SSMSE", ref = "404023f")
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
  scen_name = c("no_recdevs", "rand_recdevs"),
  rec_devs = c("none", "rand")
)

# function to call SSSMSE
call_SSMSE <- function(scen_name, rec_dev, sample_struct) {
  out <- SSMSE::run_SSMSE(scen_name_vec = scen_name, # name of the scenario
           out_dir_scen_vec = "model_runs", # directory in which to run the scenario
           iter_vec = 1, # run with 5 iterations each
           OM_name_vec = "cod",
           EM_name_vec = "cod",
           MS_vec = "EM",       # The management strategy is specified in the EM
           use_SS_boot_vec = TRUE, # use the SS bootstrap module for sampling
           nyrs_vec = 5,        # Years to project OM forward
           nyrs_assess_vec = 5, # Years between assessments
           rec_dev_pattern = rec_dev, # Don't use recruitment deviations
           impl_error_pattern = c("none"), # Don't use implementation error
           sample_struct_list = list(sample_struct), # How to sample data for running the EM.
           seed = 12345) #Set a fixed integer seed that allows replication 
  out
}
future::plan(multiprocess) # run in parallel
# Run SSMSE useing the variables in the data fram.e
output <- furrr::future_pmap(scenarios, ~call_SSMSE(scen_name = .x, rec_dev = .y, sample_struct = sample_struct))

output[[1]][[1]]$rec_devs # verify recdevs are 0
output[[2]][[1]]$rec_devs # verify recdevs are not 0
