# load pkgs set options ----
devtools::install_github("r4ss/r4ss", ref = "15444de")
devtools::install_github("nmfs-fish-tools/SSMSE", ref = "a5089d8")
library(SSMSE)
library(r4ss)
source("code/get_metrics.R")

# specify locations, create folders ----
model_path <- file.path("input_models")
cod_mod_path <- system.file("extdata", "models", "cod", package = "SSMSE")
datfile_path <- file.path(cod_mod_path, "ss3.dat")
fig_path <- "figures"
runs_path <- "model_runs"
dir.create(fig_path)
dir.create(runs_path)
dir.create(model_path)
# change cod model forecasting?? ----
file.copy(from = cod_mod_path, to = model_path, recursive = TRUE)
fore <- r4ss::SS_readforecast(file.path(cod_mod_path, "forecast.ss"), verbose = FALSE)
fore$BforconstantF <- 0.4
fore$BfornoF <- 0.10  # make similar to west coast groundfish
r4ss::SS_writeforecast(fore, dir = file.path(model_path, "cod"), 
                       overwrite = TRUE, verbose = FALSE)
# run model with no estimation to make consistent
SSMSE:::run_ss_model(dir = file.path(model_path, "cod"), 
                     admb_options = "-maxfn 0 -phase 50 -nohess",
                     verbose = FALSE)
# get the sampling scheme ----
sample_struct <- create_sample_struct(dat = datfile_path, nyrs = 20)
# modify
sample_struct$lencomp <- data.frame(Yr = seq(105, 120, by = 5), 
                                    Seas = sample_struct$lencomp$Seas,
                                    FltSvy = sample_struct$lencomp$FltSvy, 
                                    Sex = sample_struct$lencomp$Sex, 
                                    Part = sample_struct$lencomp$Part,
                                    Nsamp = sample_struct$lencomp$Nsamp)
# self test the cod model, no rec devs ----
# run 1 iteration and 1 scenario of SSMSE using an EM.
sampling_df <- run_SSMSE(scen_name_vec = c("self-test"), # name of the scenario
                out_dir_scen_vec = runs_path, # directory in which to run the scenario
                iter_vec = 100, # run with 5 iterations each
                OM_in_dir_vec = file.path(model_path, "cod"),# specify directories instead
                EM_in_dir_vec = file.path(model_path, "cod"), # cod is included in package data
                MS_vec = "EM",       # The management strategy is specified in the EM
                use_SS_boot_vec = TRUE, # use the SS bootstrap module for sampling
                nyrs_vec = 20,        # Years to project OM forward
                scope = 2,
                nyrs_assess_vec = 5, # Years between assessments
                rec_dev_pattern = c("none"), # Don't use recruitment deviations
                impl_error_pattern = c("none"), # Don't use implementation error
                sample_struct_list = list(sample_struct), # How to sample data for running the EM.
                seed = 12345) #Set a fixed integer seed that allows replication 

# examine results ----
summary <- SSMSE_summary_all(runs_path)
# convergence: make sure no params on bounds, and no SSB much greater than the OM.
SSB_df <- check_convergence(summary)
# If no really lg or really small SSB, and no params on bounds, consider the model converged.

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
