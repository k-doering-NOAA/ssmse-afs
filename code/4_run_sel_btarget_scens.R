# Run scenarios relating to q 

# load pkgs set options ----
#devtools::install_github("r4ss/r4ss", ref = "15444de")
# devtools::install_github("nmfs-fish-tools/SSMSE", ref = "404023f")
library(SSMSE)
library(r4ss)
library(dplyr)
library(furrr) # to run in parallel
source("code/get_metrics.R") # not used yet

# specify locations, create folders ----
cod_mod_path <- system.file("extdata", "models", "cod", package = "SSMSE")
datfile_path <- file.path(cod_mod_path, "ss3.dat")
fig_path <- "figures"
runs_path <- "model_runs"
mods_path <- "input_models"
dir.create(fig_path)
dir.create(runs_path)
dir.create(mods_path)

# define the scenarios ----
scen_vals <- c(4.5, 5, 5.5) 
names(scen_vals) <-   c("sel-low", "sel-med", "sel-hi")
HCR_vals <- c(30, 40)
names(HCR_vals)  <- c("HCR-30-10", "HCR-40-10")
scenarios <- data.frame(
  scen_name = c(paste0(names(scen_vals), "-", names(HCR_vals)[1]),
                paste0(names(scen_vals), "-", names(HCR_vals)[2])),
  OM_path = rep(c(file.path(mods_path, names(scen_vals))),times = 2),
  EM_path = rep(c(file.path(mods_path, names(HCR_vals))), times = c(3,3))
)
# manipulate the OM ctl file---- 


cod_mod_path <- system.file("extdata", "models", "cod", package = "SSMSE")
# copy to a new location:
for (i in scen_vals) {
  tmp_scen_name <- names(scen_vals)[scen_vals == i]
  tmp_cod_path <- file.path(mods_path, tmp_scen_name)
  file.copy(from = cod_mod_path, to = mods_path, recursive = TRUE)
  file.rename(from = file.path(mods_path, "cod"), to = tmp_cod_path)
  # make model read initial values from control file and not ss.par
  start <- r4ss::SS_readstarter(file = file.path(tmp_cod_path, "starter.ss"), verbose = FALSE)
  start$init_values_src # verify reading from the control file
  r4ss::SS_changepars(dir = tmp_cod_path, ctlfile = "control.ss_new",
                      newctlfile = "control_modified.ss", 
                      strings = "Size_DblN_ascend_se_Fishery(1)", newvals = i)
  file.remove(file.path(tmp_cod_path, "control.ss_new"))
  file.remove(file.path(tmp_cod_path, "control.ss"))
  file.remove(file.path(tmp_cod_path, "ss.par"))
  file.rename(from = file.path(tmp_cod_path, "control_modified.ss"),
              to = file.path(tmp_cod_path, "control.ss"))
  SSMSE:::run_ss_model(dir = tmp_cod_path, 
                       admb_options = "-maxfn 0 -phase 50 -nohess",
                       verbose = FALSE)
  r4ss::SS_plots(r4ss::SS_output(tmp_cod_path, verbose = FALSE, printstats = FALSE), verbose = FALSE)
}

# manipulate EM HCRs ----

for (i in HCR_vals) {
  tmp_scen_name <- names(HCR_vals)[HCR_vals == i]
  tmp_cod_path <- file.path(mods_path, tmp_scen_name)
  file.copy(from = cod_mod_path, to = mods_path, recursive = TRUE)
  file.rename(from = file.path(mods_path, "cod"), to = tmp_cod_path)
  # make model read initial values from control file and not ss.par
  fore <- r4ss::SS_readforecast(file.path(tmp_cod_path, "forecast.ss"), 
                                verbose = FALSE)
  # manipulate HCRS.
  fore$MSY <- 1
  fore$Forecast <- 1
  fore$ControlRuleMethod <- 1
  fore$BforconstantF <- 0.01*i 
  fore$BfornoF <- 0.1
  fore$Flimitfraction <- 0.75
  r4ss::SS_writeforecast(fore, tmp_cod_path, verbose = FALSE, overwrite = TRUE)
}

# get the sampling scheme ----
sample_struct <- SSMSE::create_sample_struct(dat = datfile_path, nyrs = 20)
# modify
sample_struct$lencomp <- data.frame(Yr = seq(105, 120, by = 5), 
                                    Seas = sample_struct$lencomp$Seas,
                                    FltSvy = sample_struct$lencomp$FltSvy, 
                                    Sex = sample_struct$lencomp$Sex, 
                                    Part = sample_struct$lencomp$Part,
                                    Nsamp = sample_struct$lencomp$Nsamp)

# call SSSMSE ----
call_SSMSE <- function(scen_name, OM_dir_path, EM_dir_path, sample_struct) {
  out <- SSMSE::run_SSMSE(scen_name_vec = scen_name, # name of the scenario
                          out_dir_scen_vec = "model_runs", # directory in which to run the scenario
                          iter_vec = 100, # run with 5 iterations each
                          OM_in_dir_vec = OM_dir_path,
                          EM_in_dir_vec = EM_dir_path,
                          MS_vec = "EM",       # The management strategy is specified in the EM
                          use_SS_boot_vec = TRUE, # use the SS bootstrap module for sampling
                          nyrs_vec = 20,        # Years to project OM forward
                          scope = 2,
                          nyrs_assess_vec = 5, # Years between assessments
                          rec_dev_pattern = "rand", # Don't use recruitment deviations
                          impl_error_pattern = "none", # Don't use implementation error
                          sample_struct_list = list(sample_struct), # How to sample data for running the EM.
                          seed = 12345) #Set a fixed integer seed that allows replication 
  out
}
future::plan(multiprocess) # run in serial b/c just 1 scenario
# Run SSMSE using the variables in the data fram.e
output <- furrr::future_pmap(scenarios, 
                             ~call_SSMSE(scen_name = ..1, OM_dir_path = ..2, 
                                         EM_dir_path = ..3, 
                                         sample_struct = sample_struct))

# look at results ----
summary <- SSMSE::SSMSE_summary_all(dir = "model_runs")

summary <- list()
summary$ts <- read.csv("model_runs/ss3sim_ts.csv")
summary$scalar <- read.csv("model_runs/ss3sim_scalar.csv")

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

OM_metrics <- NULL
for (i in c("sel-low", "sel-med","sel-hi")) { # scenarios$scen_name to make general
  OM_name <- grep("_OM$",
                  list.dirs(file.path("model_runs", i, "1"), full.names = FALSE), 
                  value = TRUE)
  OM_dat <- file.path("model_runs", i, as.character(1:5), OM_name, "ss3.dat")
  tot_catch <- unlist(lapply(OM_dat, function(x) get_total_catch(x)))
  avg_catch <- unlist(lapply(OM_dat, function(x) get_avg_catch(x)))
  catch_sd <- unlist(lapply(OM_dat, function(x) get_catch_sd(x)))
  tmp_df <- data.frame(iteration = 1:5, scenario = i, tot_catch = tot_catch,
                       avg_catch = avg_catch, catch_sd = catch_sd)
  OM_metrics <- rbind(OM_metrics, tmp_df)
}
SSB_120 <- get_SSB_yr(summary, yr = 120)

all_metrics <- full_join(OM_metrics, SSB_120)
all_metrics_long <- tidyr::gather(all_metrics, "metric", "value", 3:6)
all_metrics_long$value_bils <- all_metrics_long$value/1000000000

ggplot(data = all_metrics_long, aes(x = scenario, y = value_bils)) +
  geom_boxplot() +
  facet_wrap(~metric, scales = "free")+
  scale_y_continuous(limits = c(0, NA))+
  theme_classic()

ggsave(file.path("figures", "placeholder_plot.png"))

# how close were the point estimates of select. par to the truth? (Just look at RE)
# and the plot as barplots
