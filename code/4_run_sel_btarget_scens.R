# Run scenarios relating to q 

# load pkgs set options ----
#devtools::install_github("r4ss/r4ss", ref = "15444de")
# devtools::install_github("nmfs-fish-tools/SSMSE", ref = "404023f")
library(SSMSE)
library(r4ss)
library(dplyr)
library(furrr) # to run in parallel
library(ggplot2)
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
scen_vals <- c(3.5, 4.5, 5.5) 
names(scen_vals) <-   c("sel-low", "sel-med", "sel-hi")
Btgt_vals <- c(0.4, 0.6)
names(Btgt_vals)  <- c("Btgt_0.4", "Btgt_0.6")
scenarios <- data.frame(
  scen_name = c(paste0(names(scen_vals), "-", names(Btgt_vals)[1]),
                paste0(names(scen_vals), "-", names(Btgt_vals)[2])),
  OM_path = rep(c(file.path(mods_path, names(scen_vals))),times = 2),
  EM_path = rep(c(file.path(mods_path, names(Btgt_vals))), times = c(3,3))
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
  parfile <- r4ss::SS_readpar_3.30(parfile = file.path(tmp_cod_path, "ss.par"),
                                   datsource = file.path(tmp_cod_path, "ss3.dat"), 
                          ctlsource = file.path(tmp_cod_path, "control.ss_new"))
  file.remove(file.path(tmp_cod_path, "control.ss_new"))
  file.remove(file.path(tmp_cod_path, "control.ss"))
  file.remove(file.path(tmp_cod_path, "ss.par"))
  file.rename(from = file.path(tmp_cod_path, "control_modified.ss"),
              to = file.path(tmp_cod_path, "control.ss"))
  SSMSE:::run_ss_model(dir = tmp_cod_path, 
                       admb_options = "-maxfn 0 -phase 50 -nohess",
                       verbose = FALSE)
  # add back original recdevs into the model (b/c not specified through the ctl file)
  new_parfile <- r4ss::SS_readpar_3.30(parfile = file.path(tmp_cod_path, "ss.par"),
                                       datsource = file.path(tmp_cod_path, "ss3.dat"), 
                                       ctlsource = file.path(tmp_cod_path, "control.ss"))
  new_parfile$recdev1[, "recdev"] <- parfile$recdev1[, "recdev"] # add in the recdevs to new the parfile
  r4ss::SS_writepar_3.30(new_parfile, outfile = file.path(tmp_cod_path, "ss.par"))
  start <- r4ss::SS_readstarter(file = file.path(tmp_cod_path, "starter.ss"), verbose = FALSE)
  start$init_values_src <- 1 
  r4ss::SS_writestarter(start, file = file.path(tmp_cod_path, "starter.ss"),
                        verbose = FALSE, overwrite = TRUE)
  #run model 1 more time to make sure consistent with the .par file
  SSMSE:::run_ss_model(dir = tmp_cod_path, 
                       admb_options = "-maxfn 0 -phase 50 -nohess",
                       verbose = FALSE)
  r4ss::SS_plots(r4ss::SS_output(tmp_cod_path, verbose = FALSE, printstats = FALSE), verbose = FALSE)
}

# manipulate EM Forecasting ----
# no need to re-run model for the EM, 
for (i in Btgt_vals) {
  tmp_scen_name <- names(Btgt_vals)[Btgt_vals == i]
  tmp_cod_path <- file.path(mods_path, tmp_scen_name)
  file.copy(from = cod_mod_path, to = mods_path, recursive = TRUE)
  file.rename(from = file.path(mods_path, "cod"), to = tmp_cod_path)
  # make model read initial values from control file and not ss.par
  fore <- r4ss::SS_readforecast(file.path(tmp_cod_path, "forecast.ss"), 
                                verbose = FALSE)
  # manipulate the forecasting file.
  fore$MSY <- 1
  fore$SPRtarget <- 0.45
  fore$Btarget <- i # differs between scenarios
  fore$Forecast <- 3 # to just forecast based on a biomass target
  fore$ControlRuleMethod <- 3 # doesn't really matter, b/c not using a control rule
  fore$BforconstantF <- 0.03 # set low to avoid using 
  fore$BfornoF <- 0.01 # set low to avoid using
  fore$Flimitfraction <- 1 # no buffer
  r4ss::SS_writeforecast(fore, tmp_cod_path, verbose = FALSE, overwrite = TRUE)
  file.remove(file.path(tmp_cod_path, "forecast.ss_new")) # to make sure it is not used.
}

# get the sampling scheme ----
sample_struct <- SSMSE::create_sample_struct(dat = datfile_path, nyrs = 50)
# modify
sample_struct$lencomp <- data.frame(Yr = seq(105, 150, by = 5), 
                                    Seas = sample_struct$lencomp$Seas,
                                    FltSvy = sample_struct$lencomp$FltSvy, 
                                    Sex = sample_struct$lencomp$Sex, 
                                    Part = sample_struct$lencomp$Part,
                                    Nsamp = sample_struct$lencomp$Nsamp)

# call SSSMSE ----
call_SSMSE <- function(scen_name, OM_dir_path, EM_dir_path, sample_struct) {
  out <- SSMSE::run_SSMSE(scen_name_vec = scen_name, # name of the scenario
                          out_dir_scen_vec = "model_runs", # directory in which to run the scenario
                          iter_vec = 100,
                          OM_in_dir_vec = OM_dir_path,
                          EM_in_dir_vec = EM_dir_path,
                          MS_vec = "EM",       # The management strategy is specified in the EM
                          use_SS_boot_vec = TRUE, # use the SS bootstrap module for sampling
                          nyrs_vec = 50,        # Years to project OM forward
                          scope = 2,
                          nyrs_assess_vec = 10, # Years between assessments
                          rec_dev_pattern = "rand", # Don't use recruitment deviations
                          impl_error_pattern = "none", # Don't use implementation error
                          sample_struct_list = list(sample_struct), # How to sample data for running the EM.
                          seed = 12345) #Set a fixed integer seed that allows replication 
  out
}
future::plan(multiprocess) # run in serial b/c just 1 scenario
# Run SSMSE using the variables in the data frame
output <- furrr::future_pmap(scenarios, 
                             ~call_SSMSE(scen_name = ..1, OM_dir_path = ..2, 
                                         EM_dir_path = ..3, 
                                         sample_struct = sample_struct))

# look at results ----
summary <- SSMSE::SSMSE_summary_all(dir = "model_runs")

# summary <- list()
# summary$ts <- read.csv("model_runs/ss3sim_ts.csv")
# summary$scalar <- read.csv("model_runs/ss3sim_scalar.csv")

SSB_df <- check_convergence(summary, n_EMs = 6, max_yr = 150)

# calculate performance metrics ----
# look at catch in OM from yrs 125:150
OM_metrics <- NULL
for (i in scenarios$scen_name) { # scenarios$scen_name to make general

  iterations <- list.dirs(file.path("model_runs", i), recursive = FALSE, full.names = FALSE)
  OM_name <- grep("_OM$",
                  list.dirs(file.path("model_runs", i, iterations[1]), full.names = FALSE), 
                  value = TRUE)
  OM_dat <- file.path("model_runs", i, iterations, OM_name, "ss3.dat")
  avg_catch <- unlist(lapply(OM_dat, function(x) get_avg_catch(x, yrs = 126:150)))
  catch_sd <- unlist(lapply(OM_dat, function(x) get_catch_sd(x, yrs = 126:150)))
  tmp_df <- data.frame(iteration = as.integer(iterations), scenario = i,
                       avg_catch = avg_catch, catch_sd = catch_sd)
  OM_metrics <- rbind(OM_metrics, tmp_df)
}
SSB_avg <- get_SSB_avg(summary, min_yr = 126, max_yr = 150)
SSB_rel <- get_rel_SSB_avg(summary, min_yr = 126, max_yr = 150)
SSB_rel$scen_fac <- factor(SSB_rel$scenario, 
  levels = c("sel-low-Btgt_0.4", "sel-med-Btgt_0.4", "sel-hi-Btgt_0.4",
             "sel-low-Btgt_0.6", "sel-med-Btgt_0.6", "sel-hi-Btgt_0.6" ), 
  labels = c("low", "med", "high", "low", "med", "high"))
SSB_rel <- SSB_rel %>%
            tidyr::separate(col = scenario,
                            into = c("OM_sel", "Btgt"), 
                            sep = "-Btgt_",
                            remove = FALSE)

all_metrics <- full_join(OM_metrics, SSB_avg)
all_metrics_long <- tidyr::gather(all_metrics, "metric", "value", 3:5)
all_metrics_long$value_bils <- all_metrics_long$value/1000000000
all_metrics_long$scen_fac <- factor(all_metrics_long$scenario, 
  levels = c("sel-low-Btgt_0.4", "sel-med-Btgt_0.4", "sel-hi-Btgt_0.4",
             "sel-low-Btgt_0.6", "sel-med-Btgt_0.6", "sel-hi-Btgt_0.6" ), 
  labels = c("low", "med", "high", "low", "med", "high"))

all_metrics_long <- all_metrics_long %>%
                      tidyr::separate(col = scenario,
                               into = c("OM_sel", "Btgt"), 
                               sep = "-Btgt_", 
                               remove = FALSE)

metrics <- unique(all_metrics_long$metric)
# todo: convert to useing violin plots
plots <- lapply(metrics, function(i, all_metrics_long) {
  title_lab <- switch(i, 
                  avg_catch = "Long-term average catch (years 126-150)",
                  avg_SSB = "Long-term average SSB (years 126-150)",
                  catch_sd = "Long-term catch variability (years 126-150)")
  yaxis_lab <- switch(i, 
                      avg_catch = "Catch (billion metric tons)",
                      avg_SSB = "Biomass (billion metric tons)",
                      catch_sd = "Catch (billion metric tons)")
  plot <- ggplot(data = all_metrics_long[all_metrics_long$metric == i, ],
         aes(x = scen_fac, y = value_bils)) +
    geom_violin(draw_quantiles = 0.5, aes(fill = Btgt)) +
    scale_y_continuous(limits = c(0, NA))+
    labs(title = title_lab, x = "OM selectivity", y = yaxis_lab) +
    theme_classic(base_size = 22)
  plot
}, all_metrics_long = all_metrics_long)

for (i in seq_len(length(plots))) {
  ggsave(file.path("figures", paste0("run_sel_btarget_scens_", metrics[i], ".png")), 
         plot = plots[[i]], width = 8, height = 6, units = "in", device = "png")
}

# make relative biomass plot ----
plot_relative <- ggplot(data = SSB_rel, aes(x = scen_fac, y = avg_SSB)) +
                  geom_hline(yintercept = 0.6, color = "gray") +
                  geom_hline(yintercept = 0.4, color = "gray")+
                  geom_violin(draw_quantiles = 0.5, aes(fill = Btgt)) +
                  scale_y_continuous(limits = c(0, 0.8))+
                  labs(title = "Long-term average relative SSB\n(years 126-150)", 
                       x = "OM selectivity", y = "SSB/Bo") +
                  theme_classic(base_size = 22)
ggsave(file.path("figures", paste0("run_sel_btarget_scens_", "SSB_rel", ".png")), 
       width = 8, height = 6, units = "in", device = "png")

# get cv catch ----

catch_cv_df <- NULL
for (i in scenarios$scen_name) { # scenarios$scen_name to make general
  
  iterations <- list.dirs(file.path("model_runs", i), recursive = FALSE, full.names = FALSE)
  OM_name <- grep("_OM$",
                  list.dirs(file.path("model_runs", i, iterations[1]), full.names = FALSE), 
                  value = TRUE)
  OM_dat <- file.path("model_runs", i, iterations, OM_name, "ss3.dat")
  catch_cv <- unlist(lapply(OM_dat, function(x) get_catch_cv(x, yrs = 126:150)))
  tmp_df <- data.frame(iteration = as.integer(iterations), scenario = i,
                       catch_cv = catch_cv)
  catch_cv_df <- rbind(catch_cv_df, tmp_df)
}
catch_cv_df$scen_fac <- factor(catch_cv_df$scenario, 
                           levels = c("sel-low-Btgt_0.4", "sel-med-Btgt_0.4", "sel-hi-Btgt_0.4",
                                      "sel-low-Btgt_0.6", "sel-med-Btgt_0.6", "sel-hi-Btgt_0.6" ), 
                           labels = c("low", "med", "high", "low", "med", "high"))
catch_cv_df <- catch_cv_df %>%
  tidyr::separate(col = scenario,
                  into = c("OM_sel", "Btgt"), 
                  sep = "-Btgt_",
                  remove = FALSE)

plot_cv <- ggplot(data = catch_cv_df, aes(x = scen_fac, y = catch_cv)) +
  geom_violin(draw_quantiles = 0.5, aes(fill = Btgt)) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(title = "Long-term catch variability (years 126-150)", 
       x = "OM selectivity", y = "coefficient of variation") +
  theme_classic(base_size = 22)
ggsave(file.path("figures", paste0("run_sel_btarget_scens_", "catch_CV", ".png")), 
       width = 8, height = 6, units = "in", device = "png")
