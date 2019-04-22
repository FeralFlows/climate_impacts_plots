# Purpose: Plots Climate Impacts on Hydropower from Xanthos

library("dplyr")
library("tidyr")
library("ggplot2")
library("readr")
library("zoo")
library(scales)
library(stats)
library(magrittr)
library(dplyr)

source('C:/Users/twild/all_git_repositories/idb_results/downscaling/Water/Xanthos/xanthos_postprocessing_fns.R')

gcm_colors <- c("noresm1-m" = "#736F6E",
                "miroc-esm-chem" = "#C0C0C0",
                "ipsl-cm5a-lr" = "#98AFC7",
                "hadgem2-es" = "#6698FF",
                "gfdl-esm2m" = "#153E7E",
                "watch+wfdei" = 'black')
rcp_colors <- c("rcp8p5" = "#736F6E",
                "rcp6p0" = "#C0C0C0",
                "rcp4p5" = "#98AFC7",
                "rcp2p6" = "#6698FF",
                "historical" = 'black')

figures_basepath <- 'C:/Users/twild/all_git_repositories/idb_results/downscaling/Water/Xanthos/output/figures/hydro'
results_basepath <- 'C:/Users/twild/all_git_repositories/idb_results/downscaling/Water/Xanthos/output/pic_impacts_runs'
xanthos_config_names <- c('pm_abcd_mrtm')
gcm_names <- c('noresm1-m', 'miroc-esm-chem', 'ipsl-cm5a-lr', 'hadgem2-es', 'gfdl-esm2m')
gcm_names_incl_hist <- append(gcm_names, 'watch+wfdei')
rcp_names <- c('rcp2p6', 'rcp4p5', 'rcp6p0', 'rcp8p5')
rcp_names_incl_hist <- append(rcp_names, "historical")
xanthos_var_names <- c('actual_hydro_by_gcam_region_ej')
time_scale <- '1950_2099'
country_list <- c('Colombia', 'Argentina')
filter_list_2 <- list("actual_hydro_by_gcam_region_ej" = country_list)
stored_in_dir <- 0  # = 1 if dragged whole xanthos folder off pic; 0 if just dragged file down into results dir on comp
# Add historical values to dataframe
add_historical <- 1

df_all_runs_hydro <- xanthos_proc(xanthos_var_names, xanthos_config_names, gcm_names, rcp_names, time_scale, results_basepath, 
                                  filter_list_2)$output
df_all_runs_hydro <- xanthos_hist_proc(xanthos_var_names, xanthos_config_names, df_all_runs_hydro, stored_in_dir, 
                                       results_basepath, filter_list_2, add_historical=add_historical)$output
df_all_runs_hydro_hist <- df_all_runs_hydro %>% filter(rcp == 'historical')
df_all_runs_hydro <- df_all_runs_hydro %>% filter(rcp != 'historical')

df_all_runs_hydro$year <- as.numeric(df_all_runs_hydro$year)

# Adjust by delta factors
adjust_delta <- 0
if (adjust_delta==1){
  base_dir <- 'C:/Users/twild/all_git_repositories/idb_results/downscaling/Water/Xanthos/output/pic_impacts_runs'
  level2_out_dir <- 'C:/Users/twild/all_git_repositories/idb_results/downscaling/Water/Xanthos/output/pic_impacts_runs/level2'
  deltas_gcm_all <- adjust_gcm_hydro_mean(base_dir, level2_out_dir, country_list)$deltas_gcm_all
  df_all_runs_hydro <- df_all_runs_hydro %>% 
    left_join(deltas_gcm_all, by=c('name', 'gcm', 'rcp', 'year')) %>%
    mutate(value = delta_factor*value) %>% 
    select(-delta_factor)
}

df_all_runs_hydro_hist$year <- as.numeric(df_all_runs_hydro_hist$year)

# Compute rolling mean
# Projected
df_2_all_runs_hydro <- roll_mean(df_all_runs_hydro, xanthos_var_names, xanthos_config_names, gcm_names_incl_hist, 
                                 rcp_names_incl_hist, filter_list_2)$output
df_2_all_runs_hydro$year <- as.numeric(df_2_all_runs_hydro$year)
# Historical
df_2_all_runs_hydro_hist <- roll_mean(df_all_runs_hydro_hist, xanthos_var_names, xanthos_config_names, gcm_names_incl_hist, 
                                 rcp_names_incl_hist, filter_list_2)$output
df_2_all_runs_hydro_hist$year <- as.numeric(df_2_all_runs_hydro_hist$year)

# Compute percentage change from 2010 hydro value for each region of interest
#df_2_all_runs_hydro <- hydro_perc_change(df_2_all_runs_hydro, country_list)


# Create faceted plot across GCMs and RCPs for country hydropower production
roll <- 0
start_yr <- 2010 # 2010
end_yr <- 2100 # 2100
start_yr_hist <- 1970
end_yr_hist <- 2009
xanthos_var_names <- c('actual_hydro_by_gcam_region_ej')
filter_list_2 <- list("actual_hydro_by_gcam_region_ej" = country_list)
for(var_1 in xanthos_var_names){
  for(reg in filter_list_2[[var_1]]){
    fig_name <- paste0(figures_basepath, '/', reg, "_", var_1, "_", 'gcm_rcp_facet.png')
    plot_df <- df_2_all_runs_hydro %>% filter(name==reg, year>=start_yr, year<=end_yr, var==var_1) %>% 
      filter(gcm %in% gcm_names_incl_hist, rcp %in% rcp_names_incl_hist)
    plot_df_hist <- df_all_runs_hydro_hist %>% filter(name==reg, year>=start_yr_hist, year<=end_yr_hist, var==var_1)
    facet_grid_plot(plot_df, fig_name, rolling=roll, y_lbl=y_ax_lbl, df_all_runs_hist=plot_df_hist, historical=1)
  }
}

# Create smooth faceted plot across GCMs and RCPs for country hydropower production
roll <- 2
# Use all other values (except "roll" from above)
for(var_1 in xanthos_var_names){
  for(reg in filter_list_2[[var_1]]){
    fig_name <- paste0(figures_basepath, '/', reg, "_", var_1, "_", 'gcm_rcp_facet_smooth.png')
    plot_df <- df_2_all_runs_hydro %>% filter(name==reg, year>=start_yr, year<=end_yr, var==var_1) %>% 
      filter(gcm %in% gcm_names_incl_hist, rcp %in% rcp_names_incl_hist)
    plot_df_hist <- df_all_runs_hydro_hist %>% filter(name==reg, year>=start_yr_hist, year<=end_yr_hist, var==var_1)
    facet_grid_plot(plot_df, fig_name, rolling=roll, y_lbl=y_ax_lbl, df_all_runs_hist=plot_df_hist, historical=1)
  }
}

# Hydropower: Countries
y_ax_lbl <- expression(Annual~Hydropower~(TWh))
input <- df_2_all_runs_hydro
roll <- 0
start_yr <- 2010
end_yr <- 2050
var_names <- c('actual_hydro_by_gcam_region_ej')
region_list <- country_list
region_single_plot(var_names, region_list, input, figures_basepath, start_yr, end_yr, 
                   gcm_names, rcp_names, roll, y_ax_lbl, trendline=0)

# Smoothed Hydropower by Country
y_ax_lbl <- expression(Annual~Hydropower~(TWh))
input <- df_2_all_runs_hydro
roll <- 2
start_yr <- 2010
end_yr <- 2050
var_names <- c('actual_hydro_by_gcam_region_ej')
region_list <- country_list
region_single_plot(var_names, region_list, input, figures_basepath, start_yr, end_yr, 
                   gcm_names, rcp_names, roll, y_ax_lbl, trendline=0)

# Plot country hydropower where all the GCM and RCP combinations are
# combined on the same plot
y_ax_lbl <- expression(Annual~Hydropower~(TWh))
input <- df_2_all_runs_hydro %>% filter(year>=2009, year<=2100)
roll <- 2
start_yr <- 1970
end_yr <- 2100
var_names <- c('actual_hydro_by_gcam_region_ej')
region_list <- country_list
region_single_plot(var_names, region_list, input, figures_basepath, start_yr, end_yr, gcm_names, rcp_names,
                   roll, y_ax_lbl, trendline=0, combined_lines=1, plot_df_hist=df_2_all_runs_hydro_hist)
