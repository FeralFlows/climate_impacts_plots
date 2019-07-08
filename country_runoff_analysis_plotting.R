# Purpose: Plots Climate Impacts on Water Availability by Country from Xanthos

library("dplyr")
library("tidyr")
library("ggplot2")
library("readr")
library("zoo")
library(scales)
library(stats)
library(magrittr)
library(dplyr)

source('C:/Users/twild/all_git_repositories/idb_results/downscaling/climate_impacts_plots/xanthos_postprocessing_fns.R')

gcm_colors <- c("NorESM1-M" = "#736F6E",
                "MIROC-ESM-CHEM" = "#C0C0C0",
                "IPSL-CM5A-LR" = "#98AFC7",
                "HadGEM2-ES" = "#6698FF",
                "GFDL-ESM2M" = "#153E7E",
                "watch+wfdei" = 'black')
rcp_colors <- c("rcp8p5" = "#736F6E",
                "rcp6p0" = "#C0C0C0",
                "rcp4p5" = "#98AFC7",
                "rcp2p6" = "#6698FF",
                "historical" = 'black')

figures_basepath <- 'C:/Users/twild/all_git_repositories/idb_results/downscaling/Water/Xanthos/output/figures/country'
results_basepath <- 'C:/Users/twild/all_git_repositories/Xanthos_final_4/example/output'
xanthos_config_names <- c('clim_impacts')
gcm_names <- c('NorESM1-M', 'MIROC-ESM-CHEM', 'IPSL-CM5A-LR', 'HadGEM2-ES', 'GFDL-ESM2M')
gcm_names_incl_hist <- append(gcm_names, 'watch+wfdei')
rcp_names <- c('rcp2p6', 'rcp4p5', 'rcp6p0', 'rcp8p5')
rcp_names_incl_hist <- append(rcp_names, "historical")
gridded_runoff <- c('q_km3peryear')
time_scale <- '1950_2099'
country_list <- c('Colombia', 'Uruguay', 'Argentina')
stored_in_dir <- 1  # = 1 if dragged whole xanthos folder off pic; 0 if just dragged file down into results dir on comp
add_historical <- 1  # Add historical values to dataframe
run_name <- c('clim_impacts')

# for gridded runoff by country
country_grid_id_filepath <- 'C:/Users/twild/all_git_repositories/idb_results/downscaling/Water/Xanthos/output/region33_grids_Uruguay.csv'
country_names_id <- 'C:/Users/twild/all_git_repositories/idb_results/downscaling/Water/Xanthos/output/Rgn33Names_Uruguay.csv'

filter_list_3 <- list("q_km3peryear" = country_list)
df_all_runs_ctry <- xanthos_proc(gridded_runoff, xanthos_config_names, gcm_names, rcp_names, time_scale, results_basepath,
                                 filter_liist=filter_list_3, country_grid_id_filepath=country_grid_id_filepath, 
                                 country_names_id=country_names_id)$output
df_all_runs_ctry <- xanthos_hist_proc(gridded_runoff, xanthos_config_names, df_all_runs_ctry, stored_in_dir,
                                      results_basepath, add_historical, filter_list = filter_list_3,
                                      country_grid_id_filepath=country_grid_id_filepath,
                                      country_names_id=country_names_id)$output
df_all_runs_ctry_hist <- df_all_runs_ctry %>% filter(rcp == 'historical')
df_all_runs_ctry <- df_all_runs_ctry %>% filter(rcp != 'historical')
df_all_runs_ctry$year <- as.numeric(df_all_runs_ctry$year)
df_all_runs_ctry_hist$year <- as.numeric(df_all_runs_ctry_hist$year)

# Compute rolling mean--COUNTRY
roll_window <- 1  # Establish target window for rolling mean; k=1 is no rolling mean, just returns regular values.
df_2_all_runs_ctry <- roll_mean(df_all_runs_ctry, gridded_runoff, xanthos_config_names, gcm_names_incl_hist,
                                rcp_names_incl_hist, region_list = filter_list_3, k=roll_window)$output
df_2_all_runs_ctry$year <- as.numeric(df_2_all_runs_ctry$year)
gcm_names_TEMP_hist <- 'watch+wfdei'
rcp_names_TEMP_hist <- 'historical'
df_2_all_runs_ctry_hist <- roll_mean(df_all_runs_ctry_hist, gridded_runoff, xanthos_config_names, gcm_names_TEMP_hist,
                                     rcp_names_TEMP_hist, region_list=filter_list_3, k=1)$output
df_2_all_runs_ctry_hist$year <- as.numeric(df_2_all_runs_ctry_hist$year)

# Runoff: countries
roll <- 0
start_yr <- 2010
end_yr <- 2100
var_names <- c('Basin_runoff_km3peryear')
y_ax_lbl <- expression(Annual~Runoff~(km^3))
input <- df_2_all_runs_ctry
region_list <- country_list
region_single_plot(gridded_runoff, region_list, input, figures_basepath, start_yr, end_yr,
                   gcm_names, rcp_names, roll, y_ax_lbl, trendline=0)

# Create plot of Historical National water availability through 2010
var_names <- c('Basin_runoff_km3peryear')
y_ax_lbl <- expression(Annual~Runoff~(km^3))
input <- df_2_all_runs_ctry_hist
region_list <- country_list
region_single_plot(gridded_runoff, region_list, input, figures_basepath, 1971, 2009,
                   gcm_names_TEMP_hist, rcp_names_TEMP_hist, roll, y_ax_lbl, trendline=0)

# Create COUNTRY faceted plot across GCMs and RCPs, FROM GRIDDED DATA
roll <- 0
start_yr <- 2010
end_yr <- 2100
end_yr_hist <- 2009
start_yr_hist <- 1970

for(var_1 in gridded_runoff){
  for(ctry in country_list){
    fig_name <- paste0(figures_basepath, '/', ctry, "_", 'gcm_rcp_', var_1, '_runoff_facet.png')
    plot_df <- df_2_all_runs_ctry %>% filter(name==ctry, year>=start_yr, year<=end_yr, var==var_1) %>%
      filter(gcm %in% gcm_names_incl_hist, rcp %in% rcp_names_incl_hist)
    plot_df_hist <- df_all_runs_ctry_hist %>% filter(name==ctry, year>=start_yr_hist, year<=end_yr_hist, var==var_1)
    facet_grid_plot(plot_df, fig_name, rolling=roll, y_lbl=y_ax_lbl, df_all_runs_hist=plot_df_hist, historical=1)
  }
}

# Create SMOOTH COUNTRY faceted plot across GCMs and RCPs, FROM GRIDDED DATA
roll <- 2
# Use all other values (except "roll" from above)
for(var_1 in gridded_runoff){
  for(ctry in country_list){
    fig_name <- paste0(figures_basepath, '/', ctry, "_", 'gcm_rcp_', var_1, '_runoff_facet_smooth.png')
    plot_df <- df_2_all_runs_ctry %>% filter(name==ctry, year>=start_yr, year<=end_yr, var==var_1) %>%
      filter(gcm %in% gcm_names_incl_hist, rcp %in% rcp_names_incl_hist)
    plot_df_hist <- df_all_runs_ctry_hist %>% filter(name==ctry, year>=start_yr_hist, year<=end_yr_hist, var==var_1)
    facet_grid_plot(plot_df, fig_name, rolling=roll, y_lbl=y_ax_lbl, df_all_runs_hist=plot_df_hist, historical=1)
  }
}
