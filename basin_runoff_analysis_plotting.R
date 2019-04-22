# Purpose: Plots Climate Impacts on Water Availability by Basin from Xanthos

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
setwd('C:/Users/twild/all_git_repositories/idb_results/downscaling/Water/Xanthos')
gcm_colors <- c("noresm1-m" = "#736F6E",
                "miroc-esm-chem" = "#C0C0C0",
                "ipsl-cm5a-lr" = "#98AFC7",
                "hadgem2-es" = "#6698FF",
                "gfdl-esm2m" = "#153E7E",
                "watch+wfdei" = 'black',
                'historical' = 'black',
                'historical mean' = 'black')
rcp_colors <- c("rcp8p5" = "#736F6E",
                "rcp6p0" = "#C0C0C0",
                "rcp4p5" = "#98AFC7",
                "rcp2p6" = "#6698FF",
                "historical" = 'black',
                'historical mean' = 'black')

figures_basepath <- 'C:/Users/twild/all_git_repositories/idb_results/downscaling/Water/Xanthos/output/figures/basin'
results_basepath <- 'C:/Users/twild/all_git_repositories/idb_results/downscaling/Water/Xanthos/output/pic_impacts_runs'
xanthos_config_names <- c('pm_abcd_mrtm')
gcm_names <- c('noresm1-m', 'miroc-esm-chem', 'ipsl-cm5a-lr', 'hadgem2-es', 'gfdl-esm2m')
gcm_names_incl_hist <- append(gcm_names, 'watch+wfdei')
rcp_names <- c('rcp2p6', 'rcp4p5', 'rcp6p0', 'rcp8p5')
rcp_names_incl_hist <- append(rcp_names, "historical")
xanthos_var_names <- c('Basin_runoff_km3peryear')
time_scale <- '1950_2099'
water_basins <- c("La Plata", 'Caribbean Coast', 'Magdalena', 'Orinoco', 'Amazon',
                  'Colombia - Ecuador Pacific Coast')
filter_list <- water_basins
filter_list_2 <- list("Basin_runoff_km3peryear" = water_basins)
stored_in_dir <- 0  # = 1 if dragged whole xanthos folder off pic; 0 if just dragged file down into results dir on comp

# Set base input files directory
base_dir <- 'C:/Users/twild/all_git_repositories/idb_results/downscaling/Water/Xanthos/output/pic_impacts_runs'
level2_out_dir <- 'C:/Users/twild/all_git_repositories/idb_results/downscaling/Water/Xanthos/output/pic_impacts_runs/level2'
delta <- adjust_gcm_mean(base_dir, level2_out_dir, water_basins)
deltas_gcm_all <- delta$deltas_gcm_all
runoff_gcm_all_GCAM <- delta$runoff_gcm_all_GCAM_2
runoff_gcm_all_GCAM_3 <- delta$runoff_gcm_all_GCAM_3

# Read in xanthos runoff data; separate out historical and projected data
add_historical <- 1  # Add historical values to dataframe

df_all_runs_basin <- xanthos_proc(xanthos_var_names, xanthos_config_names, gcm_names, rcp_names, time_scale, results_basepath,
                                  filter_list_2)$output
df_all_runs_basin <- xanthos_hist_proc(xanthos_var_names, xanthos_config_names, df_all_runs_basin, stored_in_dir,
                                       results_basepath, filter_list_2, add_historical=add_historical)$output
df_all_runs_basin_hist <- df_all_runs_basin %>% filter(rcp == 'historical')
df_all_runs_basin <- df_all_runs_basin %>% filter(rcp != 'historical')
df_all_runs_basin$year <- as.numeric(df_all_runs_basin$year)
# Adjust by delta factors
df_all_runs_basin <- df_all_runs_basin %>% 
  left_join(deltas_gcm_all, by=c('name', 'gcm', 'rcp', 'year')) %>%
  mutate(value = delta_factor*value) %>% 
  select(-delta_factor)
df_all_runs_basin$year <- as.numeric(df_all_runs_basin$year)
df_all_runs_basin_hist$year <- as.numeric(df_all_runs_basin_hist$year)

# Compute rolling mean--BASIN
roll_window <- 1  # Establish target window for rolling mean; k=1 is no rolling mean, just returns regular values.
df_2_all_runs_basin <- roll_mean(df_all_runs_basin, xanthos_var_names, xanthos_config_names, gcm_names_incl_hist,
                                 rcp_names_incl_hist, filter_list_2, k=roll_window)$output
df_2_all_runs_basin$year <- as.numeric(df_2_all_runs_basin$year)

# Plot basin runoff (individual plot for each basin specified earlier, including all the RCP and GCAM combinations).
# This is not separated out by country, this is the entire basin's runoff.
y_ax_lbl <- expression(Annual~Runoff~(km^3))
input <- df_2_all_runs_basin
roll <- 0
start_yr <- 2010
end_yr <- 2100
var_names <- c('Basin_runoff_km3peryear')
region_list <- water_basins
region_single_plot(var_names, region_list, input, figures_basepath, start_yr, end_yr,
                   gcm_names, rcp_names, roll, y_ax_lbl, trendline=0)

# Plot smoothed individual basin runoff plots.
y_ax_lbl <- expression(Annual~Runoff~(km^3))
input <- df_2_all_runs_basin
roll <- 2
start_yr <- 2010
end_yr <- 2050
var_names <- c('Basin_runoff_km3peryear')
region_list <- water_basins
region_single_plot(var_names, region_list, input, figures_basepath, start_yr, end_yr,
                   gcm_names, rcp_names, roll, y_ax_lbl, trendline=0)

# Create faceted plot across GCMs and RCPs for Basin runoff
roll <- 0
start_yr <- 2010
end_yr <- 2100
start_yr_hist <- 1970
end_yr_hist <- 2009
xanthos_var_names <- c('Basin_runoff_km3peryear')
filter_list_2 <- list("Basin_runoff_km3peryear" = water_basins)
for(var_1 in xanthos_var_names){
  for(reg in filter_list_2[[var_1]]){
    fig_name <- paste0(figures_basepath, '/', reg, "_", var_1, "_", 'gcm_rcp_facet.png')
    plot_df <- df_2_all_runs_basin %>% filter(name==reg, year>=start_yr, year<=end_yr, var==var_1) %>%
      filter(gcm %in% gcm_names_incl_hist, rcp %in% rcp_names_incl_hist)
    plot_df_hist <- df_all_runs_basin_hist %>% filter(name==reg, year>=start_yr_hist, year<=end_yr_hist, var==var_1)
    facet_grid_plot(plot_df, fig_name, rolling=roll, y_lbl=y_ax_lbl, df_all_runs_hist=plot_df_hist, historical=1)
  }
}

# Create a smooth faceted plot across GCMs and RCPs for Basin runoff
roll <- 2
# Use all other values (except "roll" from above)
for(var_1 in xanthos_var_names){
  for(reg in filter_list_2[[var_1]]){
    fig_name <- paste0(figures_basepath, '/', reg, "_", var_1, "_", 'gcm_rcp_facet_smooth.png')
    plot_df <- df_2_all_runs_basin %>% filter(name==reg, year>=start_yr, year<=end_yr, var==var_1) %>%
      filter(gcm %in% gcm_names_incl_hist, rcp %in% rcp_names_incl_hist)
    plot_df_hist <- df_all_runs_basin_hist %>% filter(name==reg, year>=start_yr_hist, year<=end_yr_hist, var==var_1)
    facet_grid_plot(plot_df, fig_name, rolling=roll, y_lbl=y_ax_lbl, df_all_runs_hist=plot_df_hist, historical=1)
  }
}

# Plot basin runoff (individual plot for each basin specified earlier, where all the GCM and RCP combinations are
# combined on the same plot
y_ax_lbl <- expression(Annual~Runoff~(km^3))
input <- df_2_all_runs_basin %>% filter(year>=2009, year<=2100)
roll <- 0
start_yr <- 1970
end_yr <- 2100
var_names <- c('Basin_runoff_km3peryear')
region_list <- water_basins
region_single_plot(xanthos_var_names, region_list, input, figures_basepath, start_yr, end_yr, gcm_names, rcp_names,
                   roll, y_ax_lbl, trendline=0, combined_lines=1, plot_df_hist=df_all_runs_basin_hist)

# Plot facet of values smoothed in Sean's separate module
start_yr <- 2010
start_yr_mod <- 2010
end_yr <- 2100
start_yr_hist <- 1970
end_yr_hist <- 2010
end_yr_hist_mod <- 2010
color_palette <- c('gcm_colors')
model <- c('pm_abcd_mrtm')
xanthos_var_names <- c('Basin_runoff_km3peryear')
# Add columns that plotting module expects to find
runoff_gcm_all_GCAM <- runoff_gcm_all_GCAM %>% mutate(FillPalette = color_palette, 
                                                                var=xanthos_var_names, mod=model)
runoff_gcm_all_GCAM_3 <- runoff_gcm_all_GCAM_3 %>% mutate(FillPalette = color_palette, 
                                                      var=xanthos_var_names, mod=model)
# Break into a historical portion and a future portion, so they can be plotted separately
runoff_gcm_all_GCAM_hist <- runoff_gcm_all_GCAM %>% filter(year>=start_yr_hist, year<=end_yr_hist_mod) %>% 
  mutate(rcp='historical mean', gcm='historical mean')
runoff_gcm_all_GCAM_fut <- runoff_gcm_all_GCAM_3 %>% filter(year>=start_yr_mod, year<=end_yr) 
# Produce faceted plot
roll <- 0
filter_list_2 <- list("Basin_runoff_km3peryear" = water_basins)
for(var_1 in xanthos_var_names){
  for(reg in filter_list_2[[var_1]]){
    fig_name <- paste0(figures_basepath, '/', reg, "_", var_1, "_", 'gcm_rcp_facet_GCAM.png')
    plot_df <- runoff_gcm_all_GCAM_fut %>% filter(name==reg, year>=start_yr, year<=end_yr, var==var_1) %>%
      filter(gcm %in% gcm_names_incl_hist, rcp %in% rcp_names_incl_hist)
    plot_df_hist <- runoff_gcm_all_GCAM_hist %>% filter(name==reg, year>=start_yr_hist, year<=end_yr_hist, var==var_1)
    facet_grid_plot(plot_df, fig_name, rolling=roll, y_lbl=y_ax_lbl, df_all_runs_hist=plot_df_hist, historical=1)
  }
}

# Plot all of above lines that appear in facet, but all on the same plot
y_ax_lbl <- expression(Annual~Runoff~(km^3))
input <- runoff_gcm_all_GCAM_fut %>% filter(year>=2010, year<=2100)
roll <- 0
start_yr <- 2010
end_yr <- 2100
start_yr_hist <- 1970
end_yr_hist <- 2010
var_names <- c('Basin_runoff_km3peryear')
region_list <- water_basins
region_single_plot(xanthos_var_names, region_list, input, figures_basepath, start_yr, end_yr, gcm_names, rcp_names,
                   roll, y_ax_lbl, trendline=0, combined_lines=1, plot_df_hist=runoff_gcm_all_GCAM_hist,
                   all_same_color = 0, titles = 'Yes', legend_on=F)