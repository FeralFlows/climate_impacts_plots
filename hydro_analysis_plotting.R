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

source('C:/Users/twild/all_git_repositories/idb_results/downscaling/climate_impacts_plots/xanthos_postprocessing_fns.R')

rcp_colors <- c("rcp8p5" = "#736F6E",
                "rcp6p0" = "#C0C0C0",
                "rcp4p5" = "#98AFC7",
                "rcp2p6" = "#6698FF",
                "historical" = 'black')
gcm_colors <- c("NorESM1-M" = "#736F6E",
                "MIROC-ESM-CHEM" = "#C0C0C0",
                "IPSL-CM5A-LR" = "#98AFC7",
                "HadGEM2-ES" = "#6698FF",
                "GFDL-ESM2M" = "#153E7E",
                "watch+wfdei" = 'black')

figures_basepath <- 'C:/Users/twild/all_git_repositories/idb_results/downscaling/Water/Xanthos/output/figures/hydro'
results_basepath <- 'C:/Users/twild/all_git_repositories/Xanthos_final_4/example/output'
xanthos_config_names <- c('clim_impacts')
gcm_names <- c('NorESM1-M', 'MIROC-ESM-CHEM', 'IPSL-CM5A-LR', 'HadGEM2-ES', 'GFDL-ESM2M')
gcm_names_incl_hist <- append(gcm_names, 'watch+wfdei')
rcp_names <- c('rcp2p6', 'rcp4p5', 'rcp6p0', 'rcp8p5')
rcp_names_incl_hist <- append(rcp_names, "historical")
xanthos_var_names <- c('actual_hydro_by_gcam_region_EJperyr')
time_scale <- '1950_2099'
country_list <- c('Colombia', 'Argentina', 'Uruguay')
filter_list_2 <- list("actual_hydro_by_gcam_region_EJperyr" = country_list)
run_name <- c('clim_impacts')
stored_in_dir <- 1  # = 1 if dragged whole xanthos folder off pic; 0 if just dragged file down into results dir on comp
# Add historical values to dataframe
add_historical <- 1
delta_correction <- 1

df_all_runs_hydro <- xanthos_proc(xanthos_var_names, xanthos_config_names, gcm_names, rcp_names, time_scale, results_basepath,
                                  filter_list_2)$output
df_all_runs_hydro <- xanthos_hist_proc(xanthos_var_names, xanthos_config_names, df_all_runs_hydro, stored_in_dir,
                                       results_basepath, filter_list_2, add_historical=add_historical)$output
df_all_runs_hydro_hist <- df_all_runs_hydro %>% filter(rcp == 'historical')
df_all_runs_hydro <- df_all_runs_hydro %>% filter(rcp != 'historical')

df_all_runs_hydro$year <- as.numeric(df_all_runs_hydro$year)



# Process GCAM hydro values
gcam_ref_hydro_basepath <- '//essi12.umd.edu/documents/twild/Documents/idb_colombia_ref/reference/Electricity/Colombia_reference_hydro.csv'
gcam_ref_hydro_traject <- read.csv(gcam_ref_hydro_basepath, skip = 4)
gcam_ref_hydro_basepath <- '//essi12.umd.edu/documents/twild/Documents/idb_uruguay_ref/reference/Electricity/Uruguay_reference_hydro.csv'
gcam_ref_hydro_traject <- rbind(gcam_ref_hydro_traject, read.csv(gcam_ref_hydro_basepath, skip = 4))
gcam_ref_hydro_basepath <- '//essi12.umd.edu/documents/twild/Documents/idb_argentina_ref/reference/Electricity/Argentina_reference_hydro.csv'
gcam_ref_hydro_traject<- rbind(gcam_ref_hydro_traject, read.csv(gcam_ref_hydro_basepath, skip = 4))
gcam_ref_hydro_traject <- gcam_ref_hydro_traject %>% select(-supplysector, -subsector, -stub.technology, -share.weight) %>% 
  rename(value=fixedOutput)
gcam_ref_hydro_traject_final <- gcam_ref_hydro_traject[FALSE,]
# Loop through years, and add year row if it doesnt already exist
year_vector <- 2010:2100
gcam_year_list <- gcam_ref_hydro_traject$period
for (reg in country_list){
  append_row <- data.frame('region'=reg, 'period'=9999, 'value' = NA, 'delta'=1)
  temp <- gcam_ref_hydro_traject %>% filter(region==reg) %>% mutate('delta'=1)
  for(t in year_vector){
    if(!t %in% unique(gcam_year_list)){
      append_row$period <- t
      temp <- rbind(temp, append_row)
    }
  }
  temp <- temp %>% mutate(value=na.approx(value, period))  # interpolate value between gcam time steps
  value_2010 <- (gcam_ref_hydro_traject %>% filter(region==reg, period==2010))$value[1]
  temp <- temp %>% mutate(delta = value/value_2010)
  gcam_ref_hydro_traject_final <- rbind(gcam_ref_hydro_traject_final, temp)
}

# calculate delta values for all years
gcam_ref_hydro_traject_final <- gcam_ref_hydro_traject_final %>% rename(name=region, year=period)
#-----------------------------------------------------------------------------------------------------------------------
# Section temporarily removed because it was originally applied to conduct a secondary bias correction (see description)
# in function documentation for further details.

# Adjust by delta factors
#adjust_delta <- 0
#if (adjust_delta==1){
#  level2_out_dir <- 'C:/Users/twild/all_git_repositories/idb_results/downscaling/Water/Xanthos/output/level2'
#  deltas_gcm_all <- adjust_gcm_hydro_mean(results_basepath, extras_dir, level2_out_dir, country_list, time_scale,
#                                          stored_in_dir, run_name, xanthos_var_names)$deltas_gcm_all
#  df_all_runs_hydro <- df_all_runs_hydro %>%
#    left_join(deltas_gcm_all, by=c('name', 'gcm', 'rcp', 'year')) %>%
#    mutate(value = delta_factor*value) %>%
#    select(-delta_factor)
#}
#-----------------------------------------------------------------------------------------------------------------------
df_all_runs_hydro_hist$year <- as.numeric(df_all_runs_hydro_hist$year)

# Compute rolling mean
# Projected
df_2_all_runs_hydro <- roll_mean(df_all_runs_hydro, xanthos_var_names, xanthos_config_names, gcm_names_incl_hist,
                                 rcp_names_incl_hist, filter_list_2, loess_span=1)$output
df_2_all_runs_hydro$year <- as.numeric(df_2_all_runs_hydro$year)
# Historical
df_2_all_runs_hydro_hist <- roll_mean(df_all_runs_hydro_hist, xanthos_var_names, xanthos_config_names, gcm_names_incl_hist,
                                 rcp_names_incl_hist, filter_list_2, loess_span=1)$output
df_2_all_runs_hydro_hist$year <- as.numeric(df_2_all_runs_hydro_hist$year)

# Apply delta factor for installed capacity to the smoothed future hydro gen values that are only affected by runoff.
df_2_all_runs_hydro <- df_2_all_runs_hydro %>% left_join(gcam_ref_hydro_traject_final %>% select(-value), by=c('name', 'year'))
df_2_all_runs_hydro$delta[is.na(df_2_all_runs_hydro$delta)] <- 1  # Set NA values to 1
# adjust smoothedY and rolling_mean by delta, and then remove delta column
df_2_all_runs_hydro <- df_2_all_runs_hydro %>% mutate(RollingMeanDelta=rolling_mean*delta) %>% 
  mutate(smoothedYDelta=smoothedY*delta) %>% select(-delta)
# Adjust smoothedY so it does or does not reflect a delta correction, depending on user preferences
if(delta_correction==1){
  df_2_all_runs_hydro$smoothedY <- df_2_all_runs_hydro$smoothedYDelta
}

# Create faceted plot across GCMs and RCPs for country hydropower production
roll <- 0
start_yr <- 2010 # 2010
end_yr <- 2100 # 2100
start_yr_hist <- 1970
end_yr_hist <- 2009
xanthos_var_names <- c('actual_hydro_by_gcam_region_EJperyr')
filter_list_2 <- list("actual_hydro_by_gcam_region_EJperyr" = country_list)
y_ax_lbl <- expression(Annual~Hydropower~(TWh))
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
var_names <- c('actual_hydro_by_gcam_region_EJperyr')
region_list <- country_list
region_single_plot(var_names, region_list, input, figures_basepath, start_yr, end_yr,
                   gcm_names, rcp_names, roll, y_ax_lbl, trendline=0)

# Smoothed Hydropower by Country
y_ax_lbl <- expression(Annual~Hydropower~(TWh))
input <- df_2_all_runs_hydro
roll <- 2
start_yr <- 2010
end_yr <- 2050
var_names <- c('actual_hydro_by_gcam_region_EJperyr')
region_list <- country_list
region_single_plot(var_names, region_list, input, figures_basepath, start_yr, end_yr,
                   gcm_names, rcp_names, roll, y_ax_lbl, trendline=0)

# Plot country hydropower where all the GCM and RCP combinations are
# combined on the same plot
y_ax_lbl <- expression(Annual~Hydropower~(TWh))
input <- df_2_all_runs_hydro %>% filter(year>=2010, year<=2100)
roll <- 0
start_yr <- 2010
end_yr <- 2100
start_yr_hist <- 1970
end_yr_hist <- 2010
var_names <- c('actual_hydro_by_gcam_region_EJperyr')
region_list <- country_list
region_single_plot(var_names, region_list, input, figures_basepath, start_yr, end_yr, gcm_names, rcp_names,
                   roll, y_ax_lbl, trendline=0, combined_lines=1, plot_df_hist=df_2_all_runs_hydro_hist,
                   all_same_color = 0, titles = 'Yes', legend_on=F)

# Plot percentage reduction in 2010 hydropower production
y_ax_lbl <- expression(atop(Change~('%')~'in'~hydropower,
                       ~generation~from~2010))
input <- df_2_all_runs_hydro %>% filter(year>=2010, year<=2100)
input['mean_2010'] <- 0 # add mean_2010 column
for (reg in country_list){
  for (gcm1 in gcm_names){
    for (rcp1 in rcp_names){
      print(paste(reg, gcm1, rcp1))
      mean_val <- (input %>% filter(name==reg, gcm==gcm1, rcp==rcp1, year==2010))$smoothedY[1]
      input <- input %>% mutate(mean_2010 = if_else(name==reg & gcm==gcm1 & rcp==rcp1, mean_val, mean_2010))
    }
  }
}
input <- input %>% mutate(smoothedY = 100*(smoothedY-mean_2010)/mean_2010) %>% select(-mean_2010)
roll <- 2
start_yr <- 2010
end_yr <- 2100
start_yr_hist <- 1970
end_yr_hist <- 2010
var_names <- c('actual_hydro_by_gcam_region_EJperyr')
region_list <- country_list
region_single_plot(var_names, region_list, input, figures_basepath, start_yr, end_yr, gcm_names, rcp_names,
                   roll, y_ax_lbl, trendline=0, combined_lines=1, plot_df_hist=df_2_all_runs_hydro_hist,
                   all_same_color = 0, titles = 'Yes', legend_on=F, plot_hist=FALSE, plot_var='perc_red')