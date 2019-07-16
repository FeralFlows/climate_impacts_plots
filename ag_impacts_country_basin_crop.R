# Purpose: Plots Climate Impacts on Water Availability and Hydropower using Xanthos runs

library("dplyr")
library("tidyr")
library("ggplot2")
library("readr")
library("zoo")
library(scales)
source('C:/Users/twild/all_git_repositories/idb_results/downscaling/climate_impacts_plots/xanthos_postprocessing_fns.R')

gcm_colors <- c("noresm1" = "#736F6E",
                "miroc" = "#C0C0C0",
                "ipsl" = "#98AFC7",
                "hadgem2" = "#6698FF",
                "gfdl" = "#153E7E",
                "watch+wfdei" = 'black',
                'historical' = 'black',
                'historical mean' = 'black')

rcp_colors <- c("rcp8p5" = "gray60",
                "rcp6p0" = "blue",
                "rcp4p5" = "green",
                "rcp2p6" = "red",
                "historical" = 'black')
ssp_colors <- c("ssp1" = "gray60",
                "ssp2" = "blue",
                "ssp3" = "green",
                "ssp4" = "red",
                "ssp5" = 'black')

figures_basepath <- 'C:/Users/twild/all_git_repositories/idb_results/downscaling/Land/agmip/output/figures'
results_basepath <- 'C:/Users/twild/all_git_repositories/idb_results/impacts/ag'
gcm_names <- c('noresm1', 'miroc', 'ipsl', 'hadgem2', 'gfdl')
rcp_names <- c('rcp2p6', 'rcp4p5', 'rcp6p0', 'rcp8p5')
ssp_names <- c('ssp2')
agmip_config_names <- c('pdssat')
agmip_var_names <- c('ag_prodchange')
country_list <- c('Colombia', 'Uruguay')
water_basins <- c('SAmerCstN', 'MagdalenaR', 'OrinocoR', 'AmazonR', 'ColEcuaCst', 'RioLaPlata', 'BrzCstS')
water_basin_abbrevc <- list('SAmerCstN' = 'Caribbean Coast', 'MagdalenaR' = 'Magdalena', 'OrinocoR'='Orinoco',
                            'AmazonR'='Amazon', 'ColEcuaCst' = 'Colombia - Ecuador Pacific Coast',
                            'RioLaPlata' = "La Plata", 'BrzCstS' = "Uruguay - Brazil South Atlantic")
filter_list <- list("ag_prodchange" = country_list)  # country_list
filter_list_2 <- list("ag_prodchange" = water_basins)  # country_list
stored_in_dir <- 0  # = 1 if dragged whole xanthos folder off pic; 0 if just dragged file down into results dir on comp

# Read in agmip runoff data
add_historical <- 0  # Add historical values to dataframe

df_all_runs_basin <- agmip_proc(agmip_var_names, agmip_config_names, gcm_names, rcp_names, results_basepath,
                                  filter_list, water_basin_abbrevc=water_basin_abbrevc,
                                filter_list_2=filter_list_2)$output
df_all_runs_basin$year <- as.numeric(df_all_runs_basin$year)
df_2_all_runs_basin <- df_all_runs_basin
df_2_all_runs_basin$year <- as.numeric(df_2_all_runs_basin$year)

# Plot basin crop yield (individual plot for each basin specified earlier, including all the RCP and GCAM combinations).
y_ax_lbl <- expression(Crop~Yield~Change~('%'))
input <- df_2_all_runs_basin
roll <- 0
start_yr <- 2010
end_yr <- 2050
var_names <- c('ag_prodchange')
region_list <- country_list
basin_list <- water_basins
basin_list_NEW <- list('Caribbean Coast', 'Magdalena', 'Orinoco', 'Amazon', 'Colombia - Ecuador Pacific Coast',
                       'Uruguay - Brazil South Atlantic', 'La Plata')
basin_list_NEW <- list('Uruguay - Brazil South Atlantic', 'La Plata')
crop_list <- c("Root_Tuber", "Corn", "FiberCrop", "MiscCrop", "OilCrop",  "OtherGrain", "PalmFruit",  "Rice",
               "SugarCrop",  "Wheat",  "biomass")
region_single_plot_ag(var_names, region_list, basin_list_NEW, crop_list, input, figures_basepath, start_yr, end_yr,
                   gcm_names, rcp_names, roll, y_ax_lbl, trendline=0)

# Create combined plots, where 20 gcm/rcp runs appear on the same plot

roll <- 0
y_ax_lbl <- expression(Crop~Yield~Change~('%'))
start_yr <- 2015
end_yr <- 2050
xanthos_var_names <- c('ag_prodchange')
basin_list_NEW <- c('La Plata')
basin_list_NEW <- c('Caribbean Coast', 'Magdalena', 'Orinoco', 'Amazon', 'Colombia - Ecuador Pacific Coast')
y_ax_lbl <- expression(Crop~Yield~Change~('%'))
input <- df_2_all_runs_basin %>% mutate(smoothedY=value)
roll <- 2
country_list <- c('Colombia')
region_list <- country_list
for(var_1 in xanthos_var_names){
    for (crp in crop_list){
      for (water_type in c("irr", "rfd")){
        if(water_type == "irr"){
          plot_df <- input %>% 
            filter(year>=start_yr, year<=end_yr, var==var_1, crop==crp, irr==T) %>% 
            filter(gcm %in% gcm_names, rcp %in% rcp_names)
        }else{
          plot_df <- input %>% 
            filter(year>=start_yr, year<=end_yr, var==var_1, crop==crp, rfd==T) %>% 
            filter(gcm %in% gcm_names, rcp %in% rcp_names)
        }
        fig_name_append <- paste0(crp,'_',water_type)
        if(nrow(plot_df) > 0){
          plot_df <- plot_df %>% rename(name=basin)
          region_single_plot(xanthos_var_names, basin_list_NEW, plot_df, figures_basepath, start_yr, end_yr, gcm_names, rcp_names,
                             roll, y_ax_lbl, trendline=0, combined_lines=1, all_same_color = 0, titles = 'Yes', legend_on=F, 
                             xmin=2010, xmax=2050, plot_hist=FALSE, fig_name_append=fig_name_append)            
        }
      }
    }
}



# Create faceted plot across GCMs and RCPs for Basin agprodchange

roll <- 0
y_ax_lbl <- expression(Crop~Yield~Change~('%'))
start_yr <- 2010
end_yr <- 2050
xanthos_var_names <- c('ag_prodchange')
basin_list_NEW <- c('Caribbean Coast', 'Magdalena', 'Orinoco', 'Amazon', 'Colombia - Ecuador Pacific Coast')
basin_list_NEW <- c('Uruguay - Brazil South Atlantic', 'La Plata')

for(var_1 in xanthos_var_names){
  for(bas in basin_list_NEW){
    for(reg in country_list){
      for (crp in crop_list){
        for (water_type in c("irr", "rfd")){
          fig_name <- paste0(figures_basepath, '/', reg, "_", bas, "_", crp, "_", water_type, "_", 'facet.png')
          if(water_type == "irr"){
            plot_df <- df_2_all_runs_basin %>% filter(region==reg, year>=start_yr, year<=end_yr, var==var_1, basin==bas,
                                                      crop==crp, irr==T) %>% filter(gcm %in% gcm_names, rcp %in% rcp_names)
          }else{
            plot_df <- df_2_all_runs_basin %>% filter(region==reg, year>=start_yr, year<=end_yr, var==var_1, basin==bas,
                                                      crop==crp, rfd==T) %>% filter(gcm %in% gcm_names, rcp %in% rcp_names)
          }
          if(nrow(plot_df) > 0){
            facet_grid_plot(plot_df, fig_name, rolling=roll, y_lbl=y_ax_lbl, historical=0)
          }
        }
      }
    }
  }
}
