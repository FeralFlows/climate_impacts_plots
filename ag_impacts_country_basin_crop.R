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
gcm_conversion <- list("noresm1" = 'NorESM1-M', 'miroc'= "MIROC-ESM-CHEM", 'ipsl' = "IPSL-CM5A-LR", 
                      'hadgem2' = "HadGEM2-ES", 'gfdl' = "GFDL-ESM2M")  # country_list

figures_basepath <- 'C:/Users/twild/all_git_repositories/idb_results/downscaling/Land/agmip/output/figures'
results_basepath <- 'C:/Users/twild/all_git_repositories/idb_results/impacts/ag'
yield_2010 <- paste0(results_basepath, '/', 'AgYield2010GCAM.csv')
gcm_names <- c('noresm1', 'miroc', 'ipsl', 'hadgem2', 'gfdl')
rcp_names <- c('rcp2p6', 'rcp4p5', 'rcp6p0', 'rcp8p5')
ssp_names <- c('ssp2')
agmip_config_names <- c('pdssat')
agmip_var_names <- c('ag_prodchange')
country_list <- c('Colombia', 'Uruguay', 'Argentina')
water_basins <- list('SAmerCstN', 'MagdalenaR', 'OrinocoR', 'AmazonR', 'ColEcuaCst', 'RioLaPlata', 'BrzCstS', 
                     'MarChiq', 'LaPuna', 'Salinas', 'Pampas', 'ArgCstN', 
                    'ArgColoR', 'NegroR', 'Patagonia', 'ArgCstS', 'ChileCstS')
water_basin_abbrevc <- list('SAmerCstN' = 'Caribbean Coast', 'MagdalenaR' = 'Magdalena', 'OrinocoR'='Orinoco',
                            'AmazonR'='Amazon', 'ColEcuaCst' = 'Colombia - Ecuador Pacific Coast',
                            'RioLaPlata' = "La Plata", 'BrzCstS' = "Uruguay - Brazil South Atlantic", 
                            'MarChiq' = 'Mar Chiquita', 'LaPuna' = 'La Puna Region', 'Salinas' = 'Salinas Grandes', 
                            'Pampas' = 'Pampas Region', 'ArgCstN' = 'North Argentina South Atlantic Coast', 
                            'ArgColoR' = 'South America Colorado', 'NegroR' = 'Negro',
                            'Patagonia' = 'Central Patagonia Highlands', 
                            'ArgCstS' = 'South Argentina South Atlantic Coast', 
                            'ChileCstS' = 'South Chile Pacific Coast')

filter_list <- list("ag_prodchange" = country_list)  # country_list
filter_list_2 <- list("ag_prodchange" = water_basins)  # country_list
stored_in_dir <- 0  # = 1 if dragged whole xanthos folder off pic; 0 if just dragged file down into results dir on comp
gcam_years <- c(2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060, 2065, 2070, 2075, 
                2080, 2085, 2090, 2095, 2100)


# Read in agmip agprodchange data
add_historical <- 0  # Add historical values to dataframe

df_all_runs_basin <- agmip_proc(agmip_var_names, agmip_config_names, gcm_names, rcp_names, results_basepath,
                                  filter_list, water_basin_abbrevc=water_basin_abbrevc,
                                filter_list_2=filter_list_2, gcm_conversion=gcm_conversion)$output
df_all_runs_basin$year <- as.numeric(df_all_runs_basin$year)
df_2_all_runs_basin <- df_all_runs_basin
df_2_all_runs_basin$year <- as.numeric(df_2_all_runs_basin$year)

# Read in yield data
agmip_var_names_yld <- c('crop_yield')
df_yield <- yield_proc(agmip_var_names_yld, yield_2010,
                                country_list, water_basin_abbrevc=water_basin_abbrevc,
                                filter_list_2=water_basins)$output

# Merge ag yield data with agprodchange data
df_2_all_runs_basin <- df_2_all_runs_basin %>% 
  left_join(df_yield, by=c('region', 'crop', 'AgProductionTechnology', 'rfd', 'irr', 'basin')) %>%
  mutate(new_yield=hist_yield) # Add new_yield column; eventually will use agprodchange to calc this column

# Now need to create a 2010 entry to story the original historical yield value, and put that into the DF so it can be plotted as 
# same starting point for each of 20 gcm/rcp lines
new_df <- df_2_all_runs_basin %>% 
  filter(year==2015) %>% # grab random year
  mutate(year=2010) %>%  # original yield value will all be 2010
  select(-hist_yield) %>% # get rid of yield as we will add new yield column through merge
  left_join(df_yield, by=c('region', 'crop', 'AgProductionTechnology', 'rfd', 'irr', 'basin')) %>%
  mutate(new_yield=hist_yield) # Add new_yield column; eventually will use agprodchange to calc this column
# Add new_df to the df_2_all_runs_basin
df_2_all_runs_basin <- rbind(df_2_all_runs_basin, new_df)

# Loop through and calculate new yield using previous yield plus agprodchange
for(reg in unique(df_2_all_runs_basin$region)){
  tech_list <- unique((df_2_all_runs_basin %>% filter(region==reg))$AgProductionTechnology)
  for(tech in tech_list){
    for(yr in gcam_years){
      if(yr==2015){
        df_2_all_runs_basin <- df_2_all_runs_basin %>% 
          mutate(new_yield=if_else(region==reg & AgProductionTechnology==tech & year==2015, new_yield*(1+value/100)^5, new_yield))
      }else{
        yield_prev_dt <- (df_2_all_runs_basin %>% filter(region==reg, AgProductionTechnology==tech, year==yr-5))$new_yield[1]
        df_2_all_runs_basin <- df_2_all_runs_basin %>% 
          mutate(new_yield=if_else(region==reg & AgProductionTechnology==tech & year==yr, yield_prev_dt*(1+value/100)^5, new_yield))          
      }
    }
  }
}
# Convert value to agprodchange, and new_yield to value, for plotting purposes
df_2_all_runs_basin <- df_2_all_runs_basin %>% 
  mutate(AgProdChange=value) %>% 
  mutate(value=new_yield)
df_2_all_runs_basin$var = 'yield'
  
# Read in reference scenario info
varName <- c('AgProdChange')
ref_AgProdChange <- paste0(results_basepath, '/', 'L2052.AgProdChange_ag_irr_ref.csv')  # has reference agprodchange from gcam
ref_AgProdChange_Bio <- paste0(results_basepath, '/', 'L2052.AgProdChange_bio_irr_ref.csv')  # has reference agprodchange from gcam for Bio
df_yield_reference <- yield_proc(varName, ref_AgProdChange, country_list, water_basin_abbrevc=water_basin_abbrevc,
                       filter_list_2=water_basins)$output  # use function to process out the basin names and crops and such
df_yield_reference_Bio <- yield_proc(varName, ref_AgProdChange_Bio, country_list, water_basin_abbrevc=water_basin_abbrevc,
                                     filter_list_2=water_basins)$output  # do the same for bio
df_yield_reference <- rbind(df_yield_reference, df_yield_reference_Bio)  # Combine bio file with regular file
df_yield_reference <- df_yield_reference %>% rename(ref_APC=AgProdChange) %>% mutate(ref_APC=100*ref_APC)  # name the column so we know it's reference
df_2_all_runs_basin <- df_2_all_runs_basin %>% 
  left_join(df_yield_reference, by=c('region', 'crop', 'AgProductionTechnology', 'rfd', 'irr', 'basin', 'year')) %>%
  mutate(reference=hist_yield)  # So that you grab correct values for 2010, using gcm/rcp values for 2010 since all the same

# compute actual yields using ref_APC and 2010 yield values, much like you did above
for(reg in unique(df_2_all_runs_basin$region)){
  tech_list <- unique((df_2_all_runs_basin %>% filter(region==reg))$AgProductionTechnology)
  for(tech in tech_list){
    for(yr in gcam_years){
      if(yr==2015){
        df_2_all_runs_basin <- df_2_all_runs_basin %>% 
          mutate(reference=if_else(region==reg & AgProductionTechnology==tech & year==2015, reference*(1+ref_APC/100)^5, reference))
      }else{
        yield_prev_dt <- (df_2_all_runs_basin %>% filter(region==reg, AgProductionTechnology==tech, year==yr-5))$reference[1]
        df_2_all_runs_basin <- df_2_all_runs_basin %>% 
          mutate(reference=if_else(region==reg & AgProductionTechnology==tech & year==yr, yield_prev_dt*(1+ref_APC/100)^5, reference))          
      }
    }
  }
}

#--------------------------------------
#PLOTS

# Create combined plots, where 20 gcm/rcp runs appear on the same plot
roll <- 0
y_ax_lbl <- expression(Crop~Yield~(Mt/thous~km^2))
start_yr <- 2010
end_yr <- 2050
xanthos_var_names <- c('yield')
basin_list_NEW <- list('Caribbean Coast', 'Magdalena', 'Orinoco', 'Amazon', 'Colombia - Ecuador Pacific Coast',
                       "La Plata", "Uruguay - Brazil South Atlantic", 'Mar Chiquita', 'La Puna Region', 'Salinas Grandes',
                       'Pampas Region', 'North Argentina South Atlantic Coast', 'South America Colorado', 'Negro',
                       'Central Patagonia Highlands', 'South Argentina South Atlantic Coast', 'South Chile Pacific Coast')
input <- df_2_all_runs_basin %>% mutate(smoothedY=value)
roll <- 2
country_list <- c('Colombia', 'Uruguay', 'Argentina')
region_list <- country_list
gcm_list <- 'GFDL-ESM2M'  # 'IPSL-CM5A-LR' 
rcp_list <- c('rcp2p6', 'rcp8p5')
crop_list <- c("Root_Tuber", "Corn", "FiberCrop", "MiscCrop", "OilCrop",  "OtherGrain", "PalmFruit",  "Rice",
               "SugarCrop",  "Wheat",  "biomass", 'FodderGrass', 'FodderHerb')
#crop_list <- c("SugarCrop")  # ,  "Corn", "SugarCrop"
gcm_names <- c('NorESM1-M', "MIROC-ESM-CHEM", "IPSL-CM5A-LR", "HadGEM2-ES", "GFDL-ESM2M")
for(var_1 in xanthos_var_names){
    for (crp in crop_list){
      for (water_type in c("irr", "rfd")){
        for(reg in region_list){
          if(water_type == "irr"){
            plot_df <- input %>% 
              filter(year>=start_yr, year<=end_yr, var==var_1, crop==crp, irr==T) %>% 
              filter(gcm %in% gcm_names, rcp %in% rcp_names, region == reg)
          }else{
            plot_df <- input %>% 
              filter(year>=start_yr, year<=end_yr, var==var_1, crop==crp, rfd==T) %>% 
              filter(gcm %in% gcm_names, rcp %in% rcp_names, region == reg)
          }
          fig_name_append <- paste0(crp,'_',water_type, '_', reg)
          print(paste0(crp, '_', reg))
          print(plot_df)
          if(nrow(plot_df) > 0){
            plot_df <- plot_df %>% rename(name=basin)
            region_single_plot(xanthos_var_names, basin_list_NEW, plot_df, figures_basepath, start_yr, end_yr, gcm_names, rcp_names,
                               roll, y_ax_lbl, trendline=0, combined_lines=1, all_same_color = 1, titles = NULL, legend_on=F, 
                               xmin=2010, xmax=2050, plot_hist=FALSE, fig_name_append=fig_name_append, 
                               gcm_list=gcm_list, rcp_list=rcp_list, plot_reference=TRUE)  # titles='Yes', ymin=0, ymax=40
          }
        }
      }
    }
}

#-----------------------------------------------------------------------------------------------------------------------

# Functional plots we are no longer using

# Plot basin agprodchange (individual plot for each basin specified earlier, including all the RCP and GCAM combinations).
y_ax_lbl <- expression(Crop~Yield~Change~('%'))
input <- df_2_all_runs_basin
roll <- 0
start_yr <- 2010
end_yr <- 2050
var_names <- c('yield')
region_list <- country_list
basin_list <- water_basins
basin_list_NEW <- list('Caribbean Coast', 'Magdalena', 'Orinoco', 'Amazon', 'Colombia - Ecuador Pacific Coast',
                       "La Plata", "Uruguay - Brazil South Atlantic", 'Mar Chiquita', 'La Puna Region', 'Salinas Grandes',
                       'Pampas Region', 'North Argentina South Atlantic Coast', 'South America Colorado', 'Negro',
                       'Central Patagonia Highlands', 'South Argentina South Atlantic Coast', 'South Chile Pacific Coast')

basin_list_NEW <- list('Caribbean Coast')
crop_list <- c("Root_Tuber", "Corn", "FiberCrop", "MiscCrop", "OilCrop",  "OtherGrain", "PalmFruit",  "Rice",
               "SugarCrop",  "Wheat",  "biomass", 'FodderGrass', 'FodderHerb')
region_single_plot_ag(var_names, region_list, basin_list_NEW, crop_list, input, figures_basepath, start_yr, end_yr,
                      gcm_names, rcp_names, roll, y_ax_lbl, trendline=0)

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
