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

gcam_ref_hydro_basepath <- '//essi12.umd.edu/documents/twild/Documents/idb_colombia_ref/reference/Electricity/Colombia_reference_hydro.csv'
gcam_ref_hydro_traject <- read.csv(gcam_ref_hydro_basepath, skip = 4)
gcam_ref_hydro_basepath <- '//essi12.umd.edu/documents/twild/Documents/idb_uruguay_ref/reference/Electricity/Uruguay_reference_hydro.csv'
gcam_ref_hydro_traject <- rbind(gcam_ref_hydro_traject, read.csv(gcam_ref_hydro_basepath, skip = 4))
gcam_ref_hydro_basepath <- '//essi12.umd.edu/documents/twild/Documents/idb_argentina_ref/reference/Electricity/Argentina_reference_hydro.csv'
gcam_ref_hydro_traject<- rbind(gcam_ref_hydro_traject, read.csv(gcam_ref_hydro_basepath, skip = 4))
gcam_ref_hydro_traject <- gcam_ref_hydro_traject %>% select(-supplysector, -subsector, -stub.technology, -share.weight) %>%
  rename(reference=fixedOutput)

# Plot percentage reduction in smoothed 2010 hydropower production
y_ax_lbl <- expression(atop(Change~('%')~'in'~hydropower,
                            ~generation~from~2010))
input <- df_2_all_runs_hydro %>% filter(year>=2010, year<=2100)
input['mean_2010'] <- 0 # add mean_2010 column
for (reg in country_list_plot){
  for (gcm1 in gcm_names){
    for (rcp1 in rcp_names){
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
region_list <- country_list_plot
region_single_plot(var_names, region_list, input, figures_basepath, start_yr, end_yr, gcm_names, rcp_names,
                   roll, y_ax_lbl, trendline=0, combined_lines=1, plot_df_hist=df_2_all_runs_hydro_hist,
                   all_same_color = 0, titles = 'Yes', legend_on=F, plot_hist=FALSE, plot_var='perc_red')


adjust_gcm_hydro_mean <- function(base_dir, extras_dir, level2_out_dir, country_filter, time_scale, stored_in_dir,
                                  run_name, xanthos_var_names){

  ## Performs same function as adjust_gcm_mean, except for hydro values.

  # Required input files
  reanalysis_data <- paste0(extras_dir, '/', xanthos_var_names, '_', 'watch+wfdei', '_', time_scale, '.csv')
  gcam_countries <- paste0(extras_dir, '/', "Rgn33Names_Uruguay.csv")

  # read watch reanalysis data
  read_csv(reanalysis_data) %>% rename(id=region) %>%
    gather(year, hydro, -id) %>%
    mutate(year = as.integer(year)) -> hydro_wfdei

  baseline_years <- 1970:2009

  # prepare watch output for GCAM (km3 per year)
  hydro_wfdei %>% group_by(id) %>%
    filter(year %in% baseline_years) %>%
    summarise(hydro = mean(hydro)) %>%
    mutate(year = 1970) %>%
    complete(year = seq(1970, 2100, 5), id) %>%
    group_by(id) %>%
    tidyr::fill(hydro) %>% ungroup() %>%
    arrange(id, year) %>%
    rename(country.id = id,
           hydro.max = hydro) %>%
    select(country.id, hydro.max, year) %>%
    mutate(hydro.max = round(hydro.max, 3)) %>%
    write_csv("hydro_noCC_wfdei.csv")

  # get wfdei mean values for baseline years (GCM deltas to be applied to these values)
  hydro_wfdei %>%
    filter(year %in% baseline_years) %>%
    group_by(id) %>%
    summarise(hydro = mean(hydro)) %>%
    mutate(gcm = "wfdei") ->
    hydro_mean_wfdei_hist

  # gcm = "GFDL-ESM2M"; rcp = "2p6"
  # read in historical GCM values
  get_gcm <- function(gcm, rcp, base_dir, stored_in_dir, run_name, time_scale){
    run_name_2 <- paste0(run_name, "_", gcm, "_", 'rcp', rcp, "_", time_scale)
    if (stored_in_dir==1){
      xanthos_dir <- paste0(base_dir, '/', run_name_2)
    }else{
      xanthos_dir <- paste0(base_dir)
    }
    xanthos_file <- paste0(xanthos_var_names, "_", gcm, "_", 'rcp', rcp, '_', time_scale, '.csv')
    xanthos_output_filepath <- paste0(xanthos_dir, '/', xanthos_file)
    read_csv(xanthos_output_filepath) %>%
      rename(id = region) %>%
      gather(year, hydro, -id) %>%
      mutate(gcm = gcm, rcp = rcp, year = as.integer(year)) %>%
      mutate(rcp = paste0('rcp', rcp))
  }

  bind_rows(
    get_gcm("GFDL-ESM2M", "2p6", base_dir, stored_in_dir, run_name, time_scale), get_gcm("GFDL-ESM2M", "4p5", base_dir, stored_in_dir, run_name, time_scale),
    get_gcm("GFDL-ESM2M", "6p0", base_dir, stored_in_dir, run_name, time_scale), get_gcm("GFDL-ESM2M", "8p5", base_dir, stored_in_dir, run_name, time_scale),
    get_gcm("HadGEM2-ES", "2p6", base_dir, stored_in_dir, run_name, time_scale), get_gcm("HadGEM2-ES", "4p5", base_dir, stored_in_dir, run_name, time_scale),
    get_gcm("HadGEM2-ES", "6p0", base_dir, stored_in_dir, run_name, time_scale), get_gcm("HadGEM2-ES", "8p5", base_dir, stored_in_dir, run_name, time_scale),
    get_gcm("IPSL-CM5A-LR", "2p6", base_dir, stored_in_dir, run_name, time_scale), get_gcm("IPSL-CM5A-LR", "4p5", base_dir, stored_in_dir, run_name, time_scale),
    get_gcm("IPSL-CM5A-LR", "6p0", base_dir, stored_in_dir, run_name, time_scale), get_gcm("IPSL-CM5A-LR", "8p5", base_dir, stored_in_dir, run_name, time_scale),
    get_gcm("MIROC-ESM-CHEM", "2p6", base_dir, stored_in_dir, run_name, time_scale), get_gcm("MIROC-ESM-CHEM", "4p5", base_dir, stored_in_dir, run_name, time_scale),
    get_gcm("MIROC-ESM-CHEM", "6p0", base_dir, stored_in_dir, run_name, time_scale), get_gcm("MIROC-ESM-CHEM", "8p5", base_dir, stored_in_dir, run_name, time_scale),
    get_gcm("NorESM1-M", "2p6", base_dir, stored_in_dir, run_name, time_scale), get_gcm("NorESM1-M", "4p5", base_dir, stored_in_dir, run_name, time_scale),
    get_gcm("NorESM1-M", "6p0", base_dir, stored_in_dir, run_name, time_scale), get_gcm("NorESM1-M", "8p5", base_dir, stored_in_dir, run_name, time_scale)
  ) ->
    hydro_gcm_all

  # Determine the mean annual hydro in historical years (baseline yers) in the GCM runs, so they can be compared to the
  # watch data
  hydro_gcm_all %>%
    filter(year %in% baseline_years) %>%
    group_by(gcm, rcp, id) %>%
    summarise(mean_hydro = mean(hydro)) %>% ungroup() ->
    hydro_gcm_baseline_means

  hydro_gcm_all %>%
    left_join(hydro_gcm_baseline_means,
              by = c("id", "gcm", "rcp")) %>%
    mutate(delta_factor = hydro / mean_hydro) %>%
    select(id, year, gcm, rcp, delta_factor) ->
    deltas_gcm_all

  # Apply the delta factor to correct all xanthos hydro values produced with GCMs, so they reflect WATCH mean value in
  # historical years (1970-2010)
  deltas_gcm_all %>%
    left_join(hydro_mean_wfdei_hist %>% select(-gcm),
              by = c("id")) %>%
    mutate(hydro_adj = delta_factor * hydro) %>%
    select(id, year, hydro_adj, gcm, rcp) ->
    hydro_gcm_all_adj

  # apply smoothing
  # get baseline period
  # Given all values from 1970-2010 are equal to the mean for all basins, you end up with only very
  # slightly smoothed values during this time, that flow nicely into the future periods.
  hydro_gcm_all_adj %>%
    left_join(hydro_mean_wfdei_hist %>% rename(baseline_mean = hydro) %>%
                select(-gcm),
              by = c("id")) %>%
    filter(year >= min(baseline_years)) %>%
    mutate(hydro_adj_basemean = if_else(year %in% baseline_years,
                                        baseline_mean, hydro_adj)) %>%
    group_by(id, gcm, rcp) %>%
    nest() %>%
    mutate(model = data %>% map(~loess(hydro_adj_basemean ~ year, data = .))) %>%
    mutate(Pred = map2(model, data, predict)) %>%
    unnest(Pred, data) %>%
    select(id, gcm, rcp, year, Pred) %>%
    rename(hydro_ej = Pred) %>%
    mutate(hydro_ej = if_else(hydro_ej < 0, 0, hydro_ej)) ->
    hydro_gcm_all_adj_smooth

  # fix 1975 - 2010 to mean hist, so they dont apppear smoothed.
  hydro_gcm_all_adj_smooth %>%
    left_join(hydro_mean_wfdei_hist %>% select(-gcm),
              by = "id") %>% rename(hydro_hist = hydro) %>%
    mutate(hydro_ej = if_else(year <= 2010,
                              hydro_hist, hydro_ej)) %>%
    select(-hydro_hist) %>%
    filter(year %in% c(1975, 1990, seq(2005, 2095, 5), 2099)) %>%
    mutate(year = if_else(year == 2099, 2100, as.double(year))) ->
    hydro_gcm_all_GCAM


  hydro_gcm_all_GCAM %>%
    filter(id == 15) %>%
    group_by(year, gcm, rcp) %>% summarise(hydro = sum(hydro_ej)) %>%
    ungroup() %>%
    ggplot(aes(year, hydro, colour = gcm)) + geom_line() +
    facet_wrap(~rcp) + expand_limits(y = 0)

  GCAM_yrs <- hydro_gcm_all_GCAM %>% .$year %>% unique()

  # prepare L2 gcam files for ISI-MIP scenarios
  # Insert these into GCAM and rebuild the data system
  gcm <- "GFDL-ESM2M"
  rcp <- "2p6"


  write_gcm_csv <- function(gcm, rcp){
    # Creates the Level-2 style csv files that serve as input to GCAM
    runoff_gcm_all_GCAM %>%
      filter(gcm == !! gcm, rcp == !! rcp) %>%
      left_join(basin_ids, by = c("id" = "GCAM_basin_ID")) %>%
      mutate(renewresource = paste0(GCAM_basin_name, "-water withdrawals")) %>%
      left_join(region_basin) %>%
      rename(maxSubResource = runoff_km3perYr,
             year.fillout = year) %>%
      mutate(sub.renewable.resource = "runoff") %>%
      select(region, renewresource, sub.renewable.resource, year.fillout, maxSubResource) %>%
      filter(renewresource %in% read_csv(renewrsc_max_gcam, skip = 4)$renewresource) %>%
      arrange(region, renewresource, year.fillout) ->
      runoff_max

    fileName <- paste0(level2_out_dir, '/', "L201.GrdRenewRsrcMax_wfdei_", gcm, "_", rcp, ".csv")
    write(readLines(L201.GrdRenewRsrcMax_runoff)[1:4], file = fileName)
    write.table(runoff_max, file = fileName, row.names = FALSE,
                sep = ",", quote = FALSE, append=TRUE)

  }

  write_gcm_csv("GFDL-ESM2M", "2p6")
  write_gcm_csv("GFDL-ESM2M", "4p5")
  write_gcm_csv("GFDL-ESM2M", "6p0")
  write_gcm_csv("GFDL-ESM2M", "8p5")
  write_gcm_csv("HadGEM2-ES", "2p6")
  write_gcm_csv("HadGEM2-ES", "4p5")
  write_gcm_csv("HadGEM2-ES", "6p0")
  write_gcm_csv("HadGEM2-ES", "8p5")
  write_gcm_csv("IPSL-CM5A-LR", "2p6")
  write_gcm_csv("IPSL-CM5A-LR", "4p5")
  write_gcm_csv("IPSL-CM5A-LR", "6p0")
  write_gcm_csv("IPSL-CM5A-LR", "8p5")
  write_gcm_csv("MIROC-ESM-CHEM", "2p6")
  write_gcm_csv("MIROC-ESM-CHEM", "4p5")
  write_gcm_csv("MIROC-ESM-CHEM", "6p0")
  write_gcm_csv("MIROC-ESM-CHEM", "8p5")
  write_gcm_csv("NorESM1-M", "2p6")
  write_gcm_csv("NorESM1-M", "4p5")
  write_gcm_csv("NorESM1-M", "6p0")
  write_gcm_csv("NorESM1-M", "8p5")
  # Modify deltas_gcm_all to include basins so it can be  used in separate plotting module that organizes by basin.
  read_csv(gcam_countries) %>% rename(id=ctry_code) %>% rename(name=region) %>% left_join(deltas_gcm_all, by='id') %>%
    select(-id) %>% filter(name %in% country_filter) ->deltas_gcm_all
  return(list('deltas_gcm_all' = deltas_gcm_all, 'hydro_gcm_all_adj_smooth' = hydro_gcm_all_adj_smooth))
}


# Creates the Level-2 style csv files that serve as input to GCAM
input <- input %>% filter(gcm == !! clim_mod, rcp == !! forc)
if(!is.null(basin_ids)){
  input <- input %>% left_join(basin_ids, by = c("id" = "GCAM_basin_ID"))
}
input %>% 
  mutate(renewresource = paste0(GCAM_basin_name, "-water withdrawals")) %>% 
  left_join(region_basin) %>% 
  rename(maxSubResource = runoff_km3perYr,
         year.fillout = year) %>% 
  mutate(sub.renewable.resource = "runoff") %>% 
  select(region, renewresource, sub.renewable.resource, year.fillout, maxSubResource) %>% 
  filter(renewresource %in% read_csv(renewrsc_max_gcam, skip = 4)$renewresource) %>% 
  arrange(region, renewresource, year.fillout) ->
  runoff_max

fileName <- paste0(level2_out_dir, '/', "L201.GrdRenewRsrcMax_wfdei_", clim_mod, "_", forc, ".csv")
write(readLines(L201.GrdRenewRsrcMax_runoff)[1:4], file = fileName)
write.table(runoff_max, file = fileName, row.names = FALSE,
            sep = ",", quote = FALSE, append=TRUE)