Author: Kevin A. Ryan, karyan@usgs.gov
2024

r1_sample_meta.R
  input: sample IDs output from putative biochemical transformation calculation
  input: sample site and dates for Yakima watershed
  input: sample site and dates for Watershed Rules of Life watersheds
  input: seasonal classification for Yakima watershed samples
  output: dataframe of sample IDs and associated metadata
  
r2_geospatial_indices.R
  input: sample IDS and metadata
  input: geospatial data for all samples
  input: geospatial data for additional samples
  output: dataframe of selected geospatial variables and evenness indices

r3_data_compilation.R
  input: sample IDs and metadata
  input: COMID for sites
  input: geospatial indices
  input: compound class data
  input: elemental composition data
  input: molecular indices data
  input: water chemistry for WROL sites
  input: water chemistry for YRB sites
  input: surface water residence time for all sites
  input: water temperature data from YRB
  input: water temperature data from WROL sites
  input: Damkohler numbers per sample
  output: list of cleaned input dataframes
  output: wide dataframe of all combined data
  
r4_descriptive_statistics.R
  input: wide dataframe of all combined data
  output: tables of descriptive statistics and data summaries
  
r5_models_plots.R
  input: wide dataframe of all combined data
  output: table of linear and linear-log model results
  output: plots