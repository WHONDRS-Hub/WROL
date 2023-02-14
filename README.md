# WROL-RC2 Readme

This repository includes ultra-high resolution mass spectrometry data collected via FT-ICR-MS for the Watershed Rules of Life Project (WROL) and the RC-SFA Research Campain 2 (RC2) Temporal Study samples. 

WROL samples were collected across the Connecticut River, Deschutes River, Gunnison River, and Willamette River watersheds. Data collected and analyzed for this project can be accessed via [ESS-DIVE](https://data.ess-dive.lbl.gov/view/doi:10.15485/1895159)

RC2 temporal samples were collected across 7 sites within the Yakima River Basin in Washington State 2021 to 2022. Data collected and analyzed for this project can be accessed via [ESS-DIVE](https://data.ess-dive.lbl.gov/view/doi:10.15485/1898912)

## FT-ICR-MS Data Readme
FT-ICR-MS raw data (XML files) were processed following instructions in “FTICR_Instructions-Report_Generation_SOP_v3.pdf” present in the ESS-DIVE data package. Molecular formula were assigned using Formularity software. Data were further processed and sample molecular properties were assigned using the R package “ftmsRanalysis”. Futher, sample replicates from each site were merge such that a peak was kept in the merged sample if it was present in at least one of the reps. Putative biochemical transformations were inferred following protocols previously described in Garayburu-Caruso et al., 2020. Briefly, pairwise mass differences were calculated between every peak (with and without molecular fomula assigned) in a merged sample and compared to a reference list of reference transfomations (Transformation_Database_07-2020,csv). Mass differences were matched to the compounds in the reference list (within 1 ppm) to infer the gain or loss of that compound via a biochemical transformation. 
Total number of transformations per sample and total number of transformations normalized per number of peaks per sample were calculated. 
