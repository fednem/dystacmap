library(tidyverse)
#read functional data

source("D:/r_script/ExtractROIsValues.r")

directory <- "D:/DysTacMap/rs-fmri_all/rs_fmri_combined_center/results/firstlevel/ANALYSIS_01/"

CouplesToExtract <- "ROIsCouple.txt"
mat <- ExtractROIsValues(directory, CouplesToExtract, sep = ";")
mat <- as_data_frame(mat)
save(file = "rs_fmri_extraction_data.RData", mat)