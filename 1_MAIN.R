# ____________________________________________________________
cat(toupper("Global alternatives of natural vegetation cover (GANVC)"), "\n")
# ____________________________________________________________

rm(list = ls())

##### R packages ----
suppressMessages({
  require(sf)
  require(stars)

  require(tidyverse)
  require(magrittr)

  require(gridExtra)

  require(viridisLite)

  require(fs)
  require(pbapply)

  # https://torch.mlverse.org/docs/articles/installation.html
  Sys.setenv(CUDA = "11.7")
  require(torch)
  # torch_tensor(1, device = "cuda")

  require(blockCV)

  require(DALEX)

  require(threejs)

  require(leaflet)
  require(leafem)

  require(RSQLite)

  require(Rdimtools)
  require(NbClust)

  require(svglite)
  require(hexbin)

  require(zeallot) # for %<-%
})

##### Project config ----
scripts_dir <- "d:/Scripts/GANVC/"
wd <- "d:/Compute/GANVC/"
setwd(wd)
set.seed(1)
sf_use_s2(F) # mandatory, otherwise bugs! (for DataPreparation & BlockCV)
## reprocess?
reprocess <- F # default to F

##### Data preparation ----
clim_data_sources <- c("CHELSA", "CRU", "ERA5", "WC", "MODIS", "NEX")
clim_data_variables <- c(
  "Mean_temp", "sd_temp",
  "Min_temp", "Max_temp",
  "Mean_precip", "sd_precip",
  "Min_precip", "Max_precip"
)[c(1, 2, 5, 6)]
clim_data_names <- paste0(rep(clim_data_sources, each = length(clim_data_variables)), ".", clim_data_variables)
cat("\n\nData preparation")
dir.create("DataPreparation")
source(str_c(scripts_dir, "2_DataPreparation.R"), local = T)

##### BlockCV ----
cat("\n\nBlockCV")
dir.create("BlockCV")
kf <- 10 # number of folds
if (reprocess) {
  source(str_c(scripts_dir, "3_BlockCV.R"), local = T)
}

##### Data & Model ----
cat("\n\nData & Model")
torch_manual_seed(1)
# device <- torch_device("cpu")
device <- torch_device("cuda", 0)
source(str_c(scripts_dir, "4_Data&Model.R"), local = T)

##### Model training ----
cat("\n\nModel training")
proportion_names <- c("tree", "grasshrub", "ground")
target_data_names <- proportion_names
weight_data_names <- c("cont", "cat", "props")
dir.create("ModelTraining")
if (reprocess) {
  source(str_c(scripts_dir, "5_ModelTraining.R"), local = T)
}

##### Model evaluation ----
cat("\n\nModel evaluation")
dir.create("ModelEvaluation")
if (reprocess) {
  source(str_c(scripts_dir, "6_ModelEvaluation.R"), local = T)
}

##### Mapping ----
cat("\n\nMapping", "...")
dir.create("Maps")
file_sqlite <- "Maps/all_combinations.sqlite"
if (reprocess) {
  source(str_c(scripts_dir, "7_Mapping.R"), local = T)
}

##### Illustration ----
if (reprocess) {
  source(str_c(scripts_dir, "8_llustration.R"), local = T)
}
