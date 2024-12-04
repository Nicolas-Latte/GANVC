##### Raster data + to points ----
if (T) {
  cat("\nRaster data + to points", "...")
  files_tif <- list.files("RawData", full.names = T, pattern = "\\.tif$", recursive = T)

  file_continent <- files_tif %>% str_subset("continent")

  ## soil + fire
  file_soilfire <- files_tif %>% str_subset("soil_fire")
  strs_soilfire <- read_stars(file_soilfire)
  names(strs_soilfire) <- "soilfire"
  soilfire_data_names <- st_dimensions(strs_soilfire)$band$values

  ## herbivory
  herb_data_names <- c("biomass", "intake", "litter")
  files_herb <- map_chr(herb_data_names, ~ str_subset(files_tif, .))
  # band 28 == "sum_all"
  strs_herb <- files_herb %>%
    read_stars(along = "band", RasterIO = list(bands = 28)) %>%
    st_set_crs(st_crs(4326))
  strs_herb %<>% st_set_dimensions("band", values = herb_data_names)
  names(strs_herb) <- "herb"

  ## clim data sources
  strss <- map(clim_data_sources, ~ {
    if (. != "MODIS") {
      file <- str_subset(files_tif, .)
      if (. == "NEX") {
        file %<>% str_subset("_hist")
      }
      strs <- read_stars(file, RasterIO = list(bands = 1:8))
      strs <- st_set_dimensions(strs, "band", values = str_c(., ".", st_dimensions(strs)$band$values))
    } else {
      ## composed clim data source = CRU + MODIS
      pthfl1 <- str_subset(files_tif, "MODIS")
      pthfl2 <- str_subset(files_tif, "CRU")
      strs1 <- read_stars(pthfl1, RasterIO = list(bands = 1:4))
      strs2 <- read_stars(pthfl2, RasterIO = list(bands = 5:8))
      strs <- c(strs1, strs2, along = "band")
      st_dimensions(strs)$band$values <- str_c("MODIS.", st_dimensions(strs)$band$values)
    }
    return(strs)
  })
  strs_clim <- do.call(c, c(... = strss, along = "band"))

  ## fire / herbivory scenarios
  file_fireherb_sce <- files_tif %>% str_subset("ER_fire_herb")
  strs_fireherb_sce <- read_stars(file_fireherb_sce)
  names(strs_fireherb_sce) <- "herbfire_sce"
  fireherb_sce_data_names <- st_dimensions(strs_fireherb_sce)$band$values

  ## climate scenarios
  files_clim_sce <- files_tif %>% str_subset("NEX_2050")
  strss <- map(files_clim_sce, ~ {
    strs <- read_stars(., RasterIO = list(bands = 1:8))
    . <- basename(.) %>% path_ext_remove()
    strs <- st_set_dimensions(strs, "band", values = str_c(., ".", st_dimensions(strs)$band$values))
    return(strs)
  })
  strs_clim_sce <- do.call(c, c(... = strss, along = "band"))

  ### scenarios + soilfire and clim
  ## common bbox
  bbxs <- cbind(
    soilfire = st_bbox(strs_soilfire),
    clim = st_bbox(strs_clim),
    fireherb_sce = st_bbox(strs_fireherb_sce),
    clim_sce = st_bbox(strs_clim_sce)
  ) %>%
    t() %>%
    as_tibble()
  nwbbx <- c(
    xmin = max(bbxs$xmin), ymin = max(bbxs$ymin),
    xmax = min(bbxs$xmax), ymax = min(bbxs$ymax)
  ) %>%
    st_bbox() %>%
    st_set_crs(st_crs(4326))
  ## together
  strs_var <- c(
    strs_soilfire %>% slice("band", -6) %>% st_crop(nwbbx),
    strs_clim %>% st_crop(nwbbx),
    strs_fireherb_sce %>% st_crop(nwbbx),
    strs_clim_sce %>% st_crop(nwbbx),
    along = "band"
  )
  var_data_names <- st_dimensions(strs_var)$band$values %>% as.character()
  ## to points + template
  pts_var <- strs_var %>%
    split(f = "band") %>%
    st_as_sf(as_points = T, na.rm = F) %>%
    mutate(id = 1:nrow(.))
  pts_template <- pts_var %>%
    select(geometry, id) %>%
    bind_cols(st_coordinates(.))
  pts_var_y <- pts_var %>%
    bind_cols(st_coordinates(.)) %>%
    filter(Y == unique(Y)[1]) %>%
    select(-X, -Y)
  pts_var_x <- pts_var %>%
    bind_cols(st_coordinates(.)) %>%
    filter(X == unique(X)[1]) %>%
    select(-X, -Y)
  pts_var %<>% na.omit()
  pts_var %<>% bind_rows(pts_var_x, pts_var_y) %>% unique()
}

##### WDPA data ----
files_wdpa <- list.files("RawData/WDPA", full.names = T, pattern = "\\.shp$", recursive = T) %>% str_subset("polygon")
file_wdpa <- "DataPreparation/WDPA.gpkg"
if (F) {
  cat("\nPreparation of WDPA data", "...")
  wdpa <- files_wdpa %>%
    map_dfr(~ read_sf(.))
  wdpa %<>% st_make_valid()
  wdpa %<>% filter(st_is_valid(geometry))
  write_sf(wdpa, file_wdpa, delete_dsn = T)
}

##### Photointerpretation data (1 on 2) ----
file_photointer <- "RawData/db_all_fin_20_10_2021.csv"
if (F) {
  cat("\nPhotointerpretation data (1 on 2)", "...")
  ### get points from disk
  d <- read_delim(file_photointer, delim = ",", show_col_types = FALSE) %>%
    select(-X, -...1) %>%
    na.omit()

  ### duplicated points?
  d %<>% group_by(location_x, location_y) %>%
    mutate(cnt = n()) %>%
    ungroup()
  any(d$cnt > 1) # ok no
  d %<>% select(-cnt)

  ### dataframe to sf
  pts <- st_as_sf(d, coords = c("location_x", "location_y"), crs = 4326)

  ### add WDPA categories
  wdpa <- read_sf(file_wdpa)
  wdpa %<>% mutate(area = st_area(geom) %>% as.numeric())
  pts %<>% st_join(wdpa[c("IUCN_CAT", "area")], left = T)

  ### save on disk
  saveRDS(pts, "DataPreparation/pts.Rdata")
  write_sf(pts, "DataPreparation/pts.gpkg", delete_dsn = T)
}

##### Photointerpretation data (2 on 2) ----
if (F) {
  cat("\nPhotointerpretation data (2 on 2)", "...")
  pts2 <- readRDS("DataPreparation/pts.Rdata")

  ### WDPA categories: subsets + new field 'cat'
  pts2 %<>% group_by(geometry) %>% arrange(desc(area))
  table(pts2 %>% group_by(geometry) %>% summarize(n = n()) %>% pull(n))
  pts2 %<>% slice_head(n = 1) %>% ungroup()
  table(pts2 %>% group_by(geometry) %>% summarize(n = n()) %>% pull(n))
  table(pts2$IUCN_CAT, useNA = "always")
  pts2 %<>% mutate(cat = case_when(
    IUCN_CAT %in% c("Ia", "Ib", "II", "III") ~ 0,
    IUCN_CAT %in% c("IV", "V", "VI") ~ 1,
    T ~ 2
  ))
  table(pts2$cat, useNA = "always")

  ### keep 2000 points in total for Hyperarid lands
  table(pts2$dryland_category, pts2$cat, useNA = "always")
  good <- sum(pts2$cat %in% c(0, 1) & pts2$dryland_category == "Hyperarid")
  hpts2 <- pts2 %>%
    filter(dryland_category == "Hyperarid" & cat == 2) %>%
    sample_n(2000 - good)
  hpts2$cat <- rep(0:1, length.out = 2000 - good)
  pts2 <- bind_rows(pts2, hpts2)
  table(pts2$dryland_category, pts2$cat, useNA = "always")

  ### subset cat
  pts2 %<>% filter(cat %in% c(0, 1))

  ### add continent
  xtrct <- read_stars(file_continent) %>% st_extract(pts2)
  pts2 %<>% bind_cols(xtrct %>% st_drop_geometry()) %>% rename(continent = continent_025.tif)
  ### regroup + subset + new field 'cont'
  table(pts2$continent, useNA = "always")
  pts2 %<>% mutate(continent = ifelse(continent == 5, 3, continent))
  pts2 %<>% filter(!continent %in% c(0, 7))
  pts2 %<>% mutate(cont = (continent %>% as.factor() %>% as.integer()) - 1)
  table(pts2$cont, useNA = "always")

  ### add soil_fire variables
  xtrct <- strs_soilfire %>%
    st_extract(pts2) %>%
    st_as_sf()
  pts2 %<>% bind_cols(xtrct %>% st_drop_geometry())

  ### add herbivore variables
  xtrct <- strs_herb %>%
    st_extract(pts2) %>%
    st_as_sf()
  pts2 %<>% bind_cols(xtrct %>% st_drop_geometry())

  ### add clim variables
  xtrct <- strs_clim %>%
    st_extract(pts2) %>%
    st_as_sf() %>%
    st_drop_geometry()
  pts2 %<>% bind_cols(xtrct)

  #### cover proportions
  ### sum of proportions != 100?
  pts2 %<>% mutate(summins = rowSums(across(min_tree_c:min_ground)))
  any(pts2$summins != 100) # no ok!
  pts2 %<>% select(-summins)
  ### proportions in decimal
  pts2 %<>% mutate(across(c(min_tree_c:min_ground), ~ . / 100))
  ### subsets land use and cover proportions
  table(pts2$calculated_final_land_use_label, useNA = "always")
  pts2 %<>% filter(!calculated_final_land_use_label %in% c("Cropland", "Settlement"))
  table(pts2$calculated_final_land_use_label, useNA = "always")
  pts2 %<>% filter(min_crop_c == 0 & min_infra_cover == 0)
  ### new grass + shrub proportion
  pts2 %<>% mutate(min_grasshrub = min_grass + min_shrub_c)

  ### save on disk
  saveRDS(pts2, "DataPreparation/pts2.Rdata")
  write_sf(pts2, "DataPreparation/pts2.gpkg", delete_dsn = T)
}
pts <- readRDS("DataPreparation/pts2.Rdata")
pts %<>% mutate(id = 1:n())
clnms <- str_replace_all(names(pts), "min_", "")
clnms <- str_replace_all(clnms, "_c", "")
names(pts) <- clnms
pts %<>% na.omit(pts)

##### Lows and Highs for normalization ----
if (F) {
  cat("\nLows and Highs for normalization", "...")
  all_input_data_names <- c(soilfire_data_names, herb_data_names, clim_data_names, "cat")
  input_data <- pts %>%
    st_drop_geometry() %>%
    select(all_of(all_input_data_names))
  lows <- apply(input_data, 2, function(.) {
    min(.) # quantile(., 0.01)
  })
  saveRDS(lows, "DataPreparation/lows.Rdata")
  highs <- apply(input_data, 2, function(.) {
    max(.) # quantile(., 0.99)
  })
  saveRDS(highs, "DataPreparation/highs.Rdata")
}
