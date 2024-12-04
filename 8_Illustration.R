####### All predictions ----
file_sqlite <- "Maps/all_combinations.sqlite"
dir.create("Maps/prediction_maps/scenarios")
con <- dbConnect(SQLite(), dbname = file_sqlite)

####### Custom prediction example ----
cat("\n\nCustom  prediction", "...")
# ER <- read_sf("RawData\\tnc_terr_ecoregions.shp")
percs <- c("perc05", "perc25", "median", "perc75", "perc95")
percs <- str_c(percs, "_ER_")
percs_herb <- str_c(percs[1:5], "herb")
percs_fire <- str_c(percs[1:5], "fire")
prd <- custom_predict(
  clim_data_source = c(clim_data_sources, "NEX_2050")[6],
  clim_sce = c("", "ssp2", "ssp5")[1],
  perc_herb = percs_herb[3],
  perc_fire = percs_fire[3],
  cat = c(0, 1)[1],
  BurnDate = NA,
  biomass = NA,
  intake = NA,
  litter = NA
)
# prd2 <- prd %>% st_join(ER)
# summary(prd2)
# summary(prd2 %>% select(tree))
# plot(prd2["ECO_NAME"])

####### Alternative covers (examples)  ----
### data preparation ----
ER <- read_sf("RawData\\tnc_terr_ecoregions.shp")
ER_2 <- ER[ER$ECO_NAME %in% "Northeast Siberian Taiga", "ECO_NAME"]
ER_3 <- ER[ER$ECO_NAME %in% "West Sudanian Savanna", "ECO_NAME"]
ER_4 <- ER[ER$ECO_NAME %in% "Dinaric Mountains Mixed Forests", "ECO_NAME"]
ER_5 <- ER[ER$ECO_NAME %in% "Cerrado", "ECO_NAME"]
if (F) {
  ### ER_2 ----
  prd <- custom_predict(
    clim_data_source = "NEX",
    clim_sce = "",
    perc_herb = "median_ER_herb",
    perc_fire = "median_ER_fire",
    cat = 0,
    BurnDate = NA,
    biomass = NA,
    intake = NA,
    litter = NA
  )
  prd_ER2_lowHERB_lowFire_hist <- st_join(ER_2, prd) %>%
    select(all_of(c("id", target_data_names)))
  
  prd <- custom_predict(
    clim_data_source = "NEX",
    clim_sce = "ssp5",
    perc_herb = "median_ER_herb",
    perc_fire = "median_ER_fire",
    cat = 0,
    BurnDate = 0,
    biomass = NA,
    intake = NA,
    litter = NA
  )
  prd_ER2_lowHERB_lowFire_ssp5 <- st_join(ER_2, prd) %>%
    select(all_of(c("id", target_data_names)))
  
  prd <- custom_predict(
    clim_data_source = "NEX",
    clim_sce = "ssp5",
    perc_herb = "median_ER_herb",
    perc_fire = "median_ER_fire",
    cat = 0,
    BurnDate = 0,
    biomass = 30000,
    intake = NA,
    litter = NA
  )
  prd_ER2_highHERB_lowFire_ssp5 <- st_join(ER_2, prd) %>%
    select(all_of(c("id", target_data_names)))
  
  prd <- custom_predict(
    clim_data_source = "NEX",
    clim_sce = "",
    perc_herb = "median_ER_herb",
    perc_fire = "median_ER_fire",
    cat = 0,
    BurnDate = 0,
    biomass = NA,
    intake = NA,
    litter = NA
  )
  prd_ER2_lowHERB_lowFire_hist <- st_join(ER_2, prd) %>%
    select(all_of(c("id", target_data_names)))
  
  prd <- custom_predict(
    clim_data_source = "NEX",
    clim_sce = "",
    perc_herb = "median_ER_herb",
    perc_fire = "median_ER_fire",
    cat = 0,
    BurnDate = 0,
    biomass = 30000,
    intake = NA,
    litter = NA
  )
  prd_ER2_highHERB_lowFire_hist <- st_join(ER_2, prd) %>%
    select(all_of(c("id", target_data_names)))
  
  
  prd <- custom_predict(
    clim_data_source = "NEX",
    clim_sce = "ssp2",
    perc_herb = "median_ER_herb",
    perc_fire = "median_ER_fire",
    cat = 0,
    BurnDate = 0,
    biomass = NA,
    intake = NA,
    litter = NA
  )
  prd_ER2_lowHERB_lowFire_ssp2 <- st_join(ER_2, prd) %>%
    select(all_of(c("id", target_data_names)))
  
  prd <- custom_predict(
    clim_data_source = "NEX",
    clim_sce = "ssp2",
    perc_herb = "median_ER_herb",
    perc_fire = "median_ER_fire",
    cat = 0,
    BurnDate = 0,
    biomass = 30000,
    intake = NA,
    litter = NA
  )
  prd_ER2_highHERB_lowFire_ssp2 <- st_join(ER_2, prd) %>%
    select(all_of(c("id", target_data_names)))
  
  ### ER_3 ----
  prd <- custom_predict(
    clim_data_source = "NEX",
    clim_sce = "",
    perc_herb = "median_ER_herb",
    perc_fire = "median_ER_fire",
    cat = 0,
    BurnDate = NA,
    biomass = NA,
    intake = NA,
    litter = NA
  )
  prd_ER3_lowHERB_lowFire_hist <- st_join(ER_3, prd) %>%
    select(all_of(c("id", target_data_names)))
  
  prd <- custom_predict(
    clim_data_source = "NEX",
    clim_sce = "ssp5",
    perc_herb = "median_ER_herb",
    perc_fire = "median_ER_fire",
    cat = 0,
    BurnDate = 0,
    biomass = NA,
    intake = NA,
    litter = NA
  )
  prd_ER3_lowFire_ssp5 <- st_join(ER_3, prd) %>%
    select(all_of(c("id", target_data_names)))
  
  prd <- custom_predict(
    clim_data_source = "NEX",
    clim_sce = "ssp5",
    perc_herb = "median_ER_herb",
    perc_fire = "median_ER_fire",
    cat = 0,
    BurnDate = NA,
    biomass = NA,
    intake = NA,
    litter = NA
  )
  prd_ER3_highFire_ssp5 <- st_join(ER_3, prd) %>%
    select(all_of(c("id", target_data_names)))
  
  prd <- custom_predict(
    clim_data_source = "NEX",
    clim_sce = "ssp5",
    perc_herb = "median_ER_herb",
    perc_fire = "median_ER_fire",
    cat = 0,
    BurnDate = 15,
    biomass = NA,
    intake = NA,
    litter = NA
  )
  prd_ER3_prescribedFire_ssp5 <- st_join(ER_3, prd) %>%
    select(all_of(c("id", target_data_names)))
  
  prd <- custom_predict(
    clim_data_source = "NEX",
    clim_sce = "",
    perc_herb = "median_ER_herb",
    perc_fire = "median_ER_fire",
    cat = 0,
    BurnDate = 0,
    biomass = NA,
    intake = NA,
    litter = NA
  )
  prd_ER3_lowFire_hist <- st_join(ER_3, prd) %>%
    select(all_of(c("id", target_data_names)))
  
  prd <- custom_predict(
    clim_data_source = "NEX",
    clim_sce = "",
    perc_herb = "median_ER_herb",
    perc_fire = "median_ER_fire",
    cat = 0,
    BurnDate = NA,
    biomass = NA,
    intake = NA,
    litter = NA
  )
  prd_ER3_highFire_hist <- st_join(ER_3, prd) %>%
    select(all_of(c("id", target_data_names)))
  
  prd <- custom_predict(
    clim_data_source = "NEX",
    clim_sce = "",
    perc_herb = "median_ER_herb",
    perc_fire = "median_ER_fire",
    cat = 0,
    BurnDate = 15,
    biomass = NA,
    intake = NA,
    litter = NA
  )
  prd_ER3_prescribedFire_hist <- st_join(ER_3, prd) %>%
    select(all_of(c("id", target_data_names)))
  
  prd <- custom_predict(
    clim_data_source = "NEX",
    clim_sce = "ssp2",
    perc_herb = "median_ER_herb",
    perc_fire = "median_ER_fire",
    cat = 0,
    BurnDate = 0,
    biomass = NA,
    intake = NA,
    litter = NA
  )
  prd_ER3_lowFire_ssp2 <- st_join(ER_3, prd) %>%
    select(all_of(c("id", target_data_names)))
  
  prd <- custom_predict(
    clim_data_source = "NEX",
    clim_sce = "ssp2",
    perc_herb = "median_ER_herb",
    perc_fire = "median_ER_fire",
    cat = 0,
    BurnDate = NA,
    biomass = NA,
    intake = NA,
    litter = NA
  )
  prd_ER3_highFire_ssp2 <- st_join(ER_3, prd) %>%
    select(all_of(c("id", target_data_names)))
  
  ### ER_4 ----
  prd <- custom_predict(
    clim_data_source = "NEX",
    clim_sce = "",
    perc_herb = "median_ER_herb",
    perc_fire = "median_ER_fire",
    cat = 0,
    BurnDate = NA,
    biomass = NA,
    intake = NA,
    litter = NA
  )
  prd_ER4_lowHERB_lowFire_hist <- st_join(ER_4, prd) %>%
    select(all_of(c("id", target_data_names)))
  
  prd <- custom_predict(
    clim_data_source = "NEX",
    clim_sce = "ssp5",
    perc_herb = "median_ER_herb",
    perc_fire = "median_ER_fire",
    cat = 0,
    BurnDate = NA,
    biomass = NA,
    intake = NA,
    litter = NA
  )
  prd_ER4_lowbiomass_ssp5 <- st_join(ER_4, prd) %>%
    select(all_of(c("id", target_data_names)))
  
  prd <- custom_predict(
    clim_data_source = "NEX",
    clim_sce = "ssp5",
    perc_herb = "median_ER_herb",
    perc_fire = "median_ER_fire",
    cat = 0,
    BurnDate = NA,
    biomass = 30000,
    intake = NA,
    litter = NA
  )
  prd_ER4_highbiomass_ssp5 <- st_join(ER_4, prd) %>%
    select(all_of(c("id", target_data_names)))
  
  prd <- custom_predict(
    clim_data_source = "NEX",
    clim_sce = "",
    perc_herb = "median_ER_herb",
    perc_fire = "median_ER_fire",
    cat = 0,
    BurnDate = NA,
    biomass = NA,
    intake = NA,
    litter = NA
  )
  prd_ER4_lowbiomass_hist <- st_join(ER_4, prd) %>%
    select(all_of(c("id", target_data_names)))
  
  prd <- custom_predict(
    clim_data_source = "NEX",
    clim_sce = "",
    perc_herb = "median_ER_herb",
    perc_fire = "median_ER_fire",
    cat = 0,
    BurnDate = NA,
    biomass = 30000,
    intake = NA,
    litter = NA
  )
  prd_ER4_highbiomass_hist <- st_join(ER_4, prd) %>%
    select(all_of(c("id", target_data_names)))
  
  prd <- custom_predict(
    clim_data_source = "NEX",
    clim_sce = "ssp2",
    perc_herb = "median_ER_herb",
    perc_fire = "median_ER_fire",
    cat = 0,
    BurnDate = NA,
    biomass = NA,
    intake = NA,
    litter = NA
  )
  prd_ER4_lowbiomass_ssp2 <- st_join(ER_4, prd) %>%
    select(all_of(c("id", target_data_names)))
  
  prd <- custom_predict(
    clim_data_source = "NEX",
    clim_sce = "ssp2",
    perc_herb = "median_ER_herb",
    perc_fire = "median_ER_fire",
    cat = 0,
    BurnDate = NA,
    biomass = 30000,
    intake = NA,
    litter = NA
  )
  prd_ER4_highbiomass_ssp2 <- st_join(ER_4, prd) %>%
    select(all_of(c("id", target_data_names)))
  
  ### ER_5 ----
  prd <- custom_predict(
    clim_data_source = "NEX",
    clim_sce = "",
    perc_herb = "median_ER_herb",
    perc_fire = "median_ER_fire",
    cat = 0,
    BurnDate = NA,
    biomass = NA,
    intake = NA,
    litter = NA
  )
  prd_ER5_lowHERB_lowFire_hist <- st_join(ER_5, prd) %>%
    select(all_of(c("id", target_data_names)))
  
  prd <- custom_predict(
    clim_data_source = "NEX",
    clim_sce = "ssp5",
    perc_herb = "median_ER_herb",
    perc_fire = "median_ER_fire",
    cat = 0,
    BurnDate = 0,
    biomass = NA,
    intake = NA,
    litter = NA
  )
  prd_ER5_lowFire_ssp5 <- st_join(ER_5, prd) %>%
    select(all_of(c("id", target_data_names)))
  
  prd <- custom_predict(
    clim_data_source = "NEX",
    clim_sce = "ssp5",
    perc_herb = "median_ER_herb",
    perc_fire = "median_ER_fire",
    cat = 0,
    BurnDate = NA,
    biomass = NA,
    intake = NA,
    litter = NA
  )
  prd_ER5_highFire_ssp5 <- st_join(ER_5, prd) %>%
    select(all_of(c("id", target_data_names)))
  
  prd <- custom_predict(
    clim_data_source = "NEX",
    clim_sce = "ssp5",
    perc_herb = "median_ER_herb",
    perc_fire = "median_ER_fire",
    cat = 0,
    BurnDate = 10,
    biomass = NA,
    intake = NA,
    litter = NA
  )
  prd_ER5_prescribedFire_ssp5 <- st_join(ER_5, prd) %>%
    select(all_of(c("id", target_data_names)))
  
  
  prd <- custom_predict(
    clim_data_source = "NEX",
    clim_sce = "",
    perc_herb = "median_ER_herb",
    perc_fire = "median_ER_fire",
    cat = 0,
    BurnDate = 0,
    biomass = NA,
    intake = NA,
    litter = NA
  )
  prd_ER5_lowFire_hist <- st_join(ER_5, prd) %>%
    select(all_of(c("id", target_data_names)))
  
  prd <- custom_predict(
    clim_data_source = "NEX",
    clim_sce = "",
    perc_herb = "median_ER_herb",
    perc_fire = "median_ER_fire",
    cat = 0,
    BurnDate = NA,
    biomass = NA,
    intake = NA,
    litter = NA
  )
  prd_ER5_highFire_hist <- st_join(ER_5, prd) %>%
    select(all_of(c("id", target_data_names)))
  
  prd <- custom_predict(
    clim_data_source = "NEX",
    clim_sce = "",
    perc_herb = "median_ER_herb",
    perc_fire = "median_ER_fire",
    cat = 0,
    BurnDate = 10,
    biomass = NA,
    intake = NA,
    litter = NA
  )
  prd_ER5_prescribedFire_hist <- st_join(ER_5, prd) %>%
    select(all_of(c("id", target_data_names)))
  
  
  ### Save rds ----
  df_ER2 <- cbind(prd_ER2_highHERB_lowFire_ssp5, prd_ER2_lowHERB_lowFire_ssp5, prd_ER2_lowHERB_lowFire_hist, prd_ER2_highHERB_lowFire_hist) %>%
    st_drop_geometry() %>%
    select(-contains("geometry"))
  df_ER3 <- cbind(prd_ER3_highFire_ssp5, prd_ER3_lowFire_ssp5, prd_ER3_lowHERB_lowFire_hist, prd_ER3_prescribedFire_ssp5, prd_ER3_highFire_hist, prd_ER3_lowFire_hist, prd_ER3_prescribedFire_hist) %>%
    st_drop_geometry() %>%
    select(-contains("geometry"))
  df_ER4 <- cbind(prd_ER4_highbiomass_ssp5, prd_ER4_lowbiomass_ssp5, prd_ER4_lowHERB_lowFire_hist, prd_ER4_highbiomass_hist, prd_ER4_lowbiomass_hist) %>%
    st_drop_geometry() %>%
    select(-contains("geometry"))
  df_ER5 <- cbind(prd_ER5_highFire_ssp5, prd_ER5_lowFire_ssp5, prd_ER5_lowHERB_lowFire_hist, prd_ER5_prescribedFire_ssp5, prd_ER5_highFire_hist, prd_ER5_lowFire_hist, prd_ER5_prescribedFire_hist) %>%
    st_drop_geometry() %>%
    select(-contains("geometry"))
  saveRDS(df_ER2, file = "df_ER2.rds")
  saveRDS(df_ER3, file = "df_ER3.rds")
  saveRDS(df_ER4, file = "df_ER4.rds")
  saveRDS(df_ER5, file = "df_ER5.rds")
} else {
  df_ER2_all <- readRDS("df_ER2.rds")
  df_ER3_all <- readRDS("df_ER3.rds")
  df_ER4_all <- readRDS("df_ER4.rds")
  df_ER5_all <- readRDS("df_ER5.rds")
}

### graphs of examples ----
library(gridExtra)
library(grid)

df_ER2 <- df_ER2_all %>% select(id.1, tree, tree.1, tree.2)
names(df_ER2) <- c("id", "High herbivory + CC", "present herbivory + CC", "present")
pivot_df_ER2 <- df_ER2 %>% pivot_longer(cols = names(df_ER2)[2:ncol(df_ER2)], names_to = "Scenario")
pivot_df_ER2 %<>% mutate(Scenario = factor(Scenario, levels = c("High herbivory + CC", "present herbivory + CC", "present"))) %>% st_drop_geometry()
p_df_ER2 <- ggplot(data = pivot_df_ER2, aes(x = value, fill = Scenario)) +
  geom_density(alpha = .5) +
  scale_fill_manual(values = c("darkgreen", "lightgreen", "grey")) +
  ggtitle(ER_2$ECO_NAME) +
  xlab("Tree Cover") +
  ylab("Density") +
  xlim(0, 1)

df_ER3 <- df_ER3_all %>% select(id, grasshrub.2, grasshrub, grasshrub.1, grasshrub.5, grasshrub.3, grasshrub.6)
names(df_ER3) <- c("id", "present", "present fire + CC", "fire exclusion", "fire exclusion + CC", "prescribed fire", "prescribed fire + CC")
pivot_df_ER3 <- df_ER3 %>% pivot_longer(cols = names(df_ER3)[2:ncol(df_ER3)], names_to = "Scenario")
pivot_df_ER3 %<>% mutate(Scenario = factor(Scenario, levels = c("present", "present fire + CC", "fire exclusion", "fire exclusion + CC", "prescribed fire", "prescribed fire + CC"))) %>% st_drop_geometry()
p_df_ER3 <- ggplot(data = pivot_df_ER3, aes(x = value, fill = Scenario)) +
  geom_density(alpha = .5) +
  scale_fill_manual(values = c("grey50", "grey75", "white", "bisque1", "coral", "lightcoral")) +
  ggtitle(ER_3$ECO_NAME) +
  xlab("Short Vegetation Cover") +
  ylab("Density") +
  xlim(0, 1)

df_ER4 <- df_ER4_all %>% select(id, tree, tree.1, tree.2)
names(df_ER4) <- c("id", "High herbivory + CC", "present herbivory + CC", "present")
pivot_df_ER4 <- df_ER4 %>% pivot_longer(cols = names(df_ER4)[2:ncol(df_ER4)], names_to = "Scenario")
pivot_df_ER4 %<>% mutate(Scenario = factor(Scenario, levels = c("High herbivory + CC", "present herbivory + CC", "present"))) %>% st_drop_geometry()
p_df_ER4 <- ggplot(data = pivot_df_ER4, aes(x = value, fill = Scenario)) +
  geom_density(alpha = .5) +
  scale_fill_manual(values = c("darkgreen", "lightgreen", "grey")) +
  ggtitle(ER_4$ECO_NAME) +
  xlab("Tree Cover") +
  ylab("Density") +
  xlim(0, 1)

df_ER5 <- df_ER5_all %>% select(id, grasshrub.2, grasshrub, grasshrub.1, grasshrub.5, grasshrub.3, grasshrub.6)
names(df_ER5) <- c("id", "present", "present fire + CC", "fire exclusion", "fire exclusion + CC", "prescribed fire", "prescribed fire + CC")
pivot_df_ER5 <- df_ER5 %>% pivot_longer(cols = names(df_ER5)[2:ncol(df_ER5)], names_to = "Scenario")
pivot_df_ER5 %<>% mutate(Scenario = factor(Scenario, levels = c("present", "present fire + CC", "fire exclusion", "fire exclusion + CC", "prescribed fire", "prescribed fire + CC"))) %>% st_drop_geometry()
p_df_ER5 <- ggplot(data = pivot_df_ER5, aes(x = value, fill = Scenario)) +
  geom_density(alpha = .5) +
  scale_fill_manual(values = c("grey50", "grey75", "white", "bisque1", "coral", "lightcoral")) +
  ggtitle(ER_5$ECO_NAME) +
  xlab("Short Vegetation Cover") +
  ylab("Density") +
  xlim(0, 1)

p <- grid.arrange(p_df_ER2, p_df_ER3, p_df_ER4, p_df_ER5)
ggsave(str_c("ModelEvaluation/", "alternative_potentials_examples.svg"), p, width = 25, height = 21, units = "cm")


### maps of examples ----
# ER_2 "Northeast Siberian Taiga"
# ER_3 "West Sudanian Savanna"
# ER_4 "Dinaric Mountains Mixed Forests"
# ER_5 "Cerrado"
strs <- read_stars("Maps/prediction_maps/global_map_1Band_perCover_Mean.tif")
er_ <- ER[ER$ECO_NAME %in% c("Northeast Siberian Taiga", "West Sudanian Savanna", "Dinaric Mountains Mixed Forests", "Cerrado"), "ECO_NAME"]
er_ %<>% mutate(ECO_NAME = factor(ECO_NAME, levels = c("Northeast Siberian Taiga", "West Sudanian Savanna", "Dinaric Mountains Mixed Forests", "Cerrado")), er = ECO_NAME %>% as.integer())
# plot(er_)
strs_er <- er_ %>%
  select(-ECO_NAME) %>%
  st_rasterize(template = strs %>% slice("band", 1))
# plot(strs_er)
strs %<>% split()
strs$er <- strs_er$er
strs %<>% merge()
# strs%<>%merge()
# strs2=c(strs%>%split(),strs_er)%>%merge()
# strs2=c(strs,strs_er)%>%merge()
eer <- 2
plts <- map(1:4, function(eer) {
  print(eer)
  
  strs2 <- strs
  
  a <- strs2[, , , 4] == eer
  plta <- ggplot() +
    geom_stars(data = a, na.action = na.omit) +
    coord_equal() +
    theme_void() +
    theme(legend.position = "top") +
    labs(fill = er_ %>% filter(er == eer) %>% pull(ECO_NAME)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))
  
  # b <- st_crop(strs2, er_ %>% filter(er == eer))
  b <- strs2[er_ %>% filter(er == eer)]
  b <- b[, , , 1:3]
  # pltb <- ggplot() +
  #   geom_stars(data = b, na.action = na.omit) +
  #   scale_fill_viridis_c(limits = c(0, 1)) +
  #   facet_wrap(~attributes, nrow = 1, labeller = as_labeller(c(
  #     grasshrub = "short vegetation",
  #     ground = "bare ground",
  #     tree = "tree"
  #   ))) +
  #   coord_equal() +
  #   theme_void() +
  #   theme(legend.position = "top") +
  #   labs(fill = "proportion") +
  #   scale_x_continuous(expand = c(0, 0)) +
  #   scale_y_continuous(expand = c(0, 0))
  
  # ER_2 "Northeast Siberian Taiga"
  # ER_3 "West Sudanian Savanna"
  # ER_4 "Dinaric Mountains Mixed Forests"
  # ER_5 "Cerrado"
  if (eer == 1) {
    pff <- pivot_df_ER2
  }
  if (eer == 2) {
    pff <- pivot_df_ER3
  }
  if (eer == 3) {
    pff <- pivot_df_ER4
  }
  if (eer == 4) {
    pff <- pivot_df_ER5
  }
  
  pff %<>% pivot_wider(names_from = Scenario)
  print(summary(pff))
  pff %<>% inner_join(pts_var["id"]) %>%
    select(-id) %>%
    st_as_sf()
  
  st_bbox(b)
  st_bbox(pff)
  
  pt <- pff %>%
    st_rasterize(template = b %>% slice("attributes", 1)) %>%
    merge()
  pltt <- ggplot() +
    geom_stars(data = pt, na.action = na.omit) +
    scale_fill_viridis_c() + # limits = c(0, 1)) +
    facet_wrap(~attributes, ncol = 3) +
    coord_equal() +
    theme_void() +
    theme(legend.position = "top") +
    labs(fill = ifelse(eer %in% c(1, 2), "short vegegation", "tree")) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))
  
  # plt = arrangeGrob(plta, pltb, pltt, ncol = 1)
  # plot(plt)
  plt <- arrangeGrob(plta, pltt, ncol = 1)
  # plot(plt)
  
  ggsave(
    filename = str_c("ModelEvaluation/", str_c("alternative_potentials_examples_maps_", eer, ".svg")), plot = plt, width = 8, height = 8
  )
  
  return(plt)
})
PLT <- do.call(grid.arrange, args = c(... = plts, ncol = 2))
ggsave(
  filename = str_c("ModelEvaluation/", "alternative_potentials_examples_maps_all.svg"), plot = PLT, width = 2 * 8, height = 2 * 8
)



###### Features domain ----
### pts and pts_var => together with ER ----
ER <- read_sf("RawData\\tnc_terr_ecoregions.shp")
pts_ER <- st_join(pts, ER) %>% mutate(source = "pts")
pts_var_ER <- st_join(pts_var, ER) %>% mutate(source = "pts_var")
# boxplot(pts_var_ER$NEX.Mean_temp ~ pts_var_ER$WWF_MHTNAM)
pts_together <- bind_rows(pts_ER, pts_var_ER)
pts_together %<>% st_drop_geometry() %>% filter(!is.na(WWF_MHTNAM))

### variables selection ----
## 4 clim variables
# vars_ <- c("source", "WWF_MHTNAM", str_c("NEX.", clim_data_variables))
## more variables (minus 'cat')
## but biomass, litter, intake and burndate/firefreq can not be compared
## (as no data outside protected areas ==> scenarios)
vars_ <- c("source", "WWF_MHTNAM", str_c("NEX.", clim_data_variables), soilfire_data_names, herb_data_names, str_c("NEX.", clim_data_variables))
vars_ <- vars_[!vars_ %in% c("BurnDate", herb_data_names)]

### boxplot ----
pts_together %<>% select(all_of(vars_))
pts_together2 <- pts_together %>% pivot_longer(cols = vars_[-1:-2])
pts_together2$name <- pts_together2$name %>%
  str_replace("NEX.", "") %>%
  str_replace("sd", "Sd")
plt <- ggplot(data = pts_together2) +
  geom_boxplot(aes(x = WWF_MHTNAM, y = value, color = source)) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(x = "", y = "", color = "data source") +
  facet_wrap(. ~ name, scale = "free_y") +
  scale_color_manual(labels = c("World", "NaturalReserve"), values = c("blue", "red")) +
  theme(legend.position = "top", axis.text = element_text(size = 8))
ggsave(str_c("ModelEvaluation/", "var_domain.png"), plt, width = 18, height = 25, units = "cm")
ggsave(str_c("ModelEvaluation/", "var_domain.svg"), plt, width = 18, height = 25, units = "cm")


### convex hull ----
## scale or not => same results
pts_together %<>% mutate(across(contains("NEX."), scale))

pts_ <- pts_together %>%
  filter(source == "pts") %>%
  st_drop_geometry() %>%
  select(contains("NEX.")) %>%
  as.matrix() %>%
  unique()
pts_var_ <- pts_together %>%
  filter(source == "pts_var") %>%
  st_drop_geometry() %>%
  select(contains("NEX.")) %>%
  as.matrix() %>%
  unique()

## points of pts not in pts_var hull
require(geometry)
hull <- convhulln(pts_var_)
inhull <- inhulln(hull, pts_)
pts_2 <- pts_ %>%
  as_tibble() %>%
  mutate(inhull = !!inhull %>% as.logical())
# summary(pts_2)
pts_2 %>%
  group_by(inhull) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  mutate(prop = n / n()) %>%
  select(inhull, n, prop) %>%
  unique()

## points of pts not in pts_var hull
hull <- convhulln(pts_)
inhull <- inhulln(hull, pts_var_)
pts_var_2 <- pts_var_ %>%
  as_tibble() %>%
  mutate(inhull = !!inhull %>% as.logical())
# summary(pts_var_2)
pts_var_2 %>%
  group_by(inhull) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  mutate(prop = n / n()) %>%
  select(inhull, n, prop) %>%
  unique()

table(pts_var_2$inhull)[1] / (table(pts_var_2$inhull)[1] + table(pts_var_2$inhull)[2])
## --> 4,1% of data are outside of the convex hull

##### Additional evaluation: CAST - Meyer's methods ----
if (F) {
  percs <- c("perc05", "perc25", "median", "perc75", "perc95")
  percs <- str_c(percs, "_ER_")
  percs_herb <- str_c(percs[1:5][3], "herb")
  percs_fire <- str_c(percs[1:5][3], "fire")
  crss <- tidyr::crossing(
    perc_herb = percs_herb,
    perc_fire = percs_fire
  )

  # require(FNN)
  require(CAST)
  require(furrr)

  # clim_data_source <- "NEX"
  clim_data_source <- clim_data_sources[1]
  # for (clim_data_source in clim_data_sources) {
  plan(multisession, workers = 4L)
  null <- future_map(clim_data_sources, function(clim_data_source) {
    # null <- map(clim_data_sources, function(clim_data_source) {
    cat("\n  ", clim_data_source, "...")

    ### data names
    input_data_names <- c(
      soilfire_data_names,
      herb_data_names,
      clim_data_names %>% str_subset(clim_data_source),
      "cat"
    )

    ### pts_var2
    to_be_named <- c(herb_data_names, "BurnDate")
    names <- c(
      str_replace(crss[1, "perc_herb"], "herb", herb_data_names),
      str_replace(crss[1, "perc_fire"], "fire", "firefreq")
    )
    lookup <- names %>% setNames(to_be_named)
    pts_var2 <- pts_var %>%
      mutate(cat = 0) %>%
      rename(all_of(lookup)) %>%
      select(all_of(c(input_data_names, "id")))
    pts_var2 %<>% mutate(BurnDate = BurnDate * 19)

    #### code using CAST package with adding variable importance  ----
    ## out of memory if not transforming 4326 to 4087
    ## also out of memory if  clustering = "hierarchical" ==> "kmeans"
    if (T) {
      ## code to be kept but not used (back-up)
      if (F) {
        # tpoints <- pts_geom
        # predpoints <- pts_var_geom
        # islonglat <- sf::st_is_longlat(tpoints)
        #
        # ## too long!
        # # nr=map_int(1:nrow(pts_geom),function(r){
        # #   suppressMessages(st_nearest_feature(pts_geom[r,],pts_geom[-r,]))
        # # },.progress=T)
        #
        # require(nngeo)
        # tst <- nngeo::st_nn(tpoints, tpoints, k = 2, returnDist = T, parallel = parallel::detectCores() - 1)
        # nn <- cbind(nn = unlist(transpose(tst$nn)[[2]]), dist = unlist(transpose(tst$dist)[[2]])) %>% as_tibble()
        # Gj <- nn %>% pull(dist)
        #
        # tst2 <- nngeo::st_nn(predpoints, tpoints, k = 1, returnDist = T, parallel = parallel::detectCores() - 1)
        # nn2 <- cbind(nn = unlist(tst2$nn), dist = unlist(tst2$dist)) %>% as_tibble()
        # Gij <- nn2 %>% pull(dist)

        # ## sample to sample
        # nn <- get.knnx(ll %>% filter(source == "pts") %>% select(-source), ll %>% filter(source == "pts") %>% select(-source), k = 2)
        # # str(nn,1)
        # stos <- tibble(num = nn[[1]][, 2], dist = nn[[2]][, 2])
        # ggplot(stos, aes(x = dist, after_stat(ndensity))) +
        #   geom_histogram(bins = 1000)
        #
        # # ## sample to prediction
        # # nn <- get.knnx(ll %>% filter(source == "pts") %>% select(-source), ll %>% filter(source == "pts_var") %>% select(-source), k = 2)
        # # stop <- tibble(num = nn[[1]][, 2], dist = nn[[2]][, 2])
        # # ggplot(stop, aes(x = dist, after_stat(ndensity))) +
        # #   geom_histogram(bins = 1000)
        #
        # ## prediction to sample
        # nn <- get.knnx(
        #   ll %>% filter(source == "pts_var") %>% select(-source),
        #   ll %>% filter(source == "pts") %>% select(-source),
        #   k = 1
        # )
        # ptos <- tibble(num = nn[[1]][, 1], dist = nn[[2]][, 1])
        # ggplot(ptos, aes(x = dist, after_stat(ndensity))) +
        #   geom_histogram(bins = 1000)
      }

      ### geo (one time is sufficient)
      if (!file.exists(str_c("ModelEvaluation/", "knndm_folds_geo.svg"))) {
        cat("\n    ", "knndm_folds_geo", "(one time only) ...")
        pts_geom <- pts %>%
          select(geometry) %>%
          unique()
        pts_var_geom <- pts_var2 %>%
          select(geometry) %>%
          unique()
        ## 3395 is another possible projection
        knndm_folds_geo <- knndm(
          tpoints = pts_geom %>% st_set_crs(4326) %>% st_transform(4087),
          predpoints = pts_var_geom %>% st_set_crs(4326) %>% st_transform(4087),
          space = c("geographical", "feature")[1],
          clustering = c("hierarchical", "kmeans")[2],
          k = 5
        )
        saveRDS(knndm_folds_geo, str_c("ModelEvaluation/", "knndm_folds_geo.Rdata"))
        plt <- plot(knndm_folds_geo, type = "simple", stat = "density") # To visualize densities rather than ECDFs
        ggsave(str_c("ModelEvaluation/", "knndm_folds_geo.svg"), plot = plt, width = 8, height = 8)
      }

      ### feature
      if (!file.exists(str_c("ModelEvaluation/", "knndm_folds_feat_", clim_data_source, ".svg"))) {
        cat("\n    ", "knndm_folds_feat", "...")

        ## variable importance (mean of all proportions)
        ## from APES_ModelEvaluation (PR[[vartokeep]] <- list(exp, mp, mpr))
        pr <- readRDS(str_c("ModelEvaluation/", "PR_", clim_data_source, ".Rdata"))
        prop <- proportion_names[1]
        vls <- map_dfr(proportion_names, function(prop) {
          pr[[prop]][[2]] %>%
            mutate(prop = !!prop)
        })
        vls %<>% group_by(variable, prop) %>% summarise_at("dropout_loss", .funs = list(min = min, mean = mean, max = max))
        vi <- vls %>%
          group_by(variable) %>%
          summarise(vi = mean(mean, na.rm = T)) %>%
          filter(str_sub(variable, 1, 1) != "_")
        print(vi)
        plt <- ggplot(data = vi) +
          geom_point(aes(y = vi, x = variable)) +
          theme(axis.text.x = element_text(angle = 90))
        print(plt)
        saveRDS(vi, str_c("ModelEvaluation/", "knndm_VI_", clim_data_source, ".Rdata"))

        ## pts weighted with vi
        pts_b <- pts %>%
          st_drop_geometry() %>%
          select(all_of(input_data_names))
        pts_c <- cbind(id = pts$id, pts_b) %>%
          as_tibble() %>%
          pivot_longer(-id, names_to = "variable")
        pts_c %<>% inner_join(vi) %>% mutate(weighted = value * vi)
        pts_d <- pts_c %>%
          select(id, variable, weighted) %>%
          pivot_wider(values_from = weighted, names_from = variable)
        pts_e <- pts_d %>%
          select(-id) %>%
          unique()

        ## pts_var weighted with vi
        pts_var_b <- pts_var2 %>%
          mutate(cat = 0) %>%
          st_drop_geometry() %>%
          select(all_of(input_data_names))
        pts_var_c <- cbind(id = pts_var$id, pts_var_b) %>%
          as_tibble() %>%
          pivot_longer(-id, names_to = "variable")
        pts_var_c %<>% inner_join(vi) %>% mutate(weighted = value * vi)
        pts_var_d <- pts_var_c %>%
          select(id, variable, weighted) %>%
          pivot_wider(values_from = weighted, names_from = variable)
        pts_var_e <- pts_var_d %>%
          select(-id) %>%
          unique()

        ## weighted pts&pts_var + scaling
        ll <- bind_rows(
          pts_e %>% mutate(source = "pts"),
          pts_var_e %>% mutate(source = "pts_var")
        ) %>% na.omit()
        ll %<>% mutate(across(where(is.numeric), scale))

        ## feature space knndm
        pts__ <- ll %>%
          filter(source == "pts") %>%
          select(all_of(input_data_names))
        pts_var__ <- ll %>%
          filter(source == "pts_var") %>%
          select(all_of(input_data_names))

        knndm_folds_feat <- knndm(
          tpoints = pts__,
          predpoints = pts_var__,
          space = c("geographical", "feature")[2],
          clustering = c("hierarchical", "kmeans")[2],
          k = 5
        )
        saveRDS(knndm_folds_feat, str_c("ModelEvaluation/", "knndm_folds_feat_", clim_data_source, ".Rdata"))
        # plot(knndm_folds_feat, type = "simple") # For more accessible legend labels
        plt <- plot(knndm_folds_feat, type = "simple", stat = "density") # To visualize densities rather than ECDFs
        ggsave(str_c("ModelEvaluation/", "knndm_folds_feat_", clim_data_source, ".svg"), plot = plt, width = 8, height = 8)
      }
    }

    return(NULL)
    # }, .progress = T)
  }, .progress = T, .options = furrr_options(scheduling = T))
  # plan(sequential)


  ### feat plot with all clim_data_sources
  clim_data_source <- clim_data_sources[1]
  datas <- map_dfr(clim_data_sources, function(clim_data_source) {
    knndm_folds_feat <- readRDS(str_c("ModelEvaluation/", "knndm_folds_feat_", clim_data_source, ".Rdata"))
    plt <- plot(knndm_folds_feat, type = "simple", stat = "density")
    dt <- plt$data
    dt$clim_data_source <- clim_data_source
    return(dt)
  })
  knndm_folds_feat <- readRDS(str_c("ModelEvaluation/", "knndm_folds_feat_", clim_data_source, ".Rdata"))
  plt <- plot(knndm_folds_feat, type = "simple", stat = "density")
  plt2 <- plt %+% datas
  plt2 <- plt2 + facet_wrap(~clim_data_source, nrow = 2) + xlim(0, 3) + xlab("feature space distances") +
    labs(title = "knndm in feature space", subtitle = "All explanatory variables - Median herb and fire models\n ('no fold' and 'no repetition')")
  ggsave(str_c("ModelEvaluation/", "knndm_folds_feat_all_clim_data_sources.svg"), plot = plt2, width = 8 * 3 / 1.8, height = 8 * 2 / 1.8)

  plt3 <- plt2
  # plt3$data$r = plt3$data$r#+0.000001
  plt3 <- plt3 + xlab("feature space distances (log10 scale)") + scale_x_log10(limits = c(0.001, 10), breaks = c(0.01, 0.01, 0.1, 1, 10), labels = c(0.01, 0.01, 0.1, 1, 10)) # scales::comma)
  ggsave(str_c("ModelEvaluation/", "knndm_folds_feat_all_clim_data_sources_2.svg"), plot = plt3, width = 8 * 3 / 1.8, height = 8 * 2 / 1.8)


  ### geo plot
  knndm_folds_geo <- readRDS(str_c("ModelEvaluation/", "knndm_folds_geo.Rdata"))
  plt <- plot(knndm_folds_geo, type = "simple", stat = "density")
  # plt$data$r <- log10(plt$data$r / 1000)
  plt$data$r <- plt$data$r / 1000
  plt$data %<>% filter(r >= 0.5 & r <= 10000)
  plt2 <- plt +
    # xlab("log10(km)")+xlim(0.5,3.5) +
    scale_x_log10() +
    xlab("geographic distances (km) (log10 scale)") +
    labs(title = "knndm in geographical space", subtitle = "Coordinates projected in EPSG:4087")
  ggsave(str_c("ModelEvaluation/", "knndm_folds_geo_2.svg"), plot = plt2, width = 8 * 1.4 / 1.6, height = 8 * 1.2 / 1.6)

  plt3 <- plt2 +
    # ggplot2::geom_density(adjust = 1.5, alpha = 0.5, stat = density", lwd = 0.3)
    # ggplot2::geom_density(adjust = 1.5, alpha = 0.5, stat = "density", lwd = 0.3,n=50, bw=0.1)
    ggplot2::geom_density(adjust = 1.5, alpha = 0.5, stat = "density", lwd = 0.3, bw = 0.1) +
    xlab("geographic distances (km) (log10 scale) ")
  plt3$layers <- plt3$layers[2]
  ggsave(str_c("ModelEvaluation/", "knndm_folds_geo_3.svg"), plot = plt3, width = 8 * 1.4 / 1.6, height = 8 * 1.2 / 1.6)
}

##### Correlation between predictive variables ----
require(corrplot)
require(ggcorrplot)
clim_data_source <- clim_data_sources[1]
plts <- map(clim_data_sources, function(clim_data_source) {
  input_data_names <- c(
    soilfire_data_names,
    herb_data_names,
    clim_data_names %>% str_subset(clim_data_source),
    "cat"
  )
  pts_ <- pts %>% select(all_of(input_data_names))
  names(pts_) <- names(pts_) %>% map_chr(function(vr) {
    ifelse(str_detect(vr, "\\."), str_split(vr, "\\.") %>% pluck(1, 2), vr)
  })
  pts_ %<>% st_drop_geometry()

  M <- cor(pts_)
  # plt <- corrplot(M)
  plt <- ggcorrplot(M, hc.order = TRUE, outline.col = "white") # , method = "circle")

  plt$data <- plt$data %>% mutate(clim_data_source)

  return(plt)
})
plt <- plts[[1]]
dt <- map_dfr(plts, function(x) {
  x$data
})
plt$data <- dt
plt2 <- plt + facet_wrap(~clim_data_source)
ggsave(str_c("ModelEvaluation/", "corrplot.svg"), plot = plt2, width = 16 * 1.4 / 1.6, height = 13 * 1.2 / 1.6)

##### Variable importance all together (==> VIs) ----
if (F) {
  clim_data_source <- clim_data_sources[1]
  VIs <- map_dfr(clim_data_sources, function(clim_data_source) {
    pr <- readRDS(str_c("ModelEvaluation/", "PR_", clim_data_source, ".Rdata"))
    prop <- proportion_names[1]
    vls <- map_dfr(proportion_names, function(prop) {
      pr[[prop]][[2]] %>%
        mutate(prop = !!prop)
    })
    vi <- vls %>%
      group_by(variable, prop) %>%
      summarise_at("dropout_loss", .funs = list(min = min, mean = mean, max = max)) %>%
      filter(str_sub(variable, 1, 1) != "_")
    vi %<>% mutate(clim_data_source = !!clim_data_source)
    # print(vi)

    return(vi)
  }, .progress = T)
  saveRDS(VIs, "ModelEvaluation/VIs.RData")
} else {
  VIs <- readRDS("ModelEvaluation/VIs.RData")
}
VIs2 <- VIs %>%
  rowwise() %>%
  mutate(variable2 = ifelse(str_detect(variable, "\\."), str_split(variable, "\\.") %>% pluck(1, 2), variable))
VIs2 %<>% mutate(prop = case_when(prop == "grasshrub" ~ "short vegetation", prop == "ground" ~ "bareground", prop == "tree" ~ "tree", T ~ NA))
table(VIs2$prop, useNA = "always")
plt <- ggplot(data = VIs2) +
  geom_bar(aes(x = variable2, y = mean), stat = "identity", fill = "skyblue", alpha = 0.7) +
  geom_errorbar(aes(x = variable2, ymin = min, ymax = max), width = 0.4, colour = "orange", alpha = 0.9, size = 1.3) +
  facet_wrap(clim_data_source ~ prop, nrow = length(clim_data_sources)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)) +
  xlab("Predictive variables") +
  ylab("Variable importance")
ggsave(str_c("ModelEvaluation/", "VI_together.svg"), plot = plt, width = 10 * 1.4 / 1.6, height = 13 * 1.2 / 1.6)

##### Loss during training all together (loss_tr) -----
clim_data_source <- clim_data_sources[1]
lst_fls <- list.files("ModelTraining", ".Rdata", full.names = T) %>%
  str_subset("dt_") %>%
  str_subset("_11", negate = T)
dtss <- map_dfr(clim_data_sources, function(clim_data_source) {
  fls <- lst_fls %>% str_subset(clim_data_source)
  fls <- fls[order(fls)]

  dts <- map_dfr(fls, function(fl) {
    dt <- readRDS(fl) %>% mutate(k = which(fls == fl))
  })
  dts %<>% mutate(clim_data_source = !!clim_data_source)

  return(dts)
}, .progress = T)

# ggplot(data=dtss%>%filter(ep %in% seq(0,3000,by=10))%>%filter(l_m=="loss"))+geom_point(aes(x=ep,y=value,group=k,color=t_v))+facet_wrap(clim_data_source~.)

plt <- ggplot(data = dtss %>% filter(ep %in% seq(0, 3000, by = 25)) %>% filter(l_m == "loss")) +
  geom_smooth(aes(x = ep, y = value, color = t_v, group = t_v), se = T, method = "loess") +
  # geom_point(aes(x = ep, y = value, color = t_v))+
  labs(color = "") +
  xlab("epoch") +
  ylab("MSE") +
  facet_wrap(clim_data_source ~ .)
ggsave(str_c("ModelEvaluation/", "loss_tr.svg"), plot = plt, width = 10 * 1.4 / 1.6, height = 8 * 1.2 / 1.6)

##### Areas affected by uncertainties & scen sensitivity ----
### based on "sd maps" ==> areas affected ----
# A <- 100
# B <- runif(1000)
# sol1 <- sd(B) * A
# sol2 <- sd(B * A)
# sol2 - sol1
# sol2 == sol1 ## ==> ok
if (T) {
  ### sources of variabiliy (one by one) ----
  rp_tif <- "Maps/prediction_maps/uncertain_model_alone_1Band_perCover_MeanThenSd.tif"
  dt <- read_stars(rp_tif) %>%
    st_as_sf() %>%
    st_transform(8857) %>%
    mutate(area = (st_area(.) %>% as.numeric()) / (10000 * 1000000)) %>%
    mutate(grasshrub = grasshrub * area, ground = ground * area, tree = tree * area)
  sm_rp <- dt %>%
    select(-area) %>%
    st_drop_geometry() %>%
    colSums()
  CI_rp <- ((sm_rp * 1.96) / (10^0.5)) %>% setNames(str_c("CI_", names(.)))

  k_fold_tif <- "Maps/prediction_maps/uncertain_spatial_alone_1Band_perCover_MeanThenSd.tif"
  dt <- read_stars(k_fold_tif) %>%
    st_as_sf() %>%
    st_transform(8857) %>%
    mutate(area = (st_area(.) %>% as.numeric()) / (10000 * 1000000)) %>%
    mutate(grasshrub = grasshrub * area, ground = ground * area, tree = tree * area)
  sm_k_fold <- dt %>%
    select(-area) %>%
    st_drop_geometry() %>%
    colSums()
  CI_k_fold <- ((sm_k_fold * 1.96) / (10^0.5)) %>% setNames(str_c("CI_", names(.)))

  clim_tif <- "Maps/prediction_maps/uncertain_clim_alone_1Band_perCover_Sd.tif"
  dt <- read_stars(clim_tif) %>%
    st_as_sf() %>%
    st_transform(8857) %>%
    mutate(area = (st_area(.) %>% as.numeric()) / (10000 * 1000000)) %>%
    mutate(grasshrub = grasshrub * area, ground = ground * area, tree = tree * area)
  sm_clim <- dt %>%
    select(-area) %>%
    st_drop_geometry() %>%
    colSums()
  CI_clim <- ((sm_clim * 1.96) / (6^0.5)) %>% setNames(str_c("CI_", names(.)))

  all_tif <- "Maps/prediction_maps/uncertain_together_model_spatial_clim_1Band_perCover_Sd.tif"
  dt <- read_stars(all_tif) %>%
    st_as_sf() %>%
    st_transform(8857) %>%
    mutate(area = (st_area(.) %>% as.numeric()) / (10000 * 1000000)) %>%
    mutate(grasshrub = grasshrub * area, ground = ground * area, tree = tree * area)
  sm_all <- dt %>%
    select(-area) %>%
    st_drop_geometry() %>%
    colSums()
  CI_all <- ((sm_all * 1.96) / (600^0.5)) %>% setNames(str_c("CI_", names(.)))

  ### together in a table ----
  tbl <- rbind(sm_rp, sm_k_fold, sm_clim, sm_all)
  tbl_CI <- rbind(CI_rp, CI_k_fold, CI_clim, CI_all)

  tbl2 <- str_c(round(tbl), " (", round(tbl_CI), ")")
  tblb <- tbl
  tblb[] <- tbl2
  tblb %<>% as_tibble() %>%
    mutate(variability_source = c("model", "spatial", "climate", "overall")) %>%
    relocate(variability_source, ground, tree)
  print(tblb)
  write_csv2(tblb, "Maps/sources_of_variability_areas_Mha_Sd.csv")

  ### total areas (not affected thus) of global map ----
  gm <- read_stars("Maps/prediction_maps/global_map_1Band_perCover_Mean.tif")
  dt <- gm %>%
    st_as_sf() %>%
    st_transform(8857) %>%
    mutate(area = (st_area(.) %>% as.numeric()) / (10000 * 1000000)) %>%
    mutate(grasshrub = grasshrub * area, ground = ground * area, tree = tree * area)
  sm_ <- dt %>%
    select(-area) %>%
    st_drop_geometry() %>%
    colSums()
  print(sm_)

  #### maximum total area (all covers) affected by scenarios ----
  scen_tif <- "Maps/prediction_maps/sensitivity_scen_alone_1Band_perCover_MeanThenSd.tif"
  dt <- read_stars(scen_tif) %>%
    st_as_sf() %>%
    st_transform(8857) %>%
    mutate(area = (st_area(.) %>% as.numeric()) / (10000 * 1000000)) %>%
    rowwise() %>%
    mutate(mx = max(grasshrub, ground, tree), mxA = mx * area)
  sm_scen_ <- dt %>%
    select(-area, -grasshrub, -ground, -tree, -mx) %>%
    st_drop_geometry() %>%
    colSums()
  CI_scen_ <- ((sm_scen_ * 1.96) / (25^0.5)) %>% setNames(str_c("CI_", names(.)))
  print(str_c(round(sm_scen_), " (", round(CI_scen_), ")"))
}


### based on "mean maps" ==> area differences from global map ----
if (F) {
  if (F) {
    ## global map
    dt_ <- read_stars("Maps/prediction_maps/global_map_1Band_perCover_Mean.tif") %>%
      st_as_sf() %>%
      st_transform(8857) %>%
      mutate(area = (st_area(.) %>% as.numeric()) / (10000 * 1000000)) %>%
      mutate(grasshrub = grasshrub * area, ground = ground * area, tree = tree * area)
    sm_ <- dt_ %>%
      select(-area) %>%
      st_drop_geometry() %>%
      colSums()
    names(sm_) <- str_c(names(sm_), "_comp")

    ## all sources of variability
    strs_paths <- str_c(
      "Maps/prediction_maps/",
      c(
        "uncertain_model_alone_1Band_perRep_perCover_Mean.nc", ## model
        "uncertain_spatial_alone_1Band_perFold_perCover_Mean.nc", ## spatial
        "global_maps_1Band_perClim_perCover_Mean.nc", ## climate
        "sensitivity_scen_alone_1Band_perScen_perCover_Mean.nc" ## scenarios
      )
    )
    strs_paths %<>% setNames(c("model", "spatial", "clim", "scen"))
    # strs_path <- strs_paths[3]

    fct <- function(strs_path) {
      print(strs_path)
      strs <- read_mdim(strs_path)
      print(dim(strs))
      # v <- 1
      df <- map_dfr(1:dim(strs)[3], function(v) {
        # print(v)
        strsb <- strs[, , , v, , drop = T]
        dt <- strsb %>%
          st_as_sf() %>%
          st_transform(8857) %>%
          mutate(area = (st_area(.) %>% as.numeric()) / (10000 * 1000000)) %>%
          mutate(grasshrub = grasshrub * area, ground = ground * area, tree = tree * area)
        sm <- dt %>%
          select(-area) %>%
          st_drop_geometry() %>%
          colSums()
        sm["v"] <- v
        sm %<>% t() %>%
          as_tibble() %>%
          mutate(v = v)
      }, .progress = F)
      df$source_var <- names(strs_paths)[strs_paths == strs_path]
      print(df)
      return(df)
    }

    lst <- map(strs_paths, fct)

    dff <- bind_rows(lst)
    dff %<>% bind_cols(sm_ %>% t() %>% as_tibble())
    dff %<>% mutate(diff_sv = grasshrub_comp - grasshrub, diff_t = tree_comp - tree, diff_bg = ground_comp - ground)
    summary(dff)
    write_csv2(dff, "Maps/sources_of_variability_areas_diff_Mean.csv")
  } else {
    dff <- read_csv2("Maps/sources_of_variability_areas_diff_Mean.csv")
  }

  dff2 <- dff %>%
    select(source_var, diff_bg, diff_sv, diff_t) %>%
    pivot_longer(c(diff_bg, diff_sv, diff_t))
  dff2$value2 <- abs(dff2$value) # sqrt(dff2$value^2)
  dff2$source_var <- factor(dff2$source_var, levels = c("model", "spatial", "clim", "scen"))

  ## plot
  p <- ggplot(dff2, aes(y = value, fill = source_var)) +
    geom_boxplot() +
    scale_fill_manual(values = c("grey", "lightblue", "coral", "orange")) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    ylab("Area difference (Mha)") +
    facet_wrap(~name, labeller = labeller(name = c(diff_bg = "bare ground", diff_sv = "short vegeation", diff_t = "tree")))
  p
  ggsave("Maps/sources_of_variability_areas_diff_Mean.csv.svg", p, width = 10, height = 5)

  p <- ggplot(dff2, aes(y = value2, fill = source_var)) +
    geom_boxplot() +
    scale_fill_manual(values = c("grey", "lightblue", "coral", "orange")) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    ylab("Absolute area difference (Mha)") +
    facet_wrap(~name, labeller = labeller(name = c(diff_bg = "bare ground", diff_sv = "short vegeation", diff_t = "tree")))
  p
  ggsave("Maps/sources_of_variability_areas_diff_Mean_abs.csv.svg", p, width = 10, height = 5)
}

##### Model profiles together ----
# fl <- "PR_CRU.Rdata"
DF <- map_dfr(c("PR_CRU.Rdata", "PR_CHELSA.Rdata", "PR_ERA5.Rdata", "PR_MODIS.Rdata", "PR_NEX.Rdata", "PR_WC.Rdata"), function(fl) {
  lst <- readRDS(str_c("./ModelEvaluation/", fl))
  # vr <- "tree"
  dff <- map_dfr(names(lst), function(vr) {
    df <- lst[[vr]][[3]][["agr_profiles"]]
    clim_source <- fl %>%
      str_split("_") %>%
      pluck(1, 2) %>%
      str_replace(".Rdata", "")
    df$cim_source <- clim_source
    df$var <- vr
    df$`_vname_` <- str_replace(df$`_vname_`, str_c(clim_source, "."), "")
    return(df)
  })
}, .progress = T)
# table(DF$clim_source, DF$var)
names(DF) <- c("vname", "label", "x", "yhat", "ids", "clim_source", "var")
# table(DF$vname)
DF %<>% select(-ids, -label)
DF %<>% filter(vname != "cat")
DF %<>% arrange(vname, x, var) %>% relocate(vname, x, var)

pt <- DF %>%
  group_by(clim_source, vname) %>%
  summarise_at("x", .funs = c(min = ~ min(., na.rm = T), mean = ~ mean(., na.rm = T), max = ~ max(., na.rm = T), sd = ~ sd(., na.rm = T), n = length))
summary(pt)

# p <- ggplot(DF) +
#   geom_smooth(aes(x = x, y = yhat, fill=var, color=var),n=100, alpha = 0.3, na.rm=T) +
#   scale_fill_manual(values = c("red", "dodgerblue", "springgreen2")) +
#   scale_color_manual(values = c("red", "dodgerblue", "springgreen4")) +
#   facet_wrap(.~vname)+
#   xlim(0, 1) +
#   ylim(0, 1)
# p

DF %<>% mutate(xx = cut(x, breaks = seq(0, 1, length.out = 7), include.lowest = T))
# table(DF$xx)
midpoints <- function(x, dp = 2) {
  lower <- as.numeric(gsub(",.*", "", gsub("\\(|\\[|\\)|\\]", "", x)))
  upper <- as.numeric(gsub(".*,", "", gsub("\\(|\\[|\\)|\\]", "", x)))
  return(round(lower + (upper - lower) / 2, dp))
}
DF %<>% mutate(mdp = midpoints(xx))
# table(DF$mdp, useNA = "always")

# DF %>% filter(clim_source=="CHELSA" & vname == "Mean_precip" & var == "tree") %>% summary()
# DF %>% filter(vname == "Mean_precip" & var == "tree") %>% summary()
# pff =DF %>% filter(vname == "Mean_precip" & var == "tree" & mdp == 0.92)
# sd(pff$yhat)

DF2 <- DF %>%
  group_by(var, vname, mdp, xx) %>%
  summarise_at("yhat", .funs = c(min = ~ min(., na.rm = T), mean = ~ mean(., na.rm = T), max = ~ max(., na.rm = T), sd = ~ sd(., na.rm = T), n = length))
# table(DF2$n, useNA = "always")
# DF2 %>% filter(vname == "Mean_precip" & var == "tree" & mdp == 0.92)
DF2 %<>% mutate(ci = 1.96 * sd / sqrt(n), ci_low = mean - ci, ci_high = mean + ci)

p <- ggplot(DF2) +
  geom_ribbon(aes(x = mdp, ymin = ci_low, ymax = ci_high, fill = var), alpha = 0.3) +
  geom_line(aes(x = mdp, y = mean, color = var)) +
  scale_fill_manual(values = c("red", "dodgerblue", "springgreen2"), labels = c("short vegetation", "bare ground", "tree")) +
  scale_color_manual(values = c("red", "dodgerblue", "springgreen4"), labels = c("short vegetation", "bare ground", "tree")) +
  facet_wrap(. ~ vname) +
  theme(legend.position = "top") +
  xlab("Normalized predictive variable") +
  ylab("Cover response") +
  labs(color = "landcover", fill = "landcover") +
  theme(panel.background = element_blank())
p
ggsave("ModelEvaluation/profiles_all.svg", p, width = 10, height = 12)
