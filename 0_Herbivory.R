# open libraries
library(tidyverse)
library(sf)
library(stars)
library(sp)
library(raster)

setwd("/Compute/Rawdata/")

library(raster)

#########################################
# Herbivores biomass (kg per km2) - kg = wet weight
#########################################

# load data and aggregate properties of interest
df <- read.csv("herbivore_biomass_05.csv")
df[is.na(df)] <- 0
summary(df)

df$sum_grazers <- rowSums(df[, c("grazer.large", "grazer.small", "gener.grazer.small")], na.rm = TRUE)
df$sum_browser <- rowSums(df[, c("browser.large", "browser.small", "gener.browser.large", "gener.grazer.small")], na.rm = TRUE)

df$sum_grazers_browsers_excl <- rowSums(df[, c("grazer.large", "browser.large", "gener.browser.large", "grazer.small", "browser.small", "gener.grazer.small")], na.rm = TRUE)
df$sum_grazers_browsers_excl <- rowSums(df[, c("grazer.large", "browser.large", "mixed.large", "gener.browser.large", "gener.mixed.large", "grazer.small", "browser.small", "mixed.small", "gener.grazer.small")], na.rm = TRUE)

df$sum_grazers_browsers_foli_generalists <- rowSums(df[, c(
  "grazer.large", "browser.large", "mixed.large", "foli.frug.browser.large", "foli.frug.mixed.large", "foli.graniv.large", "gener.browser.large", "gener.mixed.large",
  "grazer.small", "browser.small", "mixed.small", "foli.frug.grazer.small", "foli.frug.browser.small", "frugi.foli.browser.small", "frugi.foli.mixed.small", "gener.grazer.small"
)], na.rm = TRUE)
df$sum_frugi_grani_others <- rowSums(df[, c("frugivore.large", "frugi.foli.large", "grani.frugiv.large", "frugiv.small", "foli.graniv.small", "grani.frugiv.small", "granivore.small")], na.rm = TRUE)
df$sum_all <- rowSums(df[, c(
  "grazer.large", "browser.large", "grazer.large", "mixed.large", "foli.frug.browser.large", "foli.frug.mixed.large", "foli.graniv.large", "gener.browser.large", "gener.mixed.large",
  "grazer.small", "browser.small", "mixed.small", "foli.frug.grazer.small", "foli.frug.browser.small", "frugi.foli.browser.small", "frugi.foli.mixed.small", "gener.grazer.small",
  "frugivore.large", "frugi.foli.large", "grani.frugiv.large", "frugiv.small", "foli.graniv.small", "grani.frugiv.small", "granivore.small"
)], na.rm = TRUE)


# Convert data frame to sf object
df.sf <- st_as_sf(x = df, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


# generate raster from df
coordinates(df[, c(1, 2)]) <- ~ x + y
plot(df$x, df_lit$x)
plot(df$y, df_lit$y)
summary(df)
summary(df_lit)
plot(df_lit$grazer.large, df$grazer.large)


gridded(df) <- TRUE
r <- brick(df)
par(mfrow = c(2, 2))
par(mar = c(3, 3, 3, 3))

plot(r$sum_grazers_browsers_excl, main = paste(names(r$sum_grazers_browsers_excl)))
plot(r$sum_grazers_browsers_foli_generalists, main = paste(names(r$sum_grazers_browsers_foli_generalists)))
plot(r$sum_frugi_grani_others, main = paste(names(r$sum_frugi_grani_others)))
plot(r$sum_all, main = paste(names(r$sum_all)))

writeRaster(r$sum_browser, filename = file.path("browser_biomass_0_5deg.tif"), bandorder = "BIL", overwrite = TRUE)
r_graz_brows <- stack(r$sum_browser, r$sum_grazers)

r_stars_br_gr <- st_as_stars(r_graz_brows)
write_stars(r_stars_br_gr, "grazer_browser_biomass_0_5deg.tif", overwrite = TRUE)


r_stars <- read_stars("herbivores_biomass_0_5deg.tif")
plot(r_stars_grazer)

#########################################
# Intake - daily intake (kg per km2) - kg = dry mass
#########################################

# load data and aggregate properties of interest
df_in <- read.csv("intake_05.csv")
summary(df_in)
df_in$sum_grazers_browsers_excl <- rowSums(df_in[, c("grazer.large", "browser.large", "mixed.large", "gener.browser.large", "gener.mixed.large", "grazer.small", "browser.small", "mixed.small", "gener.grazer.small")], na.rm = TRUE)
df_in$sum_grazers_browsers_foli_generalists <- rowSums(df_in[, c(
  "grazer.large", "browser.large", "mixed.large", "foli.frug.browser.large", "foli.frug.mixed.large", "foli.graniv.large", "gener.browser.large", "gener.mixed.large",
  "grazer.small", "browser.small", "mixed.small", "foli.frug.grazer.small", "foli.frug.browser.small", "frugi.foli.browser.small", "frugi.foli.mixed.small", "gener.grazer.small"
)], na.rm = TRUE)
df_in$sum_frugi_grani_others <- rowSums(df_in[, c("frugivore.large", "frugi.foli.large", "grani.frugiv.large", "frugiv.small", "foli.graniv.small", "grani.frugiv.small", "granivore.small")], na.rm = TRUE)
df_in$sum_all <- rowSums(df_in[, c(
  "grazer.large", "browser.large", "grazer.large", "mixed.large", "foli.frug.browser.large", "foli.frug.mixed.large", "foli.graniv.large", "gener.browser.large", "gener.mixed.large",
  "grazer.small", "browser.small", "mixed.small", "foli.frug.grazer.small", "foli.frug.browser.small", "frugi.foli.browser.small", "frugi.foli.mixed.small", "gener.grazer.small",
  "frugivore.large", "frugi.foli.large", "grani.frugiv.large", "frugiv.small", "foli.graniv.small", "grani.frugiv.small", "granivore.small"
)], na.rm = TRUE)
# generate raster from df
coordinates(df_in) <- ~ x + y
gridded(df_in) <- TRUE
r_in <- brick(df_in)
par(mfrow = c(2, 2))
plot(r_in$sum_grazers_browsers_excl, main = paste(names(r$sum_grazers_browsers_excl)))
plot(r_in$sum_grazers_browsers_foli_generalists, main = paste(names(r$sum_grazers_browsers_foli_generalists)))
plot(r_in$sum_frugi_grani_others, main = paste(names(r$sum_frugi_grani_others)))
plot(r_in$sum_all, main = paste(names(r$sum_all)))

r_in_stars <- st_as_stars(r_in)
write_stars(r_in_stars, "herbivores_intake_0_5deg.tif", overwrite = TRUE)

#########################################
# daily litter intake (kg per km2) - kg = dry mass
#########################################

# load data and aggregate properties of interest
df_lit <- read.csv("intake_litter.csv")

df_lit <- read.csv("intake_litter_05.csv")

df_lit$sum_grazers <- rowSums(df_lit[, c("grazer.large", "grazer.small", "gener.grazer.small")], na.rm = TRUE)
df_lit$sum_browser <- rowSums(df_lit[, c("browser.large", "browser.small", "gener.browser.large", "gener.grazer.small")], na.rm = TRUE)


df_lit$sum_grazers_browsers_excl <- rowSums(df_lit[, c("grazer.large", "browser.large", "mixed.large", "gener.browser.large", "gener.mixed.large", "grazer.small", "browser.small", "mixed.small", "gener.grazer.small")], na.rm = TRUE)
df_lit$sum_grazers_browsers_foli_generalists <- rowSums(df_lit[, c(
  "grazer.large", "browser.large", "mixed.large", "foli.frug.browser.large", "foli.frug.mixed.large", "foli.graniv.large", "gener.browser.large", "gener.mixed.large",
  "grazer.small", "browser.small", "mixed.small", "foli.frug.grazer.small", "foli.frug.browser.small", "frugi.foli.browser.small", "frugi.foli.mixed.small", "gener.grazer.small"
)], na.rm = TRUE)
df_lit$sum_frugi_grani_others <- rowSums(df_lit[, c("frugivore.large", "frugi.foli.large", "grani.frugiv.large", "frugiv.small", "foli.graniv.small", "grani.frugiv.small", "granivore.small")], na.rm = TRUE)
df_lit$sum_all <- rowSums(df_lit[, c(
  "grazer.large", "browser.large", "grazer.large", "mixed.large", "foli.frug.browser.large", "foli.frug.mixed.large", "foli.graniv.large", "gener.browser.large", "gener.mixed.large",
  "grazer.small", "browser.small", "mixed.small", "foli.frug.grazer.small", "foli.frug.browser.small", "frugi.foli.browser.small", "frugi.foli.mixed.small", "gener.grazer.small",
  "frugivore.large", "frugi.foli.large", "grani.frugiv.large", "frugiv.small", "foli.graniv.small", "grani.frugiv.small", "granivore.small"
)], na.rm = TRUE)
# generate raster from df_lit
coordinates(df_lit) <- ~ x + y
gridded(df_lit) <- TRUE
r_lit <- brick(df_lit)
plot(r_lit$sum_browser)
plot(r_lit$sum_grazers)

par(mfrow = c(2, 2))
plot(r_lit$sum_grazers_browsers_excl, main = paste(names(r$sum_grazers_browsers_excl)))
plot(r_lit$sum_grazers_browsers_foli_generalists, main = paste(names(r$sum_grazers_browsers_foli_generalists)))
plot(r_lit$sum_frugi_grani_others, main = paste(names(r$sum_frugi_grani_others)))
plot(r_lit$sum_all, main = paste(names(r$sum_all)))

# writeRaster(r_lit, filename=file.path("litter_intake_0_5deg.tif"), bandorder='BIL', overwrite=TRUE)
r_lit_stars <- st_as_stars(r_lit)
write_stars(r_lit_stars, "litter_intake_0_5deg.tif", overwrite = TRUE)


r_lit_graz_brows <- stack(r_lit$sum_browser, r_lit$sum_grazers)

r_stars_br_gr <- st_as_stars(r_lit_graz_brows)
write_stars(r_stars_br_gr, "grazer_browser_litter_0_5deg.tif", overwrite = TRUE)


r_stars <- read_stars("herbivores_biomass_0_5deg.tif")

# Raster of interest
r_final <- brick(r$sum_all, r_in$sum_all, r_lit$sum_all)
par(mfrow = c(1, 3))

plot(r_final)
r_final_stars <- st_as_stars(r_final)
write_stars(r_final_stars, "sum_all_0_5deg.tif", overwrite = TRUE)
