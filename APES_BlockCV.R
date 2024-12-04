##### BlockCV with 'kf' folds + 1
if (T) {
  cat("\nBlockCV with 'kf' folds + 1", "...\n")
  strs_clim2 <- strs_clim %>% slice("band", "WC.Mean_temp")

  if (F) {
    cvs <- suppressMessages(cv_spatial(
      x = pts, r = strs_clim2, k = kf,
      iteration = 100L, seed = 1, plot = F,
      progress = F, report = F
    ))
    ## RUN0 is no folds ==> all data used to train
    cvs$folds_table <- cvs$biomod_table %<>% cbind(RUN0 = rep(T, nrow(.)))
    print(summary(cvs$folds_table))
    saveRDS(cvs, str_c("BlockCV/", "cv_sptial.Rdata"))
  }
  cvs <- readRDS(str_c("BlockCV/", "cv_sptial.Rdata"))

  plt <- ggplot() +
    geom_stars(data = strs_clim2, na.action = na.omit, show.legend = F) +
    scale_fill_viridis_c() +
    geom_sf(data = pts, colour = "red", cex = 0.5) +
    geom_sf(data = cvs$blocks, colour = "black", fill = NA) +
    geom_sf_text(data = cvs$blocks, aes(label = folds)) +
    ylab("") +
    xlab("")
  ggsave(plot = plt, filename = str_c("BlockCV/", "cv_sptial.svg"), width = 15 * 2 / 2.54, height = 10 * 2 / 2.54)

  plt <- ggplot() +
    geom_stars(data = strs_clim2, na.action = na.omit, show.legend = F) +
    scale_fill_viridis_c() +
    geom_sf(data = cvs$blocks, colour = "black", fill = NA) +
    geom_sf(data = cvs$blocks %>% filter(folds == 6), fill = "grey", fill = NA) +
    geom_sf_text(data = cvs$blocks, aes(label = folds)) +
    ylab("") +
    xlab("")
  ggsave(plot = plt, filename = str_c("BlockCV/", "cv_sptial2.svg"), width = 15 * 2 / 2.54, height = 10 * 2 / 2.54)
}
