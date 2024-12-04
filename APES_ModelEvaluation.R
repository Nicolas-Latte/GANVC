##### Residuals ----
if (T) {
  cat("\nResiduals", "...")
  clim_data_source <- clim_data_sources[1]
  pred <- map_dfr(clim_data_sources, function(clim_data_source) {
    cat("\n", clim_data_source, "...")

    ### data names ----
    input_data_names <- c(
      soilfire_data_names,
      herb_data_names,
      clim_data_names %>% str_subset(clim_data_source),
      "cat"
    )

    ### dataset ----
    ds <- apes_dataset(
      pts,
      input_data_names,
      target_data_names,
      weights = F,
      device = device
    )

    ### prediction ----
    c(x, y, w, f, i) %<-% ds$.getbatch()
    pred <- predictions(x, i,
      clim_source = clim_data_source,
      folds = 11, repetitions = 0,
      device = device
    )

    ### true and pred together ----
    pred <- bind_rows(
      pred %>%
        mutate(type = "pred") %>% pivot_longer(all_of(target_data_names), names_to = "proportion"),
      bind_cols(
        convert(y, target_data_names),
        pred %>% select(-one_of(target_data_names))
      ) %>%
        mutate(type = "true") %>% pivot_longer(all_of(target_data_names), names_to = "proportion")
    )
    return(pred)
  })
  pred %<>% pivot_wider(names_from = type, values_from = value) %>% mutate(resid = true - pred)

  plt <- ggplot(data = pred, mapping = aes(x = pred, y = true)) +
    geom_hex(bins = 25, mapping = aes(fill = log2(..count..), color = log2(..count..))) +
    # geom_bin_2d(bins = 25, mapping = aes(fill = log2(..count..), color = log2(..count..)))+
    scale_fill_continuous(type = "viridis") +
    scale_color_continuous(type = "viridis") +
    theme(legend.position = "top") +
    geom_abline(slope = 1) +
    geom_smooth(method = "lm", se = F, formula = "y ~ x", col = "red") +
    facet_wrap(clim_source ~ proportion, labeller = label_both, ncol = 3) +
    labs(title = "Scatter plot of true (y) and pred (x) proportions")
  ggsave(plot = plt, filename = str_c("ModelEvaluation/scatterplot_true_pred.svg"), width = 10 * 3 / 2.54, height = 10 * 5 / 2.54)

  plt <- ggplot(data = pred, mapping = aes(x = true, y = resid)) +
    geom_hex(bins = 25, mapping = aes(fill = log2(..count..), color = log2(..count..))) +
    scale_fill_continuous(type = "viridis") + # , trans='log') +
    scale_color_continuous(type = "viridis") + # , trans='log') +
    theme(legend.position = "top") +
    geom_hline(yintercept = 0) +
    geom_smooth(method = "lm", se = F, formula = "y ~ x", col = "red") +
    facet_wrap(clim_source ~ proportion, labeller = label_both, ncol = 3) +
    labs(title = "Scatter plot of residuals (y) and true proportion (x)")
  ggsave(plot = plt, filename = str_c("ModelEvaluation/scatterplot_resid.svg"), width = 10 * 3 / 2.54, height = 10 * 5 / 2.54)

  pred %<>% mutate(
    pred = pred + runif(length(pred), -0.001, 0.001),
    true = true + runif(length(true), -0.001, 0.001)
  )
  pf <- pred %>%
    group_split(proportion) %>%
    map(~ {
      prop <- .$proportion
      plt <- ggplot(data = ., mapping = aes(x = pred, y = true)) +
        stat_density_2d(n = 100, h = c(0.1, 0.1), aes(fill = after_stat(density)), geom = "raster", contour = FALSE) +
        scale_fill_continuous(type = "viridis") + # ,trans="log") +
        theme(legend.position = "top") +
        geom_abline(slope = 1) +
        geom_smooth(method = "lm", se = F, formula = "y ~ x", col = "red") +
        facet_wrap(~clim_source, labeller = label_both, ncol = 1) +
        labs(title = prop)
    })
  plt <- arrangeGrob(grobs = pf, ncol = 3, top = "Scatter plot of true (y) and pred (x) proportions")
  ggsave(plot = plt, filename = str_c("ModelEvaluation/scatterplot_true_pred_2.svg"), width = 10 * 3 / 2.54, height = 10 * 5 / 2.54)
}

##### Explanatory variables  ----
if (T) {
  cat("\nExplanatory variables", "\n")
  #### Variable importance ----
  cat("\n Variable importance", "...", "\n")
  clim_data_source <- clim_data_sources[1]
  zz <- pblapply(clim_data_sources, function(clim_data_source) {
    cat("\n  ", clim_data_source, "...")
    ### data names ----
    input_data_names <- c(
      soilfire_data_names,
      herb_data_names,
      clim_data_names %>% str_subset(clim_data_source),
      "cat"
    )

    ### dataset ----
    ds <- apes_dataset(
      pts,
      input_data_names,
      target_data_names,
      weights = F,
      device = device
    )
    c(x, y, w, f, i) %<-% ds$.getbatch()

    ### model ----
    model <- str_c("ModelTraining/model_", clim_data_source, "_", 11, ".pt")
    model <- torch_load(model)$to(device = device)
    model$eval()

    ### variable importance ----
    x2 <- convert(x, input_data_names)
    y2 <- convert(y, target_data_names)
    PR <- list()
    vartokeep <- target_data_names[1]
    for (vartokeep in target_data_names) {
      cat("\n      ", vartokeep, "...")

      predict_model_shap <- function(model, x2) {
        x <- reconvert(x2, device)
        z <- model(x)
        z2 <- convert(z, target_data_names)
        return(z2 %>% pull(vartokeep))
      }
      # zz <- predict_model_shap(model, x2)

      exp <- DALEX::explain(model,
        data = x2, y = y2[, vartokeep, drop = T],
        predict_function = predict_model_shap,
        type = "regression", verbose = F
      )
      mp <- model_parts(exp,
        type = c("variable_importance", "ratio", "difference")[1],
        n_sample = 10000
      )
      # mp %>% plot(show_boxplots = T, max_vars = 20)
      mpr <- model_profile(exp,
        type = c("partial", "conditional", "accumulated")[1]
      )
      # mpr %>% plot()
      PR[[vartokeep]] <- list(exp, mp, mpr)
    }
    saveRDS(PR, str_c("ModelEvaluation/", "PR_", clim_data_source, ".Rdata"))
  })

  #### Model parts plots ----
  cat("\n Model parts plots", "...")
  ### custom function ----
  plot2.model_parts <- function(x, ..., max_vars = NULL,
                                show_boxplots = TRUE, bar_width = 10, desc_sorting = TRUE,
                                title = "Feature Importance", subtitle = NULL) {
    if (!is.logical(desc_sorting)) {
      stop("desc_sorting is not logical")
    }
    dfl <- c(list(x), list(...))
    if (show_boxplots) {
      dfl <- lapply(dfl, function(x) {
        result <- data.frame(
          min = tapply(x$dropout_loss,
            x$variable, min,
            na.rm = TRUE
          ),
          q1 = tapply(x$dropout_loss,
            x$variable, quantile, 0.25,
            na.rm = TRUE
          ),
          median = tapply(x$dropout_loss,
            x$variable, median,
            na.rm = TRUE
          ),
          q3 = tapply(x$dropout_loss,
            x$variable, quantile, 0.75,
            na.rm = TRUE
          ),
          max = tapply(x$dropout_loss,
            x$variable, max,
            na.rm = TRUE
          )
        )
        result$min <- as.numeric(result$min)
        result$q1 <- as.numeric(result$q1)
        result$median <- as.numeric(result$median)
        result$q3 <- as.numeric(result$q3)
        result$max <- as.numeric(result$max)
        merge(x[x$permutation == 0, ],
          cbind(
            rownames(result),
            result
          ),
          by.x = "variable",
          by.y = "rownames(result)"
        )
      })
    } else {
      dfl <- lapply(dfl, function(x) {
        x[x$permutation == 0, ]
      })
    }
    expl_df <- do.call(rbind, dfl)
    bestFits <- expl_df[expl_df$variable == "_full_model_", ]
    ext_expl_df <- merge(expl_df, bestFits[, c("label", "dropout_loss")], by = "label")
    levels(ext_expl_df$variable) <- ext_expl_df$variable
    ext_expl_df <- ext_expl_df[!(substr(
      ext_expl_df$variable,
      1, 1
    ) == "_"), ]
    if (!is.null(max_vars)) {
      trimmed_parts <- lapply(unique(ext_expl_df$label), function(label) {
        tmp <- ext_expl_df[ext_expl_df$label == label, ]
        tmp[tail(order(tmp$dropout_loss.x), max_vars), ]
      })
      ext_expl_df <- do.call(rbind, trimmed_parts)
    }
    variable <- q1 <- q3 <- dropout_loss.x <- dropout_loss.y <- label <- dropout_loss <- NULL
    nlabels <- length(unique(bestFits$label))
    pl <- ggplot(
      ext_expl_df,
      aes(
        variable,
        ymin = dropout_loss.y,
        ymax = dropout_loss.x,
        color = label
      )
    ) +
      geom_hline(
        data = bestFits,
        aes(yintercept = dropout_loss, color = label),
        lty = 3
      ) +
      geom_linerange(size = bar_width)
    if (show_boxplots) {
      pl <-
        pl + geom_boxplot(
          aes(
            ymin = min,
            lower = q1,
            middle = median,
            upper = q3,
            ymax = max
          ),
          stat = "identity",
          fill = "#371ea3",
          color = "#371ea3",
          width = 0.25
        )
    }
    if (!is.null(attr(x, "loss_name"))) {
      y_lab <- paste(attr(x, "loss_name")[1], "loss after permutations")
    } else {
      y_lab <- "Loss function after variable's permutations"
    }
    pl + coord_flip() + scale_color_manual(values = DALEX::colors_discrete_drwhy(nlabels)) +
      facet_wrap(~label, ncol = 1, scales = "free_y") +
      DALEX::theme_drwhy_vertical() + ylab(y_lab) + xlab("") +
      labs(title = title, subtitle = subtitle) + theme(legend.position = "none")
  }
  ### plotting ----
  for (clim_data_source in clim_data_sources) {
    cat("\n  ", clim_data_source, "...")
    pr <- readRDS(str_c("ModelEvaluation/", "PR_", clim_data_source, ".Rdata"))
    plts <- map(target_data_names, function(vartokeep) {
      pr[[vartokeep]][[2]] %>%
        mutate(label = "") %>%
        plot2.model_parts(show_boxplots = T, title = vartokeep, subtitle = "")
    })
    plt <- do.call(arrangeGrob, c(... = plts, ncol = 1))
    ggsave(plot = plt, str_c("ModelEvaluation/", "ModelParts_", clim_data_source, ".svg"), width = 8, height = 15 * 3)
  }

  #### Model profiles plot ----
  cat("\n Model profiles plots", "...")
  ### custom function ----
  plot2.model_profile <- function(x, ...) {
    tmp <- c(x = list(x), list(...), color = x$color)
    n_profiles <- sum(unlist(sapply(tmp, class)) == "model_profile")
    if (n_profiles > 1) {
      tmp$color <- "_label_"
    }
    tmp <- lapply(tmp, function(el) {
      if ("model_profile" %in% class(el)) {
        el$agr_profiles
      } else {
        el
      }
    })
    tmp
  }
  ### plotting ----
  for (clim_data_source in clim_data_sources) {
    cat("\n  ", clim_data_source, "...")
    PFF <- lst()
    for (vartokeep in target_data_names) {
      pr <- readRDS(str_c("ModelEvaluation/", "PR_", clim_data_source, ".Rdata"))
      rr <- plot2.model_profile(pr[[vartokeep]][[3]])[[1]]
      rr$"_label_" <- vartokeep
      PFF[[vartokeep]] <- rr
    }
    PFF2 <- do.call(rbind, PFF)
    ggsave(plot = plot(PFF2), str_c("ModelEvaluation/", "ModelProfile_", clim_data_source, ".svg"), width = 12, height = 12)
  }
}

##### Hidden weights ----
if (T) {
  cat("\nHidden weights")
  clim_data_source <- clim_data_sources[2]
  null <- lapply(clim_data_sources, function(clim_data_source) {
    cat("\n ", clim_data_source, "...")

    ### data names ----
    input_data_names <- c(
      soilfire_data_names,
      herb_data_names,
      clim_data_names %>% str_subset(clim_data_source),
      "cat"
    )

    ### model ----
    model <- str_c("ModelTraining/model_", clim_data_source, "_", 11, ".pt")
    model <- torch_load(model)

    ### hidden weights
    tst <- fc1_weights(model, input_data_names)
    plt <- ggplot(tst) +
      geom_text(mapping = aes(y = V2, x = V1, col = cl, label = VAR)) +
      theme(legend.position = "none", plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
      ylim(-0.1, 1.1) +
      xlim(-0.1, 1.1) +
      labs(title = str_c(clim_data_source, "_", 11)) +
      coord_cartesian(clip = "off")
    ggsave(filename = str_c("ModelEvaluation/", "HiddenWeights_", clim_data_source, ".svg"), plot = plt, width = 12 * 2 / 2.54, height = 12 * 2 / 2.54)

    return(NULL)
  })
}
