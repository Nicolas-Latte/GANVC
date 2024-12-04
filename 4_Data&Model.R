##### NN dataset ----
cat("\nNN dataset", "...")
apes_dataset <- dataset(
  name = "APESdataset",
  initialize = function(pts,
                        input_data_names,
                        target_data_names,
                        weights = T,
                        device) {
    dt <- pts %>% st_drop_geometry()

    ### input_data
    input_data <- dt %>%
      select(all_of(input_data_names)) %>%
      as.matrix()

    ### (un)normalization
    lows <- readRDS("DataPreparation/lows.Rdata")[input_data_names]
    highs <- readRDS("DataPreparation/highs.Rdata")[input_data_names]
    normalize <- function(x, highs, lows) {
      t((t(x) - lows) / (highs - lows))
    }
    unnormalize <- function(x, highs, lows) {
      t(t(x) * (highs - lows) + lows)
    }
    input_data_norm <- normalize(input_data, highs, lows)

    ### target data
    if (all(!is.na(target_data_names))) {
      target_data <- dt %>%
        select(all_of(c(target_data_names))) %>%
        as.matrix()
    } else {
      target_data <- NA
    }

    ### weights
    if (weights) {
      compute_weights <- function(variable, type = c("factor", "props")) {
        if (type == "factor") {
          tbl <- table(variable)
          wghts <- (sum(tbl) / tbl)
          wghts <- wghts / sum(wghts)
          wghts2 <- wghts[match(variable, names(tbl))]
        } else if (type == "props") {
          whmx <- apply(variable, 1, which.max)
          tbl <- whmx %>% table()
          wghts <- (sum(tbl) / tbl)
          wghts <- wghts / sum(wghts)
          wghts2 <- wghts[match(whmx, names(tbl))]
        }
        return(wghts2)
      }
      weights <- cbind(
        cont = compute_weights(dt[, "cont", drop = T], "factor"),
        cat = compute_weights(dt[, "cat", drop = T], "factor"),
        props = compute_weights(target_data, "props")
      )
    } else {
      weights <- NA
    }

    ### folds
    folds <- readRDS(str_c("BlockCV/", "cv_sptial.Rdata"))$folds_table

    ### self
    self$input_data_norm <- torch_tensor(input_data_norm, device = device)
    self$target_data <- torch_tensor(target_data, device = device)
    self$weights <- torch_tensor(weights, device = device)
    self$folds <- torch_tensor(folds, device = device)
    self$id <- dt$id
  },
  .getbatch = function(idx) {
    x <- self$input_data_norm
    y <- self$target_data
    w <- self$weights
    f <- self$folds
    i <- self$id
    return(list(x = x, y = y, w = w, f = f, i = i))
  },
  .length = function() {
    return(1)
  }
)

##### NN model ----
cat("\nNN model", "...")
net <- nn_module(
  "APESnet",
  initialize = function(num_features,
                        drop_rate,
                        input_dim,
                        target_dim) {
    self$fc1 <- nn_linear(input_dim, num_features)
    self$fc2 <- nn_linear(num_features, target_dim)
    self$act <- torch_tanh
    self$dp <- nn_dropout(drop_rate)
  },
  forward = function(x) {
    z <- x %>%
      self$fc1() %>%
      self$act() %>%
      self$dp() %>%
      self$fc2() %>%
      nnf_softmax(dim = 2)
    return(z)
  }
)
# model <- net(
#   num_features = 64,
#   drop_rate = 0.75,
#   input_dim = 14,
#   target_dim = 3
# )

##### NN utilities ----
cat("\nNN utilities", "...")
convert <- function(o, names) {
  o2 <- o$to(device = "cpu") %>%
    as.matrix() %>%
    as_tibble(.name_repair = "minimal") %>%
    setNames(names)
  return(o2)
}
reconvert <- function(o, device) {
  o2 <- o %>%
    as.matrix() %>%
    torch_tensor()
  return(o2$to(device = device))
}
reset_memory <- function() {
  ll <- ls(envir = .GlobalEnv)
  lst <- map(ll, function(x) (class(get(x))))
  names(lst) <- ll
  tensors <- ll[map_lgl(lst, ~ {
    any(. == "torch_tensor")
  })]
  lst2 <- c(tensors, "model")
  for (l in lst2) {
    if (exists(l, envir = .GlobalEnv)) {
      assign(l, get(l, envir = .GlobalEnv)$to(device = "cpu"), envir = .GlobalEnv)
      rm(list = l, envir = .GlobalEnv)
    }
  }
  if (exists("ds", envir = .GlobalEnv)) {
    rm(list = "ds", envir = .GlobalEnv)
  }
  # gc(reset = T,verbose=F)
}

##### NN predictions function ----
cat("\nNN predictions function", "...")
predictions <- function(x, i,
                        clim_source,
                        repetitions,
                        folds,
                        device) {
  crss <- tibble(clim_source, folds) %>%
    mutate(model = str_c("ModelTraining/model_", clim_source, "_", folds, ".pt"))
  if (repetitions == 1) {
    stop("'repetitions' can not be '1'; either 0 or >1")
  }
  if (repetitions == 0) {
    rp <- rep(0, nrow(x))
    id <- i
    xx <- x
  } else {
    rp <- rep(1:repetitions, each = nrow(x))
    id <- rep(i, repetitions)
    dm <- dim(x)
    xx <- x$expand(c(repetitions, dm))
    dm <- dim(xx)
    xx %<>% torch_reshape(c(dm[1] * dm[2], dm[3]))
  }
  zs <- apply(crss, 1, function(crs) {
    mdl <- torch_load(crs["model"])$to(device = device)
    if (repetitions == 0) {
      mdl$eval() ## dropout deactivated
    } else {
      mdl$train() ## dropout activated
    }
    z <- mdl(xx)
    z %<>% convert(target_data_names)
    z %<>% mutate(
      clim_source = crs["clim_source"],
      fold_k = crs["folds"] %>% as.integer()
    )
    z %<>% bind_cols(id = id, rp = rp)
    return(z)
  })
  zs <- bind_rows(zs)
  return(zs)
}



##### plot_fc1_weights function ----
fc1_weights <- function(model, input_data_names) {
  X <- model$parameters$fc1.weight
  X %<>% convert(names = str_c("w", 1:ncol(X)))
  tst <- X %>%
    as.matrix() %>%
    t() %>%
    do.mmds(ndim = 2) %>%
    pluck(1) %>%
    as_tibble(.name_repair = "minimal")

  cl <- NbClust(tst,
    method = "kmeans",
    min.nc = 2, max.nc = 4,
    distance = "euclidean", index = "ch"
  ) %>%
    pluck("Best.partition")

  min_max_norm <- function(x) {
    (x - min(x)) / (max(x) - min(x))
  }
  tst %<>% apply(2, min_max_norm)
  tst %<>%
    bind_cols(VAR = input_data_names, cl = cl %>% as.factor())

  return(tst)
}


##### custom_predict function ----
custom_predict <- function(
    clim_data_source = c(clim_data_sources)[6],
    clim_sce = c("", "ssp5", "ssp2")[3],
    perc_herb = percs_herb[3],
    perc_fire = percs_fire[3],
    cat = c(0, 1)[1],
    BurnDate = NA,
    biomass = NA,
    intake = NA,
    litter = NA) {
  ### data names ----
  input_data_names <- c(
    soilfire_data_names,
    herb_data_names,
    clim_data_names %>% str_subset(clim_data_source),
    "cat"
  )

  ### pts_var2 ----
  to_be_named <- c(herb_data_names, "BurnDate", clim_data_names %>% str_subset(clim_data_source))
  names <- c(
    str_replace(perc_herb, "herb", herb_data_names),
    str_replace(perc_fire, "fire", "firefreq"),
    if (clim_data_source == "NEX" & clim_sce != "") {
      str_replace(clim_data_names %>% str_subset("NEX"), "NEX.", str_c("NEX_2050_", clim_sce, "."))
    } else {
      clim_data_names %>% str_subset(clim_data_source)
    }
  )
  lookup <- names %>% setNames(to_be_named)
  print(lookup)

  if (clim_data_source == "NEX" & clim_sce == "") {
    pts_var2 <- pts_var
  } else {
    pts_var2 <- pts_var %>%
      select(-one_of(clim_data_names %>% str_subset("NEX")))
  }
  pts_var2 %<>%
    mutate(cat = !!cat) %>%
    rename(all_of(lookup)) %>%
    select(all_of(c(input_data_names, "id")))
  pts_var2 %<>% mutate(BurnDate = BurnDate * 19)

  ### changing values
  if (!is.na(biomass)) {
    pts_var2 %<>% mutate(biomass = !!biomass)
    cat("\nbiomass to: ", biomass, "\n")
  }
  if (!is.na(intake)) {
    pts_var2 %<>% mutate(intake = !!intake)
    cat("\nintake to: ", intake, "\n")
  }
  if (!is.na(litter)) {
    pts_var2 %<>% mutate(litter = !!litter)
    cat("\nlitter to: ", litter, "\n")
  }
  if (!is.na(BurnDate)) {
    pts_var2 %<>% mutate(BurnDate = !!BurnDate)
    cat("\nBurnDate to: ", BurnDate, "\n")
  }

  ### dataset ----
  ds <- apes_dataset(
    pts_var2,
    input_data_names,
    NA,
    weights = F,
    device = "cpu"
  )

  ### predictions ----
  c(x, y, w, f, i) %<-% ds$.getbatch()
  df <- predictions(x, i,
    clim_source = clim_data_source,
    folds = 11, repetitions = 0,
    device = "cpu"
  )
  df2 <- left_join(df, pts_var2, by = "id") %>% na.omit()

  xtrct <- read_stars(file_continent) %>% st_extract(df2 %>% st_as_sf())
  df2 %<>% st_as_sf() %>%
    bind_cols(xtrct %>% st_drop_geometry()) %>%
    rename(continent = continent_025.tif)

  return(df2)
}
