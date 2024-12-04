##### Training ----
if (T) {
  cat("\nTraining", "...")
  clim_data_source <- "NEX" # clim_data_sources[1]
  for (clim_data_source in clim_data_sources) {
    ### data names ----
    input_data_names <- c(
      soilfire_data_names,
      herb_data_names,
      clim_data_names %>% str_subset(clim_data_source),
      "cat"
    )

    k <- kf + 1
    for (k in c(kf + 1, 1:kf)) {
      cat("\nTraining:", str_c(clim_data_source, "_", k), "\n")
      ### reset memory ----
      reset_memory()

      ### dataset ----
      ds <- apes_dataset(
        pts,
        input_data_names,
        target_data_names,
        weights = T,
        device = device
      )
      c(x, y, w, f, i) %<-% ds$.getbatch()

      ### model + optimizer ----
      model <- net(
        num_features = 64,
        drop_rate = 0.75,
        input_dim = ncol(x),
        target_dim = ncol(y)
      )$to(device = device)
      z <- model(x)
      optimizer <- optim_adamw(
        model$parameters,
        lr = 0.001,
        weight_decay = 0.001
      )
      # optimizer <- optim_adam(
      #   model$parameters,
      #   lr = 0.001
      # )

      ### training ----
      loss_criterion <- "nnf_mse_loss"
      metric_criterion <- "nnf_l1_loss"
      epochs <- 3000
      pb <- progress::progress_bar$new(
        total = epochs,
        format = "(:spin) [:bar] Epoch :epoch | :percent | :tick_rate/sec | :elapsed / :eta"
      )
      criterion <- function(criterion, z, y, w, weighting = T) {
        fct <- get(criterion)
        v <- fct(z, y, reduction = "none")
        v <- torch_mean(v, dim = 2)
        if (weighting) {
          v_w <- v * w
          return(torch_mean(v_w))
        } else {
          return(torch_mean(v))
        }
      }
      ## loop epochs
      c(lst, lsv, mst, msv) %<-% list(c(), c(), c(), c())
      for (epoch in 1:epochs) {
        c(lt, lv, mt, mv) %<-% list(c(), c(), c(), c())
        ## batch data
        c(x, y, w, f, i) %<-% ds$.getbatch()
        ## weights
        w <- torch_mean(w, dim = 2)
        ## fold k
        f2 <- f[, k, drop = T]
        ## train
        model$train()
        optimizer$zero_grad()
        c(xt, yt, wt) %<-% list(x[f2, ], y[f2, ], w[f2])
        zt <- model(xt)
        loss_w_t <- criterion(loss_criterion, zt, yt, wt, weighting = T)
        loss_w_t$backward()
        optimizer$step()
        metric_w_t <- criterion(metric_criterion, zt, yt, wt, weighting = F)
        ## valid
        model$eval()
        c(xv, yv, wv) %<-% list(x[!f2, ], y[!f2, ], w[!f2])
        zv <- model(xv)
        loss_w_v <- criterion(loss_criterion, zv, yv, wv, weighting = T)
        metric_w_v <- criterion(metric_criterion, zv, yv, wv, weighting = F)
        ## append
        c(lt, lv, mt, mv) %<-% list(
          c(lt, loss_w_t$to(device = "cpu")$item()),
          c(lv, loss_w_v$to(device = "cpu")$item()),
          c(mt, metric_w_t$to(device = "cpu")$item()),
          c(mv, metric_w_v$to(device = "cpu")$item())
        )
        c(lst, lsv, mst, msv) %<-% list(c(lst, lt), c(lsv, lv), c(mst, mt), c(msv, mv))
        pb$tick(tokens = list(epoch = epoch))
        ## plot weights
        # if (epoch %% 100 == 0) {
        #   plt <- plot_fc1_weights(model, epoch)
        #   suppressMessages(plot(plt))
        # }
      }

      ### model/data/plot save ----
      torch_save(model, str_c("ModelTraining/model_", clim_data_source, "_", k, ".pt"))
      lt_ <- tibble(ep = 1:epoch, t_v = "train", l_m = "loss", value = lst)
      mt_ <- tibble(ep = 1:epoch, t_v = "train", l_m = "metric", value = mst)
      lv_ <- tibble(ep = 1:epoch, t_v = "valid", l_m = "loss", value = lsv)
      mv_ <- tibble(ep = 1:epoch, t_v = "valid", l_m = "metric", value = msv)
      dt <- bind_rows(lt_, mt_, lv_, mv_)
      dt %<>% na.omit()
      plt <- ggplot(data = dt) +
        geom_point(aes(y = value, x = ep, col = t_v)) +
        facet_wrap(~l_m, scale = "free", ncol = 1) +
        ylab("value") +
        xlab("epoch") +
        labs(title = str_c(clim_data_source, "_", k))
      plot(plt)
      ggsave(plot = plt, filename = str_c("ModelTraining/plot_", clim_data_source, "_", k, ".svg"), width = 15 * 2.25 / 2.54, height = 10 * 2 / 2.54)
      saveRDS(dt, str_c("ModelTraining/dt_", clim_data_source, "_", k, ".Rdata"))
    }
  }
}
