plot_hpd_vs_var = function(proj, subtitle) {
  df_ =
    summary_as_df(proj, "pred")[["pred"]] %>%
    group_by(row_) %>%
    summarise(hpd_amp_mean = mean(hpd_amp), n = n()) %>%
    rename(row = "row_") %>%
    left_join((cap4$conf$data$x %>% group_by(row) %>% summarise(var = var(value))), by = "row")
  
  ggplot(df_, aes(x=var, y=hpd_amp_mean)) +
    geom_point() +
    labs(x = "Variância amostral por linha",
         y = "Média das amplitudes dos HPD",
         title = "Amplitude dos intervalos HPD\nvs. variância amostral",
         subtitle = subtitle)
}



mvar = 
  tmax_pred$model$data %>%
  group_by(row) %>% 
  summarise(s2 = var(value),
            .groups = 'drop') %>%
  mutate(
    station.id = tmax_pred$model$labels$loading
  )

relacao_var_hpd_real =
  tmax_pred$summary$pred %>%
  matrix_to_df() %>%
  left_join(
    simdata$summary$pred %>% matrix_to_df() %>% select(row, row_),
    by = "row"
  ) %>%
  group_by(row_) %>%
  summarise(hpd_amp_mean = mean(hpd_amp),
            n = n(),
            .groups = "drop") %>%
  rename(
    row = "row_"
  ) %>%
  left_join(
    data.frame(sigma2 = tmax_pred$summary$sigma2[,,"mean"]) %>% mutate(row = 1:nrow(.)),
    by = "row"
  ) %>%
  merge(mvar, by = "row")

ggplot(relacao_var_hpd_real, aes(x=s2, y=hpd_amp_mean)) +
  geom_point() +
  labs(x = "Variância amostral",
       y = "Média das amplitudes dos HPD",
       title = "Amplitude dos intervalos HPD\nvs. variância amostral",
       subtitle = "Dados reais") +
  theme(text = element_text(size = 15))
ggsave(img("cap5_mcmc_hpd_var_real"), width = 6, height = 5, dpi = 300, bg = "white")
cap4 = list(
  expl = proj,
  conf = proj,
  sc = proj)



proj =
  blank_project() |>
  set_data(simdata = T, group.sizes = rep(30,2), columns = 20, semi.conf = F, pred = .1) |>
  set_space(type = "random", cont = T) |>
  set_prior(type = "normal", semi.conf = F) |>
  set_data(simdata = "prior") |>
  set_fit(iter = 1000, seed = 12345)

bias = function(proj){
  bias_root = function(stat) {
    (proj$summary$sigma2[,,stat]-proj$summary$sigma2[,,"real"])/proj$summary$sigma2[,,"real"]
  }
  
  stat = c("mean", "median", "mode")
  
  l = lapply(stat, bias_root)
  names(l) = stat
  l
}


bias_n = (proj2$summary$sigma2[,,"median"]-proj2$summary$sigma2[,,"real"])/proj2$summary$sigma2[,,"real"]


get_mode = function(x) {
  d = density(x)
  d$x[which.max(d$y)]
}


cap3_simdata_sc =
  blank_project() |>
  set_data(simdata = T, group.sizes = 10:12, columns = 20, semi.conf = T) |>
  set_space(type = "random", cont = T) |>
  set_prior(type = "normal", semi.conf = T) |>
  set_data(simdata = "prior") |>
  set_fit(iter = 100, seed = 12345)

cap3_esp_1p2conf =
  blank_project() |>
  {\(.) . |> save_proj()}()
p = cap3_esp_2p1
p$data$real=NULL
p=p|>set_summary()


proj=
  proj |>
  set_summary()
