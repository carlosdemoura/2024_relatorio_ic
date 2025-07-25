devtools::load_all("D:/carlos/01_pesquisa/fastan")
devtools::load_all("D:/carlos/01_pesquisa/meteobr")
library(tidyverse)
source("utils.R")

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

df = readRDS("df.rds")

cap3 = list(
  expl = readRDS("cap3_simdata_expl.rds"),
  conf = readRDS("cap3_simdata_conf.rds"),
  sc   = readRDS("cap3_simdata_sc.rds"))
cap4 = list(
  expl = readRDS("cap4_tmax_expl.rds"),
  conf = readRDS("cap4_tmax_conf.rds"),
  sc   = readRDS("cap4_tmax_sc.rds"))


##################################
#####   missing validation   #####
##################################

cap5_mv_expl =
  cap4$expl |>
  missing_validation() |>
  set_fit(iter = 1e5, seed = 12345)
save_proj(cap5_mv_expl)

cap5_mv_conf =
  cap4$conf |>
  missing_validation() |>
  set_fit(iter = 1e5, seed = 12345)
save_proj(cap5_mv_conf)

cap5_mv_sc =
  cap4$sc |>
  missing_validation() |>
  set_fit(iter = 1e5, seed = 12345)
save_proj(cap5_mv_sc)

accuracy(cap5_mv_expl)
accuracy(cap5_mv_conf)
accuracy(cap5_mv_sc)


##################################
#####   plots dos missings   #####
##################################

hpd = data.frame()
for (type in names(cap4)) {
  hpd = 
    cap4[[type]]$summary$pred[,,"hpd_amp"] |>
    {\(.) data.frame(hpd_amp = ., type = rep(type, length(.)))}() |>
    rbind(hpd)
}

ggplot(hpd, aes(y = hpd_amp, x = type)) +
  geom_boxplot() +
  labs(x = "Modelo",
       y = "Amplitude do HPD",
       title = "Boxplot das amplitudes dos\nintervalos preditivos HPD") +
  theme_classic() +
  text_size(15)
ggsave(img("cap5_boxplot"), width = 7, height = 5, dpi = 300, bg = "white")

ggplot(data = summary_as_df(cap4$conf, "pred")[["pred"]], aes(col_, row_, fill = hpd_amp)) +
  geom_tile() +
  scale_fill_gradient(
    low = "white", high = "red",
    name = "Amplitude"
  ) +
  theme_void() +
  labs(x = "Column", y = "Row", title = "Amplitude dos intervalos HPD") +
  text_size(15)
ggsave(img("cap5_conf_hpd_amp"), width = 6, height = 5, dpi = 300, bg = "white")

plot_hpd_vs_var(cap4$conf, "Modelo confirmatório") + text_size(15)
ggsave(img("cap5_mcmc_hpd_var_real"), width = 6, height = 5, dpi = 300, bg = "white")

plot_hpd_vs_var(cap3$conf, "Modelo semi-confirmatório (dados simulados)") + text_size(15)
ggsave(img(""), width = 6, height = 5, dpi = 300, bg = "white")

## Associação entre taxa de acerto e número de previsões por linha
## Fazer o mesmo com amplitude

hit =
  summary_as_df(proj, "pred")[["pred"]] %>%
  mutate(
    hit = as.numeric((.$real >= .$hpd_min) & (.$real <= .$hpd_max)) 
  ) %>%
  group_by(row_) %>%
  summarise(
    p = mean(hit),
    n = n()
  )

plot(hit$n, hit$p)

