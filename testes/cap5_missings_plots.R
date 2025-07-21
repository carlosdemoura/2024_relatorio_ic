devtools::load_all("D:/carlos/01_pesquisa/fastan")
devtools::load_all("D:/carlos/01_pesquisa/meteobr")
library(tidyverse)
# library(ggplot2)
# library(reshape2)
# library(plotly)
source("utils.R")

p_missing_max = 40
df =
  readRDS(file = "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/data_tmax.rds") %>%
  arrange(alt_tipo, station.id, semana) %>%
  filter_by_max_missing("temp_max", "station.id", p_missing_max/100)

cap3_simdata_expl = readRDS("cap3_simdata_expl.rds")
cap3_simdata_conf = readRDS("cap3_simdata_conf.rds")
cap3_simdata_sc   = readRDS("cap3_simdata_sc.rds")

cap4_tmax_expl = readRDS("cap4_tmax_expl.rds")
cap4_tmax_conf = readRDS("cap4_tmax_conf.rds")
cap4_tmax_sc   = readRDS("cap4_tmax_sc.rds")


##################################
#####   missing validation   #####
##################################

cap5_mv_expl =
  cap4_tmax_expl |>
  missing_validation() |>
  set_fit(iter = 1e5, seed = 12345)

cap5_mv_conf =
  cap4_tmax_conf |>
  missing_validation() |>
  set_fit(iter = 1e5, seed = 12345)

cap5_mv_sc =
  cap4_tmax_sc |>
  missing_validation() |>
  set_fit(iter = 1e5, seed = 12345)

accuracy(cap5_mv_expl)
accuracy(cap5_mv_conf)
accuracy(cap5_mv_sc)


####################
#####   real   #####
####################

df1 =
  tmax_pred$summary$pred %>%
  matrix_to_df() %>%
  select(!c(row, col)) %>%
  cbind(tmax_pred$model$pred[c("row", "col")]) %>%
  as_tibble() %>%
  select(c(row, col, hpd_amp))

df2 =
  tmax_pred$model$data %>%
  filter(row %in% df1$row) %>%
  mutate(
    hpd_amp = 0
  ) %>%
  select(c(row, col, hpd_amp))

df =
  rbind(df1, df2) %>%
  mutate(
    row = factor(row, levels = rev(unique(row))),
    station.id = tmax_pred$model$labels$loading[as.integer(as.character(row))]
  ) %>%
  left_join(
    stations %>% select(station.id, region),
    by = "station.id"
  ) %>%
  as_tibble()

row_labels = levels(df$row)
row_labels[seq_along(row_labels) %% 5 < 4] = ""

ggplot(df, aes(col, row, fill = hpd_amp)) +
  geom_tile() +
  scale_fill_gradient(
    low = "white", high = "red",
    name = "Amplitude"
  ) +
  theme_minimal() +
  scale_y_discrete(labels = row_labels) +
  labs(x = "Column", y = "Row", title = "Amplitude dos intervalos HPD") +
  theme(text = element_text(size = 15))
ggsave(img("cap5_mcmc_hpd_amp"), width = 6, height = 5, dpi = 300, bg = "white")


stations |>
  filter(
    station.id %in% tmax_pred$model$labels$loading[df1 |> filter(hpd_amp > quantile(hpd_amp, probs=.98)) |> select(row) |> unique() |> purrr::pluck(1)]
  ) |>
  select(c(station.id, region)) #|>
  #table()


x = df1 |> filter(hpd_amp > quantile(hpd_amp, probs=.98)) 
stations |>
  filter(
    station.id %in% tmax_pred$model$labels$loading[c(290, 309)]
  ) |>
  select(c(station.id, town.name, state))


xxx =
  df1 |>
  mutate(
    week = 
      case_when(col < 18 ~ 1,
                col > 34 ~ 3,
                .default = 2),
    week = factor(week)
  )

ggplot(xxx, aes(y=hpd_amp)) +
  geom_boxplot() +
  labs(x = "semana",
       y = "Amplitude do HPD",
       title = "Boxplot das amplitudes dos\nintervalos preditivos HPD") +
  theme_classic() +
  theme(text=element_text(size=15),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
ggsave(img("cap5_mcmc_boxplot"), width = 5, height = 5, dpi = 300, bg = "white")

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






#######################
#####   simdata   #####
#######################

simdata = readRDS("D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/cap5_simdata_pred.rds")

simdata$summary$pred = abind::abind(simdata$summary$pred, as.matrix(missing_real_value$value), along = 3)
dimnames(simdata$summary$pred)[[3]][9] = "real"

relacao_var_hpd_simdata =
  simdata$summary$pred %>%
  matrix_to_df() %>%
  group_by(row_) %>%
  summarise(hpd_amp_mean = mean(hpd_amp),
            n = n(),
            .groups = "drop") %>%
  rename(
    row = "row_"
  ) %>%
  left_join(
    data.frame(sigma2 = simdata$model$real$sigma2) %>% mutate(row = 1:nrow(.)),
    #data.frame(sigma2 = simdata$summary$sigma2[,,"mean"]) %>% mutate(row = 1:nrow(.)),
    by = "row"
  )

ggplot(relacao_var_hpd_simdata, aes(x=sigma2, y=hpd_amp_mean)) +
  geom_point() +
  labs(x = "Variância real",
       y = "Média das amplitudes dos HPD",
       title = "Amplitude dos intervalos HPD vs. variância real",
       subtitle = "Dados simulados") +
  theme(text = element_text(size = 15))
ggsave(img("cap5_mcmc_hpd_var_simdata"), width = 6, height = 5, dpi = 300, bg = "white")




hit1 =
  simdata$summary$pred %>%
  matrix_to_df() %>%
  mutate(
    hit = as.numeric((.$real >= .$hpd_min) & (.$real <= .$hpd_max)) 
  ) %>%
  select(!c(row, col)) %>%
  dplyr::rename(row = "row_", col = "col_") %>%
  select(c(row, col, hit))

hit2 =
  df2 %>%
  select(!hpd_amp) %>%
  mutate(
    hit = 1
  )

hit =
  rbind(hit1, hit2) %>%
  mutate(
    row = factor(row),
    hit = factor(hit)
  )

row_labels = levels(hit$row)
row_labels[seq_along(row_labels) %% 5 < 4] = ""

ggplot(hit, aes(col, row, fill = hit)) +
  geom_tile() +
  scale_fill_manual(
    values = c("1" = "white",   "0" = "black")
  ) +
  theme_minimal() +
  scale_y_discrete(labels = row_labels) +
  labs(x = "Column", y = "Row", title = "Missing pattern")


hit =
  simdata$summary$pred %>%
  matrix_to_df() %>%
  mutate(
    hit = as.numeric((.$real >= .$hpd_min) & (.$real <= .$hpd_max)) 
  ) %>%
  select(!c(row, col)) %>%
  dplyr::rename(row = "row_", col = "col_") %>%
  select(c(row, col, hit)) %>%
  filter(
    hit == 0 
  ) 

simdata$model$real$sigma2[hit$row,]



############################
#####   real - alpha   #####
############################

smry1 = tmax_pred$summary
smry1$alpha = smry1$alpha[unique(tmax_pred$model$pred$row),,]

smry2 = tmax_pred$summary
smry2$alpha = smry2$alpha[setdiff(1:length(tmax_pred$model$labels$loading), unique(tmax_pred$model$pred$row)),,]

smry1$alpha |>
  matrix_to_df() |>
  filter((hpd_amp > .5) & (hpd_amp < 2.0)) |>
  select(hpd_amp) |>
  purrr::pluck(1) |>
  mean()

plot_contrast(smry1)
plot_hpd(smry1, par = "alpha", col = 1, stat = "mean")


plot_lambda(tmax_pred$summary)
plot_hpd(tmax_pred$summary, par = "alpha", col = 1, stat = "mean")


###  Rascunho  ###

xxx = 
  tmax_pred$summary$pred |>
  matrix_to_df() |>
  select(!c(row, col)) |>
  cbind(tmax_pred$model$pred[c("row", "col")]) |>
  filter(
    hpd_amp > unname(quantile(hpd_amp, probs=.75))
    #hpd_amp < unname(quantile(hpd_amp, probs=.25))
  ) |>
  arrange(desc(row)) |>
  as_tibble() |>
  group_by(row) %>%
  summarise(hpd_amp_mean = mean(hpd_amp),
            n = n(),
            .groups = "drop") |>
  arrange(n)
  #filter(n>=5)
  filter(n<5)

library(gridExtra)

rows = 5:6

plots = list()
for (i in 1:length(rows)){
  df = simdata$model$pred[rows[i],]
  plots[[i]] =
    plot_posterior(simdata$fit, "pred", row = rows[i]) +
    labs(title = paste0(ellegilbe_stations[df$row], ", sem. ", df$col), x = "") +
    geom_vline(xintercept = simdata$summary$pred[i,1,"real"])
}

grid.arrange(grobs = plots, ncol=3)
