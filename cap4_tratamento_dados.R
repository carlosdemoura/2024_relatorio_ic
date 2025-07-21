devtools::load_all("D:/carlos/01_pesquisa/meteobr")
library(tidyverse)
library(lubridate)

alts = 
  stations %>%
  select(alt) %>%
  purrr::pluck(1) %>%
  quantile(probs = c(1/3, 2/3)) %>%
  unname()

df =
  get_data(first.day = "2023/01/01", last.day = "2023/12/30", vars = "temperature_max") %>%
  mutate(semana = lubridate::week(ymd_h(time))) %>%
  group_by(station, semana) %>%
  summarise(temp_max = max(temperature_max, na.rm = T),
            .groups = "drop") %>%
  mutate(
    temp_max = 
      case_when(temp_max == -Inf ~ NA,
                .default = temp_max)
  ) %>%
  rename(station.id = station) %>%
  merge(stations[c("station.id", "region", "alt")], by = "station.id") %>%
  rename(regiao = "region") %>%
  mutate(
    alt_tipo = case_when(
      alt <= alts[1] ~ "baixa",
      alt >= alts[2] ~ "alta",
      .default = "média"
    )
  ) %>%
  select(!alt) %>%
  arrange(station.id, semana) %>%
  as_tibble()

missing =
  df %>%
  filter(is.na(temp_max)) %>%
  select(station.id) %>%
  unique() %>%
  c() %>%
  purrr::pluck(1)

df =
  df %>%
  mutate(
    missing = station.id %in% missing
  )

saveRDS(df, file = "data_tmax.rds")



filter_by_max_missing = function(df, value, row, p) {
  missings_by_row = 
    df %>%
    mutate(
      missing = is.na(!!rlang::sym(value)) |> as.numeric()
    ) %>%
    group_by_at(row) %>%
    summarise(missing_p = mean(missing),
              .groups = "drop")
  
  ellegilbe_rows = 
    missings_by_row %>%
    dplyr::filter(missing_p <= p) %>%
    dplyr::select(dplyr::all_of(row)) %>%
    #select(row) %>%
    c() %>%
    purrr::pluck(1)
  
  df =
    df %>%
    dplyr::filter(!!rlang::sym(row) %in% ellegilbe_rows)
  
  df
}

p_missing_max = 40
df =
  readRDS(file = "D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/data_tmax.rds") %>%
  arrange(alt_tipo, station.id, semana) %>%
  filter_by_max_missing("temp_max", "station.id", p_missing_max/100)