library(ggplot2)
library(reshape2)

df2 = 
  df %>%
  filter(
    station.id %in% x1
  ) %>% 
  mutate(
    present = is.na(temp_max_week) |> as.numeric(),
    row = factor(station.id) |> as.numeric(),
    col = week
  ) %>%
  select(row, col, present)

ggplot(df2, aes(col, row, fill = factor(present))) +
  geom_tile() +
  scale_fill_manual(values = c("1" = "white", "0" = "black")) +
  theme_minimal() +
  labs(x = "Coluna", y = "Linha", fill = "Valor") +
  scale_y_reverse()


####   Estações elegíveis

df2 = 
  df %>%
  filter(
    station.id %in% x1
  ) %>% 
  mutate(
    missing = is.na(temp_max_week) |> as.numeric()
  ) %>%
  group_by(station.id) %>%
  summarise(missing_p = mean(missing),
            .groups = "drop")

ellegilbe_stations = filter(df2, missing_p < .5) %>% select(station.id) %>% c()
