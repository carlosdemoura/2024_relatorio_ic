library(deldir)
library(ggplot2)

df = proj$space
# Criação da tesselação
voronoi <- deldir(df$lon, df$lat)
tiles <- tile.list(voronoi)

# Convertendo para data.frame para ggplot
vor_df <- do.call(rbind, lapply(seq_along(tiles), function(i) {
  tile <- tiles[[i]]
  data.frame(
    x = tile$x,
    y = tile$y,
    id = i
  )
}))

# Plotando com ggplot2
ggplot() +
  geom_polygon(data = vor_df, aes(x, y, group = id), fill = NA, color = "black") +
  geom_point(data = df, aes(lon, lat), color = "red") +
  coord_equal()

p =
  plot_map(proj) +
  geom_point(data = df, aes(lon, lat), color = "red") +
  geom_polygon(data = vor_df, aes(x, y, group = id), fill = NA, color = "black")

plotly::ggplotly(p, tooltip = "text")






library(ggforce)
library(ggplot2)

# Mesmo df do exemplo anterior
ggplot(df, aes(x = x, y = y)) +
  geom_voronoi_segment(color = "blue") +
  geom_point(color = "red") +
  coord_equal()





df <- df |>
  mutate(
    label = glue::glue("ID: {id}\nLon: {lon}\nLat: {lat}")
  )

# Voronoi com deldir
tiles <- tile.list(deldir(df$lon, df$lat))
vor_df <- do.call(rbind, lapply(seq_along(tiles), function(i) {
  data.frame(x = tiles[[i]]$x, y = tiles[[i]]$y, id = i)
}))

# Gráfico ggplot
p <-
  plot_map(proj) +
  geom_polygon(data = vor_df, aes(x, y, group = id), fill = NA, color = "black") +
  geom_point(data = df, aes(lon, lat, text = label), color = "red")

# Converter para plotly com tooltip apenas nos pontos
plotly::ggplotly(p, tooltip = "text")
