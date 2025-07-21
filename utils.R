img = function(x, extension = "png") {paste0("D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/img/", x, ".", extension)}


save_proj = function(proj) {
  name = proj |> substitute() |> deparse() |> paste0(".rds")
  saveRDS(proj, name)
  proj$fit = NULL
  saveRDS(proj, paste0("light/", name))
}


text_size = function(x) {
  theme(text=element_text(size=x))
}