img = function(x, extension = "png") {paste0("D:/carlos/01_pesquisa/2024_bayes/2024_relatorio_ic/img/", x, ".", extension)}


save_proj = function(proj) {
  name = proj |> substitute() |> deparse() |> paste0(".rds")
  saveRDS(proj, name)
  proj$fit = NULL
  saveRDS(proj, paste0("light/", name))
}


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
    select(row) %>%
    c() %>%
    purrr::pluck(1)
  
  df =
    df %>%
    dplyr::filter(!!rlang::sym(row) %in% ellegilbe_rows)
  
  df
}


text_size = function(x) {
  theme(text=element_text(size=x))
}