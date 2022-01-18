my_fct_reorder<- function(df, col, ordem){
  df |>
    mutate({{col}} := forcats::as_factor(df[[col]]),
           {{col}} := forcats::lvls_reorder(df[[col]],ordem)) |>
    select({{col}})
}
