library(tidyverse)
library(rvest)

url <- "https://codinomes.com/"

page <- read_html(url)

proyectos_xpath <- "/html/body/div[1]/div/div/section[2]/div"

proyectos <- html_nodes(page, xpath = proyectos_xpath) %>%
  html_children()

parse_proyecto <- function(proynodes) {
  proyecto <- html_children(proynodes)
  n_nodes <- length(proyecto)
  name_idx <- ifelse(n_nodes == 3, 2, 1)
  data_idx <- ifelse(n_nodes == 3, 3, 2)
  proyecto_nombre <- html_text(proyecto[name_idx]) %>%
    str_replace(":", "")
  texto <- html_nodes(proyecto[data_idx], xpath = "li") %>%
    html_text()
  codinomes <- str_match(texto, "(.+) =")[, 2]
  nombres <-  str_match(texto, "= (.*)j*")[,2]
  tibble(
    proyecto = proyecto_nombre,
    codinome = codinomes,
    nombre = nombres
  )
}

process_proyectos <- function(proyectos) {
  out_df <- tibble(
    proyecto = NA,
    codinome = NA,
    nombre = NA
  )
  for (proy in proyectos[2:length(proyectos)]) {
    df <- parse_proyecto(proy)
    out_df <- bind_rows(out_df, df)
  }
  out_df %>%
    filter(!is.na(proyecto)) %>%
    mutate_all(str_trim) %>%
    mutate(
      nombre = ifelse(nombre == "", NA, nombre)
    )
}

df <- process_proyectos(proyectos)

write_csv(
  df,
  path = here::here("codinomes-peru.csv")
)

save(
  df,
  file = here::here("codinomes-peru.Rdata")
)

ncodinomes <- df %>%
  purrr::pluck(2) %>%
  unique() %>%
  length()

nreales <- df %>%
  filter(!is.na(nombre)) %>%
  purrr::pluck(3) %>%
  unique() %>%
  length()

updated <- glue::glue(
  "Actualizado el {ahora}\nCodinomes = {codinomes}, Nombres reales = {reales} ({pct}%)",
  ahora = lubridate::now(),
  codinomes = ncodinomes,
  reales = nreales,
  pct = round(100 * reales / codinomes, 1)
)
write(updated, file = "ACTUALIZADO.txt")
