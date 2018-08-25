library(tidyverse)
library(furrr)
library(rvest)
library(lubridate)
library(writexl)
library(readxl)

# funcoes -----------------------------------------------------------------

pega_info <- function(df, key) {

  posicao <- pull(df, X1) %>%
    str_replace_all("\\:", "") %>%
    str_to_lower() %>%
    str_which(key) %>%
    '['(1)

  if (length(posicao) == 0) {
    return(NA_character_)
  }

  pull(df, X1)[posicao + 1]
}

pega_registro <- function(x) {

  Sys.sleep(3)

  url_base_registro <- "http://sistemas.agricultura.gov.br/snpc/cultivarweb/detalhe_cultivar.php?codsr="

  url_registro <- paste0(url_base_registro, x)

  base <- tryCatch({
      read_html(url_registro) %>%
        html_nodes("table") %>%
        '['(2) %>%
        html_table() %>%
        '[['(1) %>%
        as_tibble() %>%
        select(1)
    },
    error = function(e) NULL,
    warning = function(w) NULL
  )

  if (is.null(base)) return(NULL)

  data_frame(
      cultivar = str_remove_all(pega_info(base, "cultivar"), "\t|\n|\r|\\*| "),
      linhagem = str_remove_all(pega_info(base, "linhagem"), "\t|\n|\r|\\*| "),
      evento = pega_info(base, "evento"),
      nome_comum = pega_info(base, "nome comum"),
      nome_cientifico = pega_info(base, "nome científico"),
      situacao = pega_info(base, "situação"),
      n_registro = pega_info(base, "nº registro"),
      data_registro =  dmy(pega_info(base, "data do registro")),
      mantenedor = str_extract(pega_info(base, "mantenedor"), "^.+(?=ENDEREÇO)"),
      url = url_registro,
      data_consulta = Sys.Date()
    ) %>%
    mutate(mantenedor = ifelse(str_detect(mantenedor, "^---"), NA_character_, mantenedor))
}

pega_protecao <- function(x) {

  Sys.sleep(8)

  url_base_protecao <- "http://sistemas.agricultura.gov.br/snpc/cultivarweb/detalhe_protecao.php?codsr="

  url_protecao <- paste0(url_base_protecao, x)

  base <- tryCatch({
    read_html(url_protecao) %>%
      html_nodes("table") %>%
      '['(2) %>%
      html_table() %>%
      '[['(1) %>%
      as_tibble() %>%
      select(1)
  },
  error = function(e) NULL,
  warning = function(w) NULL
  )

  if (is.null(base)) return(NULL)

  data_frame(
    cultivar = pega_info(base, "cultivar"),
    nome_comum = pega_info(base, "nome comum"),
    nome_cientifico = pega_info(base, "nome científico"),
    situacao = pega_info(base, "situação"),
    n_processo = pega_info(base, "nº processo"),
    n_certificado = pega_info(base, "nº certificado"),
    inicio_protecao = dmy(pega_info(base, "início da proteção")),
    fim_protecao =  dmy(pega_info(base, "término da proteção")),
    titular = str_extract(pega_info(base, "titular"), "^.+(?=ENDEREÇO)"),
    representante = str_extract(str_remove_all(pega_info(base, "representante"), "\t|\n|\r"), "^.+(?=ENDEREÇO)"),
    malhoristas = pega_info(base, "melhoristas"),
    url = url_protecao,
    data_consulta = Sys.Date()
  )
}


# download registro -------------------------------------------------------

n_total <- 40000
n_grupos <- 40

aux <- data_frame(id = seq_len(n_total)) %>%
  mutate(grupo = cut(id, breaks = n_grupos, labels = FALSE))

plan(multiprocess)

# i = 15
for (i in seq_len(n_grupos)[-c(1:30)]) {

  print(str_glue("Grupo {i}/{n_grupos}       {Sys.time()}"))

  ids <- filter(aux, grupo == i) %>% pull(id)

  out <- future_map_dfr(ids, pega_registro, .progress = TRUE)
  #out2 <- map_dfr(ids, pega_registro)

  if (!dir.exists("data-raw/temp_download")) dir.create("data-raw/temp_download")

  write_xlsx(out, str_glue("data-raw/temp_download/registro_grupo_{i}.xlsx"))
}

out
out2

cultivar_registrado <- list.files("data-raw/temp_download", "registro", full.names = TRUE) %>%
  map_df(read_excel)

usethis::use_data(cultivar_registrado, overwrite = TRUE)
writexl::write_xlsx(cultivar_registrado, "data-raw/cultivar_registrado.xlsx")


pega_registro(33111)

# download protecao -------------------------------------------------------


n_total <- 8000
n_grupos <- 2

aux <- data_frame(id = seq_len(n_total)) %>%
  mutate(grupo = cut(id, breaks = n_grupos, labels = FALSE))

plan(multiprocess)

for (i in seq_len(n_grupos)) {

  print(str_glue("Grupo {i}/{n_grupos}       {Sys.time()}"))

  ids <- filter(aux, grupo == i) %>% pull(id)

  out <- future_map_dfr(ids, pega_protecao, .progress = TRUE)

  if (!dir.exists("data-raw/temp_download")) dir.create("data-raw/temp_download")

  write_xlsx(out, str_glue("data-raw/temp_download/protegido_grupo_{i}.xlsx"))
}

pega_protecao(2515)

cultivar_protegido <- list.files("data-raw/temp_download", "protegido", full.names = TRUE) %>%
  map_df(read_excel)

usethis::use_data(cultivar_protegido, overwrite = TRUE)
writexl::write_xlsx(base_protegido, "data-raw/cultivar_protegido.xlsx")

