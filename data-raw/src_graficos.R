library(tidyverse)
library(readxl)
library(lubridate)


# registro ----------------------------------------------------------------

registrado <- read_excel("data-raw/cultivar_registrado.xlsx")

top_registrado <- registrado %>%
  filter(situacao == "REGISTRADA") %>%
  mutate(nome_comum = ifelse(str_detect(nome_comum, ",|/"), str_extract(nome_comum, "^.+(?=(,|/))"), nome_comum)) %>%
  mutate(nome_comum = str_replace(nome_comum, "Orquidea", "Orquídea")) %>%
  group_by(nome_comum) %>%
  tally() %>%
  arrange(-n) %>%
  slice(1:15)


top_registrado %>%
  mutate(nome_comum = fct_reorder(nome_comum, n)) %>%
  ggplot(aes(n, nome_comum)) +
    geom_segment(aes(xend = 0, yend = nome_comum), color = "cadetblue4", size = 3) +
    geom_point(size = 9, shape = 21, color = "white", fill = "cadetblue4") +
    geom_text(aes(label = n),size = 2.5,  color = "white") +
    labs(
      x = "Registros", y = "15 culturas com maior n° de registros no RNC",
      title = "Registro Nacional de Cultivares (RNC)",
      subtitle =
        "O RNC tem por finalidade habilitar previamente cultivares e espécies para a produção \ne a comercialização de sementes e mudas no País.",
      caption = "http://www.agricultura.gov.br \n@italocegatta"
    ) +
    scale_x_continuous(expand = expand_scale(mult = c(0, 0.05))) +
    ggthemes::theme_fivethirtyeight() +
    theme(axis.title = element_text()) +
    ggsave("top_cultivares_registrados.png", width = 8, height = 6)


registro_eucalipto <- registrado %>%
  filter(situacao == "REGISTRADA") %>%
  filter(str_detect(nome_cientifico, "Eucalyptus|Corymbia")) %>%
  filter(!is.na(mantenedor)) %>%
  mutate(
    mantenedor = str_to_upper(str_remove(mantenedor, "_$")),
    mantenedor = str_replace(mantenedor, "S/(A|A.)", "S.A."),
    mantenedor = str_replace(mantenedor, "S.A$", "S.A.")
  ) %>%
  group_by(mantenedor) %>%
  tally() %>%
  arrange(-n)

registro_eucalipto %>%
  mutate(mantenedor = fct_reorder(mantenedor, n)) %>%
  ggplot(aes(n, mantenedor)) +
    geom_segment(aes(xend = 0, yend = mantenedor), color = "forestgreen", alpha = 0.8, size = 3) +
    geom_point(size = 7, stroke = 2, shape = 21, color = "white", fill = "forestgreen") +
    geom_text(aes(label = n),size = 3,  color = "white", fontface = "bold") +
    labs(
      x = "Registros", y = NULL,
      title = "Registro Nacional de Cultivares (RNC)",
      subtitle =
        "Mantenedores de cultivares dos gêneros Corymbia e Eucalyptus registrados no RNC",
      caption = "http://www.agricultura.gov.br \n@italocegatta"
    ) +
    scale_x_continuous(expand = expand_scale(mult = c(0.02, 0.05))) +
    ggthemes::theme_fivethirtyeight() +
    theme(axis.title = element_text()) +
    ggsave("eucalipto_registrado.png", width = 11.5, height = 12)


# protegido ---------------------------------------------------------------

protegido <- read_excel("data-raw/cultivar_protegido.xlsx")

registro_eucalipto <- protegido %>%
  filter(situacao == "PROTEÇÃO DEFINITIVA") %>%
  filter(str_detect(nome_cientifico, "Eucalyptus|Corymbia")) %>%
  mutate_if(is.POSIXct, as.Date)

registro_eucalipto %>%
  mutate(cultivar = fct_reorder(cultivar, fim_protecao)) %>%
  ggplot(aes(x = inicio_protecao, xend = fim_protecao, y = cultivar, yend = cultivar)) +
  geom_segment(color = "forestgreen", alpha = 0.8, size = 3) +
  geom_text(
    aes(x = fim_protecao, label = cultivar), size = 2,
    hjust = 1.1,  color = "grey10", fontface = "bold"
  ) +
  labs(
    title = "Serviço Nacional de Proteção de Cultivares (SNPC)",
    x = "Vigência da proteção",
    subtitle =
      "Cultivares de Eucalyptus regitrados no SNPC",
    caption = "http://www.agricultura.gov.br \n@italocegatta"
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_bw(8) +
  theme(
    axis.text.y = element_blank(), axis.ticks.y = element_blank(),
    axis.title.y = element_blank(), panel.grid.major.y = element_blank()
  ) +
  ggsave("eucalipto_protegido_periodo.png", width = 5, height = 10)

  registro_eucalipto %>%
  left_join(., group_by(., titular) %>% tally()) %>%
  select(cultivar, titular, inicio_protecao, fim_protecao)
  ggplot(aes())

registro_eucalipto %>%
  select(cultivar, titular, inicio_protecao, fim_protecao) %>%
  group_by(titular) %>%
  tally() %>%
  mutate(titular = fct_reorder(titular, n)) %>%
  ggplot(aes(n, titular)) +
  geom_segment(aes(xend = 0, yend = titular), color = "forestgreen", alpha = 0.8, size = 3) +
  geom_point(size = 7, stroke = 2, shape = 21, color = "white", fill = "forestgreen") +
  geom_text(aes(label = n),size = 3,  color = "white", fontface = "bold") +
  labs(
    x = "Cultivares protegidos (#)", y = NULL,
    title = "Serviço Nacional de Proteção de Cultivares (SNPC)",
    subtitle =
      "Titulares de cultivares do gênero Eucalyptus com proteção definitiva no SNPC",
    caption = "http://www.agricultura.gov.br \n@italocegatta"
  ) +
  scale_x_continuous(expand = expand_scale(mult = c(0.02, 0.05))) +
  ggthemes::theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  ggsave("eucalipto_protegido.png", width = 10, height = 7)

