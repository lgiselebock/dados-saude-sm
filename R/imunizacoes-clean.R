

# ADICIONA FONTE ----------------------------------------------------------

sysfonts::font_add_google(name = "Roboto", family = "Roboto")
showtext::showtext_opts(dpi = 300)
showtext::showtext_auto()


# PACOTES NECESSARIOS -----------------------------------------------------

library(ggplot2)


# IMPORTAÇÃO DOS DADOS ----------------------------------------------------

imunizacoes <- readr::read_rds("dados/imunizacoes.rds")


# TIDY --------------------------------------------------------------------

imunizacoes_tidy <- imunizacoes |>
  # empilhar as colunas
  tidyr::pivot_longer(cols = !c(sigla_uf, id_municipio, ano),
                      names_to = "tipos",
                      values_to = "n")


imunizacoes_tidy_filtro <- imunizacoes_tidy |>
  dplyr::filter(
    tipos == "cobertura_total" |
      tipos == "cobertura_bcg" |
      tipos == "cobertura_dtp" |
      tipos == "cobertura_poliomielite" |
      tipos == "cobertura_febre_amarela" |
      tipos == "cobertura_hepatite_b"
  )


funcao_para_iterar <- function(tipo_painel, dados = imunizacoes_tidy_filtro) {
  imunizacoes_tidy_filtro |>
    dplyr::mutate(
      realce = factor(dplyr::if_else(tipos == tipo_painel, 1, 0)),
      painel = tipo_painel
    )
}


# VISUALIZACAO ------------------------------------------------------------

## TODAS VACINAS

imunizacoes_visualizar <-
  purrr::map_dfr(unique(imunizacoes_tidy_filtro$tipos),
                 funcao_para_iterar) |>
  dplyr::filter(ano >= 2001) |>
  dplyr::mutate(
    tipos = dplyr::case_when(
      tipos == "cobertura_total" ~ "Total",
      tipos == "cobertura_bcg" ~ "BCG",
      tipos == "cobertura_dtp" ~ "DTP",
      tipos == "cobertura_poliomielite" ~ "Poliomelite",
      tipos == "cobertura_febre_amarela" ~ "Febre \nAmarela",
      tipos == "cobertura_hepatite_b" ~ "Hepatite B"
    ),
    painel = dplyr::case_when(
      painel == "cobertura_total" ~ "Total",
      painel == "cobertura_bcg" ~ "BCG",
      painel == "cobertura_dtp" ~ "DTP",
      painel == "cobertura_poliomielite" ~ "Poliomelite",
      painel == "cobertura_febre_amarela" ~ "Febre \nAmarela",
      painel == "cobertura_hepatite_b" ~ "Hepatite B"
    ),
    numero = ifelse((ano == min(ano) |
                       ano == max(ano)) & realce == 1, 1, 0),
    hjust = dplyr::case_when(ano == min(ano) ~ 1.2,
                             ano == max(ano) ~ -0.2,
                             TRUE ~ NA_real_)
  )

imunizacoes_todas_clean <- imunizacoes_visualizar |>
  dplyr::filter(realce == 0) |>
  ggplot() +
  aes(
    x = ano,
    y = n,
    group = tipos,
    color = realce
  ) +
  geom_line(color = "grey80",
            size = 1.3,
            show.legend = FALSE) +
  geom_line(
    data = dplyr::filter(imunizacoes_visualizar, realce == 1),
    size = 1.3,
    color = "royal blue",
    show.legend = FALSE
  ) +
  geom_point(
    data = dplyr::filter(imunizacoes_visualizar, numero == 1),
    aes(x = ano,
        y = n),
    color = "black",
    show.legend = FALSE
  ) +
  # geom_text(
  #   data = dplyr::filter(imunizacoes_visualizar, numero == 1),
  #   aes(
  #     x = ano,
  #     y = n,
  #     label = stringr::str_glue("{n}%"),
  #     hjust = hjust
  #   ),
  #   color = "black",
  #   size = 3.5
  # ) +
  scale_x_continuous(
    position = "top",
    limits = c(1998, 2023),
    breaks = seq(2001, 2021, 2)
  ) +
  # labs(
  #   title = "Cobertura vacinal na cidade de Santa Maria, RS",
  #   subtitle = "para os últimos 20 anos",
  #   x = "",
  #   y = "",
  #   caption = "Dados: Ministério do Saúde | @lppufsm"
  # ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.line.x = element_blank(),
    # plot.margin = unit(c(0.3, 1, 1, 1), "cm"),
    text = element_blank()
  ) +
  facet_wrap(vars(painel), nrow = 6, strip.position = "left") +
  coord_cartesian(clip = "off")


# ggsave(
#   filename = "imunizacoes_todas_clean.png",
#   plot = imunizacoes_todas_clean,
#   path = "output/img/clean",
#   width = 1800,
#   height = 1800,
#   units = "px",
#   dpi = 300
# )


## BCG

imunizacoes_bcg_clean <- imunizacoes_tidy |>
  dplyr::filter(tipos == "cobertura_bcg" & ano >= 2001) |>
  dplyr::mutate(n = n/100) |>
  ggplot() +
  aes(x = ano, y = n) +
  geom_line(size = 1.3, color = "royal blue") +
  # annotate("text",
  #          x = 2019.5,
  #          y = 1.75,
  #          label = "175.64%",
  #          size = 3.5) +
  # annotate(
  #   "text",
  #   x = 2013,
  #   y = .5,
  #   label = "Após atingir o pico em 2018, \na cobertura vacinal atingiu \nseu menor nível em 2021, \ncom 27.76%.",
  #   size = 3.5
  # ) +
  # geom_curve(
  #   aes(
  #     x = 2014.5,
  #     y = 0.3,
  #     xend = 2020.5,
  #     yend = 0.29
  #   ),
  #   size = 0.5,
  #   arrow = arrow(length = unit(0.03, "npc")),
  #   color = "grey30"
  # ) +
  scale_x_continuous(limits = c(2001, 2021),
                     breaks = seq(2001, 2021, 2)) +
  scale_y_continuous(
    limits = c(0, 1.8),
    breaks = seq(0, 1.8, 0.4),
    labels = scales::percent
  ) +
  # labs(
  #   title = "Cobertura vacinal para a BCG na cidade de Santa Maria, RS",
  #   # subtitle = "",
  #   x = "",
  #   y = "Cobertura vacinal",
  #   caption = "Dados: Ministério da Saúde | @lppufsm"
  # ) +
  theme_minimal() +
  theme(
    # plot.margin = unit(c(0.3, 1, 1, 0.1), "cm"),
    axis.title = element_blank(),
    text = element_blank()
  )

# ggsave(
#   filename = "imunizacoes_bcg_clean.png",
#   plot = imunizacoes_bcg_clean,
#   path = "output/img/clean",
#   width = 1800,
#   height = 1800,
#   units = "px",
#   dpi = 300
# )


## DTP

imunizacoes_dtp_clean <- imunizacoes_tidy |>
  dplyr::filter(tipos == "cobertura_dtp") |>
  dplyr::mutate(n = n/100) |>
  ggplot() +
  aes(x = ano, y = n) +
  geom_line(size = 1.3, color = "royal blue") +
  # annotate(
  #   "text",
  #   x = 2016,
  #   y = 0.55,
  #   label = "Após 2016, \nnão há mais informações \na respeito da cobertura vacinal \nde DTP em Santa Maria, RS.",
  #   size = 3.5
  # ) +
  # geom_curve(
  #   aes(
  #     x = 2017.5,
  #     y = 0.67,
  #     xend = 2016.5,
  #     yend = 0.95
  #   ),
  #   size = 1,
  #   arrow = arrow(length = unit(0.03, "npc")),
  #   color = "grey30"
  # ) +
  scale_x_continuous(
    limits = c(1995, 2021),
    breaks = seq(1995, 2021, 2)
  ) +
  scale_y_continuous(
    limits = c(0, 1.10),
    breaks = seq(0, 1.10, 0.20),
    labels = scales::percent
  ) +
  # labs(
  #   title = "Cobertura vacinal para a DTP na cidade de Santa Maria, RS",
  #   # subtitle = ""
  #   x = "",
  #   y = "Cobertura vacinal",
  #   caption = "Dados: Ministério da Saúde | @lppufsm"
  # ) +
  theme_minimal() +
  theme(
    # plot.margin = unit(c(0.3, 1, 1, 0.1), "cm"),
    axis.title = element_blank(),
    text = element_blank()
  )

# ggsave(
#   filename = "imunizacoes_dtp_clean.png",
#   plot = imunizacoes_dtp_clean,
#   path = "output/img/clean",
#   width = 1800,
#   height = 1800,
#   units = "px",
#   dpi = 300
# )


## FEBRE AMARELA

imunizacoes_febre_amarela_clean <- imunizacoes_tidy |>
  dplyr::filter(tipos == "cobertura_febre_amarela" & ano >= 2001) |>
  dplyr::mutate(n = n/100) |>
  ggplot() +
  aes(x = ano, y = n) +
  geom_line(size = 1.3, color = "royal blue") +
  # annotate(
  #   "text",
  #   x = 2004.5,
  #   y = 0.60,
  #   label = "Em 2009, foi atingido o pico de 88.91%, \napós o número crescente de casos \nde Febre Amarela no estado do RS \nentre os anos de 2008 e 2009.",
  #   size = 2.9
  # ) +
  # geom_curve(
  #   aes(
  #     x = 2004,
  #     y = 0.70,
  #     xend = 2008.5,
  #     yend = 0.90
  #   ),
  #   size = 1,
  #   curvature = -0.5,
  #   arrow = arrow(length = unit(0.03, "npc")),
  #   color = "grey30"
  # ) +
  # annotate(
  #   "text",
  #   x = 2015,
  #   y = 0.30,
  #   label = "Após o aumento no final da década de 2000, \na cobertura vacinal se manteve estável \nentre 40% e 70%.",
  #   size = 3
  # ) +
  scale_x_continuous(
    limits = c(2001, 2021),
    breaks = seq(2001, 2021, 2)
  ) +
  scale_y_continuous(
    limits = c(0, 1.0),
    breaks = seq(0, 1.0, 0.20),
    labels = scales::percent
  ) +
  # labs(
  #   title = "Cobertura vacinal para a Febre Amarela na cidade de Santa Maria, RS",
  #   # subtitle = "",
  #   x = "",
  #   y = "Cobertura vacinal",
  #   caption = "Dados: Ministério da Saúde | @lppufsm"
  # ) +
  theme_minimal() +
  theme(
    # plot.margin = unit(c(0.3, 1, 1, 0.1), "cm"),
    axis.title = element_blank(),
    text = element_blank()
  )

# ggsave(
#   filename = "imunizacoes_febre_amarela_clean.png",
#   plot = imunizacoes_febre_amarela_clean,
#   path = "output/img/clean",
#   width = 1800,
#   height = 1800,
#   units = "px",
#   dpi = 300
# )


## HEPATITE B

imunizacoes_hepatite_b_clean <- imunizacoes_tidy |>
  dplyr::filter(tipos == "cobertura_hepatite_b" & ano >= 2001) |>
  dplyr::mutate(n = n/100) |>
  ggplot() +
  aes(x = ano, y = n) +
  geom_line(size = 1.3, color = "royal blue") +
  scale_x_continuous(
    limits = c(2001, 2021),
    breaks = seq(2001, 2021, 2)
  ) +
  scale_y_continuous(
    limits = c(0, 1.4),
    breaks = seq(0, 1.4, 0.20),
    labels = scales::percent
  ) +
  # labs(
  #   title = "Cobertura vacinal para Hepatite B na cidade de Santa Maria, RS",
  #   # subtitle = "",
  #   x = "",
  #   y = "Cobertura vacinal",
  #   caption = "Dados: Ministério da Saúde | @lppufsm"
  # ) +
  theme_minimal() +
  theme(
    # plot.margin = unit(c(0.3, 1, 1, 0.1), "cm"),
    axis.title = element_blank(),
    text = element_blank()
  )

# ggsave(
#   filename = "imunizacoes_hepatite_b_clean.png",
#   plot = imunizacoes_hepatite_b_clean,
#   path = "output/img/clean",
#   width = 1800,
#   height = 1800,
#   units = "px",
#   dpi = 300
# )


## POLIOMELITE

imunizacoes_poliomelite_clean <- imunizacoes_tidy |>
  dplyr::filter(tipos == "cobertura_poliomielite" & ano >= 2001) |>
  dplyr::mutate(n = n/100) |>
  ggplot() +
  aes(x = ano, y = n) +
  geom_line(size = 1.3, color = "royal blue") +
  # annotate(
  #   "text",
  #   x = 2020,
  #   y = 1.25,
  #   label = "124.76%",
  #   size = 3.5
  # ) +
  # annotate(
  #   "text",
  #   x = 2013,
  #   y = 0.35,
  #   label = "Após atingir o pico em 2018, \na cobertura vacinal atingiu \nseu menor nível em 2021, \ncom 68.01%.",
  #   size = 3.5
  # ) +
  # geom_curve(
  #   aes(
  #     x = 2017.5,
  #     y = 0.35,
  #     xend = 2021,
  #     yend = 0.65
  #   ),
  #   size = 1,
  #   arrow = arrow(length = unit(0.03, "npc")),
  #   color = "grey30"
  # ) +
  scale_x_continuous(
    limits = c(2001, 2021),
    breaks = seq(2001, 2021, 2)
  ) +
  scale_y_continuous(
    limits = c(0, 1.4),
    breaks = seq(0, 1.4, 0.2),
    labels = scales::percent
  ) +
  # labs(
  #   title = "Cobertura vacinal para a Poliomelite na cidade de Santa Maria, RS",
  #   x = "",
  #   y = "Cobertura vacinal",
  #   caption = "Dados: Ministério da Saúde | @lppufsm"
  # ) +
  theme_minimal() +
  theme(
    # plot.margin = unit(c(0.3, 1, 1, 0.1), "cm"),
    axis.title = element_blank(),
    text = element_blank()
  )

# ggsave(
#   filename = "imunizacoes_poliomelite_clean.png",
#   plot = imunizacoes_poliomelite_clean,
#   path = "output/img/clean",
#   width = 1800,
#   height = 1800,
#   units = "px",
#   dpi = 300
# )


## TOTAL

imunizacoes_totais_clean <- imunizacoes_tidy |>
  dplyr::filter(tipos == "cobertura_total" & ano >= 2001) |>
  dplyr::mutate(n = n/100) |>
  ggplot() +
  aes(x = ano, y = n) +
  geom_line(size = 1.3, color = "royal blue") +
  # annotate(
  #   "text",
  #   x = 2009,
  #   y = 0.16,
  #   label = "Após atingir 104.71%, em 2018, \na cobertura vacinal da cidade caiu para 58.28% em 2021, \nnúmero superior apenas em relação ao ano de 2016, com 55.33%.",
  #   size = 3
  # ) +
  # geom_curve(
  #   aes(
  #     x = 2014.5,
  #     y = 0.20,
  #     xend = 2016,
  #     yend = 0.50
  #   ),
  #   size = 1,
  #   arrow = arrow(length = unit(0.03, "npc")),
  #   color = "grey30"
  # ) +
  # geom_curve(
  #   aes(
  #     x = 2014.5,
  #     y = 0.20,
  #     xend = 2018,
  #     yend = 0.90
  #   ),
  #   size = 1,
  #   arrow = arrow(length = unit(0.03, "npc")),
  #   color = "grey30"
  # ) +
  # geom_curve(
  #   aes(
  #     x = 2014.5,
  #     y = 0.20,
  #     xend = 2021,
  #     yend = 0.55
  #   ),
  #   size = 1,
  #   arrow = arrow(length = unit(0.03, "npc")),
  #   color = "grey30"
  # ) +
  scale_x_continuous(
    limits = c(2001, 2021),
    breaks = seq(2001, 2021, 2)
  ) +
  scale_y_continuous(
    limits = c(0, 1.2),
    breaks = seq(0, 1.2, 0.20),
    labels = scales::percent
  ) +
  # labs(
  #   title = "Cobertura vacinal total na cidade de Santa Maria, RS",
  #   x = "",
  #   y = "Cobertura vacinal",
  #   caption = "Dados: Ministério da Saúde | @lppufsm"
  # ) +
  theme_minimal() +
  theme(
    # plot.margin = unit(c(0.3, 1, 1, 0.1), "cm"),
    axis.title = element_blank(),
    text = element_blank()
  )

# ggsave(
#   filename = "imunizacoes_totais_clean.png",
#   plot = imunizacoes_totais_clean,
#   path = "output/img/clean",
#   width = 1800,
#   height = 1800,
#   units = "px",
#   dpi = 300
# )
