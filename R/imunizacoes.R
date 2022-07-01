

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
      tipos == "cobertura_hepatite_b" |
      tipos == "cobertura_triplice_bacteriana"
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
      tipos == "cobertura_hepatite_b" ~ "Hepatite B",
      tipos == "cobertura_triplice_bacteriana" ~ "Tríplice \nBacteriana"
    ),
    painel = dplyr::case_when(
      painel == "cobertura_total" ~ "Total",
      painel == "cobertura_bcg" ~ "BCG",
      painel == "cobertura_dtp" ~ "DTP",
      painel == "cobertura_poliomielite" ~ "Poliomelite",
      painel == "cobertura_febre_amarela" ~ "Febre \nAmarela",
      painel == "cobertura_hepatite_b" ~ "Hepatite B",
      painel == "cobertura_triplice_bacteriana" ~ "Tríplice \nBacteriana"
    ),
    numero = ifelse((ano == min(ano) |
                       ano == max(ano)) & realce == 1, 1, 0),
    hjust = dplyr::case_when(ano == min(ano) ~ 1.2,
                             ano == max(ano) ~ -0.2,
                             TRUE ~ NA_real_)
  )

imunizacoes_todas <- imunizacoes_visualizar |>
  dplyr::filter(realce == 0) |>
  ggplot() +
  aes(
    x = ano,
    y = n,
    group = tipos,
    color = realce
  ) +
  geom_line(color = "grey60",
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
  geom_text(
    data = dplyr::filter(imunizacoes_visualizar, numero == 1),
    aes(
      x = ano,
      y = n,
      label = stringr::str_glue("{n}%"),
      hjust = hjust
    ),
    color = "black",
    size = 3.5
  ) +
  scale_x_continuous(
    position = "top",
    limits = c(1998, 2023),
    breaks = seq(2001, 2021, 2)
  ) +
  labs(
    title = "Cobertura vacinal na cidade de Santa Maria, RS",
    subtitle = "para os últimos 20 anos",
    x = "",
    y = "",
    caption = "Dados: Base dos Dados (Ministério do Saúde) | @lppufsm"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.line.x = element_line(),
    # plot.margin = unit(c(0.3, 1, 1, 1), "cm"),
    text = element_text(family = "Roboto"),
    plot.background = element_rect(fill = "grey90"),
    plot.caption = element_text(hjust = .95, size = 7)
  ) +
  facet_wrap(vars(painel), nrow = 7, strip.position = "left") +
  coord_cartesian(clip = "off")


# ggsave(
#   filename = "imunizacoes_todas.png",
#   plot = imunizacoes_todas,
#   path = "output/img/",
#   width = 1800,
#   height = 1800,
#   units = "px",
#   dpi = 300
# )


## BCG

imunizacoes_bcg <- imunizacoes_tidy |>
  dplyr::filter(tipos == "cobertura_bcg" & ano >= 2001) |>
  dplyr::mutate(n = n/100) |>
  ggplot() +
  aes(x = ano, y = n) +
  geom_line(size = 1.3, color = "royal blue") +
  annotate("text",
           x = 2019.5,
           y = 1.75,
           label = "175.64%",
           size = 3.5) +
  annotate(
    "text",
    x = 2013,
    y = .5,
    label = "Após atingir o pico em 2018, \na cobertura vacinal atingiu \nseu menor nível em 2021, \ncom 27.76%.",
    size = 3.5
  ) +
  geom_curve(
    aes(
      x = 2014.5,
      y = 0.3,
      xend = 2020.5,
      yend = 0.29
    ),
    size = 0.5,
    arrow = arrow(length = unit(0.03, "npc")),
    color = "grey30"
  ) +
  scale_x_continuous(limits = c(2001, 2021),
                     breaks = seq(2001, 2021, 2)) +
  scale_y_continuous(
    limits = c(0, 1.8),
    breaks = seq(0, 1.8, 0.4),
    labels = scales::percent
  ) +
  labs(
    title = "Cobertura vacinal para a BCG na cidade de Santa Maria, RS",
    # subtitle = "",
    x = "",
    y = "Cobertura vacinal",
    caption = "Dados: Base dos Dados (Ministério da Saúde) | @lppufsm"
  ) +
  theme_minimal() +
  theme(
    # plot.margin = unit(c(0.3, 1, 1, 0.1), "cm"),
    axis.title = element_text(size = 10),
    text = element_text(family = "Roboto"),
    plot.background = element_rect(fill = "grey90"),
    plot.caption = element_text(hjust = .95, size = 7)
  )

# ggsave(
#   filename = "imunizacoes_bcg.png",
#   plot = imunizacoes_bcg,
#   path = "output/img/",
#   width = 1800,
#   height = 1800,
#   units = "px",
#   dpi = 300
# )


## DTP

imunizacoes_dtp <- imunizacoes_tidy |>
  dplyr::filter(tipos == "cobertura_dtp") |>
  dplyr::mutate(n = n/100) |>
  ggplot() +
  aes(x = ano, y = n) +
  geom_line(size = 1.3, color = "royal blue") +
  annotate(
    "text",
    x = 2016,
    y = 0.55,
    label = "Após 2016, \nnão há mais informações \na respeito da cobertura vacinal \nde DTP em Santa Maria, RS.",
    size = 3.5
  ) +
  geom_curve(
    aes(
      x = 2017.5,
      y = 0.67,
      xend = 2016.5,
      yend = 0.95
    ),
    size = 1,
    arrow = arrow(length = unit(0.03, "npc")),
    color = "grey30"
  ) +
  scale_x_continuous(
    limits = c(1995, 2021),
    breaks = seq(1995, 2021, 2)
  ) +
  scale_y_continuous(
    limits = c(0, 1.10),
    breaks = seq(0, 1.10, 0.20),
    labels = scales::percent
  ) +
  labs(
    title = "Cobertura vacinal para a DTP na cidade de Santa Maria, RS",
    # subtitle = ""
    x = "",
    y = "Cobertura vacinal",
    caption = "Dados: Base dos Dados (Ministério da Saúde) | @lppufsm"
  ) +
  theme_minimal() +
  theme(
    # plot.margin = unit(c(0.3, 1, 1, 0.1), "cm"),
    axis.title = element_text(size = 10),
    text = element_text(family = "Roboto"),
    plot.background = element_rect(fill = "grey90"),
    plot.caption = element_text(hjust = .95, size = 7)
  )

# ggsave(
#   filename = "imunizacoes_dtp.png",
#   plot = imunizacoes_dtp,
#   path = "output/img/",
#   width = 1800,
#   height = 1800,
#   units = "px",
#   dpi = 300
# )


## FEBRE AMARELA

imunizacoes_febre_amarela <- imunizacoes_tidy |>
  dplyr::filter(tipos == "cobertura_febre_amarela" & ano >= 2001) |>
  dplyr::mutate(n = n/100) |>
  ggplot() +
  aes(x = ano, y = n) +
  geom_line(size = 1.3, color = "royal blue") +
  annotate(
    "text",
    x = 2004.5,
    y = 0.60,
    label = "Em 2009, foi atingido o pico de 88.91%, \napós o número crescente de casos \nde Febre Amarela no estado do RS \nentre os anos de 2008 e 2009.",
    size = 2.9
  ) +
  geom_curve(
    aes(
      x = 2004,
      y = 0.70,
      xend = 2008.5,
      yend = 0.90
    ),
    size = 1,
    curvature = -0.5,
    arrow = arrow(length = unit(0.03, "npc")),
    color = "grey30"
  ) +
  annotate(
    "text",
    x = 2015,
    y = 0.30,
    label = "Após o aumento no final da década de 2000, \na cobertura vacinal se manteve estável \nentre 40% e 70%.",
    size = 3
  ) +
  scale_x_continuous(
    limits = c(2001, 2021),
    breaks = seq(2001, 2021, 2)
  ) +
  scale_y_continuous(
    limits = c(0, 1.0),
    breaks = seq(0, 1.0, 0.20),
    labels = scales::percent
  ) +
  labs(
    title = "Cobertura vacinal para a Febre Amarela na cidade de Santa Maria, RS",
    # subtitle = "",
    x = "",
    y = "Cobertura vacinal",
    caption = "Dados: Base dos Dados (Ministério da Saúde) | @lppufsm"
  ) +
  theme_minimal() +
  theme(
    # plot.margin = unit(c(0.3, 1, 1, 0.1), "cm"),
    axis.title = element_text(size = 10),
    text = element_text(family = "Roboto"),
    plot.background = element_rect(fill = "grey90"),
    plot.caption = element_text(hjust = .95, size = 7)
  )

# ggsave(
#   filename = "imunizacoes_febre_amarela.png",
#   plot = imunizacoes_febre_amarela,
#   path = "output/img/",
#   width = 1800,
#   height = 1800,
#   units = "px",
#   dpi = 300
# )


## HEPATITE B

imunizacoes_hepatite_b <- imunizacoes_tidy |>
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
  labs(
    title = "Cobertura vacinal para Hepatite B na cidade de Santa Maria, RS",
    # subtitle = "",
    x = "",
    y = "Cobertura vacinal",
    caption = "Base dos Dados (Ministério da Saúde) | @lppufsm"
  ) +
  theme_minimal() +
  theme(
    # plot.margin = unit(c(0.3, 1, 1, 0.1), "cm"),
    axis.title = element_text(size = 10),
    text = element_text(family = "Roboto"),
    plot.background = element_rect(fill = "grey90"),
    plot.caption = element_text(hjust = .95, size = 7)
  )

# ggsave(
#   filename = "imunizacoes_hepatite_b.png",
#   plot = imunizacoes_hepatite_b,
#   path = "output/img/",
#   width = 1800,
#   height = 1800,
#   units = "px",
#   dpi = 300
# )


## POLIOMELITE

imunizacoes_poliomelite <- imunizacoes_tidy |>
  dplyr::filter(tipos == "cobertura_poliomielite" & ano >= 2001) |>
  dplyr::mutate(n = n/100) |>
  ggplot() +
  aes(x = ano, y = n) +
  geom_line(size = 1.3, color = "royal blue") +
  annotate(
    "text",
    x = 2020,
    y = 1.25,
    label = "124.76%",
    size = 3.5
  ) +
  annotate(
    "text",
    x = 2013,
    y = 0.35,
    label = "Após atingir o pico em 2018, \na cobertura vacinal atingiu \nseu menor nível em 2021, \ncom 68.01%.",
    size = 3.5
  ) +
  geom_curve(
    aes(
      x = 2017.5,
      y = 0.35,
      xend = 2021,
      yend = 0.65
    ),
    size = 1,
    arrow = arrow(length = unit(0.03, "npc")),
    color = "grey30"
  ) +
  scale_x_continuous(
    limits = c(2001, 2021),
    breaks = seq(2001, 2021, 2)
  ) +
  scale_y_continuous(
    limits = c(0, 1.4),
    breaks = seq(0, 1.4, 0.2),
    labels = scales::percent
  ) +
  labs(
    title = "Cobertura vacinal para a Poliomelite na cidade de Santa Maria, RS",
    x = "",
    y = "Cobertura vacinal",
    caption = "Dados: Base dos Dados (Ministério da Saúde) | @lppufsm"
  ) +
  theme_minimal() +
  theme(
    # plot.margin = unit(c(0.3, 1, 1, 0.1), "cm"),
    axis.title = element_text(size = 10),
    text = element_text(family = "Roboto"),
    plot.background = element_rect(fill = "grey90"),
    plot.caption = element_text(hjust = .95, size = 7)
  )

# ggsave(
#   filename = "imunizacoes_poliomelite.png",
#   plot = imunizacoes_poliomelite,
#   path = "output/img/",
#   width = 1800,
#   height = 1800,
#   units = "px",
#   dpi = 300
# )


## TRÍPLICE BACTERIANA

imunizacao_triplice_bacteriana <- imunizacoes_tidy |>
  dplyr::filter(tipos  == "cobertura_triplice_bacteriana" & ano >= 2001) |>
  dplyr::mutate(n = n/100) |>
  ggplot() +
  aes(x = ano, y = n) +
  geom_line(size = 1.3, color = "royal blue") +
  scale_x_continuous(
    limits = c(2001, 2021),
    breaks = seq(2001, 2021, 2)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    labels = scales::percent
  ) +
  labs(
    title = "Cobertura vacinal para a Tríplice Bacteriana na \ncidade de Santa Maria, RS",
    x = "",
    y = "Cobertura vacinal",
    caption = "Dados: Base dos Dados (Ministério da Saúde) | @lppufsm"
  ) +
  theme_minimal() +
  theme(
    # plot.margin = unit(c(0.3, 1, 1, 0.1), "cm"),
    axis.title = element_text(size = 10),
    text = element_text(family = "Roboto"),
    plot.background = element_rect(fill = "grey90"),
    plot.caption = element_text(hjust = .95, size = 7)
  )


# ggsave(
#   filename = "imunizacao_triplice_bacteriana.png",
#   plot = imunizacao_triplice_bacteriana,
#   path = "output/img/",
#   width = 1800,
#   height = 1800,
#   units = "px",
#   dpi = 300
# )


## DTP + TRÍPLICE BACTERIANA

imunizacoes_dtp_trip_bac <- imunizacoes_tidy |>
  dplyr::filter(tipos == "cobertura_dtp" | tipos == "cobertura_triplice_bacteriana") |>
  tidyr::pivot_wider(
    names_from = tipos,
    values_from = "n"
  )


imunizacoes_dtp_trip_bacteriana <- imunizacoes_dtp_trip_bac |>
  dplyr::filter(ano >= 2001) |>
  dplyr::mutate(
    cobertura_dtp = cobertura_dtp/100,
    cobertura_triplice_bacteriana = cobertura_triplice_bacteriana/100
  ) |>
  tidyr::pivot_longer(cols = !c(ano, sigla_uf, id_municipio),
                      names_to = "cobertura",
                      values_to = "n") |>
  ggplot() +
  aes(x = ano, y = n, color = cobertura) +
  geom_line(size = 1.3) +
  scale_color_manual(
    values = c("royal blue", "red"),
    labels = c("DTP", "Tríplice Bacteriana"),
    guide = guide_legend(),
    name = "Cobertura"
  ) +
  scale_x_continuous(
    limits = c(2001, 2021),
    breaks = seq(2001, 2021, 2)
  ) +
  scale_y_continuous(
    limits = c(0, 1.1),
    breaks = seq(0, 1.1, 0.2),
    labels = scales::percent
  ) +
  labs(
    title = "Cobertura vacinal para DTP e Tríplice Bacteriana na \ncidade de Santa Maria, RS",
    x = "",
    y = "Cobertura vacinal",
    caption = "Base dos Dados (Ministério da Saúde) | @lppufsm"
  ) +
  theme_minimal() +
  theme(
    # plot.margin = unit(c(0.3, 1, 1, 0.1), "cm"),
    axis.title = element_text(size = 10),
    text = element_text(family = "Roboto"),
    plot.background = element_rect(fill = "grey90"),
    plot.caption = element_text(hjust = .95, size = 7),
    legend.position = "bottom",
    legend.spacing.x = unit(0.5, "cm")
  )


# ggsave(
#   filename = "imunizacoes_dtp_trip_bacteriana.png",
#   plot = imunizacoes_dtp_trip_bacteriana,
#   path = "output/img/",
#   width = 1800,
#   height = 1800,
#   units = "px",
#   dpi = 300
# )
