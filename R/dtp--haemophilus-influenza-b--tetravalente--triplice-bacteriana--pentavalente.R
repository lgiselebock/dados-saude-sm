
# ADICIONA FONTE ----------------------------------------------------------

sysfonts::font_add_google(name = "Roboto", family = "Roboto")
showtext::showtext_opts(dpi = 300)
showtext::showtext_auto()


# PACOTES NECESSARIOS -----------------------------------------------------

library(ggplot2)


# IMPORTAÇÃO DOS DADOS ----------------------------------------------------

imunizacoes <- readr::read_rds("dados/imunizacoes.rds")


# TIDY --------------------------------------------------------------------

imuno <- imunizacoes |>
  dplyr::select(ano,
                cobertura_dtp,
                cobertura_haemophilus_influenza_b,
                cobertura_tetravalente,
                cobertura_hepatite_b,
                cobertura_penta) |>
  dplyr::mutate(
    cobertura_dtp = cobertura_dtp/100,
    cobertura_haemophilus_influenza_b = cobertura_haemophilus_influenza_b/100,
    cobertura_tetravalente = cobertura_tetravalente/100,
    cobertura_hepatite_b = cobertura_hepatite_b/100,
    cobertura_penta = cobertura_penta/100
  ) |>
  tidyr::pivot_longer(
    cols = !(ano),
    names_to = "cobertura",
    values_to = "n"
  )


## FUNÇÃO

funcao_para_iterar <- function(tipo_painel, dados = imuno) {
  imuno |>
    dplyr::mutate(
      realce = factor(dplyr::if_else(cobertura == tipo_painel, 1, 0)),
      painel = tipo_painel
    )
}


# VISUALIZAÇÃO ------------------------------------------------------------

imuno_visualizar <-
  purrr::map_dfr(unique(imuno$cobertura),
                 funcao_para_iterar) |>
  # dplyr::filter(ano >= 2001) |>
  dplyr::mutate(
    cobertura = dplyr::case_when(
      cobertura == "cobertura_dtp" ~ "DTP",
      cobertura == "cobertura_haemophilus_influenza_b" ~ "Haemophilus \nInfluenza B",
      cobertura == "cobertura_tetravalente" ~ "Tetravalente",
      cobertura == "cobertura_hepatite_b" ~ "Hepatite B",
      cobertura == "cobertura_penta" ~ "Pentavalente"
    ),
    painel = dplyr::case_when(
      painel == "cobertura_dtp" ~ "DTP",
      painel == "cobertura_haemophilus_influenza_b" ~ "Haemophilus \nInfluenza B",
      painel == "cobertura_tetravalente" ~ "Tetravalente",
      painel == "cobertura_hepatite_b" ~ "Hepatite B",
      painel == "cobertura_penta" ~ "Pentavalente"
    ),
    numero = ifelse((ano == min(ano) |
                       ano == max(ano)) & realce == 1, 1, 0),
    hjust = dplyr::case_when(ano == min(ano) ~ 1.2,
                             ano == max(ano) ~ -0.2,
                             TRUE ~ NA_real_)
  )


imuno_visualizar |>
  dplyr::filter(realce == 0) |>
  dplyr::mutate(
    cobertura = factor(
      cobertura,
      levels = c(
        "DTP",
        "Haemophilus \nInfluenza B",
        "Tetravalente",
        "Hepatite B",
        "Pentavalente"
      )
    )
  ) |>
  ggplot() +
  aes(
    x = ano,
    y = cobertura,
    group = cobertura,
    color = realce
  ) +
  geom_line(
    color = "grey60",
    size = 1.3,
    show.legend = FALSE
  ) +
  geom_line(
    data = dplyr::filter(imuno_visualizar, realce == 1),
    size = 1.5,
    color = "royal blue",
    show.legend = FALSE
  ) +
  # geom_point(
  #   data = dplyr::filter(imuno_visualizar, numero == 1),
  #   aes(
  #     x = ano,
  #     y = n
  #   ),
  #   color = "black",
  #   show.legend = FALSE
  # ) +
  scale_x_continuous(
    position = "top",
    limits = c(1995, 2021),
    breaks = seq(1995, 2021, 3)
  ) +
  labs(
    title = "Cobertura vacinal na cidade de Santa Maria, RS",
    subtitle = "A evolução da DTP para a Pentavalente",
    x = "",
    y = "",
    caption = "Dados: Base dos Dados (Ministério da Saúde) | @lppufsm"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.line.x = element_line(),
    text = element_text(family = "Roboto"),
    plot.background = element_rect(fill = "grey90"),
    plot.caption = element_text(hjust = 0.95, size = 7),
    strip.text.y.left = element_text(angle = 0, hjust = 0, size = 8),
    plot.subtitle = element_text(size = 10)
  ) +
  facet_wrap(vars(painel), nrow = 5, strip.position = "left") +
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



imuno_dtp_penta <- imuno_visualizar |>
  dplyr::mutate(
    cobertura = factor(
      cobertura,
      levels = c(
        "DTP",
        "Haemophilus \nInfluenza B",
        "Tetravalente",
        "Hepatite B",
        "Pentavalente"
      )
    )
  ) |>
  ggplot() +
  aes(x = ano, y = n, color = cobertura) +
  geom_line(size = 2.5, show.legend = FALSE) +
  scale_x_continuous(
    limits = c(1995, 2021),
    breaks = seq(1995, 2021, 3),
    position = "top"
  ) +
  scale_color_manual(
    values = c(
      "#98bbde",
      "#5588bb",
      "#2763a2",
      "#004388",
      "#002c53"
    )
  ) +
  labs(
    title = "Cobertura vacinal na cidade de Santa Maria, RS",
    subtitle = "A evolução da DTP para a Pentavalente",
    x = "",
    y = "",
    caption = "Dados: Base dos Dados (Ministério da Saúde) | @lppufsm"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.line.x = element_line(),
    text = element_text(family = "Roboto"),
    plot.background = element_rect(fill = "grey90"),
    plot.caption = element_text(hjust = 0.95, size = 7),
    strip.text.y.left = element_text(angle = 0, hjust = 0, size = 8),
    plot.subtitle = element_text(size = 10),
    # legend.position = "bottom"
  ) +
  facet_wrap(vars(cobertura), nrow = 5, strip.position = "left")


ggsave(
  filename = "imuno_dtp_penta.png",
  plot = imuno_dtp_penta,
  path = "output/img/",
  width = 1800,
  height = 1800,
  units = "px",
  dpi = 300
)


# #berry mist
# scale_color_manual(
#   values = c(
#     "#5737f5",
#     "#3240af",
#     "#2633a1",
#     "#0d0d6d",
#     "#00003d"
#   )
# ) +
