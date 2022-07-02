
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


# VISUALIZAÇÃO ------------------------------------------------------------


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
