
# ADICIONA FONTE ----------------------------------------------------------

sysfonts::font_add_google(name = "Roboto", family = "Roboto")
showtext::showtext_opts(dpi = 300)
showtext::showtext_auto()


# PACOTES NECESSARIOS -----------------------------------------------------

library(ggplot2)


# IMPORTAÇÃO DOS DADOS ----------------------------------------------------

imunizacoes <- readr::read_rds("dados/imunizacoes.rds")


# TIDY --------------------------------------------------------------------

imunizacoes_tidy_lollipop <- imunizacoes |>
  dplyr::filter(ano == 2011 | ano == 2021) |>
  dplyr::select(
    ano,
    id_municipio,
    cobertura_total,
    cobertura_bcg,
    cobertura_febre_amarela,
    cobertura_hepatite_b,
    cobertura_meningococo,
    cobertura_pneumococica,
    cobertura_poliomielite,
    cobertura_rotavirus,
    cobertura_triplice_viral_d1
  ) |>
  tidyr::pivot_longer(
    cols = !c(ano, id_municipio),
    names_to = "cobertura",
    values_to = "n"
  ) |>
  tidyr::pivot_wider(names_from = ano,
                     values_from = "n") |>
  dplyr::mutate(
    cobertura = dplyr::case_when(
      cobertura == "cobertura_total" ~ "Total",
      cobertura == "cobertura_bcg" ~ "BCG",
      cobertura == "cobertura_febre_amarela" ~ "Febre Amarela",
      cobertura == "cobertura_hepatite_b" ~ "Hepatite B",
      cobertura == "cobertura_meningococo" ~ "Meningococo",
      cobertura == "cobertura_pneumococica" ~ "Pneumocócica",
      cobertura == "cobertura_poliomielite" ~ "Poliomelite",
      cobertura == "cobertura_rotavirus" ~ "Rotavirus",
      cobertura == "cobertura_triplice_viral_d1" ~ "Dose 1 \nTríplice Viral"
    )
  )


# VISUALIZAÇÃO ------------------------------------------------------------

imunizacao_lollipop <- imunizacoes_tidy_lollipop |>
  dplyr::mutate(
    round_2011 = round(`2011`),
    round_2021 = round(`2021`),
    cobertura = forcats::fct_rev(cobertura),
    hjust_2011 = round_2011+1,
    hjust_2021 = round_2021+1
  ) |>
  ggplot() +
  geom_segment(
    aes(
      x = `2011`,
      xend = `2021`,
      y = cobertura,
      yend = cobertura
    ),
    color = "grey",
    size = 1.3
  ) +
  geom_point(
    aes(
      x = `2011`,
      y = cobertura,
      color = "`2011`"
    ),
    size = 3
  ) +
  geom_point(
    aes(
      x = `2021`,
      y = cobertura,
      color = "`2021`"
    ),
    size = 3
  ) +
  geom_text(
    aes(
      x = `2011`,
      y = cobertura,
      label = stringr::str_glue("{`round_2011`}%")
    ),
    vjust = -1.5,
    nudge_x = -1.5,
    size = 8/.pt
  ) +
  geom_text(
    aes(
      x = `2021`,
      y = cobertura,
      label = stringr::str_glue("{`round_2021`}%")
    ),
    vjust = -1.5,
    nudge_x = +1.5,
    size = 8/.pt
  ) +
  scale_color_manual(
    # values = c("#93C9E1", "#E43125"),
    values = c("royal blue", "red"),
    labels = c(2011, 2021),
    guide = guide_legend(),
    name = "Anos"
  ) +
  labs(
    x = "",
    y = "",
    caption = "Dados: Ministério da Saúde | @lppufsm",
    title = "Cobertura vacinal para a cidade de Santa Maria, RS",
    subtitle = "Uma comparação entre os anos 2011 e 2021"
  ) +
  theme_minimal() +
  theme(
    # plot.margin = unit(c(0.3, 1, 1, 1), "cm"),
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "bottom",
    text = element_text(family = "Roboto", size = 10)
  )



# SALVANDO O GRÁFICO ------------------------------------------------------

# ggsave(
#   filename = "imunizacao_lollipop.png",
#   plot = imunizacao_lollipop,
#   path = "output/img/",
#   width = 1800,
#   height = 1800,
#   units = "px",
#   dpi = 300
# )
