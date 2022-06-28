
# AUTENTICACAO ------------------------------------------------------------

basedosdados::set_billing_id("luisagisele-ufsm")


# ATENCAO BASICA ----------------------------------------------------------

atencao_basica <-
  "SELECT *
  FROM `basedosdados.br_ms_atencao_basica.municipio`
  WHERE id_municipio = '4316907'" |>
  basedosdados::read_sql() |>
  dplyr::mutate_if(bit64::is.integer64, as.integer)


# IMUNIZACOES -------------------------------------------------------------

imunizacoes <-
  "SELECT *
  FROM `basedosdados.br_ms_imunizacoes.municipio`
  WHERE id_municipio = '4316907'" |>
  basedosdados::read_sql() |>
  dplyr::mutate_if(bit64::is.integer64, as.integer)


# MORTALIDADE -------------------------------------------------------------

mortalidade <-
  "SELECT *
  FROM `basedosdados.br_ms_sim.municipio_causa_idade_sexo_raca`
  WHERE id_municipio = '4316907'" |>
  basedosdados::read_sql() |>
  dplyr::mutate_if(bit64::is.integer64, as.integer)


# ESTABELECIMENTOS --------------------------------------------------------

estabelecimentos <-
  "SELECT *
  FROM `basedosdados.br_ms_cnes.estabelecimento`
  WHERE id_municipio = '4316907'" |>
  basedosdados::read_sql() |>
  dplyr::mutate_if(bit64::is.integer64, as.integer)


# SALVANDO OS DADOS ---------------------------------------------------------

## ATENÇÃO BÁSICA

readr::write_rds(atencao_basica, "dados/atencao_basica.rds")

## IMUNIZAÇÕES

readr::write_rds(imunizacoes, "dados/imunizacoes.rds")

## MORTALIDADE

readr::write_rds(mortalidade, "dados/mortalidade.rds")

## ESTABELECIMENTOS

readr::write_rds(estabelecimentos, "dados/estabelecimentos.rds")
