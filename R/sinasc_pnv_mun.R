#' Scrapes SINASC data from cities
#'
#' This function allows the user to retrive data from
#' SINASC database much in the same way that is done
#' by the online portal. The argument options refer to
#' data focused on brazilian cities.
#'
#' @usage sinasc_pnv_mun(linha = "Município", coluna = "Não ativa",
#'   conteudo = 1, periodo = "last", municipio = "all", capital = "all",
#'   cir = "all", macrorregiao_de_saude = "all", microrregiao_ibge = "all",
#'   ride = "all", territorio_da_cidadania = "all", mesorregiao_pndr = "all",
#'   amazonia_legal = "all", semiarido = "all", faixa_de_fronteira = "all",
#'   zona_de_fronteira = "all", municipio_de_extrema_pobreza = "all",
#'   local_ocorrencia = "all", idade_da_mae = "all", instrucao_da_mae = "all",
#'   estado_civil_mae = "all", duracao_gestacao = "all", tipo_de_gravidez = "all",
#'   tipo_de_parto = "all", consult_pre_natal = "all", sexo = "all", cor_raca = "all",
#'   apgar_1_minuto = "all", apgar_5_minuto = "all", peso_ao_nascer = "all",
#'   anomalia_congenita = "all", tipo_anomal_congen = "all")
#' @param linha A character describing which element will be displayed in the rows of the data.frame. Defaults to "Município".
#' @param coluna A character describing which element will be displayed in the columns of the data.frame. Defaults to "Não ativa".
#' @param conteudo A character of length = 1 with the state's acronym of interest.
#' @param periodo A character vector describing the period of data. Defaults to the last available.
#' @param municipio "all" or a numeric vector with the IBGE's city codes codes to filter the data. Defaults to "all".
#' @param capital "all" or a numeric vector with the IBGE's cities codes to filter the data. Defaults to "all".
#' @param cir "all" or a numeric vector with the CIR's codes to filter the data. Defaults to "all".
#' @param macrorregiao_de_saude "all" or a numeric vector with the Health macro-region's codes to filter the data. Defaults to "all".
#' @param microrregiao_ibge "all" or a numeric vector with the IBGE's micro-region codes to filter the data. Defaults to "all".
#' @param ride "all" or a numeric vector with the IBGE's metropolitan-region codes to filter the data. Defaults to "all".
#' @param territorio_da_cidadania "all" or a numeric vector with the territory of citizenship codes to filter the data. Defaults to "all".
#' @param mesorregiao_pndr "all" or a numeric vector with the PNDR's mesoregion codes to filter the data. Defaults to "all".
#' @param amazonia_legal "all" or a character ("Sim" or "Não") indicating if only the Legal Amazon region must be included. Defaults to "all".
#' @param semiarido "all" or a character ("Sim" or "Não") indicating if only the semiarid region must be included. Defaults to "all".
#' @param faixa_de_fronteira "all" or a character ("Sim" or "Não") indicating if only the border area must be included. Defaults to "all".
#' @param zona_de_fronteira "all" or a character ("Sim" or "Não") indicating if only the border strip must be included. Defaults to "all".
#' @param municipio_de_extrema_pobreza "all" or a character ("Sim" or "Não") indicating if only the municipalities of extreme poverty must be included. Defaults to "all".
#' @param local_ocorrencia "all" or a character vector with the place of ocurrence to filter the data. Defaults to "all".
#' @param idade_da_mae "all" or a character vector with the age range (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param instrucao_da_mae "all" or a character vector with the mother's instruction (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param estado_civil_mae "all" or a character vector with the marital status of the mother (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param duracao_gestacao "all" or a character vector with the marital status of the mother (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param tipo_de_gravidez "all" or a character vector with the type of pregnancy (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param tipo_de_parto "all" or a character vector with the Parturition type (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param consult_pre_natal "all" or a character vector with the amount of prenatal consultation  (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param sexo "all" or a character vector with the gender (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param cor_raca "all" or a character vector with the color/race (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param apgar_1_minuto "all" or a character vector with the value of the apgar exam of the first minute (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param apgar_5_minuto "all" or a character vector with the value of the apgar exam of the fifth minute (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param peso_ao_nascer "all" or a character vector with the birth weight (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param anomalia_congenita "all" or a character vector with the indicative of congenital anomaly (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @param tipo_anomal_congen "all" or a character vector with the congenital anomaly type (written in the same way) or the number corresponding to the order of the option in the online layout to filter the data. Defaults to "all".
#' @return The function returns a data frame printed by parameters input.
#' @author Renato Prado Siqueira \email{<rpradosiqueira@@gmail.com>}
#' @seealso \code{\link{sinasc_pnv_uf}}
#' @examples
#' \dontrun{
#' ## Requesting data from the city of Campo Grande/MS
#' sinasc_pnv_mun(municipio = 500270)
#' }
#'
#' @keywords SINASC datasus
#' @importFrom magrittr %>%
#' @importFrom utils head
#' @export

sinasc_pnv_mun <- function(linha = "Munic\u00edpio", coluna = "N\u00e3o ativa", conteudo = 1, periodo = "last", municipio = "all",
                          capital = "all", cir = "all", macrorregiao_de_saude = "all", microrregiao_ibge = "all", ride = "all",
                          territorio_da_cidadania = "all", mesorregiao_pndr = "all", amazonia_legal = "all", semiarido = "all",
                          faixa_de_fronteira = "all", zona_de_fronteira = "all", municipio_de_extrema_pobreza = "all",
                          local_ocorrencia = "all", idade_da_mae = "all", instrucao_da_mae = "all", estado_civil_mae = "all",
                          duracao_gestacao = "all", tipo_de_gravidez = "all", tipo_de_parto = "all", consult_pre_natal = "all",
                          sexo = "all", cor_raca = "all", apgar_1_minuto = "all", apgar_5_minuto = "all", peso_ao_nascer = "all",
                          anomalia_congenita = "all", tipo_anomal_congen = "all") {


  page <- xml2::read_html("http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sinasc/cnv/pnvbr.def")

  #### DF ####
  linha.df <- data.frame(id = page %>% rvest::html_nodes("#L option") %>% rvest::html_text() %>% trimws(),
                         value = page %>% rvest::html_nodes("#L option") %>% rvest::html_attr("value"))
  linha.df[] <- lapply(linha.df, as.character)

  coluna.df <- data.frame(id = page %>% rvest::html_nodes("#C option") %>% rvest::html_text() %>% trimws(),
                          value = page %>% rvest::html_nodes("#C option") %>% rvest::html_attr("value"))
  coluna.df[] <- lapply(coluna.df, as.character)

  conteudo.df <- data.frame(id1 = c(1, 2),
                            id2 = c("Nascim p/resid.m\u00e3e", "Nascim p/ocorr\u00eanc"),
                            value = c("Nascim_p/resid.m\u00e3e", "Nascim_p/ocorr\u00eanc"))

  periodos.df <- data.frame(id = page %>% rvest::html_nodes("#A option") %>% rvest::html_text() %>% as.numeric(),
                            value = page %>% rvest::html_nodes("#A option") %>% rvest::html_attr("value"))

  municipios.df <- suppressWarnings(data.frame(id = page %>% rvest::html_nodes("#S1 option") %>% rvest::html_text() %>% readr::parse_number(),
                                               value = page %>% rvest::html_nodes("#S1 option") %>% rvest::html_attr("value")))

  capital.df <- suppressWarnings(data.frame(id = page %>% rvest::html_nodes("#S2 option") %>% rvest::html_text() %>% readr::parse_number(),
                                            value = page %>% rvest::html_nodes("#S2 option") %>% rvest::html_attr("value")))

  cir.df <- suppressWarnings(data.frame(id = page %>% rvest::html_nodes("#S3 option") %>% rvest::html_text() %>% readr::parse_number(),
                                        value = page %>% rvest::html_nodes("#S3 option") %>% rvest::html_attr("value")))

  macrorregiao_de_saude.df <- suppressWarnings(data.frame(id = page %>% rvest::html_nodes("#S4 option") %>% rvest::html_text() %>% readr::parse_number(),
                                                          value = page %>% rvest::html_nodes("#S4 option") %>% rvest::html_attr("value")))

  microrregiao_ibge.df <- suppressWarnings(data.frame(id = page %>% rvest::html_nodes("#S5 option") %>% rvest::html_text() %>% readr::parse_number(),
                                                      value = page %>% rvest::html_nodes("#S5 option") %>% rvest::html_attr("value")))

  ride.df <- suppressWarnings(data.frame(id = page %>% rvest::html_nodes("#S6 option") %>% rvest::html_text() %>% readr::parse_number(),
                                         value = page %>% rvest::html_nodes("#S6 option") %>% rvest::html_attr("value")))

  territorio_da_cidadania.df <- suppressWarnings(data.frame(id = page %>% rvest::html_nodes("#S7 option") %>% rvest::html_text() %>% readr::parse_number(),
                                                            value = page %>% rvest::html_nodes("#S7 option") %>% rvest::html_attr("value")))

  mesorregiao_pndr.df <- suppressWarnings(data.frame(id = page %>% rvest::html_nodes("#S8 option") %>% rvest::html_text() %>% readr::parse_number(),
                                                     value = page %>% rvest::html_nodes("#S8 option") %>% rvest::html_attr("value")))

  amazonia_legal.df <- data.frame(id = page %>% rvest::html_nodes("#S9 option") %>% rvest::html_text() %>% trimws(),
                                  value = page %>% rvest::html_nodes("#S9 option") %>% rvest::html_attr("value"))
  amazonia_legal.df[] <- lapply(amazonia_legal.df, as.character)

  semiarido.df <- data.frame(id = page %>% rvest::html_nodes("#S10 option") %>% rvest::html_text() %>% trimws(),
                             value = page %>% rvest::html_nodes("#S10 option") %>% rvest::html_attr("value"))
  semiarido.df[] <- lapply(semiarido.df, as.character)

  faixa_de_fronteira.df <- data.frame(id = page %>% rvest::html_nodes("#S11 option") %>% rvest::html_text() %>% trimws(),
                                      value = page %>% rvest::html_nodes("#S11 option") %>% rvest::html_attr("value"))
  faixa_de_fronteira.df[] <- lapply(faixa_de_fronteira.df, as.character)

  zona_de_fronteira.df <- data.frame(id = page %>% rvest::html_nodes("#S12 option") %>% rvest::html_text() %>% trimws(),
                                     value = page %>% rvest::html_nodes("#S12 option") %>% rvest::html_attr("value"))
  zona_de_fronteira.df[] <- lapply(zona_de_fronteira.df, as.character)

  municipio_de_extrema_pobreza.df <- data.frame(id = page %>% rvest::html_nodes("#S13 option") %>% rvest::html_text() %>% trimws(),
                                                value = page %>% rvest::html_nodes("#S13 option") %>% rvest::html_attr("value"))
  municipio_de_extrema_pobreza.df[] <- lapply(municipio_de_extrema_pobreza.df, as.character)

  local_ocorrencia.df <- data.frame(id = page %>% rvest::html_nodes("#S14 option") %>% rvest::html_text() %>% trimws(),
                                    value = page %>% rvest::html_nodes("#S14 option") %>% rvest::html_attr("value"))
  local_ocorrencia.df[] <- lapply(local_ocorrencia.df, as.character)

  idade_da_mae.df <- data.frame(id = page %>% rvest::html_nodes("#S15 option") %>% rvest::html_text() %>% trimws(),
                                value = page %>% rvest::html_nodes("#S15 option") %>% rvest::html_attr("value"))
  idade_da_mae.df[] <- lapply(idade_da_mae.df, as.character)

  instrucao_da_mae.df <- data.frame(id = page %>% rvest::html_nodes("#S16 option") %>% rvest::html_text() %>% trimws(),
                                    value = page %>% rvest::html_nodes("#S16 option") %>% rvest::html_attr("value"))
  instrucao_da_mae.df[] <- lapply(instrucao_da_mae.df, as.character)

  estado_civil_mae.df <- data.frame(id = page %>% rvest::html_nodes("#S17 option") %>% rvest::html_text() %>% trimws(),
                                    value = page %>% rvest::html_nodes("#S17 option") %>% rvest::html_attr("value"))
  estado_civil_mae.df[] <- lapply(estado_civil_mae.df, as.character)

  duracao_gestacao.df <- data.frame(id = page %>% rvest::html_nodes("#S18 option") %>% rvest::html_text() %>% trimws(),
                                    value = page %>% rvest::html_nodes("#S18 option") %>% rvest::html_attr("value"))
  duracao_gestacao.df[] <- lapply(duracao_gestacao.df, as.character)

  tipo_de_gravidez.df <- data.frame(id = page %>% rvest::html_nodes("#S19 option") %>% rvest::html_text() %>% trimws(),
                                    value = page %>% rvest::html_nodes("#S19 option") %>% rvest::html_attr("value"))
  tipo_de_gravidez.df[] <- lapply(tipo_de_gravidez.df, as.character)

  tipo_de_parto.df <- data.frame(id = page %>% rvest::html_nodes("#S20 option") %>% rvest::html_text() %>% trimws(),
                                 value = page %>% rvest::html_nodes("#S20 option") %>% rvest::html_attr("value"))
  tipo_de_parto.df[] <- lapply(tipo_de_parto.df, as.character)

  consult_pre_natal.df <- data.frame(id = page %>% rvest::html_nodes("#S21 option") %>% rvest::html_text() %>% trimws(),
                                     value = page %>% rvest::html_nodes("#S21 option") %>% rvest::html_attr("value"))
  consult_pre_natal.df[] <- lapply(consult_pre_natal.df, as.character)

  sexo.df <- data.frame(id = page %>% rvest::html_nodes("#S22 option") %>% rvest::html_text() %>% trimws(),
                        value = page %>% rvest::html_nodes("#S22 option") %>% rvest::html_attr("value"))
  sexo.df[] <- lapply(sexo.df, as.character)

  cor_raca.df <- data.frame(id = page %>% rvest::html_nodes("#S23 option") %>% rvest::html_text() %>% trimws(),
                            value = page %>% rvest::html_nodes("#S23 option") %>% rvest::html_attr("value"))
  cor_raca.df[] <- lapply(cor_raca.df, as.character)

  apgar_1_minuto.df <- data.frame(id = page %>% rvest::html_nodes("#S24 option") %>% rvest::html_text() %>% trimws(),
                                  value = page %>% rvest::html_nodes("#S24 option") %>% rvest::html_attr("value"))
  apgar_1_minuto.df[] <- lapply(apgar_1_minuto.df, as.character)

  apgar_5_minuto.df <- data.frame(id = page %>% rvest::html_nodes("#S25 option") %>% rvest::html_text() %>% trimws(),
                                  value = page %>% rvest::html_nodes("#S25 option") %>% rvest::html_attr("value"))
  apgar_5_minuto.df[] <- lapply(apgar_5_minuto.df, as.character)

  peso_ao_nascer.df <- data.frame(id = page %>% rvest::html_nodes("#S26 option") %>% rvest::html_text() %>% trimws(),
                                  value = page %>% rvest::html_nodes("#S26 option") %>% rvest::html_attr("value"))
  peso_ao_nascer.df[] <- lapply(peso_ao_nascer.df, as.character)

  anomalia_congenita.df <- data.frame(id = page %>% rvest::html_nodes("#S27 option") %>% rvest::html_text() %>% trimws(),
                                      value = page %>% rvest::html_nodes("#S27 option") %>% rvest::html_attr("value"))
  anomalia_congenita.df[] <- lapply(anomalia_congenita.df, as.character)

  tipo_anomal_congen.df <- data.frame(id = page %>% rvest::html_nodes("#S28 option") %>% rvest::html_text() %>% trimws(),
                                      value = page %>% rvest::html_nodes("#S28 option") %>% rvest::html_attr("value"))
  tipo_anomal_congen.df[] <- lapply(tipo_anomal_congen.df, as.character)

  municipios.df$id[1] <- capital.df$id[1] <- cir.df$id[1] <- macrorregiao_de_saude.df$id[1] <- microrregiao_ibge.df$id[1] <- "all"
  territorio_da_cidadania.df$id[1] <- mesorregiao_pndr.df$id[1] <- amazonia_legal.df$id[1] <- semiarido.df$id[1] <- "all"
  faixa_de_fronteira.df$id[1] <- zona_de_fronteira.df$id[1] <- municipio_de_extrema_pobreza.df$id[1] <- "all"
  ride.df$id[1] <- local_ocorrencia.df$id[1]<- idade_da_mae.df$id[1] <- instrucao_da_mae.df$id[1] <- estado_civil_mae.df$id[1] <- "all"
  duracao_gestacao.df$id[1] <- tipo_de_gravidez.df$id[1] <- tipo_de_parto.df$id[1] <- consult_pre_natal.df$id[1] <- sexo.df$id[1] <- "all"
  cor_raca.df$id[1] <- apgar_1_minuto.df$id[1] <- apgar_5_minuto.df$id[1] <- peso_ao_nascer.df$id[1] <- anomalia_congenita.df$id[1] <- "all"
  tipo_anomal_congen.df$id[1] <- "all"

  #### ERROR HANDLING ####
  if (linha != "Munic\u00edpio") {

    if (!is.character(linha)) stop("The 'linha' argument must be a character element")

    if(length(linha) != 1) stop("The 'linha' argument must have only one element")

    if (!(all(linha %in% linha.df$id))) {

      if (!(all(linha %in% linha.df$value))) {

        stop("The 'linha' argument is misspecified")

      }

    }

  }

  if (coluna != "N\u00e3o ativa") {

    if (!is.character(coluna)) stop("The 'coluna' argument must be a character element")

    if(length(coluna) != 1) stop("The 'coluna' argument must have only one element")

    if (!(all(coluna %in% coluna.df$id))) {

      if (!(all(coluna %in% coluna.df$value))) {

        stop("The 'coluna' argument is misspecified")

      }

    }

  }

  if (conteudo != 1 & conteudo != 2) {

    if (is.numeric(conteudo)) stop("The only numeric elements allowed are 1 or 2")

    if(length(conteudo) != 1) stop("The 'coluna' argument must have only one element")

    if (!(all(conteudo %in% conteudo.df$id2))) {

      if (!(all(conteudo %in% conteudo.df$value))) {

        stop("The 'conteudo' argument is misspecified")

      }

    }

  }

  if (periodo[1] != "last" & periodo[1] != "all") {

    if (is.character(periodo)) {
      periodo <- as.numeric(periodo)
    }

    if (!(all(periodo %in% periodos.df$id))) stop("The 'periodo' argument is misspecified")

  }

  if (any(municipio != "all")) {

    municipio <- as.character(municipio)

    if (!(all(municipio %in% municipios.df$id))) stop("Some element in 'municipio' argument is wrong")

  }

  if (any(capital != "all")) {

    capital <- as.character(capital)

    if (!(all(capital %in% capital.df$id))) stop("Some element in 'capital' argument is wrong")

  }

  if (any(cir != "all")) {

    cir <- as.character(cir)

    if (!(all(cir %in% cir.df$id))) stop("Some element in 'cir' argument is wrong")

  }

  if (any(macrorregiao_de_saude != "all")) {

    macrorregiao_de_saude <- as.character(macrorregiao_de_saude)

    if (!(all(macrorregiao_de_saude %in% macrorregiao_de_saude.df$id))) stop("Some element in 'macrorregiao_de_saude' argument is wrong")

  }

  if (any(microrregiao_ibge != "all")) {

    microrregiao_ibge <- as.character(microrregiao_ibge)

    if (!(all(microrregiao_ibge %in% microrregiao_ibge.df$id))) stop("Some element in 'microrregiao_ibge' argument is wrong")

  }

  if (any(ride != "all")) {

    ride <- as.character(ride)

    if (!(all(ride %in% ride.df$id))) stop("Some element in 'ride' argument is wrong")

  }

  if (any(territorio_da_cidadania != "all")) {

    territorio_da_cidadania <- as.character(as.numeric(territorio_da_cidadania))

    if (!(all(territorio_da_cidadania %in% territorio_da_cidadania.df$id))) stop("Some element in 'territorio_da_cidadania' argument is wrong")

  }

  if (any(mesorregiao_pndr != "all")) {

    mesorregiao_pndr <- as.character(as.numeric(mesorregiao_pndr))

    if (!(all(mesorregiao_pndr %in% mesorregiao_pndr.df$id))) stop("Some element in 'mesorregiao_pndr' argument is wrong")

  }

  if (any(amazonia_legal != "all")) {

    amazonia_legal <- as.character(amazonia_legal)

    if (!(all(amazonia_legal %in% amazonia_legal.df$id))) stop("The element in 'amazonia_legal' argument is wrong")

  }

  if (any(semiarido != "all")) {

    semiarido <- as.character(semiarido)

    if (!(all(semiarido %in% semiarido.df$id))) stop("The element in 'semiarido' argument is wrong")

  }

  if (any(faixa_de_fronteira != "all")) {

    faixa_de_fronteira <- as.character(faixa_de_fronteira)

    if (!(all(faixa_de_fronteira %in% faixa_de_fronteira.df$id))) stop("The element in 'faixa_de_fronteira' argument is wrong")

  }

  if (any(zona_de_fronteira != "all")) {

    zona_de_fronteira <- as.character(zona_de_fronteira)

    if (!(all(zona_de_fronteira %in% zona_de_fronteira.df$id))) stop("The element in 'zona_de_fronteira' argument is wrong")

  }

  if (any(municipio_de_extrema_pobreza != "all")) {

    municipio_de_extrema_pobreza <- as.character(municipio_de_extrema_pobreza)

    if (!(all(municipio_de_extrema_pobreza %in% municipio_de_extrema_pobreza.df$id))) stop("The element in 'municipio_de_extrema_pobreza' argument is wrong")

  }

  if (any(local_ocorrencia != "all")) {

    if (!(all(local_ocorrencia %in% local_ocorrencia.df$id))) {

      local_ocorrencia <- as.character(local_ocorrencia)

      if (!(all(local_ocorrencia %in% local_ocorrencia.df$value))) {

        stop("Some element in 'local_ocorrencia' argument is wrong")

      }

    }

  }

  if (any(idade_da_mae != "all")) {

    if (!(all(idade_da_mae %in% idade_da_mae.df$id))) {

      idade_da_mae <- as.character(idade_da_mae)

      if (!(all(idade_da_mae %in% idade_da_mae.df$value))) {

        stop("Some element in 'idade_da_mae' argument is wrong")

      }

    }

  }

  if (any(instrucao_da_mae != "all")) {

    if (!(all(instrucao_da_mae %in% instrucao_da_mae.df$id))) {

      instrucao_da_mae <- as.character(instrucao_da_mae)

      if (!(all(instrucao_da_mae %in% instrucao_da_mae.df$value))) {

        stop("Some element in 'instrucao_da_mae' argument is wrong")

      }

    }

  }

  if (any(estado_civil_mae != "all")) {

    if (!(all(estado_civil_mae %in% estado_civil_mae.df$id))) {

      estado_civil_mae <- as.character(estado_civil_mae)

      if (!(all(estado_civil_mae %in% estado_civil_mae.df$value))) {

        stop("Some element in 'estado_civil_mae' argument is wrong")

      }

    }

  }

  if (any(duracao_gestacao != "all")) {

    if (!(all(duracao_gestacao %in% duracao_gestacao.df$id))) {

      duracao_gestacao <- as.character(duracao_gestacao)

      if (!(all(duracao_gestacao %in% duracao_gestacao.df$value))) {

        stop("Some element in 'duracao_gestacao' argument is wrong")

      }

    }

  }

  if (any(tipo_de_gravidez != "all")) {

    if (!(all(tipo_de_gravidez %in% tipo_de_gravidez.df$id))) {

      tipo_de_gravidez <- as.character(tipo_de_gravidez)

      if (!(all(tipo_de_gravidez %in% tipo_de_gravidez.df$value))) {

        stop("Some element in 'tipo_de_gravidez' argument is wrong")

      }

    }

  }

  if (any(tipo_de_parto != "all")) {

    if (!(all(tipo_de_parto %in% tipo_de_parto.df$id))) {

      tipo_de_parto <- as.character(tipo_de_parto)

      if (!(all(tipo_de_parto %in% tipo_de_parto.df$value))) {

        stop("Some element in 'tipo_de_parto' argument is wrong")

      }

    }

  }

  if (any(consult_pre_natal != "all")) {

    if (!(all(consult_pre_natal %in% consult_pre_natal.df$id))) {

      consult_pre_natal <- as.character(consult_pre_natal)

      if (!(all(consult_pre_natal %in% consult_pre_natal.df$value))) {

        stop("Some element in 'consult_pre_natal' argument is wrong")

      }

    }

  }

  if (any(sexo != "all")) {

    if (!(all(sexo %in% sexo.df$id))) {

      sexo <- as.character(sexo)

      if (!(all(sexo %in% sexo.df$value))) {

        stop("Some element in 'sexo' argument is wrong")

      }

    }

  }

  if (any(cor_raca != "all")) {

    if (!(all(cor_raca %in% cor_raca.df$id))) {

      cor_raca <- as.character(cor_raca)

      if (!(all(cor_raca %in% cor_raca.df$value))) {

        stop("Some element in 'cor_raca' argument is wrong")

      }

    }

  }

  if (any(apgar_1_minuto != "all")) {

    if (!(all(apgar_1_minuto %in% apgar_1_minuto.df$id))) {

      apgar_1_minuto <- as.character(apgar_1_minuto)

      if (!(all(apgar_1_minuto %in% apgar_1_minuto.df$value))) {

        stop("Some element in 'apgar_1_minuto' argument is wrong")

      }

    }

  }

  if (any(apgar_5_minuto != "all")) {

    if (!(all(apgar_5_minuto %in% apgar_5_minuto.df$id))) {

      apgar_5_minuto <- as.character(apgar_5_minuto)

      if (!(all(apgar_5_minuto %in% apgar_5_minuto.df$value))) {

        stop("Some element in 'apgar_5_minuto' argument is wrong")

      }

    }

  }

  if (any(peso_ao_nascer != "all")) {

    if (!(all(peso_ao_nascer %in% peso_ao_nascer.df$id))) {

      peso_ao_nascer <- as.character(peso_ao_nascer)

      if (!(all(peso_ao_nascer %in% peso_ao_nascer.df$value))) {

        stop("Some element in 'peso_ao_nascer' argument is wrong")

      }

    }

  }

  if (any(anomalia_congenita != "all")) {

    if (!(all(anomalia_congenita %in% anomalia_congenita.df$id))) {

      anomalia_congenita <- as.character(anomalia_congenita)

      if (!(all(anomalia_congenita %in% anomalia_congenita.df$value))) {

        stop("Some element in 'anomalia_congenita' argument is wrong")

      }

    }

  }

  if (any(tipo_anomal_congen != "all")) {

    if (!(all(tipo_anomal_congen %in% tipo_anomal_congen.df$id))) {

      tipo_anomal_congen <- as.character(tipo_anomal_congen)

      if (!(all(tipo_anomal_congen %in% tipo_anomal_congen.df$value))) {

        stop("Some element in 'tipo_anomal_congen' argument is wrong")

      }

    }

  }


  #### FILTERS APPLICATIONS ####

  #linha
  if (linha %in% linha.df$id) {
    linha <- dplyr::filter(linha.df, linha.df$id %in% linha)
    linha <- linha$value
  }

  if (!stringi::stri_enc_isascii(linha)) {
    form_linha <- paste0("Linha=", stringi::stri_escape_unicode(linha))
  } else {
    form_linha <- paste0("Linha=", linha)
  }

  #coluna
  if (coluna %in% coluna.df$id) {
    coluna <- dplyr::filter(coluna.df, coluna.df$id %in% coluna)
    coluna <- coluna$value
  }

  if (!stringi::stri_enc_isascii(coluna)) {
    form_coluna <- paste0("Coluna=", stringi::stri_escape_unicode(coluna))
  } else {
    form_coluna <- paste0("Coluna=", coluna)
  }

  #conteudo
  form_conteudo <- conteudo.df$value[conteudo]
  if (!stringi::stri_enc_isascii(form_conteudo)) {
    form_conteudo <- paste0("Incremento=", stringi::stri_escape_unicode(form_conteudo))
  } else {
    form_conteudo <- paste0("Incremento=", form_conteudo)
  }

  #periodo
  suppressWarnings( if (periodo == "last") {periodo <- utils::head(periodos.df$id, 1)} )
  suppressWarnings( if (periodo == "all") {periodo <- periodos.df$id} )
  form_periodo <- dplyr::filter(periodos.df, periodos.df$id %in% periodo)
  form_periodo <- paste0("Arquivos=", form_periodo$value, collapse = "&")

  form_pesqmes1 <- "pesqmes1=Digite+o+texto+e+ache+f%E1cil"

  #municipio
  form_municipio <- dplyr::filter(municipios.df, municipios.df$id %in% municipio)
  form_municipio <- paste0("SMunic%EDpio=", form_municipio$value, collapse = "&")

  form_pesqmes2 <- "pesqmes2=Digite+o+texto+e+ache+f%E1cil"

  #capital
  form_capital <- dplyr::filter(capital.df, capital.df$id %in% capital)
  form_capital <- paste0("SCapital=", form_capital$value, collapse = "&")

  form_pesqmes3 <- "pesqmes3=Digite+o+texto+e+ache+f%E1cil"

  #cir
  form_cir <- dplyr::filter(cir.df, cir.df$id %in% cir)
  form_cir <- paste0("SRegi%E3o_de_Sa%FAde_%28CIR%29=", form_cir$value, collapse = "&")

  form_pesqmes4 <- "pesqmes4=Digite+o+texto+e+ache+f%E1cil"

  #macrorregiao_de_saude
  form_macrorregiao_de_saude <- dplyr::filter(macrorregiao_de_saude.df, macrorregiao_de_saude.df$id %in% macrorregiao_de_saude)
  form_macrorregiao_de_saude <- paste0("SMacrorregi%E3o_de_Sa%FAde=", form_macrorregiao_de_saude$value, collapse = "&")

  form_pesqmes5 <- "pesqmes5=Digite+o+texto+e+ache+f%E1cil"

  #microrregiao_ibge
  form_microrregiao_ibge <- dplyr::filter(microrregiao_ibge.df, microrregiao_ibge.df$id %in% microrregiao_ibge)
  form_microrregiao_ibge <- paste0("SMicrorregi%E3o_IBGE=", form_microrregiao_ibge$value, collapse = "&")

  form_pesqmes6 <- "pesqmes6=Digite+o+texto+e+ache+f%E1cil"

  #ride
  form_ride <- dplyr::filter(ride.df, ride.df$id %in% ride)
  form_ride <- paste0("SRegi%E3o_Metropolitana_-_RIDE=", form_ride$value, collapse = "&")

  form_pesqmes7 <- "pesqmes7=Digite+o+texto+e+ache+f%E1cil"

  #territorio_da_cidadania
  form_territorio_da_cidadania <- dplyr::filter(territorio_da_cidadania.df, territorio_da_cidadania.df$id %in% territorio_da_cidadania)
  form_territorio_da_cidadania <- paste0("STerrit%F3rio_da_Cidadania=", form_territorio_da_cidadania$value, collapse = "&")

  form_pesqmes8 <- "pesqmes8=Digite+o+texto+e+ache+f%E1cil"

  #mesorregiao_pndr
  form_mesorregiao_pndr <- dplyr::filter(mesorregiao_pndr.df, mesorregiao_pndr.df$id %in% mesorregiao_pndr)
  form_mesorregiao_pndr <- paste0("SMesorregi%E3o_PNDR=", form_mesorregiao_pndr$value, collapse = "&")

  #amazonia_legal
  form_amazonia_legal <- dplyr::filter(amazonia_legal.df, amazonia_legal.df$id %in% amazonia_legal)
  form_amazonia_legal <- paste0("SAmaz%F4nia_Legal=", form_amazonia_legal$value, collapse = "&")

  #semiarido
  form_semiarido <- dplyr::filter(semiarido.df, semiarido.df$id %in% semiarido)
  form_semiarido <- paste0("SSemi%E1rido=", form_semiarido$value, collapse = "&")

  #faixa_de_fronteira
  form_faixa_de_fronteira <- dplyr::filter(faixa_de_fronteira.df, faixa_de_fronteira.df$id %in% faixa_de_fronteira)
  form_faixa_de_fronteira <- paste0("SFaixa_de_Fronteira=", form_faixa_de_fronteira$value, collapse = "&")

  #zona_de_fronteira
  form_zona_de_fronteira <- dplyr::filter(zona_de_fronteira.df, zona_de_fronteira.df$id %in% zona_de_fronteira)
  form_zona_de_fronteira <- paste0("SZona_de_Fronteira=", form_zona_de_fronteira$value, collapse = "&")

  #municipio_de_extrema_pobreza
  form_municipio_de_extrema_pobreza <- dplyr::filter(municipio_de_extrema_pobreza.df, municipio_de_extrema_pobreza.df$id %in% municipio_de_extrema_pobreza)
  form_municipio_de_extrema_pobreza <- paste0("SMunic%EDpio_de_extrema_pobreza=", form_municipio_de_extrema_pobreza$value, collapse = "&")

  #local_ocorrencia
  form_local_ocorrencia <- dplyr::filter(local_ocorrencia.df, local_ocorrencia.df$id %in% local_ocorrencia)
  form_local_ocorrencia <- paste0("SLocal_ocorr%EAncia=", form_local_ocorrencia$value, collapse = "&")

  form_pesqmes15 <- "pesqmes15=Digite+o+texto+e+ache+f%E1cil"

  #idade_da_mae
  form_idade_da_mae <- dplyr::filter(idade_da_mae.df, idade_da_mae.df$id %in% idade_da_mae)
  form_idade_da_mae <- paste0("SIdade_da_m%E3e=", form_idade_da_mae$value, collapse = "&")

  form_pesqmes16 <- "pesqmes16=Digite+o+texto+e+ache+f%E1cil"

  #instrucao_da_mae
  form_instrucao_da_mae <- dplyr::filter(instrucao_da_mae.df, instrucao_da_mae.df$id %in% instrucao_da_mae)
  form_instrucao_da_mae <- paste0("SInstru%E7%E3o_da_m%E3e=", form_instrucao_da_mae$value, collapse = "&")

  #estado_civil_mae
  form_estado_civil_mae <- dplyr::filter(estado_civil_mae.df, estado_civil_mae.df$id %in% estado_civil_mae)
  form_estado_civil_mae <- paste0("SEstado_civil_m%E3e=", form_estado_civil_mae$value, collapse = "&")

  #duracao_gestacao
  form_duracao_gestacao <- dplyr::filter(duracao_gestacao.df, duracao_gestacao.df$id %in% duracao_gestacao)
  form_duracao_gestacao <- paste0("SDura%E7%E3o_gesta%E7%E3o=", form_duracao_gestacao$value, collapse = "&")

  #tipo_de_gravidez
  form_tipo_de_gravidez <- dplyr::filter(tipo_de_gravidez.df, tipo_de_gravidez.df$id %in% tipo_de_gravidez)
  form_tipo_de_gravidez <- paste0("STipo_de_gravidez=", form_tipo_de_gravidez$value, collapse = "&")

  #tipo_de_parto
  form_tipo_de_parto <- dplyr::filter(tipo_de_parto.df, tipo_de_parto.df$id %in% tipo_de_parto)
  form_tipo_de_parto <- paste0("STipo_de_parto=", form_tipo_de_parto$value, collapse = "&")

  #consult_pre_natal
  form_consult_pre_natal <- dplyr::filter(consult_pre_natal.df, consult_pre_natal.df$id %in% consult_pre_natal)
  form_consult_pre_natal <- paste0("SConsult_pr%E9-natal=", form_consult_pre_natal$value, collapse = "&")

  #sexo
  form_sexo <- dplyr::filter(sexo.df, sexo.df$id %in% sexo)
  form_sexo <- paste0("SSexo=", form_sexo$value, collapse = "&")

  #cor_raca
  form_cor_raca <- dplyr::filter(cor_raca.df, cor_raca.df$id %in% cor_raca)
  form_cor_raca <- paste0("SCor%2Fra%E7a=", form_cor_raca$value, collapse = "&")

  #apgar_1_minuto
  form_apgar_1_minuto <- dplyr::filter(apgar_1_minuto.df, apgar_1_minuto.df$id %in% apgar_1_minuto)
  form_apgar_1_minuto <- paste0("SApgar_1%BA_minuto=", form_apgar_1_minuto$value, collapse = "&")

  #apgar_5_minuto
  form_apgar_5_minuto <- dplyr::filter(apgar_5_minuto.df, apgar_5_minuto.df$id %in% apgar_5_minuto)
  form_apgar_5_minuto <- paste0("SApgar_5%BA_minuto=", form_apgar_5_minuto$value, collapse = "&")

  #peso_ao_nascer
  form_peso_ao_nascer <- dplyr::filter(peso_ao_nascer.df, peso_ao_nascer.df$id %in% peso_ao_nascer)
  form_peso_ao_nascer <- paste0("SPeso_ao_nascer=", form_peso_ao_nascer$value, collapse = "&")

  #anomalia_congenita
  form_anomalia_congenita <- dplyr::filter(anomalia_congenita.df, anomalia_congenita.df$id %in% anomalia_congenita)
  form_anomalia_congenita <- paste0("SAnomalia_cong%EAnita=", form_anomalia_congenita$value, collapse = "&")

  form_pesqmes28 <- "pesqmes28=Digite+o+texto+e+ache+f%E1cil"

  #tipo_anomal_congen
  form_tipo_anomal_congen <- dplyr::filter(tipo_anomal_congen.df, tipo_anomal_congen.df$id %in% tipo_anomal_congen)
  form_tipo_anomal_congen <- paste0("STipo_anomal_cong%EAn=", form_tipo_anomal_congen$value, collapse = "&")


  form_data <- paste(form_linha, form_coluna, form_conteudo, form_periodo, form_pesqmes1, form_municipio,
                     form_pesqmes2, form_capital, form_pesqmes3, form_cir, form_pesqmes4, form_macrorregiao_de_saude,
                     form_pesqmes5, form_microrregiao_ibge, form_pesqmes6, form_ride, form_pesqmes7,
                     form_territorio_da_cidadania, form_pesqmes8, form_mesorregiao_pndr, form_amazonia_legal,
                     form_semiarido, form_faixa_de_fronteira, form_zona_de_fronteira, form_municipio_de_extrema_pobreza,
                     form_local_ocorrencia, form_pesqmes15, form_idade_da_mae, form_pesqmes16, form_instrucao_da_mae,
                     form_estado_civil_mae, form_duracao_gestacao, form_tipo_de_gravidez, form_tipo_de_parto,
                     form_consult_pre_natal, form_sexo, form_cor_raca, form_apgar_1_minuto, form_apgar_5_minuto,
                     form_peso_ao_nascer, form_anomalia_congenita, form_pesqmes28, form_tipo_anomal_congen,
                     "formato=table&mostre=Mostra", sep = "&")

  form_data <- gsub("\\\\u00", "%", form_data)

  ##### REQUEST FORM AND DATA WRANGLING ####
  site <- httr::POST(url = paste0("http://tabnet.datasus.gov.br/cgi/tabcgi.exe?sinasc/cnv/pnvbr.def"),
                     body = form_data)

  tabdados <- httr::content(site, encoding = "Latin1") %>%
    rvest::html_nodes(".tabdados tbody td") %>%
    rvest::html_text() %>%
    trimws()

  col_tabdados <- httr::content(site, encoding = "Latin1") %>%
    rvest::html_nodes("th") %>%
    rvest::html_text() %>%
    trimws()

  f1 <- function(x) x <- gsub("\\.", "", x)
  f2 <- function(x) x <- as.numeric(as.character(x))

  tabela_final <- as.data.frame(matrix(data = tabdados, nrow = length(tabdados)/length(col_tabdados),
                                       ncol = length(col_tabdados), byrow = TRUE))

  names(tabela_final) <- col_tabdados

  tabela_final[-1] <- lapply(tabela_final[-1], f1)
  tabela_final[-1] <- suppressWarnings(lapply(tabela_final[-1], f2))

  tabela_final

}

