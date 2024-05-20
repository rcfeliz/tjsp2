# main ------------------------------------------------------------------

#' tjsp_cpopg_ler() parseia os dados de um html do cpopg.
#'
#' @param arquivos Vetor contendo o caminho para os arquivos em html do cpopg.
#' Pode receber também apenas a pasta em que estão os arquivos.
#' @param capa Formato das colunas a serem retornadas. É escolhido 'Padronizado' por default.
#' * 'Padronizado' retorna uma tibble com 14 colunas, forçando algumas a
#' aparecerem quando um processo não tiver e excluindo outras colunas
#' que possam aparecer em um processo, mas não em outros
#' * 'Completo' retorna uma tibble sem pré-definir as colunas, podendo trazer
#' resultados distintos para cada processo.
#' @param outros Informações a serem lidas do html. Por default, lê-se tudo ("Tudo").
#' Se quiser ler apenas os metadados principais, info deve ser NULL.
#' @return tibble
#'
#' @examples
#' \dontrun {
#' 1003101-38.2023.8.26.0223 - Processo principal normal (67000FE100000)
#' 0000235-93.2018.8.26.0047 - Processo principal de um incidente
#' 0000235-93.2018.8.26.0047 (8000) - Incidente (1B00023PT1PQ8)
#' 1014289-38.2022.8.26.0037 - Processo principal de um cumprimento de sentença (11000FHR30000)
#' 0004995-42.2023.8.26.0037 - Cumprimento de sentença (11000GKOJ0000)
#' 0006880-32.2020.8.26.0026 - Execução penal (0Q0001BEI0000)
#' }
tjsp_cpopg_ler <- function(arquivos = NULL,
                           formato = c("Padronizado", "Completo"),
                           outros = c("Tudo", "Delegacia", "Partes", "Movimentacoes", "Peticoes Diversas", "Incidentes", "Apensos", "Audiencias", "Historico Classes")) {

  if(is.null(arquivos)){

    arquivos <- list.files(arquivos,
                           full.names = TRUE,
                           pattern = "html$")

  }

  purrr::map_dfr(arquivos, ~{tjsp_cpopg_ler_unitario(arquivo = .x, formato = formato, outros = outros)})

}

tjsp_cpopg_ler_unitario <- function(arquivo = NULL,
                                    formato = c("Padronizado", "Completo"),
                                    outros = c("Tudo", "Delegacia", "Partes", "Movimentacoes", "Peticoes Diversas", "Incidentes", "Apensos", "Audiencias", "Historico Classes")) {

  # extrai informações de capa
  if(length(formato) > 1) {
    formato <- "Padronizado"
    warning("Mais de um 'formato' escolhido. Forçando para o formato padronizado.")
  }

  if(formato == "Padronizado") {
    aux_capa <- tjsp_cpopg_ler_capa_padronizado(arquivo)
  } else {
    aux_capa <- tjsp_cpopg_ler_capa_completo(arquivo)
  }

  if(is.null(outros)) {
    da <- aux_capa
  # extrai demais informações
  } else {
    # ajusta o parâmetro 'info'
    if(any(outros == "Tudo")) outros <- c("Movimentacoes", "Partes", "Delegacia", "Peticoes Diversas", "Incidentes", "Apensos", "Audiencias", "Historico Classes")

    outros <- outros |>
          abjutils::rm_accent() |>
          stringr::str_to_lower() |>
          stringr::str_replace_all(" ", "_")

    # usa as funções para extrair as outras informações
    aux_outros <- purrr::map(outros, ~{
      fn_nm <- glue::glue("tjsp_cpopg_ler_{.x}")
      f <- get(fn_nm)
      aux_outros <- f(arquivo) |>
        tidyr::nest(.by = c(processo, cd_processo), .key = .x)
      return(aux_outros)
    }) |>
      purrr::reduce(dplyr::left_join, by = c("processo", "cd_processo"))

    # join
    da <- aux_capa |>
      dplyr::left_join(aux_outros, by = c("processo", "cd_processo"))
  }
  return(da)
}

# capa functions ----------------------------------------------------------

tjsp_cpopg_ler_capa_padronizado <- function(arquivo = NULL) {
  html <- arquivo |>
    xml2::read_html()

  cd_processo <- html |>
    xml2::xml_find_all("//a[contains(@href,'processo.codigo')]") |>
    xml2::xml_attr("href") |>
    stringr::str_extract("(?<=processo.codigo=)\\w+") |>
    tibble::as_tibble() |>
    dplyr::count(value) |>
    dplyr::filter(n == max(n)) |>
    dplyr::pull(value)

  tipo_processo <- html |>
    xml2::xml_find_first("//span[@class='unj-label']") |>
    xml2::xml_text()

  tipo_processo <- dplyr::case_when(
    tipo_processo == "Classe" ~ "Principal",
    TRUE ~ tipo_processo
  )

  # Informações que mudam a forma de extrair com base no tipo de processo
  if(tipo_processo == "Principal") {
    processo <- html |>
      xml2::xml_find_first("//span[@id='numeroProcesso']") |>
      xml2::xml_text() |>
      abjutils::clean_cnj()

    situacao <- html |>
      xml2::xml_find_first("//span[@id='labelSituacaoProcesso']") |>
      xml2::xml_text()

    classe <- html |>
      xml2::xml_find_first("//span[@id='classeProcesso']") |>
      xml2::xml_text()

  } else {
    processo <- html |>
      xml2::xml_find_first("//span[@class='unj-larger']") |>
      xml2::xml_text() |>
      abjutils::clean_cnj() |>
      stringr::str_extract(".{20}")

    situacao <- html |>
      xml2::xml_find_first("//span[@class='unj-tag']") |>
      xml2::xml_text()

    classe <- html |>
      xml2::xml_find_first("//span[@class='unj-larger']") |>
      xml2::xml_text() |>
      stringr::str_remove_all("\\(.+\\)") |>
      stringr::str_squish()
  }

  # Informações que se extraem da mesma forma, independente do tipo de processo
  digital <- html |>
    xml2::xml_find_first("boolean(//*[@id='linkPasta'] |//*[@id='linkConsultaSG'])")

  assunto <- html |>
    xml2::xml_find_first("//span[@id='assuntoProcesso']") |>
    xml2::xml_text()

  foro <- html |>
    xml2::xml_find_first("//span[@id='foroProcesso']") |>
    xml2::xml_text() |>
    stringr::str_squish()

  vara <- html |>
    xml2::xml_find_first("//span[@id='varaProcesso']") |>
    xml2::xml_text() |>
    stringr::str_squish()

  juiz <- html |>
    xml2::xml_find_first("//span[@id='juizProcesso']") |>
    xml2::xml_text()

  dt_dist <- html |>
    xml2::xml_find_first("//div[@id='dataHoraDistribuicaoProcesso']") |>
    xml2::xml_text() |>
    stringr::str_extract("\\d{2}/\\d{2}/\\d{4}") |>
    lubridate::dmy()

  area <- html |>
    xml2::xml_find_first("//div[@id='areaProcesso']") |>
    xml2::xml_text() |>
    stringr::str_squish()

  controle <- html |>
    xml2::xml_find_first("//div[@id='numeroControleProcesso']") |>
    xml2::xml_text() |>
    stringr::str_squish()

  valor_da_acao <- html |>
    xml2::xml_find_first("//div[@id='valorAcaoProcesso']") |>
    xml2::xml_text() |>
    stringr::str_squish() |>
    stringr::str_remove_all("R\\$|\\.") |>
    stringr::str_replace(",", "\\.") |>
    as.numeric()

  da <- tibble::tibble(
    processo,
    cd_processo,
    tipo_processo,
    digital,
    situacao,
    classe,
    assunto,
    foro,
    vara,
    juiz,
    dt_dist,
    area,
    controle,
    valor_da_acao
  )

  return(da)
}

tjsp_cpopg_ler_capa_completo <- function(arquivo = NULL) {

  html <- arquivo |>
    xml2::read_html()

  cd_processo <- html |>
    xml2::xml_find_all("//a[contains(@href,'processo.codigo')]") |>
    xml2::xml_attr("href") |>
    stringr::str_extract("(?<=processo.codigo=)\\w+") |>
    tibble::as_tibble() |>
    dplyr::count(value) |>
    dplyr::filter(n == max(n)) |>
    dplyr::pull(value)

  tipo_processo <- html |>
    xml2::xml_find_first("//span[@class='unj-label']") |>
    xml2::xml_text()

  tipo_processo <- dplyr::case_when(
    tipo_processo == "Classe" ~ "Principal",
    TRUE ~ tipo_processo
  )

  # Informações que mudam a forma de extrair com base no tipo de processo
  if(tipo_processo == "Principal") {
    processo <- html |>
      xml2::xml_find_first("//span[@id='numeroProcesso']") |>
      xml2::xml_text() |>
      abjutils::clean_cnj()

  } else {
    processo <- html |>
      xml2::xml_find_first("//span[@class='unj-larger']") |>
      xml2::xml_text() |>
      abjutils::clean_cnj() |>
      stringr::str_extract(".{20}")
  }

  digital <- html |>
    xml2::xml_find_first("boolean(//*[@class='linkPasta'] |//*[@class='linkConsultaSG'])")

  situacao <- html |>
    xml2::xml_find_first("//span[@id='labelSituacaoProcesso']") |>
    xml2::xml_text()

  variavel <- html |>
    xml2::xml_find_all("//div//span[@class='unj-label']") |>
    xml2::xml_text() |>
    stringr::str_squish()

  valor <- html |>
    xml2::xml_find_all("//div[span[@class='unj-label']]/div") |>
    xml2::xml_text() |>
    stringr::str_squish()

  da <- tibble::tibble(
    processo,
    cd_processo,
    tipo_processo,
    digital,
    situacao,
    variavel,
    valor
  ) |>
    dplyr::group_by_at(dplyr::vars(-valor)) |>
    dplyr::mutate(row_id = 1:dplyr::n()) |>
    dplyr::ungroup() |>
    tidyr::spread(key = variavel, value = valor) |>
    dplyr::select(-row_id) |>
    janitor::clean_names()

  return(da)
}

# other functions ------------------------------------------------------------------

tjsp_cpopg_ler_partes <- function(arquivo = NULL) {

  html <- arquivo |>
    xml2::read_html()

  cd_processo <- html |>
    xml2::xml_find_all("//a[contains(@href,'processo.codigo')]") |>
    xml2::xml_attr("href") |>
    stringr::str_extract("(?<=processo.codigo=)\\w+") |>
    tibble::as_tibble() |>
    dplyr::count(value) |>
    dplyr::filter(n == max(n)) |>
    dplyr::pull(value)

  tipo_processo <- html |>
    xml2::xml_find_first("//span[@class='unj-label']") |>
    xml2::xml_text()

  tipo_processo <- dplyr::case_when(
    tipo_processo == "Classe" ~ "Principal",
    TRUE ~ tipo_processo
  )

  if(tipo_processo == "Principal") {
    processo <- html |>
      xml2::xml_find_first("//span[@id='numeroProcesso']") |>
      xml2::xml_text() |>
      abjutils::clean_cnj()
  } else {
    processo <- html |>
      xml2::xml_find_first("//span[@class='unj-larger']") |>
      xml2::xml_text() |>
      abjutils::clean_cnj() |>
      stringr::str_extract(".{20}")
  }

  if(xml2::xml_find_first(html,"boolean(//table[@id='tableTodasPartes'])")) {

    da <- html |>
      xml2::xml_find_first("//table[@id='tableTodasPartes']") |>
      rvest::html_table() |>
      setNames(c("tipo_parte","parte")) |>
      tidyr::separate(parte,c("parte","representante"),
                      sep = "(?<=\\S)\\s{10,}", extra = "merge",
                      fill = "right") |>
      tibble::add_column(processo = processo, .before = 1) |>
      tibble::add_column(cd_processo = cd_processo, .after = 1)

  } else {

    da <- html |>
      xml2::xml_find_first("//table[@id='tablePartesPrincipais']") |>
      rvest::html_table() |>
      setNames(c("tipo_parte","parte")) |>
      tidyr::separate(parte,c("parte","representante"),
                      sep = "(?<=\\S)\\s{10,}", extra = "merge",
                      fill = "right") |>
      tibble::add_column(processo = processo, .before = 1) |>
      tibble::add_column(cd_processo = cd_processo, .after = 1)
  }

  return(da)
}

tjsp_cpopg_ler_movimentacoes <- function (arquivo = NULL) {

  html <- arquivo |>
    xml2::read_html()

  # IDs
  cd_processo <- html |>
    xml2::xml_find_all("//a[contains(@href,'processo.codigo')]") |>
    xml2::xml_attr("href") |>
    stringr::str_extract("(?<=processo.codigo=)\\w+") |>
    tibble::as_tibble() |>
    dplyr::count(value) |>
    dplyr::filter(n == max(n)) |>
    dplyr::pull(value)

  tipo_processo <- html |>
    xml2::xml_find_first("//span[@class='unj-label']") |>
    xml2::xml_text()

  tipo_processo <- dplyr::case_when(
    tipo_processo == "Classe" ~ "Principal",
    TRUE ~ tipo_processo
  )

  if(tipo_processo == "Principal") {
    processo <- html |>
      xml2::xml_find_first("//span[@id='numeroProcesso']") |>
      xml2::xml_text() |>
      abjutils::clean_cnj()

  } else {
    processo <- html |>
      xml2::xml_find_first("//span[@class='unj-larger']") |>
      xml2::xml_text() |>
      abjutils::clean_cnj() |>
      stringr::str_extract(".{20}")
  }

  # movimentos
  movs <- html |>
    xml2::xml_find_first("//table/tbody[@id='tabelaTodasMovimentacoes']") |>
    xml2::xml_find_all("./tr")

  dt_mov <- movs |>
    xml2::xml_find_first("./td[contains(@class,'dataMovimentacao')]") |>
    xml2::xml_text(trim = TRUE) |>
    lubridate::dmy()

  anexo <- movs |>
    xml2::xml_find_first("./td[contains(@class,'descricaoMovimentacao')]/a") |>
    xml2::xml_attr("href")

  cd_documento <- anexo |>
    stringr::str_extract("(?<=cdDocumento=)\\d+")

  recurso_acessado <- anexo |>
    stringr::str_extract("(?<=Acessado=).+") |>
    URLdecode() |>
    stringr::str_replace_all("\\+", " ") |>
    stringr::str_replace("NA", NA_character_)

  url <- anexo |>
    purrr::map_chr(~{
      if (is.na(.x)) {
        url <-  NA_character_
      } else if(startsWith(cd_processo, "RI")) {
        p <- processo |>
          abjutils::build_id()

        unificado <- p |>
          stringr::str_sub(1,15)

        foro <- stringr::str_sub(p, -4)

        parseada <- structure(
          list(
            scheme = "https",
            hostname = "esaj.tjsp.jus.br",
            port = NULL,
            path = "cposg/search.do",
            query = list(
              conversationId = "",
              paginaConsulta = "0",
              cbPesquisa = "NUMPROC",
              numeroDigitoAnoUnificado = unificado,
              foroNumeroUnificado = foro,
              dePesquisaNuUnificado = p,
              dePesquisaNuUnificado = "UNIFICADO",
              dePesquisa = "",
              tipoNuProcesso = "UNIFICADO"
            ),
            params = NULL,
            fragment = stringr::str_remove(.x,"#"),
            username = NULL,
            password = NULL
          ),
          class = "url"
        )

        url <- httr2::url_build(parseada)

      } else {
        url <- xml2::url_absolute(.x,"https://esaj.tjsp.jus.br")
      }
      url
    })

  mov <- movs |>
    xml2::xml_find_first("./td[contains(@class,'descricaoMovimentacao')]") |>
    xml2::xml_text(trim = TRUE)

  da <- tibble::tibble(
    processo,
    cd_processo,
    dt_mov,
    mov,
    cd_documento,
    recurso_acessado,
    url
  ) |>
    tidyr::separate(
      col = mov,
      into = c("movimento", "descricao"),
      sep = "\n\\s+", extra = "merge",
      fill = "right"
    ) |>
    dplyr::distinct()

  return(da)

}

tjsp_cpopg_ler_delegacia <- function(arquivo = NULL) {

  html <- arquivo |>
    xml2::read_html()

  cd_processo <- html |>
    xml2::xml_find_all("//a[contains(@href,'processo.codigo')]") |>
    xml2::xml_attr("href") |>
    stringr::str_extract("(?<=processo.codigo=)\\w+") |>
    tibble::as_tibble() |>
    dplyr::count(value) |>
    dplyr::filter(n == max(n)) |>
    dplyr::pull(value)

  tipo_processo <- html |>
    xml2::xml_find_first("//span[@class='unj-label']") |>
    xml2::xml_text()

  tipo_processo <- dplyr::case_when(
    tipo_processo == "Classe" ~ "Principal",
    TRUE ~ tipo_processo
  )

  if(tipo_processo == "Principal") {
    processo <- html |>
      xml2::xml_find_first("//span[@id='numeroProcesso']") |>
      xml2::xml_text() |>
      abjutils::clean_cnj()
  } else {
    processo <- html |>
      xml2::xml_find_first("//span[@class='unj-larger']") |>
      xml2::xml_text() |>
      abjutils::clean_cnj() |>
      stringr::str_extract(".{20}")
  }

  delegacias <- html |>
    xml2::xml_find_all("//tbody[@id='dadosDaDelegacia']") |>
    xml2::xml_find_all("./tr")

  if(purrr::is_empty(delegacias)) {
    da <- tibble::tibble(
      processo,
      cd_processo,
      documento = NA_character_,
      numero = NA_character_,
      distrito = NA_character_,
      municipio = NA_character_
    )
  } else {
    da <- delegacias |>
      xml2::xml_text() |>
      stringr::str_trim() |>
      stringr::str_split("\n\\s+") |>
      purrr::map_dfr(stats::setNames, c("documento","numero", "distrito", "municipio")) |>
      tibble::add_column(processo = processo, .before = 1) |>
      tibble::add_column(cd_processo = cd_processo, .after = 1)
  }

  return(da)

}

tjsp_cpopg_ler_peticoes_diversas <- function(arquivo = NULL) {
  html <- arquivo |>
    xml2::read_html()

  cd_processo <- html |>
    xml2::xml_find_all("//a[contains(@href,'processo.codigo')]") |>
    xml2::xml_attr("href") |>
    stringr::str_extract("(?<=processo.codigo=)\\w+") |>
    tibble::as_tibble() |>
    dplyr::count(value) |>
    dplyr::filter(n == max(n)) |>
    dplyr::pull(value)

  tipo_processo <- html |>
    xml2::xml_find_first("//span[@class='unj-label']") |>
    xml2::xml_text()

  tipo_processo <- dplyr::case_when(
    tipo_processo == "Classe" ~ "Principal",
    TRUE ~ tipo_processo
  )

  if(tipo_processo == "Principal") {
    processo <- html |>
      xml2::xml_find_first("//span[@id='numeroProcesso']") |>
      xml2::xml_text() |>
      abjutils::clean_cnj()
  } else {
    processo <- html |>
      xml2::xml_find_first("//span[@class='unj-larger']") |>
      xml2::xml_text() |>
      abjutils::clean_cnj() |>
      stringr::str_extract(".{20}")
  }

  peticoes_diversas <- html |>
    xml2::xml_find_all("//div/h2[contains(text(),'Peti\u00E7\u00F5es diversas')]/../following-sibling::table[1]/tbody/tr")

  if(purrr::is_empty(peticoes_diversas)) {
    da <- tibble::tibble(
      processo,
      cd_processo,
      dt_peticao = lubridate::dmy(""),
      tipo = NA_character_
    )
  } else {
    da <- peticoes_diversas |>
      xml2::xml_text() |>
      stringr::str_trim() |>
      stringr::str_split("\n\\s+") |>
      purrr::map_dfr(stats::setNames, c("dt_peticao","tipo")) |>
      dplyr::mutate(dt_peticao = lubridate::dmy(dt_peticao)) |>
      tibble::add_column(processo = processo, .before = 1) |>
      tibble::add_column(cd_processo = cd_processo, .after = 1)
  }
}

tjsp_cpopg_ler_incidentes <- function(arquivo = NULL) {
  html <- arquivo |>
    xml2::read_html()

  cd_processo <- html |>
    xml2::xml_find_all("//a[contains(@href,'processo.codigo')]") |>
    xml2::xml_attr("href") |>
    stringr::str_extract("(?<=processo.codigo=)\\w+") |>
    tibble::as_tibble() |>
    dplyr::count(value) |>
    dplyr::filter(n == max(n)) |>
    dplyr::pull(value)

  tipo_processo <- html |>
    xml2::xml_find_first("//span[@class='unj-label']") |>
    xml2::xml_text()

  tipo_processo <- dplyr::case_when(
    tipo_processo == "Classe" ~ "Principal",
    TRUE ~ tipo_processo
  )

  if(tipo_processo == "Principal") {
    processo <- html |>
      xml2::xml_find_first("//span[@id='numeroProcesso']") |>
      xml2::xml_text() |>
      abjutils::clean_cnj()
  } else {
    processo <- html |>
      xml2::xml_find_first("//span[@class='unj-larger']") |>
      xml2::xml_text() |>
      abjutils::clean_cnj() |>
      stringr::str_extract(".{20}")
  }

  incidentes <- html |>
    xml2::xml_find_all("//div/h2[contains(text(),'Incidentes')]/../following-sibling::table[1]") |>
    xml2::xml_find_all(".//tr[@class='fundoEscuro' or @class='fundoClaro'][not(@aria-hidden)]")

  if(purrr::is_empty(incidentes)) {
    da <- tibble::tibble(
      processo,
      cd_processo,
      dt_dist = lubridate::dmy(""),
      classe = NA_character_,
      processo_incidente = NA_character_,
      cd_processo_incidente =NA_character_
    )
  } else {
    cd_processo_incidente <- incidentes |>
      xml2::xml_find_all(".//a[@class='incidente']") |>
      xml2::xml_attr("href") |>
      stringr::str_extract("(?<=processo\\.codigo=).+(?=&)")

    da <- incidentes |>
      xml2::xml_text() |>
      stringr::str_trim() |>
      tibble::as_tibble() |>
      tidyr::separate(
        value,
        c("dt_dist", "classe", "processo_incidente"),
        sep = "(?<=\\S)\\s{10,}", extra = "merge",
        fill = "right"
      ) |>
      dplyr::mutate(
        dt_dist = lubridate::dmy(dt_dist),
        classe = stringr::str_squish(classe),
        processo_incidente = processo_incidente |>
          stringr::str_remove_all("\\s|\\D") |>
          stringr::str_extract(".{20}$") |>
          tidyr::replace_na(processo),
        cd_processo_incidente = cd_processo_incidente
      ) |>
      tibble::add_column(processo = processo, .before = 1) |>
      tibble::add_column(cd_processo = cd_processo, .after = 1)
  }

  return(da)
}

tjsp_cpopg_ler_apensos <- function(arquivo = NULL) {
  html <- arquivo |>
    xml2::read_html()

  cd_processo <- html |>
    xml2::xml_find_all("//a[contains(@href,'processo.codigo')]") |>
    xml2::xml_attr("href") |>
    stringr::str_extract("(?<=processo.codigo=)\\w+") |>
    tibble::as_tibble() |>
    dplyr::count(value) |>
    dplyr::filter(n == max(n)) |>
    dplyr::pull(value)

  tipo_processo <- html |>
    xml2::xml_find_first("//span[@class='unj-label']") |>
    xml2::xml_text()

  tipo_processo <- dplyr::case_when(
    tipo_processo == "Classe" ~ "Principal",
    TRUE ~ tipo_processo
  )

  if(tipo_processo == "Principal") {
    processo <- html |>
      xml2::xml_find_first("//span[@id='numeroProcesso']") |>
      xml2::xml_text() |>
      abjutils::clean_cnj()
  } else {
    processo <- html |>
      xml2::xml_find_first("//span[@class='unj-larger']") |>
      xml2::xml_text() |>
      abjutils::clean_cnj() |>
      stringr::str_extract(".{20}")
  }

  apensos <- html |>
    xml2::xml_find_all("//tbody[@id='dadosApenso']") |>
    xml2::xml_find_all("./tr")

  if(purrr::is_empty(apensos)) {
    da <- tibble::tibble(
      processo,
      cd_processo,
      numero = NA_character_,
      classe = NA_character_,
      dt_apensamento = lubridate::dmy("")
    )
  } else {
    processo_apensamento <- apensos |>
      xml2::xml_find_all("./td[@width='15%']") |>
      xml2::xml_text() |>
      abjutils::clean_cnj() |>
      stringr::str_extract("^.{20}")

    classe <- apensos |>
      xml2::xml_find_all("./td[@width='20%']") |>
      xml2::xml_text()

    dt_apensamento <- apensos |>
      xml2::xml_find_all("./td[@width='10%']") |>
      xml2::xml_text() |>
      lubridate::dmy()

    motivo <- apensos |>
      xml2::xml_find_all("./td[@width='50%']") |>
      xml2::xml_text() |>
      stringr::str_squish()

    da <- tibble::tibble(
      processo,
      cd_processo,
      processo_apensamento,
      classe,
      dt_apensamento,
      motivo
    )
  }

  return(da)
}

tjsp_cpopg_ler_audiencias <- function(arquivo = NULL) {
  html <- arquivo |>
    xml2::read_html()

  cd_processo <- html |>
    xml2::xml_find_all("//a[contains(@href,'processo.codigo')]") |>
    xml2::xml_attr("href") |>
    stringr::str_extract("(?<=processo.codigo=)\\w+") |>
    tibble::as_tibble() |>
    dplyr::count(value) |>
    dplyr::filter(n == max(n)) |>
    dplyr::pull(value)

  tipo_processo <- html |>
    xml2::xml_find_first("//span[@class='unj-label']") |>
    xml2::xml_text()

  tipo_processo <- dplyr::case_when(
    tipo_processo == "Classe" ~ "Principal",
    TRUE ~ tipo_processo
  )

  if(tipo_processo == "Principal") {
    processo <- html |>
      xml2::xml_find_first("//span[@id='numeroProcesso']") |>
      xml2::xml_text() |>
      abjutils::clean_cnj()
  } else {
    processo <- html |>
      xml2::xml_find_first("//span[@class='unj-larger']") |>
      xml2::xml_text() |>
      abjutils::clean_cnj() |>
      stringr::str_extract(".{20}")
  }

  audiencias <- html |>
    xml2::xml_find_all("//div/h2[contains(text(),'Audi\u00eancia')]/../following-sibling::table[1]") |>
    xml2::xml_find_all(".//tr[@class='fundoEscuro' or @class='fundoClaro'][not(@aria-hidden)]")

  if(purrr::is_empty(audiencias)) {
    da <- tibble::tibble(
      processo,
      cd_processo,
      dt_audiencia = lubridate::dmy(""),
      audiencia = NA_character_,
      situacao = NA_character_,
      qt_pessoas = NA_integer_,
      depoimento_arquivo = NA_character_,
      depoimento_url = NA_character_
    )
  } else {
    dt_audiencia <- audiencias |>
      xml2::xml_find_all("./td[1]") |>
      xml2::xml_text(trim = TRUE) |>
      lubridate::dmy()

    audiencia <- audiencias |>
      xml2::xml_find_all("./td[2]") |>
      xml2::xml_find_first("./text()") |>
      xml2::xml_text() |>
      stringr::str_squish()

    situacao <- audiencias |>
      xml2::xml_find_all("./td[3]") |>
      xml2::xml_text(trim = TRUE)

    qt_pessoas <- audiencias |>
      xml2::xml_find_all("./td[4]") |>
      xml2::xml_text(trim = TRUE) |>
      as.integer()

    depoimentos <- audiencias |>
      xml2::xml_find_all("./td[2]") |>
      xml2::xml_find_all(".//div")

    if(purrr::is_empty(depoimentos)) {
      depoimento_arquivo <- NA_character_

      depoimento_url <- NA_character_
    } else {
      depoimento_arquivo <- depoimentos |>
        xml2::xml_find_all("./a") |>
        xml2::xml_text() |>
        stringr::str_squish()

      depoimento_url <- depoimentos |>
        xml2::xml_find_all("./a") |>
        xml2::xml_attr("href") |>
        paste0("https://esaj.tjsp.jus.br", . = _)
    }

    da <- tibble::tibble(
      processo,
      cd_processo,
      dt_audiencia,
      audiencia,
      situacao,
      qt_pessoas,
      depoimento_arquivo,
      depoimento_url
    )
  }

  return(da)
}

tjsp_cpopg_ler_historico_classes <- function(arquivo = NULL) {
  html <- arquivo |>
    xml2::read_html()

  cd_processo <- html |>
    xml2::xml_find_all("//a[contains(@href,'processo.codigo')]") |>
    xml2::xml_attr("href") |>
    stringr::str_extract("(?<=processo.codigo=)\\w+") |>
    tibble::as_tibble() |>
    dplyr::count(value) |>
    dplyr::filter(n == max(n)) |>
    dplyr::pull(value)

  tipo_processo <- html |>
    xml2::xml_find_first("//span[@class='unj-label']") |>
    xml2::xml_text()

  tipo_processo <- dplyr::case_when(
    tipo_processo == "Classe" ~ "Principal",
    TRUE ~ tipo_processo
  )

  if(tipo_processo == "Principal") {
    processo <- html |>
      xml2::xml_find_first("//span[@id='numeroProcesso']") |>
      xml2::xml_text() |>
      abjutils::clean_cnj()
  } else {
    processo <- html |>
      xml2::xml_find_first("//span[@class='unj-larger']") |>
      xml2::xml_text() |>
      abjutils::clean_cnj() |>
      stringr::str_extract(".{20}")
  }

  historico_classes <- html |>
    xml2::xml_find_all("//div/h2[contains(text(),'Hist\u00f3rico de classes')]/../following-sibling::table[1]") |>
    xml2::xml_find_all("./tbody/tr")

  if(purrr::is_empty(historico_classes)) {
    da <- tibble::tibble(
      processo,
      cd_processo,
      dt_modificacao = lubridate::dmy(""),
      tipo = NA_character_,
      classe = NA_character_,
      area = NA_character_,
      motivo = NA_character_
    )
  } else {

    da <- historico_classes |>
      xml2::xml_text() |>
      stringr::str_trim() |>
      stringr::str_split("\\s*\n\t\\s*") |>
      purrr::map_dfr(stats::setNames, c("dt_modificacao","tipo", "classe", "area", "motivo")) |>
      dplyr::mutate(dt_modificacao = lubridate::dmy(dt_modificacao)) |>
      tibble::add_column(processo = processo, .before = 1) |>
      tibble::add_column(cd_processo = cd_processo, .after = 1)

  }

  return(da)
}

