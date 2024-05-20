#' Baixar dados da consulta processual de primeiro grau
#'
#' @param processos Númeração Única do processo (CNJ) ou Código do Processo
#' @param diretorio Diretório onde será armazenado o html
#'
#' @return html da consulta
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tjsp_cpopg_baixar("10031013820238260223") # exemplo típico de processo
#' tjsp_cpopg_baixar("00002359320188260047") # exemplo de processo em que um único processo gera uma lista de processos
#' tjsp_cpopg_baixar("JK0001UPV0000") # exemplo típico de cd_processo
#' tjsp_cpopg_baixar("JK00024VK0000") # exemplo de cd_processo de incidente
#' tjsp_cpopg_baixar(c("10091165420168260001", "WW0015T010000"))
#' }
#'
tjsp_cpopg_baixar <- function(processos = NULL,  diretorio = ".") {

  cookies <- httr2::last_request()$options$cookiefile

  if(is.null(cookies)) {
    stop("Faça login no esaj, por meio da função tjsp_autenticar()")
  }

  purrr::walk(processos, ~{
    processo <- .x |>
      stringr::str_remove_all("\\W")

    if(stringr::str_length(processo) == 20) {
      tjsp_cpopg_baixar_processo(processo = .x, diretorio = diretorio)

      Sys.sleep(1)
    }

    if(stringr::str_length(processo) == 13) {
      tjsp_cpopg_baixar_cd_processo(cd_processo = .x, diretorio = diretorio)
    }
  })
}

#' Baixar dados da consulta processual de primeiro grau com base na numeração única do processo
#'
#' @param processo Númeração Única do processo (CNJ)
#' @param diretorio Diretório onde será armazenado o html
#'
#' @return html da consulta
#'
#' @examples
#' \dontrun{
#' tjsp_cpopg_baixar_processo("10031013820238260223") # exemplo típico
#' tjsp_cpopg_baixar_processo("00002359320188260047") # exemplo em que um único processo gera uma lista de processos
#'}
#'
tjsp_cpopg_baixar_processo <- function(processo, diretorio = "."){

  url <- "https://esaj.tjsp.jus.br/cpopg/search.do?gateway=true"

  cookies <- httr2::last_request()$options$cookiefile

  # Define request
  req <- httr2::request(url)  |>
    httr2::req_cookie_preserve(cookies) |>
    httr2::req_options(ssl_verifypeer = 0)

  # Build query
  processo <- processo |>
    stringr::str_remove_all("\\D+")

  unificado <- processo |>
    stringr::str_extract(".{13}")

  foro <- processo |>
    stringr::str_extract("\\d{4}$")

  query <- list(
    conversationId = "",
    dadosConsulta.localPesquisa.cdLocal = "-1",
    cbPesquisa = "NUMPROC",
    dadosConsulta.tipoNuProcesso = "UNIFICADO",
    numeroDigitoAnoUnificado = unificado,
    foroNumeroUnificado = foro,
    dadosConsulta.valorConsultaNuUnificado = processo,
    dadosConsulta.valorConsulta = "",
    uuidCaptcha = ""
  )

  # Perfom response
  resp <- req |>
    httr2::req_url_query(!!!query) |>  # query
    httr2::req_perform()

  # html
  conteudo <- resp |>
    httr2::resp_body_html()

  if(xml2::xml_find_first(conteudo, "boolean(//div[@id='listagemDeProcessos'])")) {

    cd_processos <- conteudo |>
      xml2::xml_find_all("//a[@class='linkProcesso']") |>
      xml2::xml_attr("href") |>
      stringr::str_extract( "(?<=processo\\.codigo=)\\w+")

    purrr::walk(cd_processos, tjsp_cpopg_baixar_cd_processo)

  } else if(xml2::xml_find_first(conteudo, "boolean(//div[@id='modalIncidentes'])")) {

    cd_processos <- conteudo |>
      xml2::xml_find_all("//input[@id='processoSelecionado']") |>
      xml2::xml_attr("value")

    purrr::walk(cd_processos, tjsp_cpopg_baixar_cd_processo)

  } else {

    cd_processo <- conteudo |>
      xml2::xml_find_first("//script[contains(text(),'processo.codigo')]") |>
      xml2::xml_text() |>
      stringr::str_extract("(?<=processo.codigo=)\\w+")

    id <- glue::glue("{processo}_{cd_processo}")

    arquivo <- fs::path(diretorio, id, ext = "html")

    writeBin(resp$body, arquivo)

  }
}

#' Baixar dados da consulta processual de primeiro grau com base no código do processo
#'
#' @param cd_processo Código do processo
#' @param diretorio Diretório onde armazenar os arquivos.
#'
#' @return html da consulta
#'
#' @examples
#' \dontrun{
#'tjsp_cpopg_baixar_cd_processo("67000FE100000") # exemplo típico
#' tjsp_cpopg_baixar_cd_processo("1B00023PT1PQ8") # exemplo de incidente
#'}
#'
tjsp_cpopg_baixar_cd_processo <- function(cd_processo, diretorio = "."){
  cd_processo <- stringr::str_extract(cd_processo,"\\w+")

  url <- glue::glue("https://esaj.tjsp.jus.br/cpopg/show.do?processo.codigo={cd_processo}&gateway=true")

  cookies <- httr2::last_request()$options$cookiefile

  resp <- url |>
    httr2::request() |>
    httr2::req_cookie_preserve(cookies) |>
    httr2::req_options(ssl_verifypeer = FALSE) |>
    httr2::req_perform()

  processo <- resp |>
    httr2::resp_body_html() |>
    xml2::xml_find_first("//span[@id='numeroProcesso']") |>
    xml2::xml_text() |>
    stringr::str_remove_all("\\W")

  if(is.na(processo)) {
    processo <- resp |>
      httr2::resp_body_html() |>
      xml2::xml_find_first("//span[@class='unj-larger']") |>
      xml2::xml_text() |>
      stringr::str_remove_all("\\W") |>
      stringr::str_extract_all("\\d{20}")
  }

  id <- glue::glue("{processo}_{cd_processo}")

  arquivo <- fs::path(diretorio, id, ext = "html")

  writeBin(resp$body, arquivo)

}

