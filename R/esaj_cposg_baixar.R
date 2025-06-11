#' Baixar dados da consulta processual de segundo grau
#'
#' @param processos Númeração Única do processo (CNJ) ou Código do Processo
#' @param diretorio Diretório onde será armazenado o html
#' @param cookies_path Caminho para salver os cookies (usar somente em casos de paralização)
#'
#' @return html da consulta
#'
#' @export
#'
#' @examples
#' \dontrun{
#' Exemplo de processo de segundo grau
#' esaj_cposg_baixar("20598403820228260000")
#'
#' Exemplo de processo de primeiro grau que gera lista de processos de segundo grau
#' esaj_cposg_baixar("10001414820158260625")
#'
#' Exemplo de cd_processo
#' esaj_cposg_baixar("RI004CVB30000")
#' }
#'
esaj_cposg_baixar <- function(processos = NULL, diretorio = ".", cookies_path = NULL) {

  if (!fs::dir_exists(diretorio)) {
    fs::dir_create(diretorio)
  }

  purrr::walk(processos, ~{
    processo <- .x |>
      stringr::str_remove_all("\\W")

    if(stringr::str_length(processo) == 20) {
      esaj_cposg_baixar_processo(
        processo = .x,
        diretorio = diretorio,
        cookies_path = cookies_path
      )
      Sys.sleep(1)
    }

    if(stringr::str_length(processo) == 13) {
      esaj_cposg_baixar_cd_processo(
        cd_processo = .x,
        diretorio = diretorio,
        cookies_path = cookies_path
      )
    }
  })

}

#' Baixar dados da consulta processual de segundo grau com base na numeração única do processo
#'
#' @param processo Númeração Única do processo (CNJ)
#' @param diretorio Diretório onde será armazenado o html
#' @param cookies_path Caminho para salver os cookies (usar somente em casos de paralização)
#'
#' @return html da consulta
#'
#' @examples
#' \dontrun{
#' Exemplo de processo de segundo grau
#' esaj_cposg_baixar("20598403820228260000")
#'
#' Exemplo de processo de primeiro grau que gera lista de processos de segundo grau
#' esaj_cposg_baixar("10001414820158260625")
#'}
#'
esaj_cposg_baixar_processo <- function(processo = NULL, diretorio = ".", cookies_path = NULL) {

  cookies <- cookies(cookies_path)

  url <- "https://esaj.tjsp.jus.br/cposg/search.do?"

  # query
  processo <- processo |>
    stringr::str_remove_all("\\D+")

  unificado <- processo |>
    stringr::str_extract(".{13}")

  foro <- processo |>
    stringr::str_extract("\\d{4}$")

  query <-  list(
    conversationId = "",
    paginaConsulta = "1",
    localPesquisa.cdLocal = "-1",
    cbPesquisa = "NUMPROC",
    tipoNuProcesso = "UNIFICADO",
    numeroDigitoAnoUnificado = unificado,
    foroNumeroUnificado = foro,
    dePesquisaNuUnificado = processo,
    dePesquisa = "",
    uuidCaptcha = "",
    pbEnviar = "Pesquisar"
  )

  resp <- url |>
    httr2::request() |>
    httr2::req_cookie_preserve(cookies) |>
    httr2::req_options(ssl_verifypeer = 0) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_timeout(seconds = 2) |>
    httr2::req_perform()

  html <- httr2::resp_body_html(resp)

  tem_mais <- html |>
    xml2::xml_find_first("boolean(//div[@id='listagemDeProcessos'])")

  incidente <- html |>
    xml2::xml_find_first("boolean(//div[@id='modalIncidentes'])")

  if (tem_mais) {

    cds_processo <- html |>
      xml2::xml_find_all("//a[@class='linkProcesso']") |>
      xml2::xml_attr("href") |>
      stringr::str_extract( "(?<=processo\\.codigo=)\\w+")

    purrr::walk(cds_processo, ~{
      esaj_cposg_baixar_cd_processo(
        cd_processo = .x,
        diretorio = diretorio,
        cookies_path = cookies_path
      )
    })

  } else if (incidente) {

    cds_processo <- html |>
      xml2::xml_find_all("//input[@id='processoSelecionado']") |>
      xml2::xml_attr("value")

    purrr::walk(cds_processo, ~{
      esaj_cposg_baixar_cd_processo(
        cd_processo = .x,
        diretorio = diretorio,
        cookies_path = cookies_path
      )
    })

  } else {

    cd_processo <- html |>
      xml2::xml_find_first("//input[@name='cdProcesso']") |>
      xml2::xml_attr("value")

    processo2 <- html |>
      xml2::xml_find_first("//span[@id='numeroProcesso']") |>
      xml2::xml_find_first("//span[@id='numeroProcesso']") |>
      xml2::xml_text() |>
      stringr::str_remove_all("\\W")

    id <- glue::glue("{processo2}_{cd_processo}")

    arquivo <- fs::path(diretorio, id, ext = "html")

    writeBin(resp$body, arquivo)

  }
}


#' Baixar dados da consulta processual de segundo grau com base no código do processo
#'
#' @param cd_processo Código do processo
#' @param diretorio Diretório onde armazenar os arquivos.
#' @param cookies_path Caminho para salver os cookies (usar somente em casos de paralização)
#'
#' @return html da consulta
#'
#' @examples
#' \dontrun{
#' Exemplo de cd_processo
#' esaj_cposg_baixar("RI004CVB30000")
#'}
#'
esaj_cposg_baixar_cd_processo <- function(cd_processo = NULL, diretorio = ".", cookies_path = ".") {

  url <- "https://esaj.tjsp.jus.br/cposg/show.do?"

  cookies <- cookies(cookies_path)

  query <- list(
    processo.codigo = cd_processo
  )

  resp <- url |>
    httr2::request() |>
    httr2::req_cookie_preserve(cookies) |>
    httr2::req_options(ssl_verifypeer = 0) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_perform()

  html <- resp |>
    httr2::resp_body_html()

  processo2 <- html |>
    xml2::xml_find_first("//span[@id='numeroProcesso']") |>
    xml2::xml_find_first("//span[@id='numeroProcesso']") |>
    xml2::xml_text() |>
    stringr::str_remove_all("\\W")

  id <- glue::glue("{processo2}_{cd_processo}")

  arquivo <- fs::path(diretorio, id, ext = "html")

  writeBin(resp$body, arquivo)

}
