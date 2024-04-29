#' Baixar dados da consulta processual de primeiro grau
#'
#' @param consulta Númeração Única do processo (CNJ)
#' @param parametro Parâmetro a ser utilizado na consulta.
#' Se nenhum parâmetro for informado, o default é considerar 'NUMPROC'.
#' 'NUMPROC' = Numeração Única do Processo ou Código do Processo;
#' 'NMPARTE' = Nome da parte;
#' 'DOCPARTE' = Número do documento da parte;
#' 'NMADVOGADO' = Nome do advogado;
#' 'NUMOAB' = Número da OAB do advogado;
#' 'PRECATORIA' = Número do precatório;
#' 'DOCELEG' = Número do documento de delegacia;
#' 'NUMCDA' = Número de Certidão de Dívida Ativa (CDA)
#' @param diretorio Diretório onde será armazenado o html
#'
#' @return html da consulta
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tjsp_baixar_cpopg("10031013820238260223") # exemplo típico de processo
#' tjsp_baixar_cpopg("00002359320188260047") # exemplo de processo em que um único processo gera uma lista de processos
#' tjsp_baixar_cpopg("JK0001UPV0000") # exemplo típico de cd_processo
#' tjsp_baixar_cpopg("JK00024VK0000") # exemplo de cd_processo de incidente
#' tjsp_baixar_cpopg(c("10091165420168260001", "WW0015T010000"))
#' tjsp_baixar_cpopg("123456SP", "NUMOAB")
#' tjsp_baixar_cpopg("José de Jesus Filho", "NMADVOGADO")
#' }
#'
#' @details A estrutura do nome dos arquivos é sempre ./{parametro}_{consulta}.html.
#'Quando há várias páginas, é adicionado o número da página ao fim, ficando ./{parametro}_{consulta}_{pagina}.html
#'Quando a pesquisa é realizada pelo número do processo, ou pelo código do processo, a {consulta} se divide em dois números: primeiro vem o número do processo, em seguida o código do processo, ficando ./{parametro}_{processo}_{cd_processo}.html
#'
tjsp_baixar_cpopg <- function(consulta = NULL, parametro = c("NUMPROC", "NMPARTE", "DOCPARTE", "NMADVOGADO", "NUMOAB", "PRECATORIA", "DOCDELEG", "NUMCDA"), diretorio = ".") {

  if(length(parametro) > 1) {
    parametro <- "NUMPROC"
  }

  if(parametro == "NUMPROC") {
    # baixar pelo parâmetro processo
    purrr::walk(consulta, ~{
      processo <- .x |>
        stringr::str_remove_all("\\W")

      if(stringr::str_length(processo) == 20) {
        tjsp_baixar_cpopg_processo(processo = .x)

        Sys.sleep(1)
      }

      if(stringr::str_length(processo) == 13) {
        tjsp_baixar_cpopg_cd_processo(cd_processo = .x)
      }
    })
  } else {
    # baixar pelos demais parâmetros
    purrr::walk(consulta, ~{
      tjsp_baixar_cpopg_outros(consulta = .x, parametro = parametro)

      Sys.sleep(1)
    })
  }
}

#' Baixar dados da consulta processual de primeiro grau com base na numeração única do processo
#'
#' @param processo Númeração Única do processo (CNJ)
#' @param diretorio Diretório onde será armazenado o html
#'
#' @return html da consulta
#'
#' @example tjsp_baixar_cpopg_processo("10031013820238260223") # exemplo típico
#' @example tjsp_baixar_cpopg_processo("00002359320188260047") # exemplo em que um único processo gera uma lista de processos
#'
tjsp_baixar_cpopg_processo <- function(processo, diretorio = "."){

  url <- "https://esaj.tjsp.jus.br/cpopg/search.do?"

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

    purrr::walk(cd_processos, tjsp_baixar_cpopg_cd_processo)

  } else if(xml2::xml_find_first(conteudo, "boolean(//div[@id='modalIncidentes'])")) {

    cd_processos <- conteudo |>
      xml2::xml_find_all("//input[@id='processoSelecionado']") |>
      xml2::xml_attr("value")

    purrr::walk(cd_processos, tjsp_baixar_cpopg_cd_processo)

  } else {

    cd_processo <- conteudo |>
      xml2::xml_find_first("//script[contains(text(),'processo.codigo')]") |>
      xml2::xml_text() |>
      stringr::str_extract("(?<=processo.codigo=)\\w+")

    id <- glue::glue("NUMPROC_{processo}_{cd_processo}")

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
#' @example tjsp_baixar_cpopg_cd_processo("67000FE100000") # exemplo típico
#' @example tjsp_baixar_cpopg_cd_processo("1B00023PT1PQ8") # exemplo de incidente
#'
tjsp_baixar_cpopg_cd_processo <- function(cd_processo, diretorio = "."){
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

  id <- glue::glue("NUMPROC_{processo}_{cd_processo}")

  arquivo <- fs::path(diretorio, id, ext = "html")

  writeBin(resp$body, arquivo)

}

#' Baixar dados da consulta processual de primeiro grau com base em outros parâmetros especificados
#'
#' @param consulta Valor da consulta
#' @param parametro Parâmetro a ser utilizado na consulta. 'NMPARTE' = Nome da parte; 'DOCPARTE' = Número do documento da parte; 'NMADVOGADO' = Nome do advogado; 'NUMOAB' = Número da OAB do advogado; 'PRECATORIA' = Número do precatório; 'DOCELEG' = Número do documento de delegacia; 'NUMCDA' = Número de Certidão de Dívida Ativa (CDA)
#' @param diretorio Diretório onde será armazenado o html
#'
#' @return html da consulta
#'
#' @examples
#' \dontrun{
#' tjsp_baixar_cpopg_par(consulta="123456SP", parametro = "NUMOAB")
#' tjsp_baixar_cpopg_par(consulta = "José da Silva Pereira", parametro = "NMPARTE")
#'}
tjsp_baixar_cpopg_outros <- function(consulta = NULL, parametro = c("NMPARTE", "DOCPARTE", "NMADVOGADO", "NUMOAB", "PRECATORIA", "DOCDELEG", "NUMCDA"), diretorio = ".") {

  if(is.null(consulta)) {
    stop("Informe os valores a serem consultados")
  }

  if(parametro %in% c("NMPARTE", "NMADVOGADO") & any(stringr::str_detect(consulta, "\\d"))) {
    txt <- glue::glue("O parâmetro {parametro} requer que sejam inseridos nomes na 'consulta', entretanto, foram identificados números. \nCertifique-se de que todos os valores da 'consulta' representam nomes.")
    warning(txt)
  }

  if(parametro == "NUMOAB" | parametro == "DOCPARTE") {
    consulta <- stringr::str_remove_all(consulta, "\\W")
  }

  cookies <- httr2::last_request()$options$cookiefile

  url_base <- "https://esaj.tjsp.jus.br/cpopg"

  req <- url_base |>
    httr2::request() |>
    httr2::req_cookie_preserve(cookies) |>
    httr2::req_options(ssl_verifypeer = 0)

  # GET request for first page
  resp <- req |>
    httr2::req_url_path_append("search.do") |>
    httr2::req_url_query(
      conversationId = "",
      cbPesquisa = parametro,
      dadosConsulta.valorConsulta = consulta,
      cdForo = "-1",
      gateway = 'true'
    ) |>
    httr2::req_perform()

  id <- sprintf(glue::glue("{parametro}_{consulta}_%02d"), 1) |>
    abjutils::rm_accent() |>
    stringr::str_replace_all(" ", "_")

  arquivo <- fs::path(diretorio, id, ext = "html")

  writeBin(resp$body, arquivo)

  paginas <- resp |>
    httr2::resp_body_html() |>
    xml2::xml_find_first("//span[@id='contadorDeProcessos']") |>
    xml2::xml_text() |>
    stringr::str_extract_all("\\d+") |>
    as.integer() |>
    magrittr::divide_by(25) |>
    ceiling()

  # GET for 2+ pages
  if(is.na(paginas)) {paginas <- 1L}

  if(paginas > 1) {
    Sys.sleep(1)

    purrr::walk(2:paginas, ~{
      id <- sprintf(glue::glue("{parametro}_{consulta}_%02d"), .x)

      arquivo <- fs::path(diretorio, id, ext = "html")

      resp <- req |>
        httr2::req_url_path_append("trocarPagina.do") |>
        httr2::req_url_query(
          paginaConsulta = .x,
          conversationId = "",
          cbPesquisa = parametro,
          dadosConsulta.valorConsulta = consulta,
          cdForo = "-1",
          gateway = 'true'
        ) |>
        httr2::req_perform()

      writeBin(resp$body, arquivo)

      Sys.sleep(1)
    })
  }
}
