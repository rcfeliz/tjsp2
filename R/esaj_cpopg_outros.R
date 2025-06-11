#' Iteração da função esaj_cpopg_baixar_outros_unitario()
#'
#' @param consultas Vetor de valores a serem consultados
#' @param parametro Parâmetro a ser utilizado na consulta.
#' Se nenhum parâmetro for informado, o default é considerar 'NUMPROC'.
#' 'NMPARTE' = Nome da parte;
#' 'DOCPARTE' = Número do documento da parte;
#' 'NMADVOGADO' = Nome do advogado;
#' 'NUMOAB' = Número da OAB do advogado;
#' 'PRECATORIA' = Número do precatório;
#' 'DOCELEG' = Número do documento de delegacia;
#' 'NUMCDA' = Número de Certidão de Dívida Ativa (CDA)
#' @param diretorio Diretório onde serão armazenados os htmls
#' @param cookies_path Caminho para salver os cookies (usar somente em casos de paralização)
#'
#'
#' @return html da consulta
#'
#' @export
#'
#' @examples
#' \dontrun{
#' esaj_cpopg_baixar_outros(consultas = c("123456SP", "100000SP"), parametro = "NUMOAB")
#' esaj_cpopg_baixar_outros(consultas = "José da Silva Pereira", parametro = "NMPARTE")
#'}
#'
#' @details A estrutura do nome dos arquivos gerados é sempre
#' ./\{parametro\}_\{consulta\}_\{pagina\}.html.
#'
esaj_cpopg_baixar_outros <- function(consultas = NULL,
                                     parametro = c("NMPARTE", "DOCPARTE", "NMADVOGADO", "NUMOAB", "PRECATORIA", "DOCDELEG", "NUMCDA"),
                                     diretorio = ".",
                                     cookies_path = NULL) {

  if (!dir_exists(diretorio)) {
    fs::dir_create(diretorio)
  }

  cookies <- cookies(cookies_path)

  if(length(parametro) > 1) {
    parametros <- glue::glue_collapse(parametro, sep = ", ", last = " ou ")
    stop(glue::glue("Escolha um par\u00E2metro apenas: {parametros}."))
  }

  if(is.null(consultas)) {
    stop("Informe os valores a serem consultados.")
  }

  if(parametro %in% c("NMPARTE", "NMADVOGADO") & any(stringr::str_detect(consultas, "\\d"))) {
    txt <- glue::glue("O par\u00E2metro {parametro} requer que sejam inseridos nomes na 'consulta', entretanto, foram identificados n\u00FAmeros. \nCertifique-se de que todos os valores da 'consulta' representam nomes.")
    warning(txt)
  }

  purrr::walk(consultas, ~{

    esaj_cpopg_baixar_outros_unitario(
      consulta = .x,
      parametro = parametro,
      diretorio = diretorio,
      cookies_path = cookies
    )

    Sys.sleep(1)
  })
}

esaj_cpopg_baixar_outros_unitario <- function(consulta = NULL,
                                              parametro = c("NMPARTE", "DOCPARTE", "NMADVOGADO", "NUMOAB", "PRECATORIA", "DOCDELEG", "NUMCDA"),
                                              diretorio = ".",
                                              cookies_path = NULL) {

  if(parametro == "NUMOAB" | parametro == "DOCPARTE") {
    consulta <- stringr::str_remove_all(consulta, "\\W")
  }

  cookies <- cookies_path

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

#' Iteração da função esaj_cpopg_ler_outros_unitario()
#'
#' @param arquivos Vetor contendo o path de arquivos html
#'
#' @return tibble contendo 10 colunas
#'
#' @export
#'
esaj_cpopg_ler_outros <- function(arquivos) {
  purrr::map_dfr(arquivos, esaj_cpopg_ler_outros_unitario)
}

esaj_cpopg_ler_outros_unitario <- function(arquivo) {

  arquivo_infos <- basename(arquivo) |>
    stringr::str_remove("\\.html") |>
    stringr::str_split("_") |>
    purrr::pluck(1)

  html <- arquivo |>
    xml2::read_html() |>
    xml2::xml_find_all("//ul[@class='unj-list-row']") |>
    xml2::xml_find_all("./li")

  if(length(html) == 0) {
    da <- esaj_cpopg_ler_unitario(arquivo, formato = "Padronizado", outros = NULL) |>
      dplyr::transmute(
        arquivo = basename(arquivo),
        parametro = arquivo_infos[1],
        consulta = arquivo_infos[2],
        processo,
        cd_processo,
        classe,
        assunto,
        dt_dist
      )
  } else {
    processo <- html |>
      xml2::xml_find_all(".//a[@class='linkProcesso']") |>
      xml2::xml_text(trim = TRUE) |>
      stringr::str_remove_all("[:punct:]")

    cd_processo <- html |>
      xml2::xml_find_all("./div") |>
      xml2::xml_attr("id") |>
      stringr::str_remove("divProcesso")

    classe <- html |>
      purrr::map(~{
        .x |>
          xml2::xml_child("/div[@class='classeProcesso']") |>
          xml2::xml_text(trim = TRUE)
      }) |>
      unlist()

    assunto <- html |>
      purrr::map(~{
        .x |>
          xml2::xml_child("/div[@class='assuntoPrincipalProcesso']") |>
          xml2::xml_text(trim = TRUE)
      }) |>
      unlist()

    dt_dist <- html |>
      purrr::map(~{
        .x |>
          xml2::xml_child("/div[@class='dataLocalDistribuicaoProcesso']") |>
          xml2::xml_text(trim = TRUE)
      }) |>
      unlist() |>
      stringr::str_extract("\\S+") |>
      lubridate::dmy()

    # Return
    da <- tibble::tibble(
      arquivo = basename(arquivo),
      parametro = arquivo_infos[1],
      consulta = arquivo_infos[2],
      processo,
      cd_processo,
      classe,
      assunto,
      dt_dist
    )
  }

  return(da)
}
