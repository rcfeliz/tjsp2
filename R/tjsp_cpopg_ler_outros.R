#' Iteração da função tjsp_cpopg_ler_outros_unitario()
#'
#' @param arquivos Vetor contendo o path de arquivos html
#'
#' @return tibble contendo 10 colunas
#'
#' @export
#'
tjsp_cpopg_ler_outros <- function(arquivos) {
  purrr::map_dfr(arquivos, tjsp_cpopg_ler_outros_unitario)
}

#' Lê busca por paramêtro diferente do processo
#'
#' @param arquivo Path de um arquivo html
#'
#' @return tibble contendo 10 colunas
#'
tjsp_cpopg_ler_outros_unitario <- function(arquivo) {
  html <- arquivo |>
    xml2::read_html() |>
    xml2::xml_find_all("//ul[@class='unj-list-row']") |>
    xml2::xml_find_all("./li")

  # Parse
  processo <- html |>
    xml2::xml_find_all(".//a[@class='linkProcesso']") |>
    xml2::xml_text(trim = TRUE) |>
    stringr::str_remove_all("\\s.+")

  cd_processo <- html |>
    xml2::xml_find_all("./div") |>
    xml2::xml_attr("id") |>
    stringr::str_remove("divProcesso")

  tipo_participacao <- html |>
    purrr::map(~{
      .x |>
        xml2::xml_child("/label[@class='unj-label tipoDeParticipacao']") |>
        xml2::xml_text(trim = TRUE) |>
        stringr::str_remove_all("\\:.*")
    }) |>
    unlist()

  nome_parte <- html |>
    purrr::map(~{
      .x |>
        xml2::xml_child("/div[@class='unj-base-alt nomeParte']") |>
        xml2::xml_text(trim = TRUE) |>
        stringr::str_remove_all("\\:.*")
    }) |>
    unlist()

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

  dt_recebimento <- html |>
    purrr::map(~{
      .x |>
        xml2::xml_child("/div[@class='dataLocalDistribuicaoProcesso']") |>
        xml2::xml_text(trim = TRUE)
    }) |>
    unlist() |>
    stringr::str_extract("\\S+") |>
    lubridate::dmy()

  local_recebimento <- html |>
    purrr::map(~{
      .x |>
        xml2::xml_child("/div[@class='dataLocalDistribuicaoProcesso']") |>
        xml2::xml_text(trim = TRUE)
    }) |>
    unlist() |>
    stringr::str_extract("(?<=- ).+")

  outros_numeros <- html |>
    purrr::map(~{
      .x |>
        xml2::xml_child("/label[@class='unj-label']/following-sibling::div[1]") |>
        xml2::xml_text()
    }) |>
    unlist()

  # Return
  tibble::tibble(
    arquivo = basename(arquivo),
    processo,
    cd_processo,
    tipo_participacao,
    nome_parte,
    classe,
    assunto,
    dt_recebimento,
    local_recebimento,
    outros_numeros,
  )
}
