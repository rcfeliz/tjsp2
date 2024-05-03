#' Extrai a movimentação processual de primeira e de segunda instância
#'
#' @param arquivos se não informados, informar diretório
#' @param diretorio objeto ou diretorio  onde se encontram os htmls
#'
#' @return tibble com a movimentação processual.
#' @export
#' @examples
#' \dontrun{
#' andamento_cposg <- ler_movimentacao_cposg()
#' andamento_cpopg <- ler_movimentacao_cpopg()
#' }
#'

tjsp_ler_movimentacao <- function (arquivo = NULL) {

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

  # movimentacoes
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
    stringr::str_replace_all("\\+", " ")

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

         url <- httr::build_url(parseada)

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
    )
}
