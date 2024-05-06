#' Ler Partes do cpopg ou cposg, quando baixados por número do processo.
#'
#' @param arquivo Caminho para um arquivo html
#'
#' @details São criadas cinco colunas : processo, cd_processo,
#'     tipo_parte, parte e representante. Esta última
#'     pode ser tanto o procurador quanto o representante
#'     legal.
#' @return tibble
#' @export
#'
tjsp_ler_partes <- function(arquivo = NULL) {

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
                       fill = "right") |>       tibble::add_column(processo = processo, .before = 1) |>
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
