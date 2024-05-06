#' tjsp_cpopg_ler_capa() parseia dados de capa de um html do cpopg.
#'
#' @param arquivo Caminho para um arquivo html
#' @param result Formato das colunas a serem retornadas. É escolhido 'Padronizado' por default.
#' 'Padronizado' retorna uma tibble com 14 colunas, sendo:
#' processo, cd_processo, tipo_processo, digital, situacao, classe, assunto,
#' foro, vara, juiz, dt_dist, area, controle, valor_da_acao.
#' 'Completo' retorna uma tibble sem pré-definir as colunas. As colunas
#' retornadas serão  aquelas que aparecerem em cada processo.
#' Há duas diferenças fundamentais entre as duas opções.
#' A primeira diferença está em quais colunas são retornadas.
#' Há informações que não aparecem em todos os processos, tais como local_fisico,
#' outros_assuntos, outros_numeros, unificado_ao, apensado_ao, valor_da_acao,
#' processo_principal, recurso, entre outras. Estas colunas são excluídas do
#' result = "Padronizado", mas aparecem no result = "Completo".
#' A segunda diferença está nas informações NA. Quando result = "Padronizado",
#' como as colunas que aparecem são forçadas, então haverá colunas com resultado NA.
#' Já quando result = "Completo", então haverá dois comportamentos distintos
#' para a função. Via de regra, a coluna cujo valor for NA não aparecerá para
#' aquele processo. Entretanto, quando a função é utilizada no contexto
#' de um purrr::map_dfr(), então todas as colunas identificadas em todos os arquivos
#' a serem parseados aparecerão para todos os processos, ainda que em alguns casos
#' existam colunas com NA
#' @param wide Formato da tibble. Só aplicável para return == 'Completo', caso contrário
#' não tem nenhum efeito. Quando wide = TRUE, cada linha representa um processo, e cada coluna
#' uma informação do processo. Quando wide = FALSE, cada linha representa uma coluna
#' e seu respectivo valor.
#'
#' @return retorna uma tibble. Caso seja escolhido return == "Padronizado", esta tibble
#' terá 14 colunas: processo, cd_processo, tipo_processo, digital, situacao, classe, assunto,
#' foro, vara, juiz, dt_dist, area, controle, valor_da_acao. Caso seja escolhido return == "Completo"
#' então a tibble não terá colunas pré-definidas.
#'
#' @examples
#' 1003101-38.2023.8.26.0223 - Processo principal normal (67000FE100000)
#' 0000235-93.2018.8.26.0047 - Processo principal de um incidente
#' 0000235-93.2018.8.26.0047 (8000) - Incidente (1B00023PT1PQ8)
#' 1014289-38.2022.8.26.0037 - Processo principal de um cumprimento de sentença (11000FHR30000)
#' 0004995-42.2023.8.26.0037 - Cumprimento de sentença (11000GKOJ0000)
#' 0006880-32.2020.8.26.0026 - Execução penal (0Q0001BEI0000)
#'
tjsp_cpopg_ler_capa <- function(arquivo = NULL, return = c("Padronizado", "Completo"), wide = TRUE) {

  if(length(return) > 1) {
    return <- "Padronizado"
  }

  if(return == "Padronizado") {
    da <- tjsp_cpopg_ler_capa_padronizado(arquivo)
  } else {
    da <- tjsp_cpopg_ler_capa_completo(arquivo, wide)
  }

  return(da)
}

#' tjsp_cpopg_ler_capa_padronizado() parseia dados de capa de um html do cpopg, retornando
#' colunas padronizadas, mas sacrificando algumas colunas.
#'
#' @param arquivo Caminho para um arquivo html
#'
#' @return retorna uma tibble com 14 colunas, sendo:
#' processo, cd_processo, tipo_processo, digital, situacao, classe, assunto,
#' foro, vara, juiz, dt_dist, area, controle, valor_da_acao.
#'
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

#' tjsp_cpopg_ler_capa_completo() parseia dados de capa de um html do cpopg, retornando
#' colunas não padronizadas, mas sem sacrificar nenhuma coluna.
#'
#' @param arquivo Caminho para um arquivo html
#'
#' @return  retorna uma tibble sem pré-definir as colunas
#'
tjsp_cpopg_ler_capa_completo <- function(arquivo = NULL, wide = TRUE) {

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
    )

    if(wide) {
      da <- da |>
        dplyr::group_by_at(dplyr::vars(-valor)) |>
        dplyr::mutate(row_id = 1:dplyr::n()) |>
        dplyr::ungroup() |>
        tidyr::spread(key = variavel, value = valor) |>
        dplyr::select(-row_id) |>
        janitor::clean_names()
    }

    return(da)
}
