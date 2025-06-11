files <- fs::dir_ls(diretorio)

arquivo <- files[1]

html <- file |>
  xml2::read_html()

esaj_cposg_ler_capa <- function(arquivo = NULL) {

  html <- arquivo |>
    xml2::read_html()

  processo <- html |>
    xml2::xml_find_first("//span[@id='numeroProcesso']") |>
    xml2::xml_text(trim = TRUE) |>
    stringr::str_remove_all("\\D+")

  cd_processo <- html |>
    xml2::xml_find_first("//input[@name='cdProcesso']") |>
    xml2::xml_attr("value")

  situacao <- html |>
    xml2::xml_find_first("//span[@id='situacaoProcesso']") |>
    xml2::xml_text(trim = TRUE)

  classe <- html |>
    xml2::xml_find_first("//div[@id='classeProcesso']") |>
    xml2::xml_text(trim = TRUE)

  assunto <- html |>
    xml2::xml_find_first("//div[@id='assuntoProcesso']") |>
    xml2::xml_text(trim = TRUE)

  secao <- html |>
    xml2::xml_find_first("//div[@id='secaoProcesso']") |>
    xml2::xml_text(trim = TRUE)

  orgao_julgador <- html |>
    xml2::xml_find_first("//div[@id='orgaoJulgadorProcesso']") |>
    xml2::xml_text(trim = TRUE)

  area <- html |>
    xml2::xml_find_first("//div[@id='areaProcesso']") |>
    xml2::xml_text(trim = TRUE)

  relator <- html |>
    xml2::xml_find_first("//div[@id='relatorProcesso']") |>
    xml2::xml_text(trim = TRUE)

  valor_da_acao <- html |>
    xml2::xml_find_first("//div[@id='valorAcaoProcesso']") |>
    xml2::xml_text(trim = TRUE)

  da <- tibble::tibble(
    processo,
    cd_processo,
    tipo_processo,
    situacao,
    classe,
    assunto,,
    secao,
    orgao_julgador,
    area,
    relator,
    valor_da_acao
  )

  return(da)
}

esaj_cposg_ler_apensos <- function(arquivo = NULL) {

  html <- arquivo |>
    xml2::read_html()

  processo <- html |>
    xml2::xml_find_first("//span[@id='numeroProcesso']") |>
    xml2::xml_text(trim = TRUE) |>
    stringr::str_remove_all("\\D+")

  cd_processo <- html |>
    xml2::xml_find_first("//input[@name='cdProcesso']") |>
    xml2::xml_attr("value")

  tem_apensos <- !html |>
    xml2::xml_find_first("boolean(//td[@id='dadosApensosNaoDisponiveis'])")

  if (tem_apensos) {
    message(glue::glue("Rever apensos para {processo}"))
  } else {
    next()
  }

}

esaj_cposg_ler_numeros_pg <- function(arquivo = NULL) {

  html <- arquivo |>
    xml2::read_html()

  processo <- html |>
    xml2::xml_find_first("//span[@id='numeroProcesso']") |>
    xml2::xml_text(trim = TRUE) |>
    stringr::str_remove_all("\\D+")

  cd_processo <- html |>
    xml2::xml_find_first("//input[@name='cdProcesso']") |>
    xml2::xml_attr("value")

  tabela <- html |>
    xml2::xml_find_first(
      "//h2[@class='subtitle' and normalize-space(text()) = 'Números de 1ª Instância']/parent::div/following-sibling::table[2]"
    )

  processos_pg <- tabela |>
    xml2::xml_find_all(".//tr[contains(@class, 'fundo')]")

  da_pg <- purrr::map_dfr(processos_pg, ~{

    infos_pg <- .x |>
      xml2::xml_find_all("./td") |>
      xml2::xml_text(trim = TRUE)

    processo_origem <- infos_pg[1] |>
      stringr::str_remove_all("\\D+")

    foro_origem <- infos_pg[2]

    vara_origem <- infos_pg[3]

    juiz_origem <- infos_pg[4]

    obs <- infos_pg[5]

    tibble::tibble(
      processo_origem,
      foro_origem,
      vara_origem,
      juiz_origem,
      obs
    )
  })

  tibble::tibble(
    processo,
    cd_processo,
    da_pg
  )

}

esaj_cposg_ler_partes <- function(arquivo = NULL)  {

  html <- arquivo |>
    xml2::read_html()

  processo <- html |>
    xml2::xml_find_first("//span[@id='numeroProcesso']") |>
    xml2::xml_text(trim = TRUE) |>
    stringr::str_remove_all("\\D+")

  cd_processo <- html |>
    xml2::xml_find_first("//input[@name='cdProcesso']") |>
    xml2::xml_attr("value")

  tabela <- html |>
    xml2::xml_find_first("//table[@id='tablePartesPrincipais']")

  partes <- tabela |>
    xml2::xml_find_all(".//tr[contains(@class, 'fundo')]")

  da_pg <- purrr::map_dfr(partes, ~{

    participacao <- .x |>
      xml2::xml_find_first("./td[@class='label']") |>
      xml2::xml_text(trim = TRUE) |>
      stringr::str_remove_all("[:punct:]")

    partes <- .x |>
      xml2::xml_find_first("./td[@class='nomeParteEAdvogado']")


    partes |>
      xml2::xml_find_all("./text()") |>
      xml2::xml_text(trim = TRUE)

    partes |>
      stringr::str_squish() |>
      stringr::str_remove_all("&nbsp ") |>
      stringr::str_split(" Advogado:") |>
      purrr::pluck(1)

    tibble::tibble(
      processo_origem,
      foro_origem,
      vara_origem,
      juiz_origem,
      obs
    )
  })

  tibble::tibble(
    processo,
    cd_processo,
    da_pg
  )

}

esaj_cposg_ler_movimentacoes <- function(arquivo = NULL)  {

  html <- arquivo |>
    xml2::read_html()

  processo <- html |>
    xml2::xml_find_first("//span[@id='numeroProcesso']") |>
    xml2::xml_text(trim = TRUE) |>
    stringr::str_remove_all("\\D+")

  cd_processo <- html |>
    xml2::xml_find_first("//input[@name='cdProcesso']") |>
    xml2::xml_attr("value")

}

esaj_cposg_ler_subprocessos_recurso <- function(arquivo = NULL)  {

  html <- arquivo |>
    xml2::read_html()

  processo <- html |>
    xml2::xml_find_first("//span[@id='numeroProcesso']") |>
    xml2::xml_text(trim = TRUE) |>
    stringr::str_remove_all("\\D+")

  cd_processo <- html |>
    xml2::xml_find_first("//input[@name='cdProcesso']") |>
    xml2::xml_attr("value")

}

esaj_cposg_ler_peticoes_diversas <- function(arquivo = NULL)  {

  html <- arquivo |>
    xml2::read_html()

  processo <- html |>
    xml2::xml_find_first("//span[@id='numeroProcesso']") |>
    xml2::xml_text(trim = TRUE) |>
    stringr::str_remove_all("\\D+")

  cd_processo <- html |>
    xml2::xml_find_first("//input[@name='cdProcesso']") |>
    xml2::xml_attr("value")

}

esaj_cposg_ler_composicao_julgamento <- function(arquivo = NULL) {

  html <- arquivo |>
    xml2::read_html()

  processo <- html |>
    xml2::xml_find_first("//span[@id='numeroProcesso']") |>
    xml2::xml_text(trim = TRUE) |>
    stringr::str_remove_all("\\D+")

  cd_processo <- html |>
    xml2::xml_find_first("//input[@name='cdProcesso']") |>
    xml2::xml_attr("value")

}

esaj_cposg_ler_julgamentos <- function(arquivo = NULL)  {

  html <- arquivo |>
    xml2::read_html()

  processo <- html |>
    xml2::xml_find_first("//span[@id='numeroProcesso']") |>
    xml2::xml_text(trim = TRUE) |>
    stringr::str_remove_all("\\D+")

  cd_processo <- html |>
    xml2::xml_find_first("//input[@name='cdProcesso']") |>
    xml2::xml_attr("value")

}

