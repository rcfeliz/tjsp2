esaj_cposg_ler <- function(
    arquivos = NULL,
    outros = c("Tudo", "Apensos", "Números de 1a instância", "Partes", "Movimentacoes", "Subprocessos e Recursos", "Composição do Julgamento", "Julgamentos")
  ) {

  if(is.null(arquivos)){
    arquivos <- list.files(
      arquivos,
      full.names = TRUE,
      pattern = "html$"
    )
  }

  purrr::map_dfr(arquivos, ~{
    esaj_cposg_ler_unitario(
      arquivo = .x,
      outros = outros
    )
  })

}

esaj_cposg_ler_unitario <- function(
    arquivo = NULL,
    outros = c("Tudo", "Apensos", "Números de 1a instância", "Partes", "Movimentacoes", "Subprocessos e Recursos", "Composição do Julgamento", "Julgamentos")
) {

  aux_capa <- esaj_cposg_ler_capa(arquivo)

  if(is.null(outros)) {
    da <- aux_capa
    # extrai demais informacoes
  } else {
    # ajusta o parametro 'info'
    if(any(outros == "Tudo")) outros <- c("Apensos", "Números de 1a instância", "Partes", "Movimentacoes", "Subprocessos e Recursos", "Composição do Julgamento", "Julgamentos")

    # fix outros
    depara <- c(
      "Números de 1a instância" = "Numeros PG",
      "Subprocessos e Recursos" = "Subprocessos Recursos",
      "Composição do Julgamento" = "Composição Julgamento"
    )

    outros <- purrr::map_chr(outros, ~{
      ifelse(
        .x %in% names(depara),
        depara[[.x]],
        .x
      )
    }) |>
      abjutils::rm_accent() |>
      stringr::str_to_lower() |>
      stringr::str_replace_all(" ", "_")

    # usa as funcoes para extrair as outras informacoes
    aux_outros <- purrr::map(outros, ~{
      fn_nm <- glue::glue("esaj_cposg_ler_{.x}")
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

#' Ler dados da capa do processo no CPOSG
#'
#' Extrai informações básicas da capa do processo judicial a partir do arquivo HTML.
#'
#' @param arquivo Caminho para o arquivo HTML ou objeto compatível com `xml2::read_html()`.
#' @return Um tibble com as seguintes colunas:
#' \describe{
#'    \item{processo}{Número do processo, somente dígitos.}
#'    \item{cd_processo}{Código interno do processo.}
#'    \item{situacao}{Situação atual do processo.}
#'    \item{classe}{Classe do processo.}
#'    \item{assunto}{Assunto principal do processo.}
#'    \item{secao}{Seção judicial responsável.}
#'    \item{orgao_julgador}{Órgão julgador do processo.}
#'    \item{area}{Área temática do processo.}
#'    \item{relator}{Nome do relator do processo.}
#'    \item{valor_da_acao}{Valor da ação no processo.}
#' }
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
    situacao,
    classe,
    assunto,
    secao,
    orgao_julgador,
    area,
    relator,
    valor_da_acao
  )

  return(da)
}

#' Ler Apensos no CPOSG
#'
#' Extrai tabela de apensos do CPOSG.
#'
#' @param arquivo Caminho para o arquivo HTML ou objeto compatível com `xml2::read_html()`.
#' @return Um tibble com as seguintes colunas:
#' \describe{
#'    \item{processo}{Número do processo, somente dígitos.}
#'    \item{cd_processo}{Código interno do processo.}
#'    \item{preencher}{A preencher.}
#' }
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
    tibble::tibble(
      processo,
      cd_processo,
      apensos = NA_character_
    )
  }

}

#' Ler tabela de números de primeira instância no CPOSG
#'
#' Extrai dados dos processos de primeira instância vinculados ao processo principal a partir do arquivo HTML.
#'
#' @param arquivo Caminho para o arquivo HTML ou objeto compatível com `xml2::read_html()`.
#' @return Um tibble com as colunas:
#' \describe{
#'    \item{processo}{Número do processo principal, somente dígitos.}
#'    \item{cd_processo}{Código interno do processo principal.}
#'    \item{processo_origem}{Número do processo de primeira instância, somente dígitos.}
#'    \item{foro_origem}{Foro de origem do processo.}
#'    \item{vara_origem}{Vara de origem do processo.}
#'    \item{juiz_origem}{Nome do juiz responsável.}
#'    \item{obs}{Observações relacionadas.}
#' }
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

#' Ler partes do processo no CPOSG
#'
#' Extrai as partes relacionadas ao processo, incluindo advogados e tipos de participação.
#'
#' @param arquivo Caminho para o arquivo HTML ou objeto compatível com `xml2::read_html()`.
#' @return Um tibble com as colunas:
#' \describe{
#'    \item{processo}{Número do processo, somente dígitos.}
#'    \item{cd_processo}{Código interno do processo.}
#'    \item{da_pg}{Tibble com dados das partes contendo:}
#'    \item{id_parte}{Identificador único para cada parte.}
#'    \item{participacao}{Tipo de participação da parte (ex: autor, réu).}
#'    \item{nome}{Nome da parte ou do advogado.}
#'    \item{tipo_parte}{Tipo da parte (ex: Principal, Advogado).}
#' }
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

  mais <- html |>
    xml2::xml_find_first("boolean(//table[@id='tableTodasPartes'])")

  if(mais) {
    tabela <- html |>
      xml2::xml_find_first("//table[@id='tableTodasPartes']")
  } else {
    tabela <- html |>
      xml2::xml_find_first("//table[@id='tablePartesPrincipais']")
  }

  partes <- tabela |>
    xml2::xml_find_all(".//tr[contains(@class, 'fundo')]")

  id <- 1
  da_partes <- tibble::tibble()
  for(.x in partes) {

    participacao <- .x |>
      xml2::xml_find_first("./td[@class='label']") |>
      xml2::xml_text(trim = TRUE) |>
      stringr::str_remove_all("[:punct:]")

    td_nomes <- .x |>
      xml2::xml_find_first("./td[@class='nomeParteEAdvogado']")

    nomeParteEAdvogado <- xml2::xml_text(td_nomes, trim = TRUE)

    parte_unica <- !xml2::xml_find_first(td_nomes, "boolean(./span)")

    if (parte_unica) {
      # cenário 1: só tem parte principal, sem representante

      nome <- stringr::str_squish(nomeParteEAdvogado)
      tipo_parte <- "Principal"

    } else {
      # cenário 2: parte + representante
      lista <- nomeParteEAdvogado |>
        stringr::str_replace_all("&nbsp|\\:", "   ") |>
        stringr::str_remove_all("[:punct:]") |>
        stringr::str_split("\\s{3,}") |>
        purrr::pluck(1)

      nome <- lista[c(TRUE, FALSE)] # pega 1, 3, 5, 7 ... nomes
      tipo_parte <- c("Principal", lista[c(FALSE, TRUE)]) # pega 2, 4, 6 ... tipos

    }

    da <- tibble::tibble(
      id_parte = id,
      participacao,
      nome,
      tipo_parte
    )

    da_partes <- da_partes |>
      dplyr::bind_rows(da)

    id <- id + 1

  }

  tibble::tibble(
    processo,
    cd_processo,
    da_partes
  )

}

#' Ler movimentações do processo no CPOSG
#'
#' Extrai a lista de movimentações processuais presentes no arquivo HTML.
#'
#' @param arquivo Caminho para o arquivo HTML ou objeto compatível com `xml2::read_html()`.
#' @return Um tibble com as colunas:
#' \describe{
#'   \item{processo}{Número do processo, somente dígitos.}
#'   \item{cd_processo}{Código interno do processo.}
#'   \item{id_mov}{Identificador da movimentação (contagem regressiva).}
#'   \item{dt_mov}{Data da movimentação (classe Date).}
#'   \item{movimento}{Tipo da movimentação.}
#'   \item{descricao}{Descrição detalhada da movimentação.}
#'   \item{cd_documento}{Código do documento associado, se houver.}
#'   \item{recurso_acessado}{Descrição do recurso acessado, se houver.}
#'   \item{url}{URL do recurso/documento associado à movimentação.}
#' }
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

        base_url <- "https://esaj.tjsp.jus.br/cposg/search.do"

        parseada <- httr2::url_parse(base_url)

        parseada$query <- list(
          conversationId = "",
          paginaConsulta = "0",
          cbPesquisa = "NUMPROC",
          numeroDigitoAnoUnificado = unificado,
          foroNumeroUnificado = foro,
          dePesquisaNuUnificado = p,
          dePesquisaNuUnificado = "UNIFICADO",
          dePesquisa = "",
          tipoNuProcesso = "UNIFICADO"
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
    dplyr::distinct() |>
    dplyr::mutate(
      id_mov = dplyr::n() - dplyr::row_number() + 1
    ) |>
    dplyr::relocate(id_mov, .after = cd_processo)
}

#' Ler subprocessos e recursos do processo no CPOSG
#'
#' Verifica a existência de subprocessos e recursos associados ao processo.
#'
#' @param arquivo Caminho para o arquivo HTML ou objeto compatível com `xml2::read_html()`.
#' @return Um tibble com as seguintes colunas:
#' \describe{
#'    \item{processo}{Número do processo, somente dígitos.}
#'    \item{cd_processo}{Código interno do processo.}
#'    \item{dt_recebimento}{Data de recebimento do recurso.}
#'    \item{classe}{Classe do recurso.}
#'    \item{url}{URL no CPOSG do recurso}
#' }
esaj_cposg_ler_subprocessos_recursos <- function(arquivo = NULL)  {

  html <- arquivo |>
    xml2::read_html()

  processo <- html |>
    xml2::xml_find_first("//span[@id='numeroProcesso']") |>
    xml2::xml_text(trim = TRUE) |>
    stringr::str_remove_all("\\D+")

  cd_processo <- html |>
    xml2::xml_find_first("//input[@name='cdProcesso']") |>
    xml2::xml_attr("value")

  tem_incidentes <- !html |>
    xml2::xml_find_first("boolean(//td[@id='processoSemIncidentes'])")

  if (tem_incidentes) {

    tabela <- html |>
      xml2::xml_find_first(
        "//h2[@class='subtitle' and normalize-space(text()) = 'Subprocessos e Recursos']/parent::div/following-sibling::table[1]"
      )

    suprocessos_recursos <- tabela |>
      xml2::xml_find_all(".//tr[contains(@class, 'fundo')]")

    dt_recebimento <- suprocessos_recursos |>
      xml2::xml_find_all("./td[1]") |>
      xml2::xml_text() |>
      stringr::str_squish()

    dt_recebimento <- dt_recebimento[dt_recebimento != ""]

    classe <- suprocessos_recursos |>
      xml2::xml_find_all("./td[2]") |>
      xml2::xml_text() |>
      stringr::str_squish()

    classe <- classe[classe != ""]

    endpoint <- suprocessos_recursos |>
      xml2::xml_find_all(".//a") |>
      xml2::xml_attr("href")

    url <- paste0("https://esaj.tjsp.jus.br", endpoint)

    tibble::tibble(
      processo,
      cd_processo,
      dt_recebimento,
      classe,
      url
    )
  } else {
    tibble::tibble(
      processo,
      cd_processo,
      dt_recebimento = NA_character_,
      classe = NA_character_,
      url = NA_character_
    )
  }

}

#' Ler petições diversas do processo no CPOSG
#'
#' Extrai as petições diversas relacionadas ao processo a partir do arquivo HTML.
#'
#' @param arquivo Caminho para o arquivo HTML ou objeto compatível com `xml2::read_html()`.
#' @return Um tibble com as colunas:
#' \describe{
#'   \item{processo}{Número do processo, somente dígitos.}
#'   \item{cd_processo}{Código interno do processo.}
#'   \item{dt_pet}{Data da petição (formato dd/mm/aaaa).}
#'   \item{tipo}{Tipo da petição.}
#' }
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

  tabela <- html |>
    xml2::xml_find_first(
      "//h2[@class='subtitle' and normalize-space(text()) = 'Petições diversas']/parent::div/following-sibling::table[1]"
    )

  pet_divers <- tabela |>
    xml2::xml_find_all("./tbody/tr[contains(@class, 'fundo')]") |>
    xml2::xml_text() |>
    stringr::str_squish()

  tibble::tibble(
    processo,
    cd_processo,
    pet_divers
  ) |>
    tidyr::separate(pet_divers, sep = "(?<=\\d{2}/\\d{2}/\\d{4}) ", into = c("dt_pet", "tipo"))

}

#' Ler composição do julgamento no CPOSG
#'
#' Extrai a composição do julgamento do processo, listando magistrados e sua participação.
#'
#' @param arquivo Caminho para o arquivo HTML ou objeto compatível com `xml2::read_html()`.
#' @return Um tibble com as colunas:
#' \describe{
#'   \item{processo}{Número do processo, somente dígitos.}
#'   \item{cd_processo}{Código interno do processo.}
#'   \item{participacao}{Tipo de participação do magistrado.}
#'   \item{magistrado}{Nome do magistrado.}
#' }
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

  tabela <- html |>
    xml2::xml_find_first(
      "//h2[@class='subtitle' and normalize-space(text()) = 'Composição do Julgamento']/parent::div/following-sibling::table[2]"
    )

  composicao_julgamento <- tabela |>
    xml2::xml_find_all("./tr[contains(@class, 'fundo')]")

  participacao <- composicao_julgamento |>
    xml2::xml_find_first("./td[1]") |>
    xml2::xml_text() |>
    stringr::str_squish()

  magistrado <- composicao_julgamento |>
    xml2::xml_find_all("./td[2]") |>
    xml2::xml_text() |>
    stringr::str_squish()

  tibble::tibble(
    processo,
    cd_processo,
    participacao,
    magistrado
  )

}

#' Ler julgamentos do processo no CPOSG
#'
#' Extrai os dados dos julgamentos do processo a partir do arquivo HTML.
#'
#' @param arquivo Caminho para o arquivo HTML ou objeto compatível com `xml2::read_html()`.
#' @return Um tibble com as colunas:
#' \describe{
#'   \item{processo}{Número do processo, somente dígitos.}
#'   \item{cd_processo}{Código interno do processo.}
#'   \item{dt_julgamento}{Data do julgamento (string).}
#'   \item{situacao_julgamento}{Situação do julgamento.}
#'   \item{decisao}{Decisão do julgamento.}
#' }
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

  tabela <- html |>
    xml2::xml_find_first(
      "//h2[@class='subtitle' and normalize-space(text()) = 'Julgamentos']/parent::div/following-sibling::table[2]"
    )

  julgamentos <- tabela |>
    xml2::xml_find_all("./tr[contains(@class, 'fundo')]")

  dt_julgamento <- julgamentos |>
    xml2::xml_find_first("./td[1]") |>
    xml2::xml_text() |>
    stringr::str_squish()

  situacao_julgamento <- julgamentos |>
    xml2::xml_find_all("./td[2]") |>
    xml2::xml_text() |>
    stringr::str_squish()

  decisao <- julgamentos |>
    xml2::xml_find_all("./td[3]") |>
    xml2::xml_text() |>
    stringr::str_squish()

  tibble::tibble(
    processo,
    cd_processo,
    dt_julgamento,
    situacao_julgamento,
    decisao
  )
}

