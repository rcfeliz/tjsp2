#' Ler dados da capa do processo no CPOSG
#'
#' Extrai informações básicas da capa do processo judicial a partir do arquivo HTML.
#'
#' @param arquivos Caminho para o arquivo HTML ou objeto compatível com `xml2::read_html()`.
#' @param outros Parse das outras seções do HTML. Se escolher "Tudo" irá baixar tudo.
#' As opções são:
#' Apensos, Numeros de 1a instancia, Partes, Movimentacoes, Subprocessos e Recursos, Composicao do Julgamento, Julgamentos
#' @return Umatibble
#' @export
esaj_cposg_ler <- function(
    arquivos = NULL,
    outros = c("Tudo", "Apensos", "Numeros de 1a instancia", "Partes", "Movimentacoes", "Subprocessos e Recursos", "Composicao do Julgamento", "Julgamentos")
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
  }, .progress = TRUE)

}

#' Ler dados da capa do processo no CPOSG
#'
#' Extrai informações básicas da capa do processo judicial a partir do arquivo HTML.
#'
#' @param arquivo Caminho para o arquivo HTML ou objeto compatível com `xml2::read_html()`.
#' @param outros Parse das outras seções do HTML. Se escolher "Tudo" irá baixar tudo.
#' As opções são:
#' Apensos, Numeros de 1a instancia, Partes, Movimentacoes, Subprocessos e Recursos, Composicao do Julgamento, Julgamentos
#' @return Uma tibble
esaj_cposg_ler_unitario <- function(
    arquivo = NULL,
    outros = c("Tudo", "Apensos", "Numeros de 1a instancia", "Partes", "Movimentacoes", "Subprocessos e Recursos", "Composicao do Julgamento", "Julgamentos")
) {

  aux_capa <- esaj_cposg_ler_capa(arquivo)

  if(is.null(outros)) {
    da <- aux_capa
    # extrai demais informacoes
  } else {
    # ajusta o parametro 'info'
    if(any(outros == "Tudo")) outros <- c("Apensos", "Numeros de 1a instancia", "Partes", "Movimentacoes", "Subprocessos e Recursos", "Composicao do Julgamento", "Julgamentos")

    # fix outros
    depara <- c(
      "Numeros de 1a instancia" = "Numeros PG",
      "Subprocessos e Recursos" = "Subprocessos Recursos",
      "Composicao do Julgamento" = "Composicao Julgamento"
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

  processo <- esaj_cposg_get_processo(arquivo)

  cd_processo <- html |>
    xml2::xml_find_first("//input[@name='cdProcesso']") |>
    xml2::xml_attr("value")

  situacao <- html |>
    xml2::xml_find_first("//div[@class='col-md-13']") |>
    xml2::xml_find_first(".//span[contains(@class, 'unj-tag')]") |>
    xml2::xml_text()

  classe <- html |>
    xml2::xml_find_first("//div[@id='classeProcesso']") |>
    xml2::xml_text(trim = TRUE)

  if (is.na(classe)) {

    classe <- html |>
      xml2::xml_find_first("//div[@class='col-md-13']") |>
      xml2::xml_find_first(".//span[contains(@class, 'unj-larger')]") |>
      xml2::xml_text() |>
      stringr::str_remove_all("\\d+|[:punct:]") |>
      stringr::str_squish()

  }

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

  processo <- esaj_cposg_get_processo(arquivo)

  cd_processo <- html |>
    xml2::xml_find_first("//input[@name='cdProcesso']") |>
    xml2::xml_attr("value")

  tem_apensos <- !html |>
    xml2::xml_find_first("boolean(//td[@id='dadosApensosNaoDisponiveis'])")

  if (tem_apensos) {
    # 00041332420188260077
    message(glue::glue("Rever apensos para {processo}"))
  } else {
    tibble::tibble(
      processo,
      cd_processo,
      numero = NA_character_,
      classe = NA_character_,
      apensamento = NA_character_,
      motivo = NA_character_
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

  processo <- esaj_cposg_get_processo(arquivo)

  cd_processo <- html |>
    xml2::xml_find_first("//input[@name='cdProcesso']") |>
    xml2::xml_attr("value")

  tabela <- html |>
    xml2::xml_find_first(
      "//h2[@class='subtitle' and normalize-space(text()) = 'N\u00FAmeros de 1\u00AA Inst\u00E2ncia']/parent::div/following-sibling::table[2]"
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

  # 1506158-21.2025.8.26.0228
  # 0004132-17.2016.8.26.0495

  html <- arquivo |>
    xml2::read_html()

  processo <- esaj_cposg_get_processo(arquivo)

  cd_processo <- html |>
    xml2::xml_find_first("//script") |>
    xml2::xml_text() |>
    stringr::str_extract("(?<=processo\\.codigo=).{13}")

  mais <- html |>
    xml2::xml_find_first("boolean(//table[@id='tableTodasPartes'])")

  if(mais) {
    tabela <- html |>
      xml2::xml_find_first("//table[@id='tableTodasPartes']")
  } else {
    tabela <- html |>
      xml2::xml_find_first("//table[@id='tablePartesPrincipais']")
  }

  da_parte <- tabela |>
    rvest::html_table() |>
    setNames(c("tipo_parte","parte")) |>
    tidyr::separate(parte,c("nome", "representante"),sep = "(?<=\\S)\\s{10,}", extra = "merge") |>
    dplyr::mutate(
      id_parte = dplyr::row_number(),
      obs = stringr::str_detect(nome, "\\(Histórico da parte\\)"),
      representante = representante |>
        stringr::str_replace_all("&nbsp|\\:", "   ") |>
        stringr::str_remove_all("[:punct:]") |>
        stringr::str_split("\\s{5,}")
    ) |>
    dplyr::relocate(id_parte, .before = 1) |>
    tidyr::unnest(representante) |>
    dplyr::group_by(id_parte) |>
    dplyr::mutate(
      id_interno = dplyr::row_number(),
      obs = dplyr::case_when(
        obs & id_interno == 1 ~ representante
      ),
      representante = dplyr::case_when(
        representante == obs ~ NA_character_,
        TRUE ~ representante
      ),
      excluir = !is.na(obs) & id_interno == 1
    ) |>
    tidyr::fill(obs, .direction = "down") |>
    dplyr::filter(!excluir) |>
    dplyr::mutate(
      id_interno = dplyr::row_number()
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      tipo_representante = dplyr::case_when(
        id_interno %% 2 == 1 ~ representante
      ),
      representante = dplyr::case_when(
        representante == tipo_representante ~ NA_character_,
        TRUE ~ representante
      ),
      id_interno = ceiling(id_interno / 2),
      excluir = is.na(representante) & !is.na(tipo_representante)
    ) |>
    dplyr::group_by(id_parte, id_interno) |>
    tidyr::fill(tipo_representante, .direction = "down") |>
    dplyr::ungroup() |>
    dplyr::filter(!excluir) |>
    dplyr::select(
      id_parte,
      tipo_parte,
      nome,
      obs,
      tipo_representante,
      representante
    )

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

  processo <- esaj_cposg_get_processo(arquivo)

  cd_processo <- html |>
    xml2::xml_find_first("//input[@name='cdProcesso']") |>
    xml2::xml_attr("value")

  # movimentos
  movs <- html |>
    xml2::xml_find_first("//table/tbody[@id='tabelaTodasMovimentacoes']") |>
    xml2::xml_find_all("./tr")

  dt_mov <- movs |>
    xml2::xml_find_first("./td[contains(@class,'dataMovimentacao')]") |>
    xml2::xml_text(trim = TRUE)

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

  return(da)
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

  processo <- esaj_cposg_get_processo(arquivo)

  cd_processo <- html |>
    xml2::xml_find_first("//input[@name='cdProcesso']") |>
    xml2::xml_attr("value")

  tem_pet_divers <- xml2::xml_find_first(
    "boolean(//h2[@class='subtitle' and normalize-space(text()) = 'Peti\u00E7\u00F5es diversas']/parent::div/following-sibling::table[1])"
  )

  tabela <- html |>
    xml2::xml_find_first(
      "//h2[@class='subtitle' and normalize-space(text()) = 'Peti\u00E7\u00F5es diversas']/parent::div/following-sibling::table[1]"
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

  processo <- esaj_cposg_get_processo(arquivo)

  cd_processo <- html |>
    xml2::xml_find_first("//input[@name='cdProcesso']") |>
    xml2::xml_attr("value")

  tem_julgamento <- html |>
    xml2::xml_find_first(
      "boolean(//h2[@class='subtitle' and normalize-space(text()) = 'Composi\u00E7\u00E3o do Julgamento']/parent::div/following-sibling::table[2])"
    )

  if (tem_julgamento) {

    tabela <- html |>
      xml2::xml_find_first(
        "//h2[@class='subtitle' and normalize-space(text()) = 'Composi\u00E7\u00E3o do Julgamento']/parent::div/following-sibling::table[2]"
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

    da <- tibble::tibble(
      processo,
      cd_processo,
      participacao,
      magistrado
    )

  } else {
    da <- tibble::tibble(
      processo,
      cd_processo,
      participacao = NA_character_,
      magistrado = NA_character_
    )
  }

  return(da)

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
#'   \item{dt_julgamento}{Data do julgamento.}
#'   \item{situacao_julgamento}{Situação do julgamento.}
#'   \item{decisao}{Decisão do julgamento.}
#' }
esaj_cposg_ler_julgamentos <- function(arquivo = NULL)  {

  html <- arquivo |>
    xml2::read_html()

  processo <- esaj_cposg_get_processo(arquivo)

  cd_processo <- html |>
    xml2::xml_find_first("//input[@name='cdProcesso']") |>
    xml2::xml_attr("value")

  tem_julgamento <- !xml2::xml_find_first(html, "boolean(//td[@id='processoSemJulgamentos'])")

  if(tem_julgamento) {

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

    da <- tibble::tibble(
      processo,
      cd_processo,
      dt_julgamento,
      situacao_julgamento,
      decisao
    )
  } else {
    da <- tibble::tibble(
      processo,
      cd_processo,
      dt_julgamento = NA_character_,
      situacao_julgamento = NA_character_,
      decisao = NA_character_
    )
  }
  return(da)
}

esaj_cposg_get_processo <- function(arquivo = NULL) {

  html <- arquivo |>
    xml2::read_html()

  processo <- html |>
    xml2::xml_find_first("//span[@id='numeroProcesso']") |>
    xml2::xml_text(trim = TRUE) |>
    stringr::str_remove_all("\\D+")

  if (is.na(processo)) {

    processo <- html |>
      xml2::xml_find_first("//div[@class='col-md-13']") |>
      xml2::xml_find_first(".//span[contains(@class, 'unj-larger')]") |>
      xml2::xml_text() |>
      stringr::str_extract_all("\\d{7}-\\d{2}\\.\\d{4}\\.\\d\\.\\d{2}\\.\\d{4}") |>
      stringr::str_remove_all("\\D+") |>
      purrr::pluck(1)

  }

  return(processo)
}
