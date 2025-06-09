#' Baixa lista de processos distribuídos do sistema e-proc TJSP
#'
#' Esta função realiza o download da lista de processos distribuídos em um
#' período específico do sistema e-proc do Tribunal de Justiça de São Paulo.
#' O arquivo HTML resultante é salvo no diretório especificado.
#'
#' @param dt_ini Data inicial do período de consulta no formato "dd/mm/aaaa".
#' @param dt_fim Data final do período de consulta no formato "dd/mm/aaaa".
#' @param diretorio Caminho do diretório onde o arquivo será salvo.
#'   Padrão é "." (diretório atual).
#' @param cookies_path Caminho para o arquivo de cookies. Se NULL, será criado
#'   um arquivo temporário.
#'
#' @return Não retorna valor. Salva arquivo HTML no diretório especificado com
#'   nome no formato "ini\{data\}_fim\{data\}.html".
#'
#' @export
eproc_lista_baixar <- function(
    dt_ini = NULL,
    dt_fim = NULL,
    diretorio = ".",
    cookies_path = NULL) {

  cookies <- cookies(cookies_path)

  if (is.null(dt_ini) || is.null(dt_fim)) {
    stop("As datas dt_ini e dt_fim s\u00E3o obrigat\u00F3rias.")
  }

  tryCatch({
    dt_ini_parsed <- lubridate::dmy(dt_ini)
    dt_fim_parsed <- lubridate::dmy(dt_fim)
  }, error = function(e) {
    stop("Formato de data inv\u00E1lido. Use 'dd/mm/aaaa'.")
  })

  if (!dir.exists(diretorio)) {
    fs::dir_create(diretorio)
    message("Diret\u00F3rio criado: ", diretorio)
  }

  if (!is.null(cookies)) {
    if (!file.exists(cookies)) {
      fs::file_create(cookies)
      message("Arquivo de cookies criado em: ", cookies)
    }
  }

  url <- "https://eproc1g-consulta.tjsp.jus.br/eproc/externo_controlador.php?acao=processo_distribuicao_listar"

  body <- list(
    txtDtaInicial = dt_ini,
    txtDtaFinal = dt_fim
  )

  tryCatch({
    message(glue::glue("Baixando dados para o per\u00EDodo {dt_ini} a {dt_fim}..."))

    req <- httr2::request(url) |>
      httr2::req_cookie_preserve(cookies) |>
      httr2::req_options(ssl_verifypeer = 0) |>
      httr2::req_timeout(300) |>  # 5 minutos timeout
      httr2::req_retry(max_tries = 3, backoff = ~ 2)  # Retry com backoff

    resp <- req |>
      httr2::req_body_form(!!!body) |>
      httr2::req_perform()

    if (length(resp$body) == 0) {
      stop("Resposta vazia do servidor.")
    }

    writeBin(resp$body, arquivo)

    if (!fs::file_exists(arquivo) || fs::file_size(arquivo) == 0) {
      stop("Falha ao salvar o arquivo ou arquivo vazio.")
    }

    message(glue::glue("Arquivo salvo com sucesso: {arquivo}"))
    return(arquivo)

  }, error = function(e) {
    if (fs::file_exists(arquivo)) {
      fs::file_delete(arquivo)
    }
    stop(glue::glue("Erro ao baixar dados: {e$message}"))
  })

}

#' Extrai dados de processos de arquivo HTML do e-proc TJSP
#'
#' Esta função analisa um arquivo HTML baixado do sistema e-proc TJSP e extrai
#' os dados dos processos distribuídos, convertendo-os em um data frame estruturado.
#' O arquivo deve ser gerado pela função eproc_lista_baixar().
#'
#' @param file Caminho para o arquivo HTML contendo a lista de processos.
#'
#' @return Data frame com as seguintes colunas:
#'   \itemize{
#'     \item id_processo: Número do processo
#'     \item dt_dist: Data de distribuição (formato texto)
#'     \item classe: Classe processual
#'     \item assunto: Assunto do processo
#'     \item partes: Partes envolvidas no processo (autor e réu)
#'     \item localidade: Localidade do processo
#'     \item orgao_atual: Órgão jurisdicional atual
#'   }
#'
#' @export
eproc_lista_parse <- function(file = NULL) {

  # VALIDACOES #################################################################
  # Validacoes de entrada
  if (is.null(file)) {
    stop("O par\u00E2metro 'file' \u00E9 obrigat\u00F3rio.")
  }

  if (!fs::file_exists(file)) {
    stop(glue::glue("Arquivo n\u00E3o encontrado: {file}"))
  }

  # Verificar se eh um arquivo HTML valido
  if (!stringr::str_detect(tolower(file), "\\.html?$")) {
    warning("Arquivo pode n\u00E3o ser HTML v\u00E1lido.")
  }

  # Verificar tamanho do arquivo
  file_size <- fs::file_size(file)
  if (file_size == 0) {
    stop("Arquivo est\u00E1 vazio.")
  }

  # PARSE #################################################################
  message(glue::glue("Processando arquivo: {fs::path_file(file)} ({fs::fs_bytes(file_size)})"))

  # Buscar nodes da tabela
  nodes <- html |>
    xml2::xml_find_all("//tr[@class='infraTrEscura' or @class='infraTrClara']")

  # Extrair dados com tratamento de erro robusto
  da <- purrr::map_dfr(nodes, function(.x) {
    tryCatch({
      tds <- xml2::xml_find_all(.x, "./td")

      # Extrair e limpar dados
      id_processo <- xml2::xml_text(tds[2]) |> stringr::str_trim()
      classe <- xml2::xml_text(tds[3]) |> stringr::str_trim()
      assunto <- xml2::xml_text(tds[4]) |> stringr::str_trim()
      partes <- xml2::xml_text(tds[5]) |> stringr::str_trim()
      localidade <- xml2::xml_text(tds[6]) |> stringr::str_trim()
      orgao_atual <- xml2::xml_text(tds[7]) |> stringr::str_trim()
      dt_dist <- xml2::xml_text(tds[8]) |> stringr::str_trim()

      tibble::tibble(
        id_processo = id_processo,
        dt_dist = dt_dist,
        classe = classe,
        assunto = assunto,
        partes = partes,
        localidade = localidade,
        orgao_atual = orgao_atual
      )

    }, error = function(e) {
      warning(glue::glue("Erro ao processar linha: {e$message}"))
      return(NULL)
    })
  })

  # Relatorio final
  processos_validos <- sum(!is.na(da$id_processo) & nchar(da$id_processo) > 0)
  message(glue::glue("Extra\u00E7\u00E3o conclu\u00EDda: {nrow(da)} registros, {processos_validos} processos v\u00E1lidos."))

  return(da)
}

#' Processa e organiza dados de processos eletrônicos
#'
#' Esta função processa um data frame contendo informações de processos eletrônicos,
#' separando e organizando as partes (autores e réus) em estruturas aninhadas.
#' A função limpa os dados do CNJ, converte datas e separa as partes processuais
#' em polos ativo e passivo.
#'
#' @param da Data frame contendo os dados dos processos eletrônicos. Deve conter
#'   as colunas 'partes', 'id_processo' e 'dt_dist'.
#'
#' @return Data frame processado com as seguintes colunas:
#'   \itemize{
#'     \item id_processo: Número do processo
#'     \item dt_dist: Data de distribuição (formato texto)
#'     \item classe: Classe processual
#'     \item assunto: Assunto do processo
#'     \item polo_ativo: Lista aninhada contendo os autores do processo
#'     \item polo_passivo: Lista aninhada contendo os réus do processo
#'     \item localidade: Localidade do processo
#'     \item orgao_atual: Órgão jurisdicional atual
#'   }
#'
#' @export
eproc_lista_tidy <- function(da = NULL) {

  # VALIDACOES #################################################################
  # Verifica\u00E7\u00E3o de entrada
  if (is.null(da)) {
    stop("Voc\u00EA deve fornecer um data frame.")
  }

  if (!is.data.frame(da)) {
    stop("O par\u00E2metro 'da' deve ser um data frame.")
  }

  # Verificar se as colunas necessarias existem
  required_cols <- c("partes", "id_processo", "dt_dist")
  missing_cols <- setdiff(required_cols, names(da))
  if (length(missing_cols) > 0) {
    stop(paste("Colunas obrigat\u00F3rias n\u00E3o encontradas:", paste(missing_cols, collapse = ", ")))
  }

  # Carregar bibliotecas necessarias
  required_packages <- c("tidyr", "dplyr", "stringr", "abjutils", "lubridate")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste("Pacote", pkg, "\u00E9 necess\u00E1rio mas n\u00E3o est\u00E1 instalado."))
    }
  }

  # TIDY #################################################################

  da |>
    tidyr::separate(partes, sep = "R\u00E9u", into = c("autor", "reu")) |>
    dplyr::mutate(
      id_processo = abjutils::clean_cnj(id_processo),
      dt_dist = lubridate::dmy_hms(dt_dist),
      autor = autor |>
        stringr::str_remove("^Autor") |>
        stringr::str_split("AUTOR|EXEQUENTE|REQUERENTE|RECLAMANTE|EMBARGANTE"),
      reu = reu |>
        stringr::str_split("R\u00C9U|EXECUTADO|REQUERIDO|RECLAMADO|EMBARGADO|Requerente")
    ) |>
    tidyr::unnest(autor) |>
    dplyr::mutate(autor = stringr::str_remove(autor, " - $")) |>
    dplyr::filter(autor != "X") |>
    dplyr::group_by(id_processo) |>
    tidyr::nest(polo_ativo = autor) |>
    dplyr::ungroup() |>
    tidyr::unnest(reu) |>
    dplyr::mutate(reu = stringr::str_remove(reu, "\\s*-\\s*[^-]+$")) |>
    dplyr::filter(reu != "") |>
    dplyr::group_by(id_processo) |>
    tidyr::nest(polo_passivo = reu) |>
    dplyr::ungroup() |>
    dplyr::select(
      id_processo,
      dt_dist,
      classe,
      assunto,
      polo_ativo,
      polo_passivo,
      localidade,
      orgao_atual
    )

}
