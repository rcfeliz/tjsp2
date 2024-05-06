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
tjsp_cpopg_ler <- function(arquivos = NULL, return = c("Padronizado", "Completo"), wide = TRUE) {

  if(fs::is_dir(arquivos)) {
    arquivos <- fs::dir_ls(arquivos)
  }

  da <- purrr::map_dfr(arquivos, ~{

    capa <- tjsp_cpopg_ler_capa(.x, return = return, wide = wide)

    movimentos <- tjsp_ler_movimentacao(.x) |>
      dplyr::distinct() |>
      tidyr::nest(.by = c(processo, cd_processo), .key = "movimentos")

    partes <- tjsp_ler_partes(.x) |>
      tidyr::nest(.by = c(processo, cd_processo), .key = "partes")

    completo <- capa |>
      dplyr::left_join(movimentos, by = dplyr::join_by(processo, cd_processo)) |>
      dplyr::left_join(partes, by = dplyr::join_by(processo, cd_processo))

    completo
  })

  return(da)
}
