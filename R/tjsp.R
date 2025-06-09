#' \code{tjsp} package
#'
#' Baixa  e organiza decisões do TJSP
#'
#'
"_PACKAGE"
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "html", "partes", "id_processo", "dt_dist",
    "autor", "reu", "polo_ativo", "polo_passivo",
    "localidade", "orgao_atual", "dt_modificacao",
    "value", "dt_dist", "processo_incidente",
    "URLdecode", "setNames", "parte", "dt_peticao",
    "pagina_final", "url_doc", "classe", "assunto",
    "row_id", "n", "id_doc", "pagina_inicial"
  ))
}
