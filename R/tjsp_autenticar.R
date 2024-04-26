#' Autenticar no tjsp
#'
#' @param login CPF
#' @param password Senha
#'
#' @return Estabelece uma sessão, não é necessário salvar.
#'
#' @export
#'
#' @details Você pode informar as credenciais nos argumentos ou
#'      criar variáveis de ambiente: "LOGINADV" e "PASSWORDADV", ou
#'      chamar a função e aguardar o prompt para informar
#'      login e password
tjsp_autenticar <- function(login = NULL, password = NULL) {

  cookies <- tempfile()
  # Check if isn't already logged in
  if (check_login(cookies)) {
    return(TRUE)
  }

  # Prompt for information if necessary
  if (is.null(login) || is.null(password)) {

    login <- Sys.getenv("LOGINADV")
    password <- Sys.getenv("PASSWORDADV")

    if (login == "" || password == "") {
      login <- as.character(getPass::getPass(msg = "Enter your login: "))
      password <- as.character(getPass::getPass(msg = "Enter your password: "))

    }

  }

  # Initial access
  base <- "https://esaj.tjsp.jus.br/"

  base |>
    stringr::str_c("esaj/portal.do?servico=740000") |>
    httr2::request() |>
    # httr2::req_cookie_preserve(cookies) |>
    httr2::req_options(ssl_verifypeer = 0) |>
    httr2::req_perform()

  # Get login page file
  f_login <- stringr::str_c(
    base, "sajcas/login?service=",
    utils::URLencode(
      stringr::str_c(base, "esaj/j_spring_cas_security_check"),
      reserved = TRUE
      )
    ) |>
    httr2::request() |>
    httr2::req_cookie_preserve(cookies) |>
    httr2::req_options(ssl_verifypeer = 0) |>
    httr2::req_perform()

  # Get parameters for POST
  lt <- f_login |>
    httr2::resp_body_string() |>
    xml2::read_html() |>
    xml2::xml_find_first("//input[@name='lt']") |>
    xml2::xml_attr("value")

  e2 <- f_login |>
    httr2::resp_body_html() |>
    xml2::xml_find_first("//input[@name='execution']") |>
    xml2::xml_attr("value")

  # Create POST quert
  query_post <- list(
    username = login,
    password = password,
    lt = lt,
    execution = e2,
    "_eventId" = "submit",
    pbEntrar = "Entrar",
    signature = "",
    certificadoSelecionado = "",
    certificado = ""
  )

  # Try to login
 stringr::str_c(
    base, "sajcas/login?service=",
    utils::URLencode(
      stringr::str_c(base, "esaj/j_spring_cas_security_check"),
      reserved = TRUE
    )
  ) |>
   httr2::request() |>
   httr2::req_cookie_preserve(cookies) |>
   httr2::req_options(ssl_verifypeer = 0) |>
   httr2::req_body_form(!!!query_post) |>
   httr2::req_perform()

  # Message
  flag <- check_login(cookies)
  if (flag) {
    message("You're logged in")
  }
  else {
    message("Login failed")
  }

  return(flag)
}

check_login <- function(cookies) {
  "https://esaj.tjsp.jus.br/" |>
    stringr::str_c("sajcas/verificarLogin.js") |>
    httr2::request() |>
    httr2::req_cookie_preserve(cookies) |>
    httr2::req_options(ssl_verifypeer = FALSE) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    stringr::str_detect("true")
}
