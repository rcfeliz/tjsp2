processo <- "1003101-38.2023.8.26.0223" # em pg
cd_processo <- "67000FE100000" # em pg

processo <- "00015089620188260180" # número em pg, mas está em sg
cd_processo <- "500000TGT0000" # número em pg, mas está em sg
cd_processo <- "RI007OWYY0000" # número em sg

tjsp_cpopg_baixar_tabela_processo <- function (processo = NULL, diretorio = ".") {

  processo <- abjutils::clean_cnj(processo)

  url <- "https://esaj.tjsp.jus.br/cpopg/search.do?gateway=true"

  cookies <- httr2::last_request()$options$cookiefile

  # Define request
  req <- httr2::request(url)  |>
    httr2::req_cookie_preserve(cookies) |>
    httr2::req_options(ssl_verifypeer = 0)

  # Build query
  unificado <- processo |>
    stringr::str_extract(".{15}")

  foro <- processo |>
    stringr::str_extract("\\d{4}$")

  query <- list(
    conversationId = "",
    dadosConsulta.localPesquisa.cdLocal = "-1",
    cbPesquisa = "NUMPROC",
    dadosConsulta.tipoNuProcesso = "UNIFICADO",
    numeroDigitoAnoUnificado = unificado,
    foroNumeroUnificado = foro,
    dadosConsulta.valorConsultaNuUnificado = processo,
    dadosConsulta.valorConsulta = "",
    uuidCaptcha = ""
  )

  # Perform response
  resp <- req |>
    httr2::req_url_query(!!!query) |>  # query
    httr2::req_perform()

  # html
  conteudo <- resp |>
    httr2::resp_body_html()

  if(xml2::xml_find_first(conteudo, "boolean(//div[@id='listagemDeProcessos'])")) {

    cd_processos <- conteudo |>
      xml2::xml_find_all("//a[@class='linkProcesso']") |>
      xml2::xml_attr("href") |>
      stringr::str_extract( "(?<=processo\\.codigo=)\\w+")

  } else if(xml2::xml_find_first(conteudo, "boolean(//div[@id='modalIncidentes'])")) {

    cd_processos <- conteudo |>
      xml2::xml_find_all("//input[@id='processoSelecionado']") |>
      xml2::xml_attr("value")
  } else {

    cd_processo <- conteudo |>
      xml2::xml_find_first("//script[contains(text(),'processo.codigo')]") |>
      xml2::xml_text() |>
      stringr::str_extract("(?<=processo.codigo=)\\w+")
  }

  id <- "TABELA_{processo}_{cd_processo}.html"

  arquivo <- fs::path(diretorio, id, ext = "html")

  conteudo |>
    xml2::xml_find_first("boolean(//a[@class='linkConsultaSG btn btn-secondary btn-space'])")
}

tjsp_cpopg_baixar_tabela_cd_processo <- function (cd_processo = NULL, diretorio = ".") {

  cd_processo <- stringr::str_extract(cd_processo,"\\w+")

  url <- glue::glue("https://esaj.tjsp.jus.br/cpopg/show.do?processo.codigo={cd_processo}&gateway=true")

  cookies <- httr2::last_request()$options$cookiefile

  # Define request and perform response
  resp <- url |>
    httr2::request() |>
    httr2::req_cookie_preserve(cookies) |>
    httr2::req_options(ssl_verifypeer = FALSE) |>
    httr2::req_perform()

  # html
  html <- resp |>
    httr2::resp_body_html()

  processo <- html |>
    xml2::xml_find_first("//span[@id='numeroProcesso']") |>
    xml2::xml_text() |>
    stringr::str_remove_all("\\W")

  # Autos estão na segunda instância?
  if(xml2::xml_find_first(html, "boolean(//a[@class='linkConsultaSG btn btn-secondary btn-space'])")) {

      cd_processo_sg <- html |>
        xml2::xml_find_first("//a[@class='linkConsultaSG btn btn-secondary btn-space']") |>
        xml2::xml_attr("href") |>
        stringr::str_extract("(?<=Sg.)\\w+")

      tjsp_cposg_baixar_tabela_cd_processo(cd_processo = cd_processo_sg, diretorio = diretorio)

    } else{

      url_table <- glue::glue("https://esaj.tjsp.jus.br/cpopg/abrirPastaDigital.do?processo.codigo={cd_processo}")

      resp_table <- url_table |>
        httr2::request() |>
        httr2::req_cookie_preserve(cookies) |>
        httr2::req_options(ssl_verifypeer = FALSE) |>
        httr2::req_perform() |>
        httr2::resp_body_string() |> # extrai novo link
        httr2::request() |> # faz request imediatamente depois, pois este link é temporário
        httr2::req_cookie_preserve(cookies) |>
        httr2::req_options(ssl_verifypeer = FALSE) |>
        httr2::req_perform()

      # salva o arquivo
      id <- glue::glue("TABELA_{processo}_{cd_processo}")

      arquivo <- fs::path(diretorio, id, ext = "html")

      writeBin(resp_table$body, arquivo)
    }
}

tjsp_cposg_baixar_tabela_cd_processo <- function (cd_processo = NULL, diretorio = ".") {

  cd_processo <- stringr::str_extract(cd_processo,"\\w+")

  url <- glue::glue("https://esaj.tjsp.jus.br/cposg/show.do?processo.codigo={cd_processo}&gateway=true")

  cookies <- httr2::last_request()$options$cookiefile

  # Define request and perform response
  resp <- url |>
    httr2::request() |>
    httr2::req_cookie_preserve(cookies) |>
    httr2::req_options(ssl_verifypeer = FALSE) |>
    httr2::req_perform()

  # html
  html <- resp |>
    httr2::resp_body_html()

  processo <- html |>
    xml2::xml_find_first("//span[@id='numeroProcesso']") |>
    xml2::xml_text() |>
    stringr::str_remove_all("\\W")

  # table
  url_table <- glue::glue("https://esaj.tjsp.jus.br/cposg/verificarAcessoPastaDigital.do?cdProcesso={cd_processo}&conversationId=&_=1599440192646")

  resp_table <- url_table |>
    httr2::request() |>
    httr2::req_cookie_preserve(cookies) |>
    httr2::req_options(ssl_verifypeer = FALSE) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |> # extrai novo link
    httr2::request() |> # faz request imediatamente depois, pois este link é temporário
    httr2::req_cookie_preserve(cookies) |>
    httr2::req_options(ssl_verifypeer = FALSE) |>
    httr2::req_perform()

  # salva o arquivo
  id <- glue::glue("TABELA_{processo}_{cd_processo}")

  arquivo <- fs::path(diretorio, id, ext = "html")

  writeBin(resp_table$body, arquivo)
}

arquivo <- "apagar/TABELA_00015089620188260180_RI007OWYY0000.html"
arquivo <- "apagar/TABELA_10031013820238260223_67000FE100000.html"

tjsp_ler_tabela_docs_cd_processo <- function(arquivos = NULL, diretorio = "."){

  doc <- arquivo |>
    xml2::read_html() |>
    xml2::xml_text() |>
    stringr::str_extract("(?<=requestScope = )\\X+?(?=;)") |>
    jsonlite::fromJSON()

  doc_name <- tibble::tibble(
    doc_name= doc$data$title
  ) |>
    tibble::rownames_to_column("id_doc")

  paginas <- doc$children[[2]]$data$indicePagina

    df <- purrr::imap_dfr(doc$children,~{

      url_doc <-  .x$data$parametros

      pagina_inicial <- .x$data$title |>
        stringr::str_extract("\\d+")

      pagina_final <- .x$data$title |>
        stringr::str_extract("\\d+$")

      tibble::tibble(id_doc = .y, pagina_inicial, pagina_final, url_doc) |>
        dplyr::mutate(id_doc = as.character(id_doc))

    }) |>
      dplyr::left_join(doc_name) |>
      dplyr::select(id_doc, doc_name, pagina_inicial, pagina_final, url_doc) |>
      dplyr::mutate(url_doc = paste0("https://esaj.tjsp.jus.br/pastadigital/getPDF.do?",url_doc)) |>
      dplyr::group_by(id_doc) |>
      dplyr::ungroup()
      # tibble::add_column(cd_processo_pg, .before =1) |>
      # tibble::add_column(cd_processo_sg, .after  = 1) |>
      # dplyr::mutate(instancia = ifelse(is.na(cd_processo_sg), 1, 2), .after = 2)

}
