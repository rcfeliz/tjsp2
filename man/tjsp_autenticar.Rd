% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tjsp_autenticar.R
\name{tjsp_autenticar}
\alias{tjsp_autenticar}
\title{Autenticar no tjsp}
\usage{
tjsp_autenticar(login = NULL, password = NULL)
}
\arguments{
\item{login}{CPF}

\item{password}{Senha}
}
\value{
Estabelece uma sessão, não é necessário salvar.
}
\description{
Autenticar no tjsp
}
\details{
Você pode informar as credenciais nos argumentos ou
     criar variáveis de ambiente: "LOGINADV" e "PASSWORDADV", ou
     chamar a função e aguardar o prompt para informar
     login e password
}
