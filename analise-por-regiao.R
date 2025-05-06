# Análise por região
# 

# Bibliotecas ----
library(tidyverse) # utilizar a sintaxe tidyverse com o R
library(survey)    # análise de pesquisas complexas
library(srvyr)     # provê notação tidyverse para o pacote survey 
library(PNSIBGE)   # acesso aos dados da PNS
library(broom)     # sumariza as informações dos modelos em formato tidy
library(quarto)

source("read-pns-data.R")
rm(var_pns_2013)
rm(var_pns_2019)
rm(pns13)
rm(pns19)
rm(dfpns13)
rm(dfpns19)

svy19  <- as_tibble(svy19)
regioes <- 
  svy19 |> 
  distinct(regiao) |> 
  pull(regiao) |> 
  as.character()

quarto_render(input = "analise-regiao-report.qmd",
              output_file = "analise-regiao-norte.html",
              execute_params = list(regiao = "Norte"))

quarto_render(input = "analise-regiao-report.qmd",
              output_file = "analise-regiao-nordeste.html",
              execute_params = list(regiao = "Nordeste"))

quarto_render(input = "analise-regiao-report.qmd",
              output_file = "analise-regiao-sudeste.html",
              execute_params = list(regiao = "Sudeste"))


quarto_render(input = "analise-regiao-report.qmd",
              output_file = "analise-regiao-sul.html",
              execute_params = list(regiao = "Sul"))


quarto_render(input = "analise-regiao-report.qmd",
              output_file = "analise-regiao-centro-oeste.html",
              execute_params = list(regiao = "Centro-Oeste"))



por_regiao <- 
  tibble(
    intput = "analise-regiao-report.qmd",
    output_file = str_glue("analise-regiao-{regioes}.html") ,
    execute_params = map(regioes, ~ list(regiao = .))
)

por_regiao |> View()

pwalk(por_regiao, quarto_render)


library(quarto)
library(tidyverse)
library(gapminder)

countries <-
  gapminder |>
  distinct(country) |>
  pull(country) |>
  as.character()

reports <-
  tibble(
    input = "analise-regiao-report.qmd",
    output_file = str_glue("{regioes}.html"),
    execute_params = map(regioes, ~ list(regiao = .))
  )

pwalk(reports, quarto_render)
