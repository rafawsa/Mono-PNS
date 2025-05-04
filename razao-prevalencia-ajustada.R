# Razão de Prevalência Ajustada ----

## Cálculo da razão de prevalência ajustada para as variáveis investigadas. 
## Para estudos transversais a regressão de Poisson tem sido utilizada 
## para o cálculo da razão de prevalência, 
## utilizando a função de ligação log para garantir que os valores ajustados 
## permaneçam no intervalo de zero a infinito [@francisco_medidas_2008].

# Função para cálculo da Razão de Prevalência
# utilizando regressão de Poisson
# com filtro 
#   pns_svy: deve ser objeto do tipo tbl_svy
#   formula1: deve ser as.formula para regressão
#   filtro1: filtro para subgrupos
calc_razao_prevalencia_ajustada <- function(pns_svy, exposicao, desfecho,
                                       exp_pos = "Sim", exp_neg = "Não",
                                       des_pos = "Sim", des_neg = "Não") {

  desfecho <- srvyr::sym(desfecho)
  df <- 
    pns_svy |>
    srvyr::filter({{desfecho}} %in% c(des_pos, des_neg)) |>
    srvyr::mutate(ival = if_else({{desfecho}} == des_neg, 0, 1))
  
  modelo <- 
    survey::svyglm(formula = str_c("ival ~", exposicao, "+ C008 + V0001"),
                   design = df,
                   family = quasipoisson(link = "log"))
  
  tidy_results <-
    broom::tidy(modelo, conf.int = TRUE) |>
    dplyr::filter(term == str_c(exposicao, exp_pos)) |>
    dplyr::select(term, estimate, conf.low, conf.high) |>
    tidyr::pivot_longer(cols = -term,
                        names_to = "names",
                        values_to = "values") |>
    dplyr::mutate(values = exp(values)) |>
    tidyr::pivot_wider(names_from = names,
                       values_from = values)
}

## para testar a função:
# (calc_razao_prevalencia_ajustada(svy13, "J007", "J011"))
# (calc_razao_prevalencia_ajustada(svy19, "J007", "J011"))
# (calc_razao_prevalencia_ajustada(svy13, "J007", "J014"))

