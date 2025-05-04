# Cálculo da RP-Bruta ----

# f monta_tabela_contingencia ----
monta_tabela_contingencia <- function(df, exposicao, desfecho, des_pos = "Sim", des_neg = "Não") {
  # transforma parâmetros string em símbolos
  exposicao <- srvyr::sym(exposicao)
  desfecho <- srvyr::sym(desfecho)
  df |> # abaixo usaremos "curly-curly" para que os símbolos sejam 
    # avaliados no contexto do data frame
    srvyr::filter({{desfecho}} %in% c(des_pos, des_neg)) |> # exclui "Ignorados"
    srvyr::group_by({{exposicao}}, {{desfecho}}) |>     # agrupa...
    srvyr::summarize(
      pop = srvyr::survey_total(),                      # num absolutos
      prop = srvyr::survey_mean()                       # proporções
    ) |> 
    filter(!is.na({{exposicao}})) |> 
    filter(!is.na({{desfecho}}))
}

# testes:
# (monta_tabela_contingencia(svy13, "J007", "J014"))
# (monta_tabela_contingencia(svy13 %>% filter(J007 == "Sim"), 
#                           "C006", "J014"))
# monta_tabela_contingencia(c6_svy13_dcnt, "instrucao", "J014")

## Para testar a função acima:
# monta_tabela_contingencia(svy13, "J007", "J011")
# monta_tabela_contingencia(svy19, "J007", "J002")
# ## testar: tabela de contingência para portadores de DCNT...
# ## ... considerando sexo (C006) e ...
# ## ter usado serviço de saúde nas duas últimas semanas (J014)
# monta_tabela_contingencia(svy13 |> srvyr::filter(J007 == "Sim"), "C006", "J014")


# f calc_rp_bruta ----
calc_rp_bruta <- function(df, exposicao, desfecho,
                     exp_pos = "Sim", exp_neg = "Não",
                     des_pos = "Sim", des_neg = "Não") {
  tbl <- monta_tabela_contingencia(df, exposicao, desfecho)
  
  a <- tbl[tbl[,1] == exp_pos & tbl[,2] == des_pos,]$prop
  b <- tbl[tbl[,1] == exp_pos & tbl[,2] == des_neg,]$prop
  c <- tbl[tbl[,1] == exp_neg & tbl[,2] == des_pos,]$prop
  d <- tbl[tbl[,1] == exp_neg & tbl[,2] == des_neg,]$prop
  rp <- (a/(a+b)) / (c/(c+d))
}

## para testar a função acima:
# (calc_rp_bruta(svy13, "J007", "J011"))
# (calc_rp_bruta(svy19, "J007", "J011"))
# (calc_rp_bruta(svy13 |> srvyr::filter(sexo == "Mulher"), "J007", "J014"))
# (calc_rp_bruta(c6_svy13_dcnt, "instrucao", "J014"))


