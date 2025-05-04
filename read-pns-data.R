# read-pns-data.R ----  

# Bibliotecas ----
# Este script requer as seguintes bibliotecas:
# library(tidyverse) # utilizar a sintaxe tidyverse com o R
# library(survey)    # análise de pesquisas complexas
# library(srvyr)     # provê notação tidyverse para o pacote survey
# library(PNSIBGE)   # acesso aos dados da PNS


# Ler dados 2013 e 2019 ----
# usaremos o pacote PNSIBGE [@assuncao_pnsibge_2020]
  
## Subgrupo de variáveis desejadas

## Variáveis PNS 2013 ----
var_pns_2013 = c("V0001",  # unidade da federação
                 "C006",   # sexo
                 "C008",   # idade
                 "J007",   # possui DCNT
                 "J014",   # Usou serviço saúde nas últimas 2 semanas 
                 "J037",   # Teve internação nos últimos 12 meses
                 "J002",   # Deixou de realizar atividades nas 2 últimas semanas por motivo de saúde
                 "J011",   # Realizaou consulta médica nos últimos 12 meses
                 "I001",   # Tem plano de saúde particular
                 "VDD004A" # nível de instrução mais elevado
)

## Variáveis PNS 2019 ----
var_pns_2019 = c( "V0001",  # unidade da federação
                  "C006",   # sexo
                  "C008",   # idade
                  "J007",   # possui DCNT
                  "J014",   # Usou serviço saúde nas últimas 2 semanas 
                  "J037",   # Internação nos últimos 12 meses
                  "J002",   # Deixou de realizar atividades nas 2 últimas semanas por motivo de saúde
                  "J01101", # Consulta médica nos últimos 12 meses, consulta médica
                  "I00101", # Tem plano de saúde particular
                  "VDD004A" # nível de instrução mais elevado
)

## ler dados PNS 2013
pns13 <-
  PNSIBGE::get_pns(
    year = 2013,
    selected = FALSE,
    anthropometry = FALSE,
    vars = var_pns_2013,
    labels = TRUE,
    design = TRUE)

## ler dados PNS 2019
pns19 <-
  PNSIBGE::get_pns(
    year = 2019,
    selected = FALSE,
    anthropometry = FALSE,
    vars = var_pns_2019,
    labels = TRUE,
    design = TRUE)

## Os dados sem pesos permite obter informações 
## sobre __labels__ e __levels__.
dfpns13 <-
  PNSIBGE::get_pns(
    year = 2013,
    selected = FALSE,
    anthropometry = FALSE,
    vars = var_pns_2013,
    labels = TRUE,
    design = FALSE)
dfpns19 <-
  PNSIBGE::get_pns(
    year = 2019,
    selected = FALSE,
    anthropometry = FALSE,
    vars = var_pns_2019,
    labels = TRUE,
    design = FALSE)

# tbl-svy ---- 
# O pacote srvyr trabalha com o objeto tbl.svy 
# ao invés do survey.design2 do pacote survey. 

## Conversão de survey.design2 para tbl_svy ----
svy13 <- srvyr::as_survey(pns13)
svy19 <- srvyr::as_survey(pns19)

# Igualando 2013 e 2019 ----

## J007 - possui DCNT ----
### Ajustar o "level" de J007 ----
### para que os cálculos considerem
### portadores em referência a não portadores de DCNT
levels(dfpns13$J007)
levels(dfpns19$J007) # dados de 2019 contém "Ignorado" 
dfpns19 |> count(J007) # Não há registros com valor "Ignorado"
svy13 <- 
  svy13 |> 
  srvyr::mutate(J007 = relevel(J007, ref = "Não")) 
svy19 <- 
  svy19 |> 
  srvyr::filter(J007 %in% c("Sim", "Não")) |>  
  srvyr::mutate(
    J007 = factor(J007, levels = c("Sim", "Não")),
    J007 = relevel(J007, ref = "Não")) 

# J014 - Usou serviço de saúde nas últimas 2 semanas
levels(dfpns13$J014)
levels(dfpns19$J014) # dados de 2019 contém "Ignorado" 
dfpns19 |> count(J014) # Não há registros com valor "Ignorado"

# J037 - Teve internação nos últimos 12 meses
levels(dfpns13$J037)
levels(dfpns19$J037) # dados de 2019 contém "Ignorado" 
dfpns19 |> count(J037) # Não há registros com valor "Ignorado"

# J002 - Deixou de realizar atividades nas 2 últimas semanas por motivo de saúde
levels(dfpns13$J002)
levels(dfpns19$J002) # dados de 2019 contém "Ignorado" 
dfpns19 |> count(J002) # Não há registros com valor "Ignorado"

# J011 / J01101 - Realizaou consulta médica nos últimos 12 meses
levels(dfpns13$J011)   # escala diferente entre 2013 e 2019
levels(dfpns19$J01101) # escala diferente entre 2013 e 2019
svy13 <- 
  svy13 |> 
  srvyr::mutate(
    J011 = case_when(
      J011 == "Nos doze últimos meses" ~ "Sim",
      TRUE ~ "Não"),
    J011 = factor(J011, levels = c("Não", "Sim")))
svy19 <- 
  svy19 |> 
  srvyr::mutate(
    J011 = case_when( # nova var similar a PNS 13
      J01101 == "Até 1 ano" ~ "Sim",
      TRUE ~ "Não"),
    J011 = factor(J011, levels = c("Não", "Sim")))

# Sexo
levels(dfpns13$C006) #  "Masculino" "Feminino" 
levels(dfpns19$C006) #  "Homem"  "Mulher"

# UF
levels(dfpns13$V0001)
levels(dfpns19$V0001)

# I001 - Possui plano de saúde
levels(dfpns13$I001)
levels(dfpns19$I00101)
dfpns19 |> count(I00101) 
svy13 <- 
  svy13 |> 
  srvyr::mutate(I001 = relevel(I001, ref = "Sim"))
svy19 <- 
  svy19 |> 
  srvyr::mutate(
    I00101 = factor(I00101, levels = c("Sim", "Não")),
    I00101 = relevel(I00101, ref = "Sim"))
# svy19 |> survey_count(I00101)
# svy13 |> survey_count(I001)

# VDD004A - Grau de instrução
## Agrupa Grau de Instrução ---- 
levels(dfpns13$VDD004A)
levels(dfpns19$VDD004A)
svy13 <- 
  svy13 |>  
  srvyr::mutate(instrucao = case_when(
    VDD004A == "Fundamental incompleto ou equivalente" ~ "Sem Instrução ou Fundamental incompleto",
    VDD004A == "Fundamental completo ou equivalente" ~ "Fundamental ou Médio Incompleto",
    VDD004A == "Médio incompleto ou equivalente" ~ "Fundamental ou Médio Incompleto",
    VDD004A == "Médio completo ou equivalente" ~ "Médio ou Superior Incompleto",
    VDD004A == "Superior incompleto ou equivalente" ~ "Médio ou Superior Incompleto",
    VDD004A == "Superior completo" ~ "Superior Completo",
    TRUE ~ "Sem Instrução ou Fundamental incompleto"
  ),
  instrucao = factor(instrucao, levels = c("Superior Completo",
                                           "Médio ou Superior Incompleto",
                                           "Fundamental ou Médio Incompleto",
                                           "Sem Instrução ou Fundamental incompleto")))
svy19 <- 
  svy19 |>  
  srvyr::mutate(instrucao = case_when(
    VDD004A == "Fundamental incompleto ou equivalente" ~ "Sem Instrução ou Fundamental incompleto",
    VDD004A == "Fundamental completo ou equivalente" ~ "Fundamental ou Médio Incompleto",
    VDD004A == "Médio incompleto ou equivalente" ~ "Fundamental ou Médio Incompleto",
    VDD004A == "Médio completo ou equivalente" ~ "Médio ou Superior Incompleto",
    VDD004A == "Superior incompleto ou equivalente" ~ "Médio ou Superior Incompleto",
    VDD004A == "Superior completo" ~ "Superior Completo",
    TRUE ~ "Sem Instrução ou Fundamental incompleto"
  ),
  instrucao = factor(instrucao, levels = c("Superior Completo",
                                           "Médio ou Superior Incompleto",
                                           "Fundamental ou Médio Incompleto",
                                           "Sem Instrução ou Fundamental incompleto")))

# C008 - Idade
## Agrupa idade ----
# svy13 <- 
#   svy13 |>  
#   srvyr::mutate(ageGroup = cut(C008,
#                                c(0, 25, 49, 74, 200),
#                                labels = c("g0-24", "g25-49",
#                                           "g50-74", "g75-older")))
# svy19 <- 
#   svy19 |> 
#   srvyr::mutate(ageGroup = cut(C008,
#                                c(0, 25, 49, 74, 200),
#                                labels = c("g0-24", "g25-49",
#                                           "g50-74", "g75-older")))

# V0001
## Agrupa por região ----
svy13 <-
  svy13 |>
  srvyr::mutate(regiao = case_when(
    V0001 == "Rondônia" ~ "Norte",
    V0001 == "Acre"     ~ "Norte",
    V0001 == "Amazonas" ~ "Norte",
    V0001 == "Roraima"  ~ "Norte",
    V0001 == "Pará"     ~ "Norte",
    V0001 == "Amapá"    ~ "Norte",
    V0001 == "Tocantins"~ "Norte",
    V0001 == "Maranhão" ~ "Nordeste",
    V0001 == "Piauí"    ~ "Nordeste",
    V0001 == "Ceará"    ~ "Nordeste",
    V0001 == "Rio Grande do Norte" ~ "Nordeste",
    V0001 == "Paraíba"  ~ "Nordeste",
    V0001 == "Pernambuco"~ "Nordeste",
    V0001 == "Alagoas"  ~ "Nordeste",
    V0001 == "Sergipe"  ~ "Nordeste",
    V0001 == "Bahia"    ~ "Nordeste",
    V0001 == "Minas Gerais" ~ "Sudeste",
    V0001 == "Espírito Santo"~ "Sudeste",
    V0001 == "Rio de Janeiro"~ "Sudeste",
    V0001 == "São Paulo"     ~ "Sudeste",
    V0001 == "Paraná"        ~ "Sul",
    V0001 == "Santa Catarina"~ "Sul",
    V0001 == "Rio Grande do Sul"~ "Sul",
    V0001 == "Mato Grosso do Sul" ~ "Centro-Oeste",
    V0001 == "Mato Grosso"        ~ "Centro-Oeste",
    V0001 == "Goiás"              ~ "Centro-Oeste",
    V0001 == "Distrito Federal"   ~ "Centro-Oeste",
    TRUE ~ NA_character_))
svy19 <-
  svy19 |>
  srvyr::mutate(regiao = case_when(
    V0001 == "Rondônia" ~ "Norte",
    V0001 == "Acre"     ~ "Norte",
    V0001 == "Amazonas" ~ "Norte",
    V0001 == "Roraima"  ~ "Norte",
    V0001 == "Pará"     ~ "Norte",
    V0001 == "Amapá"    ~ "Norte",
    V0001 == "Tocantins"~ "Norte",
    V0001 == "Maranhão" ~ "Nordeste",
    V0001 == "Piauí"    ~ "Nordeste",
    V0001 == "Ceará"    ~ "Nordeste",
    V0001 == "Rio Grande do Norte" ~ "Nordeste",
    V0001 == "Paraíba"  ~ "Nordeste",
    V0001 == "Pernambuco"~ "Nordeste",
    V0001 == "Alagoas"  ~ "Nordeste",
    V0001 == "Sergipe"  ~ "Nordeste",
    V0001 == "Bahia"    ~ "Nordeste",
    V0001 == "Minas Gerais" ~ "Sudeste",
    V0001 == "Espírito Santo"~ "Sudeste",
    V0001 == "Rio de Janeiro"~ "Sudeste",
    V0001 == "São Paulo"     ~ "Sudeste",
    V0001 == "Paraná"        ~ "Sul",
    V0001 == "Santa Catarina"~ "Sul",
    V0001 == "Rio Grande do Sul"~ "Sul",
    V0001 == "Mato Grosso do Sul" ~ "Centro-Oeste",
    V0001 == "Mato Grosso"        ~ "Centro-Oeste",
    V0001 == "Goiás"              ~ "Centro-Oeste",
    V0001 == "Distrito Federal"   ~ "Centro-Oeste",
    TRUE ~ NA_character_))



