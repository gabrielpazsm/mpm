rm(list = ls())


# Setup de pacotes --------------------------------------------------------

library(rio)
library(tidyverse)
library(skimr)

sprint_bruto <- import("data/sprint_simulado.xlsx")

sprint_simulado <- sprint_bruto |>  
  rename(
  id_paciente = IDENTIFICAÇÃO,
  idade = AGE,
  sexo = SEXO,
  grupo_tratamento = GRP,
  tabagismo = TABAG,
  peso = PESO,
  altura = HEIGHT,
  imc = `INDICE DE MASSA CORPORAL`,
  pa_sist_basal = `Pressão BASAL sistólica`,
  pa_diast_basal = `Diastólica Basal`,
  pa_sist_6meses = `Pressão sistólica 6 meses`,
  pa_diast_6meses = `Diastólica 6 MESES`,
  lab_creatinina = `Lab creatinina`,
  lab_glicemia = `Laboratório - Glicemia`,
  lab_hba1c = `HbA1c laboratório`,
  lab_colesterol_total = `Colesterol total - lab`,
  lab_colesterol_ldl = `LDL colesterol`,
  lab_colesterol_hdl = `HDL colesterol`,
  data_randomizacao = `Randomizou quando?`,
  comorbidades = Comorbidades,
  uso_estatina = `Usa estatina?`,
  numero_antihipertensivos = `Quantos anti-hipertensivos?`,
  pa_categoria_planilha = `PA categoria planilha`
) |> 
  mutate(
    idade_num = parse_number(idade),
    pam_basal = (pa_sist_basal + 2*pa_diast_basal)/3
  ) |> 
  select(
    -`...24`,
    -`...25`,
    -`...26`
  )



# 2. Strings --------------------------------------------------------------

glimpse(sprint_simulado)

str_squish()
is.na("NA")

str_lower()



sprint_simulado_strings <- sprint_simulado |> 
  mutate(
    across(
      where(is.character),
      ~na_if(str_squish(.x), "NA") ## função anônima
    ), 
    sexo_txt = str_to_lower(sexo), 
    grupo_txt = str_to_lower(grupo_tratamento), 
    tabagismo_txt = str_to_lower(tabagismo), 
    comorbidades_txt = str_to_lower(comorbidades)
)

count(sprint_simulado_strings, sexo, sort = TRUE)
count(sprint_simulado_strings, sexo_txt, sort = TRUE)




# 3. Regular Expressions --------------------------------------------------

variantes_para_ia <- list(
  sexo_txt = unique(sprint_simulado_strings$sexo_txt),
  grupo_txt = unique(sprint_simulado_strings$grupo_txt),
  tabagismo_txt = unique(sprint_simulado_strings$tabagismo_txt),
  comorbidades_txt = unique(sprint_simulado_strings$comorbidades_txt)
)


### CÓDIGO DO GPT
sprint_simulado_regex <- sprint_simulado_strings |>
  mutate(
    
    sexo = case_when(
      str_detect(sexo_txt, "^(f|fem|feminino)$") ~ "Feminino",
      str_detect(sexo_txt, "^(m|masc|masculino)$") ~ "Masculino",
      .default = "REVISAR"
    ),
    
    grupo_tratamento = case_when(
      str_detect(grupo_txt, "^(int|intensivo)$") ~ "Intensivo",
      str_detect(grupo_txt, "^(std|standard|padrao|padrão)$") ~ "Padrão",
      .default = "REVISAR"
    ),
    
    tabagismo = case_when(
      str_detect(tabagismo_txt, "^(sim|ativo|fuma)$") ~ "Atual",
      str_detect(tabagismo_txt, "^(ex|ex-fumante|parou)$") ~ "Ex-tabagista",
      str_detect(tabagismo_txt, "^(nao|não|nunca)$") ~ "Nunca",
      is.na(tabagismo_txt) ~ NA_character_,
      .default = "REVISAR"
    ),
    
    comorbidades_padrao = case_when(
      str_detect(comorbidades_txt, "sem|nega|nada|nenhuma") ~ "Sem comorbidades",
      is.na(comorbidades_txt) ~ NA_character_,
      .default = comorbidades_txt
    ),
    
    has = case_when(
      str_detect(comorbidades_txt, "has|hipertens|hipertenso|pressão alta|pressao alta") ~ TRUE,
      str_detect(comorbidades_txt, "sem|nega|nada|nenhuma") ~ FALSE,
      is.na(comorbidades_txt) ~ NA,
      .default = FALSE
    ),
    
    dm = case_when(
      str_detect(comorbidades_txt, "dm2?|diabet") ~ TRUE,
      str_detect(comorbidades_txt, "sem|nega|nada|nenhuma") ~ FALSE,
      is.na(comorbidades_txt) ~ NA,
      .default = FALSE
    ),
    
    dac = case_when(
      str_detect(comorbidades_txt, "dac|doença coronariana|doenca coronariana|coronariopatia") ~ TRUE,
      str_detect(comorbidades_txt, "sem|nega|nada|nenhuma") ~ FALSE,
      is.na(comorbidades_txt) ~ NA,
      .default = FALSE
    ),
    
    iam_previo = case_when(
      str_detect(comorbidades_txt, "iam|infarto") ~ TRUE,
      str_detect(comorbidades_txt, "sem|nega|nada|nenhuma") ~ FALSE,
      is.na(comorbidades_txt) ~ NA,
      .default = FALSE
    ),
    
    avc_previo = case_when(
      str_detect(comorbidades_txt, "avc|derrame|acidente vascular cerebral") ~ TRUE,
      str_detect(comorbidades_txt, "sem|nega|nada|nenhuma") ~ FALSE,
      is.na(comorbidades_txt) ~ NA,
      .default = FALSE
    ),
    
    revisar_comorbidades = case_when(
      is.na(comorbidades_txt) ~ NA_character_,
      str_detect(
        comorbidades_txt,
        "has|hipertens|hipertenso|pressão alta|pressao alta|dm2?|diabet|dac|doença coronariana|doenca coronariana|coronariopatia|iam|infarto|avc|derrame|acidente vascular cerebral|sem|nega|nada|nenhuma"
      ) ~ "OK",
      .default = "REVISAR"
    )
  )


# 4. Critério de exclusão -------------------------------------------------


sprint_pos_exclusao <- sprint_simulado_regex |> 
  filter(
    dm == FALSE, avc_previo == FALSE
  )



# 5. Factors  -------------------------------------------------------------

sprint_pos_exclusao <- sprint_pos_exclusao |> 
  mutate(
    sexo_limpo = factor(
      sexo,
      levels = c("Masculino", "Feminino")
    ), 
    tabagismo_limpo = factor(
      tabagismo,
      levels = c("Nunca", "Ex-fumante", "Ativo")
    )
  )



# 6. Datas ----------------------------------------------------------------

sprint_pos_exclusao$data_randomizacao

sprint_datas <- sprint_pos_exclusao |> 
  mutate(
    data_randomizacao_limpa = parse_date_time(
      data_randomizacao,
      orders = c("ymd", "dmy", "mdy", "b d, Y", "d b Y")
    ) |>
      as_date()
  )

sprint_datas |> select(data_randomizacao, data_randomizacao_limpa)

class(sprint_datas$data_randomizacao_limpa)


# 7. Missing data ---------------------------------------------------------

install.packages("naniar")
install.packages("visdat")
library(naniar)
library(visdat)
vis_miss(sprint_datas)
vis_dat(sprint_simulado)
gg_miss_upset(sprint_datas)

view(sprint_simulado)

sprint_missing <- sprint_datas |> 
  mutate(
    pa_sist_basal_limpo = if_else(
      between(pa_sist_basal, 50, 220),
      pa_sist_basal,
      9999 #" REVISAR!!!! "
    ),
    pa_diast_basal_limpo = if_else(
      between(pa_diast_basal, 40, 150),
      pa_diast_basal,
      9999 #" REVISAR!!!! "
    ),
    pa_sist_6meses_limpo = if_else(
      between(pa_sist_6meses, 50, 220),
      pa_sist_6meses,
      9999 #" REVISAR!!!! "
    ),
    pa_diast_6meses_limpo = if_else(
      between(pa_diast_6meses, 40, 150),
      pa_diast_6meses,
      9999 #" REVISAR!!!! "
    ),
    imc_limpo = if_else(
      between(imc, 12, 70),
      imc,
      9999
    )
  )

# checar inconsistências
sprint_missing |> 
  filter(
    if_any(
      ends_with("limpo"),
      ~ .x == 9999
    )
  ) |> 
  select(
    pa_sist_basal, pa_sist_basal_limpo,
    pa_diast_basal, pa_diast_basal_limpo,
    pa_sist_6meses, pa_sist_6meses_limpo,
    pa_diast_6meses, pa_diast_6meses_limpo,
    imc, imc_limpo
  ) |>  view()


### 1400 !!!!!!



# 8. Join -----------------------------------------------------------------

followup_bruto <- import("data/sprint_desfechos_followup.xlsx")

names(followup_bruto)
glimpse(followup_bruto)

followup_limpo <- followup_bruto |>
  rename(
    id_paciente = `ID paciente`,
    data_ultimo_contato = `Último contato`,
    tempo_seguimento_meses = `Seguimento meses`,
    evento_cv = `Evento cardiovascular?`,
    tipo_evento_cv = `Tipo evento CV`,
    obito = `Óbito?`,
    perda_seguimento = `Perda seguimento?`
  ) |>
  mutate(
    across(
      where(is.character),
      ~ na_if(str_squish(.x), "NA") ### VEJA COMO SE REPETEM
    ),
    
    evento_cv_txt = str_to_lower(str_squish(as.character(evento_cv))),
    obito_txt = str_to_lower(str_squish(as.character(obito))),
    perda_txt = str_to_lower(str_squish(as.character(perda_seguimento))),
    
    evento_cv_limpo = case_when(
      evento_cv_txt %in% c("sim", "s", "1", "evento") ~ TRUE,
      evento_cv_txt %in% c("não", "nao", "n", "0", "sem evento") ~ FALSE,
      is.na(evento_cv_txt) ~ NA,
      .default = NA ### cuidado: "yes" >> ver o unique
    ),
    
    obito_limpo = case_when(
      obito_txt %in% c("sim", "s", "1") ~ TRUE,
      obito_txt %in% c("não", "nao", "n", "0") ~ FALSE,
      is.na(obito_txt) ~ NA,
      .default = NA
    ),
    
    perda_seguimento_limpo = case_when(
      perda_txt %in% c("sim", "s", "1", "perdeu") ~ TRUE,
      perda_txt %in% c("não", "nao", "n", "0") ~ FALSE,
      is.na(perda_txt) ~ NA,
      .default = NA
    ),
    
    tipo_evento_cv_limpo = case_when(
      evento_cv_limpo == FALSE ~ "Nenhum",
      evento_cv_limpo == TRUE ~ str_squish(as.character(tipo_evento_cv)),
      is.na(evento_cv_limpo) ~ NA_character_,
      .default = NA
    ),
    
    data_ultimo_contato_limpo = parse_date_time(
      as.character(data_ultimo_contato),
      orders = c("ymd", "dmy", "mdy")
    ) |>
      as_date(),
    
    tempo_seguimento_meses = parse_number(
      as.character(tempo_seguimento_meses)
    )
  ) 

view(followup_bruto)

followup_limpo |> 
  filter(
    !is.na(evento_cv_txt),
    is.na(evento_cv_limpo)
  ) |> 
  count(evento_cv_txt, sort = TRUE)

followup_limpo |> 
  filter(
    !is.na(obito_txt),
    is.na(obito_limpo)
  ) |> 
  count(obito_txt, sort = TRUE)
    

ids_duplicados_followup <- followup_limpo |> 
  count(id_paciente) |> 
  filter(n > 1)

sprint_com_followup <- sprint_missing |> 
  left_join(followup_limpo, by = "id_paciente")


# 9. BANCO FINAL LIMPO AGORA VAI  ---------------------------------------------------------------------

view(sprint_com_followup)


# 10. pivot longer --------------------------------------------------------

sprint_pa_long <- sprint_com_followup |> 
  select(
    id_paciente, 
    grupo_tratamento,
    pa_sist_basal,
    pa_diast_basal,
    pa_sist_6meses,
    pa_diast_6meses
  ) |> 
  pivot_longer(
    cols = starts_with("pa_"),
    names_to = c("medida", "momento"),
    names_pattern = "pa_(sist|diast)_(basal|6meses)",
    values_to = "valor"
  )

head(sprint_pa_long)



# 11. Pivot Wider ---------------------------------------------------------

sprint_pa_long |> 
  summarise(
    media = mean(valor, na.rm = TRUE), 
    dp = sd(valor, na.rm = TRUE),
    .by = c(grupo_tratamento, medida, momento)
  ) |> 
  mutate(
    media_dp = paste0(round(media, 1), " (", round(dp, 1), " )") 
  ) |> 
  select(grupo_tratamento, medida, momento, media_dp) |>
  pivot_wider(
    names_from = momento,
    values_from = media_dp
  )
