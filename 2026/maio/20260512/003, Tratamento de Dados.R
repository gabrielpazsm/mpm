install.packages("rio")





# 1 - Setup de Pacotes --------------------------------------------------------

library(rio)         # importação universal (csv, xlsx, dta, sav...)
library(tidyverse)   # dplyr, ggplot2, tibble, tidyr, readr...
library(skimr)  


# 2 - Importar o banco --------------------------------------------------------

sprint_bruto <- import("data/sprint_simulado.xlsx")

names(sprint_bruto)
view(sprint_bruto)

view(sprint_bruto[1:23])

sprint_bruto_corrigido <- sprint_bruto[1:23] ##### NUNCA FAZER!!! Dar o mesmo nome pra coisas diferentes

# Renomear as colunas
sprint_simulado <- rename(
  sprint_bruto_corrigido,
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
)


# 3 - Ectoscopia ------------------------------------------------------------

view(sprint_simulado)
glimpse(sprint_simulado)
skim(sprint_simulado)

as.numeric(sprint_simulado$idade)

is.na("NA")


count(sprint_simulado, tabagismo)


# 4 - Operando Linhas -----------------------------------------------------

paciente_idosos <- dplyr::filter(sprint_simulado, parse_number(idade) >= 75 | grupo_tratamento == "Intensivo")

paciente_idosos <- dplyr::filter(sprint_simulado, tabagismo %in% c("Ativo", "Ex", "Sim", "parou"))

# arrange
banco_ordenado_idade <- arrange(sprint_simulado, desc(idade), grupo_tratamento)


slice_sample(sprint_simulado, n = 20)


# 5 - Operando Colunas ----------------------------------------------------

# select
sprint_demograficos <- select(sprint_simulado, 
                              idade, sexo)

sprint_select <- select(
  sprint_simulado,
  contains("colesterol") ### Tente usar coisas literais e não modificáveis. Colunas de colesterol não vão deixar de ter colesterol. Ser 16 ou 18 é completamente modificável
)

head(sprint_select)

head(sprint_simulado[16:18])

# mutate
sprint_com_pam <- mutate(
  sprint_simulado,
  pam_basal = (pa_sist_basal + 2*pa_diast_basal)/3,
  idoso = idade >= 75
)



select(sprint_com_pam, idoso)


# 6 - Operador pipe -------------------------------------------------------

# resultado <- slice_head(
#   arrange(
#     select( 
#       filter(
#         sprint_simulado, idade > 60
#       ),
#       idade, sexo, pa_sist_basal
#     ), desc(pa_sist_basal)
#   ), n = 10
# )

resultado <- sprint_simulado %>% 
  filter(idade > 60) %>% 
  select(idade, sexo, pa_sist_basal) %>% 
  arrange(desc(pa_sist_basal)) %>% 
  slice_head(n=10)


# 7 - Fechamento ----------------------------------------------------------
sprint_simulado <- rename(
  sprint_bruto_corrigido,
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
) %>% 
  mutate(
    idade_num = parse_number(idade),
    pam_basal = (pa_sist_basal + 2*pa_diast_basal)/3
  ) %>% 
  select(
    -pa_categoria_planilha
  ) ### Deixe o select para o final!


# 75 anos, nunca fomou, maior IMC, usando pipe



