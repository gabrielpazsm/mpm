# 12/05: Introdução à Manipulação de Dados no R, Parte 1 (Gabriel Paz)

Ao final, espera-se:

1. Navegar pelo RStudio com desenvoltura.
2. Entender e usar projetos (.Rproj).
3. Importar dados diretamente do GitHub.
4. Instalar e carregar pacotes.
5. Inspecionar um data frame: view(), glimpse(), skimr::skim().
6. Operar linhas: filter(), arrange(), slice().
7. Operar colunas: select(), mutate(), rename().
8. Encadear transformações com o operador pipe (técnica dos "vários-mutate").

## Recomendações de leitura pontuais
R for Data Science (R4DS, 2ª ed.) de Hadley Wickham, que está gratuito online em https://r4ds.hadley.nz, especificamente: 

0. "Envio de dados” do guia da Academia Científica
https://guiaac.quarto.pub/guia-ac/enviar-dados.html

1. R4DS, capítulo "Data visualization", apenas a seção Prerequisites
Instalar R, RStudio e o tidyverse.
https://r4ds.hadley.nz/data-visualize

2. R4DS, cap. "Workflow: scripts and projects" 
Por que .Rproj
https://r4ds.hadley.nz/workflow-scripts

3. Wickham H. "Tidy Data". Journal of Statistical Software, 2014 
Leia as seções 1 e 2 (≈4 páginas), sobre filosofia tidy data (conceito mais importante do curso)
https://www.jstatsoft.org/article/view/v059i10

4. R4DS, cap. "Data tidying", só a seção inicial "Tidy data"
https://r4ds.hadley.nz/data-tidy

5. R4DS, cap. "Data transformation"
É o capítulo principal da aula. 
https://r4ds.hadley.nz/data-transform

## Se você vai ler só uma coisa
Leia o capítulo "Data transformation" do R4DS. Sozinho, ele cobre cerca de 70% do que será discutido na aula.

## Dados utilizados
O banco da aula é uma versão simulada e intencionalmente "suja" do estudo SPRINT (hipertensão, N Engl J Med 2015).