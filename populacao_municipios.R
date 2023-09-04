# Instalação e Carregamento dos Pacotes Necessários  -----------

pacotes <- c("readxl", "dplyr")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}



populacao_municipios <- read_xls("./bases/municipios/populacao_municipios_2021.xls", col_types = c("numeric","text","numeric"))
populacao_municipios$code_muni <- as.numeric(substr(populacao_municipios$code_muni,1,6))
populacao_municipios <- populacao_municipios %>%  dplyr::select("code_muni","populacao") %>% rename(populacao_municipio = populacao)

