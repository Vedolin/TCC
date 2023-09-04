# Instalação e Carregamento dos Pacotes Necessários  -----------

pacotes <- c("tidyverse","sf","lwgeom","tmap","rgdal","rgeos","adehabitatHR","knitr",
             "kableExtra","microdatasus","plotKML","DescTools", "readODS")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


# # Carregando um shapefile das regiões imediatas:
regiao_imediata <- readOGR(dsn = "./bases/regioes_imediatas/RG2017_rgi_20180911",
                    layer = "RG2017_rgi",
                    encoding = "UTF-8",
                    use_iconv = TRUE)

# tmap_options(check.and.fix = TRUE)
# 
# Visualizando o shapefile carregado:
# tmap_mode("view")
# 
# tm_shape(shp = regiao_imediata) +
#   tm_borders() +
#   # tm_dots(col = "#39568CFF", size = 0.08, popup.vars = c("nome_rgi"))
#   tm_dots(col = "#39568CFF", size = 0.08, id="nome_rgi") # pontos nos centroides


composicao_por_municipios <- read_ods("./bases/regioes_imediatas/regioes_geograficas_composicao_por_municipios_2017_20180911.ods")

composicao_por_municipios <- composicao_por_municipios %>% 
  mutate(code_muni = as.numeric(substr(CD_GEOCODI,1,6))) %>% 
  dplyr::select(-CD_GEOCODI, -cod_rgint, -nome_rgint, -nome_mun)


# Carrega população dos municípios
source("./populacao_municipios.R")

regiao_imediata_municipio_populacao <- composicao_por_municipios %>% inner_join(populacao_municipios, by = "code_muni")

populacao_regiao_imediata <- regiao_imediata_municipio_populacao %>%
  group_by(cod_rgi) %>%
  summarize(
    populacao_rgi = sum(populacao_municipio),
    .groups = 'drop'
  )


# write_parquet(
#   composicao_por_municipios,
#   "./composicao_por_municipios.parquet",
#   chunk_size = NULL,
#   version = "2.4",
#   compression = "snappy",
#   use_deprecated_int96_timestamps = FALSE,
#   coerce_timestamps = NULL,
#   allow_truncated_timestamps = FALSE
# )
# 
# composicao_por_municipios <- NULL
# 
# composicao_por_municipios <- read_parquet(file = "./composicao_por_municipios.parquet")
# 
# 
# 
# 
# write_parquet(
#   populacao_regiao_imediata,
#   "./populacao_regiao_imediata.parquet",
#   chunk_size = NULL,
#   version = "2.4",
#   compression = "snappy",
#   use_deprecated_int96_timestamps = FALSE,
#   coerce_timestamps = NULL,
#   allow_truncated_timestamps = FALSE
# )
# 
# populacao_regiao_imediata <- NULL
# 
# populacao_regiao_imediata <- read_parquet(file = "./populacao_regiao_imediata.parquet")
# 
# 
# 
# 
# 
# 
# write_parquet(
#   populacao_municipios,
#   "./populacao_municipios.parquet",
#   chunk_size = NULL,
#   version = "2.4",
#   compression = "snappy",
#   use_deprecated_int96_timestamps = FALSE,
#   coerce_timestamps = NULL,
#   allow_truncated_timestamps = FALSE
# )
# 
# populacao_municipios <- NULL
# 
# populacao_municipios <- read_parquet(file = "./populacao_municipios.parquet")





