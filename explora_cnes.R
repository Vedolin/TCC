# Explora CNES

# Instalação e Carregamento dos Pacotes Necessários  -----------

pacotes <- c("tidyverse","sf","lwgeom","tmap","rgdal","rgeos","adehabitatHR","knitr",
             "kableExtra","microdatasus","plotKML","DescTools")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Não vi a regional direito com o fetch_datasus...
# Talvez no arquivo original apareça
# ftp.datasus.gov.br/cnes


# # Carregando CNES - Cadastro Nacional de Estabelecimentos de Saúde
# # O pacote microdatasus permite filtrar por UF
# # Vamos baixar os CNES dos eSTabelecimentos (e não de pessoas físicas)
# 
# df <- fetch_datasus(year_start = 2022, month_start = 5,
#                     year_end = 2022, month_end = 5,
#                     uf = "RJ",
#                     information_system = "CNES-ST")
# 
# # Grava o objeto em arquivo local para evitar downloads repetidos
# save(df, file = 'cnes_rj.RData')
load('cnes_rj.RData')

codigo_municipio_nova_friburgo <- "330340"
codigos_municipios <- c(codigo_municipio_nova_friburgo)

# df_nova_friburgo <- df %>% dplyr::filter(CODUFMUN %in% codigos_municipios)
df_nova_friburgo <- df

cnes <- process_cnes(df_nova_friburgo, information_system = "CNES-ST" )
# cnes$longitude <- as.numeric(cnes$munResLon)

sf_cnes <- st_as_sf(x = cnes,
                    coords = c("munResLon","munResLat"),
                    crs = 4326)

rm(df)
rm(df_nova_friburgo)
# rm(cnes)

class(sf_cnes)
sf_cnes$geometry

# Plotando o objeto sf_cnes de forma espacial:
tm_shape(shp = sf_cnes) +
  # tm_dots(size = 1) + 
tm_dots(size = 0.08)


# # Carregando um shapefile de RJ:
shape_rj <- readOGR(dsn = "./shapes/RJ_Municipios_2021",
                    layer = "RJ_Municipios_2021",
                    encoding = "UTF-8",
                    use_iconv = TRUE)

# O código do município tem um dígito a mais no shape_rj (especulo que seja de autoconferência)
# Vou criar uma coluna sem esse último dígito para poder fazer os joins subsequentes

shape_rj@data <- shape_rj@data %>%  mutate(CODUFMUN = substr(CD_MUN,1,6)) 

# Inserindo a base de dados cnes ao nosso shapefile:
# shape_rj@data %>%
#   left_join(cnes, by = "CODUFMUN") -> shape_rj@data


# Inserindo a base de dados de vacinas aplicadas por municipio:
shape_rj@data %>%
  left_join(doses_aplicadas_por_municipio, by = "CODUFMUN") -> shape_rj@data

shape_rj %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = TRUE,
                font_size = 12)


# Observando os centroides dos municípios do RJ:
tm_shape(shp = shape_rj) +
  tm_dots(col = "#39568CFF", size = 0.08) 

tm_shape(shp = sf_cnes) +
  # tm_dots(size = 1) + 
  tm_dots(size = 0.08) +
tm_shape(shp = shape_rj) +
  tm_dots(col = "#39568CFF", size = 0.08) 


# Visualizando o shapefile carregado:
tmap_mode("view")

tm_shape(shp = shape_rj) +
  tm_borders() +
  # tm_dots(col = "#39568CFF", size = 0.08, popup.vars = c("NM_MUN"))
  tm_dots(col = "#39568CFF", size = 0.08, id="NM_MUN")


tmap_mode("plot")


# 
# cnes %>%
#   ggplot() +
#   geom_(aes(x = munResLon, y = munResLat),
#                color = "black") +
#   labs(x = "Longitude",
#        y = "Latitude",
#        color = "IDH") +
#   scale_fill_viridis_c() +
#   theme_bw()
# # 
# 
# 
# class(cnes)
# glimpse(cnes)
# summary(cnes)
# 
# ggplot(cnes)
# 
# 
# 
# 
# 
# 
# 

# 
# shape_rj %>% 
#   kable() %>%
#   kable_styling(bootstrap_options = "striped", 
#                 full_width = TRUE, 
#                 font_size = 12)
