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


# # Carregando um shapefile das regiões de saúde:
regiao_saude <- readOGR(dsn = "./bases/regiao_saude/regiao_saude_shp",
                    layer = "regiao_saude",
                    encoding = "UTF-8",
                    use_iconv = TRUE)

# Observando os centroides das regiões de saúde:
tm_shape(shp = regiao_saude) +
  tm_dots(col = "#39568CFF", size = 0.08) 


# Visualizando o shapefile carregado:
tmap_mode("view")

tm_shape(shp = regiao_saude) +
  tm_borders() +
  # tm_dots(col = "#39568CFF", size = 0.08, popup.vars = c("no_colegia"))
  tm_dots(col = "#39568CFF", size = 0.08, id="no_colegia")

