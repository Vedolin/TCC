# Linha 220

system.time( 
  source("./setup_ambiente.R")
)

#####################################################################################
#######                    REGIOES GEOGRAFICAS IMEDIATAS            #################
#####################################################################################

# ARTIGO!!!!
# Motivacao regiao imediata: 
# https://biblioteca.ibge.gov.br/index.php/biblioteca-catalogo?view=detalhes&id=2100600
# https://seer.ufu.br/index.php/hygeia/article/view/63335/35569

# https://www.ibge.gov.br/geociencias/organizacao-do-territorio/divisao-regional/15778-divisoes-regionais-do-brasil.html


system.time(
  
  source("./regiao_imediata.R")
)

system.time(
  
# Cumulativo de pessoas totalmente vacinadas por data por município.
  # covid_vacinas %>% filter(!is.na(people_fully_vaccinated)) %>%  head()
  # date people_fully_vaccinated code_muni
  # 1 2021-01-20                       0    312540
  # 2 2021-01-22                       0    312540
  # 3 2021-01-25                       0    312540
  # 4 2021-02-02                       0    312540
  # 5 2021-02-03                       8    312540
  # 6 2021-02-04                      14    312540
  
  source("./covid19-Datahub.R")
)

# library(explore)

# source("./carrega_ocupacao_srag.R")



# library(covid19br)
# cities <- downloadCovid19("cities")

# Nao estou utilizando a vacinacao por muncipio
# vacinas_municipios <- covid_vacinas %>% 
#   inner_join(composicao_por_municipios, by = join_by(code_muni == code_muni) )

vacinas_regiao_imediata <- covid_vacinas %>% 
  inner_join(composicao_por_municipios, by = join_by(code_muni == code_muni) ) %>% 
  inner_join(populacao_regiao_imediata, by = join_by(cod_rgi == cod_rgi))
  
  
# vacinas_por_regiao_imediata <- vacinas_regiao_imediata %>%
#   group_by(cod_rgi) %>%
#   summarize(
#     populacao_rgi = sum(populacao_municipio),
#     .groups = 'drop'
#   )

vacinas_por_regiao_imediata <- vacinas_regiao_imediata %>%
  group_by(cod_rgi, date, populacao_rgi) %>%
  summarize(
    populacao_totalmente_vacinada = sum(people_fully_vaccinated, na.rm = TRUE),
    .groups = 'drop'
  ) %>% 
  mutate(taxa_populacao_totalmente_vacinada = (populacao_totalmente_vacinada / populacao_rgi) * 100 )


## !!!!!!!!!! Algumas regioes totalizaram taxa de totalmente vacinados acima de 100% 
## Isto pode significar que meus dados têm um defeito latente
indice_regiao_imediata <- regiao_imediata@data

rgis_com_taxa_maior_que_100 <- vacinas_por_regiao_imediata %>%
  filter(taxa_populacao_totalmente_vacinada > 100) %>%
  group_by(cod_rgi) %>%
  summarize(
    taxa_totalmente_vacinada = max(taxa_populacao_totalmente_vacinada, na.rm = TRUE),
    .groups = 'drop'
  ) 

mapa_rgis_com_taxa_maior_que_100 <- regiao_imediata[regiao_imediata@data$rgi %in% rgis_com_taxa_maior_que_100$cod_rgi,]

tmap_mode("view")

mapa_rgis_com_taxa_maior_que_100 %>%
  tm_shape() +
  tm_polygons( id = "nome_rgi") 

      # z@data
      # rgi                    nome_rgi
      # 125 240005                 João Câmara
      # 218 310003  Santa Bárbara - Ouro Preto
      # 241 310026              João Monlevade
      # 250 310035 São João Nepomuceno - Bicas
      # 253 310038        Conselheiro Lafaiete
      # 323 350016                        Tupã
      # 336 350029               Fernandópolis
      # 337 350030             Santa Fé do Sul
      # 365 410005                     Pitanga









# vtemp <- vacinas_por_regiao_imediata
# vtemp$cod_rgi <- factor(vtemp$cod_rgi)
# 
# vtemp <- vtemp %>% 
#   dplyr::select(cod_rgi, date, taxa_populacao_totalmente_vacinada)
# %>% filter(date=="2022-11-29" | date=="2022-11-30") 

# vdata <- spread(data = vtemp, date,taxa_populacao_totalmente_vacinada)
# 
# #####################
# 
# spatial_vacinas_por_regiao_imediata <- regiao_imediata
# spatial_vacinas_por_regiao_imediata$rgi <- as.factor(spatial_vacinas_por_regiao_imediata$rgi)
# spatial_vacinas_por_regiao_imediata@data <- spatial_vacinas_por_regiao_imediata@data %>% 
#   left_join(vdata, by = join_by(rgi == cod_rgi))







spatial_vacinas_por_regiao_imediata <- regiao_imediata

spatial_vacinas_por_regiao_imediata@data$rgi <- as.numeric(spatial_vacinas_por_regiao_imediata@data$rgi)

sf_vacinas <- st_as_sf(x = spatial_vacinas_por_regiao_imediata,
                       crs = 4326)  

amostra_vacinas_por_regiao_imediata <- vacinas_por_regiao_imediata %>%
  filter(substr(cod_rgi,0,2)=="33") %>%
  filter(date >=as.Date("2022-01-01") & date <= as.Date("2022-08-31")) %>% 
  filter(weekdays(date, abbreviate = TRUE) == "Wed") %>%  
  filter(taxa_populacao_totalmente_vacinada > 0.1)

sf_vacinas$rgi <- as.numeric(sf_vacinas$rgi)


##############################################################################
# Agora copiando do exemplo do help tm_animation com World


# sf_vacinas <- sf_vacinas %>%  left_join(amostra_vacinas_por_regiao_imediata, by = join_by(rgi == cod_rgi))

joined_vacinas <-inner_join(sf_vacinas, amostra_vacinas_por_regiao_imediata, by = join_by(rgi == cod_rgi))


# tmap_mode("plot")
# 
# # tm_shape(joined_vacinas, projection = "+proj=eck4", simplify = 0.5) +
# tm_shape(joined_vacinas,  simplify = 0.5) +
#   tm_fill("taxa_populacao_totalmente_vacinada") +
#   # tm_facets(free.scales.symbol.size = FALSE, nrow=1,ncol=1) +
#   tm_facets(by="date") 


m <- tm_shape(joined_vacinas,  simplify = 0.5) +
    tm_fill("taxa_populacao_totalmente_vacinada",
          id = "nome_rgi",
          style = "quantile",
          n = 8,
          # popup.vars = c("rgi","populacao_rgi","populacao_totalmente_vacinada","taxa_populacao_totalmente_vacinada","date"),
          palette = "viridis") +
    tm_facets(along="date") 


tmap_mode("view")

if (FALSE) {
  tmap_animation(m,
                 delay=50,
                 outer.margins = 0,
                 filename = "evolucao_taxa_totalmente_vacinados.mp4")
}

###################   ANIMOU ########################

tm_view(text.size.variable = TRUE)

# tmap_mode("plot")
tmap_mode("view")

joined_vacinas %>%
  filter( date == "2022-06-08") %>%
  tm_shape(simplify = 0.5) +
  # tm_polygons(col = "taxa_populacao_totalmente_vacinada", id = "nome_rgi") +
  tm_polygons(col = "taxa_populacao_totalmente_vacinada",
          id = "nome_rgi",
          # style = "quantile",
          n = 8,
           popup.vars = c("rgi","populacao_rgi","populacao_totalmente_vacinada","taxa_populacao_totalmente_vacinada","date"),
          palette = "viridis"
          ) +
  tm_text("nome_rgi", size = "AREA", root = 5, remove.overlap = FALSE) 


uma_data <- joined_vacinas %>%
  filter( date == "2022-06-08") 


###########################################################################
# Testa exportar o mapa

# install.packages("devtools")
# devtools::install_github("JGCRI/rmap")

z <- as(joined_vacinas, "Spatial")
writeOGR(z, dsn = "/home/administrator/Desktop/Guarda/personal/MBA/aulas/Big Data e Deployment de Modelos II/Arquivos e Scripts 28.04.2023/TCC/exporta_mapa/joined_vacinas",
         layer = "joined_vacinas",
         driver = "ESRI Shapefile" )
"/home/administrator/Desktop/Guarda/personal/MBA/aulas/Big Data e Deployment de Modelos II/Arquivos e Scripts 28.04.2023/TCC/exporta_mapa/joined_vacinas"


writeOGR(joined_vacinas, dsn = "/home/administrator/Desktop/Guarda/personal/MBA/aulas/Big Data e Deployment de Modelos II/Arquivos e Scripts 28.04.2023/TCC/exporta_mapa/joined_vacinas",
         layer = "joined_vacinas",
         driver = "ESRI Shapefile" )
"/home/administrator/Desktop/Guarda/personal/MBA/aulas/Big Data e Deployment de Modelos II/Arquivos e Scripts 28.04.2023/TCC/exporta_mapa/joined_vacinas"

st_write(joined_vacinas, "justD3.shp")

###########################################################################

library(plotly)

# Data: volcano is provided by plotly

# Plot
p <- plot_ly( z = "volcano", type = "surface")
p 




library(plotly)
# fig <- plot_ly(txhousing, x = ~date, y = ~median, z = ~sales)
fig <- plot_ly(joined_vacinas, x = ~date, y = ~median, z = ~taxa_populacao_totalmente_vacinada, type = "surface")
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'date'),
                                   yaxis = list(title = 'median'),
                                   zaxis = list(title = 'sales')))
fig


library(plotly)
filtrado <-  joined_vacinas %>% 
  filter(date >=as.Date("2022-05-01") & date <= as.Date("2022-06-30"))
plot_ly(filtrado, x = ~rgi, y = ~date, z = ~taxa_populacao_totalmente_vacinada)


library(plotly)
# fig <- plot_ly(txhousing, x = ~date, y = ~median, z = ~sales)
fig <- plot_ly(joined_vacinas, x = ~date, y = ~median, z = ~taxa_populacao_totalmente_vacinada, type = "surface")
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'date'),
                                   yaxis = list(title = 'median'),
                                   zaxis = list(title = 'sales')))
fig




m <- tm_shape(joined_vacinas,  simplify = 0.5) +
  tm_fill("taxa_populacao_totalmente_vacinada",
          id = "nome_rgi",
          style = "quantile",
          n = 8,
          # popup.vars = c("rgi","populacao_rgi","populacao_totalmente_vacinada","taxa_populacao_totalmente_vacinada","date"),
          palette = "viridis") +
  tm_facets(along="date") 

tmap_mode("view")

tmap_animation(m, delay=100, outer.margins = 0)

# TA DANDO BOM!!!



tm_shape(sf_vacinas) +
  tm_fill("taxa_populacao_totalmente_vacinada",
          id = "nome_rgi",
          style = "quantile",
          n = 8,
          popup.vars = c("rgi","populacao_rgi","populacao_totalmente_vacinada","taxa_populacao_totalmente_vacinada","date"),
          legend.hist = TRUE,
          palette = "viridis"
  ) +
  tm_borders("black", lwd = .5)


tmap_animation(m2, delay=100, outer.margins = 0)



















library(viridis)
tmap_mode("view")

tmap_options(check.and.fix = TRUE)

tm_shape(sf_vacinas) +
  tm_fill("taxa_populacao_totalmente_vacinada",
          id = "nome_rgi",
          style = "quantile",
          n = 8,
          popup.vars = c("rgi","populacao_rgi","populacao_totalmente_vacinada","taxa_populacao_totalmente_vacinada","date"),
          legend.hist = TRUE,
          palette = "viridis"
  ) +
  tm_borders("black", lwd = .5)


tm_shape(sf_vacinas) +
  tm_polygons() +
  tm_fill("taxa_populacao_totalmente_vacinada",
          id = "nome_rgi",
          style = "quantile",
          n = 8,
          popup.vars = c("cod_rgi","populacao_rgi","populacao_totalmente_vacinada","taxa_populacao_totalmente_vacinada","date"),
          legend.hist = TRUE,
          palette = "viridis"
  )
  





animado <- tm_shape(sf_vacinas) +
  tm_polygons(col = "taxa_populacao_totalmente_vacinada", pal = viridis(10, direction = -1) ) +
  tm_facets(along = "date")

tmap_animation(
  animado, filename = "honey.gif",
  delay = 50)


################# DADNO BOM


################################################################################################
###   ATE AQUI ####################
## Agora plotar mapa


library(viridis)



tmap_options(check.and.fix = TRUE) 

animado <- tm_shape(spatial_vacinas_por_regiao_imediata) +
  tm_polygons("yellow" ) +
  tm_facets(along = "rgi") # ESTE FUNCIONA
# tm_facets(along = "date")
# tm_facets(along = "2022-11-29")

tmap_animation(
  animado, filename = "honey.gif",
  delay = 50)








animado <- tm_shape(spatial_vacinas_por_regiao_imediata) +
  tm_polygons(col = "taxa_populacao_totalmente_vacinada", pal = viridis(10, direction = -1) ) +
  tm_facets(along = "rgi")
  # tm_facets(along = "date")

tmap_animation(
  animado, filename = "honey.gif",
  delay = 50)








spatial_vacinas_por_regiao_imediata@data <- spatial_vacinas_por_regiao_imediata@data %>% 
  left_join(vacinas_por_regiao_imediata, by = join_by(rgi == cod_rgi))


## NAO PLOTAR DIRETO, SE ESTIVER COM VARIAS SEMANAS

fazer_animado <- TRUE

if(fazer_animado) {
  ######################################################
  #                        ANIMADO                     # 
  ######################################################
  
  
  # Create a set of separate maps for each year
  # honey_animation <- tm_shape(honey_sf) +
  #   tm_polygons(
  #     col = "Price_per_lb",
  #     pal = viridis(10, direction = -1)
  #   ) +
  #   tm_facets(along = "year") + # along = "year" instead of by = "year"
  #   tm_layout(legend.position = c("right", "bottom"), legend.text.size = 1)
  # # Save the animated map as a gif file
  # tmap_animation(
  #   honey_animation, filename = "honey.gif",
  #   delay = 50)
  

  
  
      
  spatial_vacinas_por_regiao_imediata <- spatial_vacinas_por_regiao_imediata[spatial_vacinas_por_regiao_imediata@data$date == "2023-03-06" , ]
  
  spatial_vacinas_por_regiao_imediata@data <- spatial_vacinas_por_regiao_imediata@data %>% 
    filter(date == "2023-03-06" | date == "2023-03-07" ) %>% 
    filter(rgi == 110001 )  %>% 
    select(date, taxa_populacao_totalmente_vacinada)

  
  
  library(viridis)
  
  animado <- tm_shape(spatial_vacinas_por_regiao_imediata) +
    tm_polygons(col = "taxa_populacao_totalmente_vacinada", pal = viridis(10, direction = -1) ) +
    tm_facets(along = "date")
  
  tmap_animation(
    animado, filename = "honey.gif",
    delay = 50)
  
  
  
  animado <- tm_shape(spatial_vacinas_por_regiao_imediata)  +
    # tm_fill("taxa_populacao_totalmente_vacinada",
    #           style = "quantile",
    #           n = 8,
    #           legend.hist = TRUE,
    #           palette = "viridis"
    #         ) +
    # tm_layout(legend.outside = TRUE ) +
    # tm_fill("ocorrencias", popup.vars = c("name_muni","ocorrencias")) +
    # tm_borders("black", lwd = .5) +
    # tm_markers() +
    # tm_facets(by = "SEM_ENT_UTI", ncol = 3) +
    tm_facets(along = "date") +
    # tm_layout(legend.outside.size = 0.2)
  # + tm_markers(text="name_muni")
  
  # Lembrar que não basta filtrar o @data, pois a quantidade de poligonos deve bater com a quantidade de linhas de dados
    
    
  tmap_animation(  animado,
                   delay = 20, width = 1200, height = 800,
                   filename = "animado.mp4")
}


spatial_vacinas_por_regiao_imediata@data <- spatial_vacinas_por_regiao_imediata@data %>% 
  filter(date == "2023-03-06")  



tmap_mode("view")
tmap_options(check.and.fix = TRUE) 

spatial_vacinas_por_regiao_imediata %>%
  tm_shape() +
  tm_fill("taxa_populacao_totalmente_vacinada",
          id = "nome_rgi",
          style = "quantile",
          n = 8,
          popup.vars = c("rgi","populacao_rgi","populacao_totalmente_vacinada","taxa_populacao_totalmente_vacinada","date"),
          legend.hist = TRUE,
          palette = "viridis"
  ) +
  tm_borders("black", lwd = .5)

# Maxima taxa
composicao_por_municipios %>%  filter(cod_rgi==(spatial_vacinas_por_regiao_imediata@data %>%  slice_max(taxa_populacao_totalmente_vacinada))$rgi)



################################################################################
#               CARREGA OCUPACAO POR SRAG NO SPARK                             #
################################################################################

# Por exemplo, SRAG de 2021:
# path="./bases/SRAG_2021_a_2023/INFLUD21-01-05-2023.csv"

arquivo = "./bases/SRAG_2021_a_2023/INFLUD21-01-05-2023.csv"
# arquivo = "./bases/SRAG_2021_a_2023/friburgo_INFLUD21-01-05-2023.csv"


ocupacao_carregada <- spark_read_csv(
  sc,
  delimiter = ";",
  name = "OCUPACAO_SRAG",
  path = arquivo,
  header = TRUE,
  infer_schema = TRUE
)

# NO coleta_ocupacao_carregada <- collect(ocupacao_carregada)


# Separa variavel de interesse por municipio e por semana
# Observando Semana de entrada na UTI no estado RJ

# copy_to(sc, ocupacao,name = "ocupacao_spark", overwrite = TRUE)
composicao_por_municipios_spark <- copy_to(
  sc,
  composicao_por_municipios,
  name = "composicao_por_municipios_spark",
  overwrite = TRUE
)
# NO coleta_composicao_por_municipios_spark <- collect(composicao_por_municipios_spark) #So vendo se esta como eu esperava

# ocupacao_por_regiao_imediata = 
ocupacao <- sparklyr::left_join(
  x = ocupacao_carregada,
  y = composicao_por_municipios_spark,
  by = join_by(CO_MUN_RES == code_muni)
)



# Ajusta o formato das colunas de data

ocupacao <- ocupacao %>%
  mutate(
    DT_NOTIFIC = to_date(DT_NOTIFIC, "d/M/y"),
    DT_SIN_PRI = to_date(DT_SIN_PRI, "d/M/y"),
    DT_NASC = to_date(DT_NASC, "d/M/y"),
    DT_UT_DOSE = to_date(DT_UT_DOSE, "d/M/y"),
    DT_VAC_MAE = to_date(DT_VAC_MAE, "d/M/y"),
    DT_DOSEUNI = to_date(DT_DOSEUNI, "d/M/y"),
    DT_1_DOSE = to_date(DT_1_DOSE, "d/M/y"),
    DT_2_DOSE = to_date(DT_2_DOSE, "d/M/y"),
    DT_ANTIVIR = to_date(DT_ANTIVIR, "d/M/y"),
    DT_INTERNA = to_date(DT_INTERNA, "d/M/y"),
    DT_ENTUTI = to_date(DT_ENTUTI, "d/M/y"),
    DT_SAIDUTI = to_date(DT_SAIDUTI, "d/M/y"),
    DT_RAIOX = to_date(DT_RAIOX, "d/M/y"),
    DT_EVOLUCA = to_date(DT_EVOLUCA, "d/M/y"),
    DT_COLETA = to_date(DT_COLETA, "d/M/y"),
    DT_PCR = to_date(DT_PCR, "d/M/y"),
    DT_ENCERRA = to_date(DT_ENCERRA, "d/M/y"),
    DT_DIGITA = to_date(DT_DIGITA, "d/M/y"),
    DT_VGM = to_date(DT_VGM, "d/M/y"),
    DT_TOMO = to_date(DT_TOMO, "d/M/y"),
    DT_RES_AN = to_date(DT_RES_AN, "d/M/y"),
    DT_CO_SOR = to_date(DT_CO_SOR, "d/M/y"),
    DT_RES = to_date(DT_RES, "d/M/y"),
    SEM_ENT_UTI = as.integer(date_format(to_date(DT_ENTUTI, "d/M/y"), "w"))
    # SEM_ENT_UTI = as.week(DT_ENTUTI, format = "d/M/y")
  )  

# NO coleta_ocupacao <- collect(ocupacao)

# NO rj_por_semana  <- coleta_ocupacao %>%
#   filter(SG_UF == 'RJ') %>% 
#   filter(!is.na(SEM_ENT_UTI))

ocupacao <-ocupacao %>% filter(VACINA_COV==1)
  
por_municipio_semanas <- ocupacao %>%
  # filter(SG_UF == 'RJ') %>% 
  filter(!is.na(SEM_ENT_UTI)) %>% 
  group_by(CO_MUN_RES, SEM_ENT_UTI) %>%
  # group_by(CD_GEOCODI, SEM_ENT_UTI) %>%
  summarize(
    ocorrencias = n(),
    .groups = 'drop'
  )
#  NO coleta_por_municipio_semanas <- collect(por_municipio_semanas)

por_regiao_intermediaria_semanas <- ocupacao %>%
  # filter(SG_UF == 'RJ') %>% 
  filter(!is.na(SEM_ENT_UTI)) %>% 
  # mutate(rgi = as.numeric(substr(CD_GEOCODI,1,6))) %>%
  group_by(cod_rgi, SEM_ENT_UTI) %>%
  summarize(
    ocorrencias = n(),
    .groups = 'drop'
  )

por_semana <- ocupacao %>%
  # filter(SG_UF == 'RJ') %>% 
  filter(!is.na(SEM_ENT_UTI)) %>% 
  # mutate(rgi = as.numeric(substr(CD_GEOCODI,1,6))) %>%
  group_by(SEM_ENT_UTI) %>%
  summarize(
    ocorrencias = n(),
    .groups = 'drop'
  )

rsemana <- collect(por_semana)

# por_dia <- ocupacao %>%
#   # filter(SG_UF == 'RJ') %>% 
#   filter(!is.na(DT_ENTUTI)) %>% 
#   # mutate(rgi = as.numeric(substr(CD_GEOCODI,1,6))) %>%
#   group_by(DT_ENTUTI) %>%
#   summarize(
#     ocorrencias = n(),
#     .groups = 'drop'
#   )
# 
# rdia <- collect(por_dia)

coletado_R_semanas <- collect(por_municipio_semanas)
coletado_R_semanas <- coletado_R_semanas %>% inner_join(populacao_municipios, by  = join_by(CO_MUN_RES == code_muni))
coletado_R_semanas <- coletado_R_semanas %>% mutate(taxa = (ocorrencias/populacao_municipio) * 100000)

coletado_R_regiao_intermediaria_semanas <- collect(por_regiao_intermediaria_semanas)
coletado_R_regiao_intermediaria_semanas <- coletado_R_regiao_intermediaria_semanas %>% inner_join(populacao_regiao_imediata, by = "cod_rgi")
coletado_R_regiao_intermediaria_semanas <- coletado_R_regiao_intermediaria_semanas %>% mutate(taxa = (ocorrencias/populacao_rgi) * 100000)

# ATE AQUI ESTA BATENDO 

# Ajusta o nome da coluna de codigo de municipio para facilitar nos joins
# Neste caso usei o municipio de residencia do paciente, pois ele pode ter
# sido internado em outro municipio
# coletado_R_semanas <- coletado_R_semanas %>% rename(code_muni = CO_MUN_RES)
# coletado_R_regiao_intermediaria_semanas <- coletado_R_regiao_intermediaria_semanas %>% rename(rgi = CD_GEOCODI)

coletado_R_semanas <- coletado_R_semanas %>% filter(SEM_ENT_UTI == 29)
coletado_R_regiao_intermediaria_semanas <- coletado_R_regiao_intermediaria_semanas  %>% filter(SEM_ENT_UTI ==29)
################################################################################
#                    BAIXA E PREPARA SHAPE FILES                               #
################################################################################


# municipios <- read_municipality(code_muni = "RJ", year = 2020)
municipios <- read_municipality(year = 2020)
municipios_RJ <- municipios %>%
                 # filter(code_state == 33) %>% # Posso filtrar por uma lista de UFs aqui
                 mutate(code_muni = as.numeric(substr(code_muni,1,6))
                  )
spatial_municipios_RJ  <- as_Spatial(municipios_RJ)

spatial_regiao_imediata  <- regiao_imediata
spatial_regiao_imediata@data$rgi <- as.numeric(spatial_regiao_imediata@data$rgi)









#######################################################################


# tm_shape(regiao_imediata) +
# tm_polygons() +
# # tm_fill(alpha = 0.3) +
# tm_borders("blue", lwd = .5) 
# tm_layout(legend.outside = TRUE ) 
# # tm_layout(legend.outside.size = 0.2)


####################     ATÉ AQUI ROLOU COM A AMOSTRA DE FRIBURGO ######################
####################           E TAMBÉM COM A OCUPACAO DE 2021    ######################

######################################################
#                        ANIMADO                     # 
######################################################

# 
# animado <- tm_shape(spatial_regiao_imediata)  +
#   tm_fill("ocorrencias",
#             style = "quantile",
#             n = 8,
#             legend.hist = TRUE,
#             palette = "viridis"
#           ) +
#   tm_layout(legend.outside = TRUE ) +
#   # tm_fill("ocorrencias", popup.vars = c("name_muni","ocorrencias")) +
#   tm_borders("black", lwd = .5) +
#   tm_markers() +
#   # tm_facets(by = "SEM_ENT_UTI", ncol = 3) +
#   tm_facets(along = "SEM_ENT_UTI") +
#   tm_layout(legend.outside.size = 0.2)
# # + tm_markers(text="name_muni")
# 
# tmap_animation(  animado,
#                  delay = 20, width = 1200, height = 800,
#                  filename = "animado.mp4")














################################################################################
#                            COMBINA OS DADOS                                  #
################################################################################



# Juntando a base de dados ao shapefile do Estado de RJ:

# joined_semanas <- left_join(
#   municipios_RJ,
#   coletado_R_semanas,
#   by= "code_muni"
# )

# coletado_R_semanas <- coletado_R_semanas %>% filter(SEM_ENT_UTI==21)


#######################  AQUI #######################
# spatial_municipios_RJ@data <- spatial_municipios_RJ@data %>% 
#   left_join(coletado_R_semanas, by =  join_by(code_muni == CO_MUN_RES))
# Left join vai dar problemas de NA no teste de Moran

spatial_municipios_RJ@data <- spatial_municipios_RJ@data %>% 
  left_join(coletado_R_semanas, by =  join_by(code_muni == CO_MUN_RES))

# Deu bom
# > spatial_municipios_RJ@data %>% filter(code_muni == 330340)
# code_muni     name_muni code_state abbrev_state     name_state code_region name_region SEM_ENT_UTI ocorrencias
# 1    330340 Nova Friburgo         33           RJ Rio de Janeiro           3     Sudeste          29           2


# Deu bom também
# > spatial_regiao_imediata@data %>% filter(rgi == 330008)
# rgi      nome_rgi SEM_ENT_UTI ocorrencias
# 1 330008 Nova Friburgo          29           2

# Left join vai dar problemas de NA no teste de Moran
# spatial_regiao_imediata@data <- spatial_regiao_imediata@data %>% 
#   left_join(coletado_R_regiao_intermediaria_semanas, by = join_by(rgi == cod_rgi))

spatial_regiao_imediata@data <- spatial_regiao_imediata@data %>% 
  left_join(coletado_R_regiao_intermediaria_semanas, by = join_by(rgi == cod_rgi))

# 
#  
# spatial_municipios_RJ@data <- spatial_municipios_RJ@data %>%
#   left_join(composicao_por_municipios, by = "code_muni")

 
tmap_mode("view")
tmap_options(check.and.fix = TRUE) 
#  AQUI EMBAIXO ESTÁ GERANDO O MAPA CORRETAMENTE (APESAR DE UM WARNING)
# PRECISO AGORA FILTRAR ESSE SPATIAL PARA NAO PLOTAR AS REGIOES DO RESTO DO BRASIL

# spatial_regiao_imediata %>% filter()


## NAO PLOTAR DIRETO, SE ESTIVER COM VARIAS SEMANAS

spatial_regiao_imediata %>%
  tm_shape() +
  tm_fill("taxa",
          id = "nome_rgi",
          style = "quantile",
          n = 8,
          popup.vars = c("rgi","populacao_rgi","ocorrencias","taxa","SEM_ENT_UTI"),
          legend.hist = TRUE,
          palette = "viridis"
  ) +
  tm_borders("black", lwd = .5)

  # tm_shape(regiao_imediata) +
  # tm_polygons() +
  # # tm_fill(alpha = 0.3) +
  # tm_borders("blue", lwd = .5) 
  # tm_layout(legend.outside = TRUE ) 
  # # tm_layout(legend.outside.size = 0.2)


####################     ATÉ AQUI ROLOU COM A AMOSTRA DE FRIBURGO ######################
####################           E TAMBÉM COM A OCUPACAO DE 2021    ######################

######################################################
#                        ANIMADO                     # 
######################################################

# 
# animado <- tm_shape(spatial_regiao_imediata)  +
#   tm_fill("ocorrencias",
#             style = "quantile",
#             n = 8,
#             legend.hist = TRUE,
#             palette = "viridis"
#           ) +
#   tm_layout(legend.outside = TRUE ) +
#   # tm_fill("ocorrencias", popup.vars = c("name_muni","ocorrencias")) +
#   tm_borders("black", lwd = .5) +
#   tm_markers() +
#   # tm_facets(by = "SEM_ENT_UTI", ncol = 3) +
#   tm_facets(along = "SEM_ENT_UTI") +
#   tm_layout(legend.outside.size = 0.2)
# # + tm_markers(text="name_muni")
# 
# tmap_animation(  animado,
#                  delay = 20, width = 1200, height = 800,
#                  filename = "animado.mp4")


# 
# spatial_municipios_RJ %>% 
#   tm_shape() +
#   tm_fill("ocorrencias",
#           id = "name_muni",
#           style = "quantile",
#           n = 8,
#           popup.vars = c("name_muni","ocorrencias","SEM_ENT_UTI"),
#           legend.hist = TRUE,
#           palette = "viridis"
#   ) +
#   tm_borders("black", lwd = .5) 
# tm_shape(regiao_imediata) +
# tm_polygons() +
# # tm_fill(alpha = 0.3) +
# tm_borders("blue", lwd = .5) 
# tm_layout(legend.outside = TRUE ) 
# # tm_layout(legend.outside.size = 0.2)

  # regiao_imediata <- regiao_imediata[(substr(,1,2)==33)]

  # regiao_imediata <- regiao_imediata[(substr(regiao_imediata$rgi,1,2)==33),]

   

  
# composicao_por_municipios


###************************************##############
# Uma olhadela

# tmap_mode("view")
# 
# joined_semanas  %>%  filter(joined_semanas$SEM_ENT_UTI ==20)  %>% 
#   tm_shape() +
#   tm_fill(
#     "ocorrencias",
#           id = "name_muni",
#           style = "quantile",
#           n = 8,
#           popup.vars = c("name_muni","ocorrencias","SEM_ENT_UTI"),
#           legend.hist = TRUE,
#           palette = "viridis"
#   ) +
#   tm_layout(legend.outside = TRUE ) +
#   tm_borders("black", lwd = .5) +
#   tm_layout(legend.outside.size = 0.2)
# 
# tmap_mode("plot")





#####################################################################################
#######                        VIZINHANCA                           #################
#####################################################################################

# Estabelecendo vizinhanças por contiguidade, critério queen:
sf_use_s2(FALSE)

vizinhos_queen <- poly2nb(pl = spatial_regiao_imediata,
                          queen = TRUE,
                          # row.names = spatial_regiao_imediata@data$nome_rgi)
                          row.names = spatial_regiao_imediata@data$rgi
                          )


# Informações relevantes sobre a vizinhança queen estabelecida:
summary(vizinhos_queen)



# Ok! Cadê a matriz W?
matrizW_queen <- nb2mat(neighbours = vizinhos_queen,
                        style = "B")

# Notaram o erro reportado pelo R? O erro diz, explicitamente, que há 
# observações sem uma vizinhança definida. Noutras palavras, há a ocorrência de 
# ilhas:
# NAO É O CASO AQUI...

plot(spatial_regiao_imediata, border = "lightgray")
plot(vizinhos_queen, 
     coordinates(spatial_regiao_imediata), 
     add = TRUE, 
     col = "#33638DFF")

# No caso, a ilha diz respeito ao município de Ilhabela. Para contornar a
# situação, podemos utilizar o argumento zero.policy = TRUE.
matrizW_queen <- nb2mat(neighbours = vizinhos_queen,
                        style = "B",
                        zero.policy = TRUE)

# Para facilitar o estudo da nossa matriz W, podemos comandar:
colnames(matrizW_queen) <- spatial_regiao_imediata@data$nome_rgi
rownames(matrizW_queen) <- spatial_regiao_imediata@data$nome_rgi

View(matrizW_queen)

# PONTO ESTAVEL

########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################
########################################################################################


# 01) Autocorrelação Global – a Estatística I de Moran --------------------

# Para o cálculo da Estatística I de Moran, nosso algoritmo esperará como
# declaração um objeto de classe listw. Como exemplificação, voltaremos a 
# utilizar o objeto matrizW_queen:
listw_queen <- mat2listw(matrizW_queen, style="B")
class(listw_queen)


# Substitui os NA ocorrencias por 0

spatial_regiao_imediata@data[is.na(spatial_regiao_imediata@data$ocorrencias),] <- 0
spatial_regiao_imediata@data[is.na(spatial_regiao_imediata@data$taxa),] <- 0

# Após isso, poderemos utilizar a função
moran.test(x = spatial_regiao_imediata@data$taxa, 
           listw = listw_queen, 
           zero.policy = TRUE
           # ,na.action = na.contiguous
           )


# 02) O Diagrama da Estatística I de Moran --------------------------------
moran.plot(x = spatial_regiao_imediata@data$taxa, 
           listw = listw_queen, 
           zero.policy = TRUE,
           xlab = "Ocorrencias", 
           ylab = "Ocorrencias Espacialmente Defasada",
           pch = 19)


# 03) Autocorrelação Local – a Estatística Moran Local --------------------

# Seguindo o proposto por Anselin (1995), devemos padronizar em linha nossa 
# matriz de pesos espaciais W:
matrizW_queen_linha <- nb2mat(vizinhos_queen,
                              style = "W",
                              zero.policy = TRUE)

# Considerando a variável idh do objeto SP.dados, podemos aferir sua Estatística 
# Moran Local, com o uso da função localmoran(), como se segue:
moran_local <- localmoran(x = spatial_regiao_imediata@data$taxa, 
                          listw = listw_queen, 
                          zero.policy = TRUE)
# Juntando os resultados da Estatística Moran Local no dataset do objeto shp_sp:
moran_local_mapa <- cbind(spatial_regiao_imediata, moran_local)

# # Plotando a Estatística Moran Local de forma espacial:
# tm_shape(shp = moran_local_mapa) +
#   tm_fill(col = "Ii", style = "quantile", n = 5, palette = "-magma") +
#   tm_borders()

# Plotando a Estatística Moran Local de forma espacial:
tm_shape(shp = moran_local_mapa) +
  tm_fill(col = "Ii", style = "quantile", n = 5, palette = "-viridis") +
  tm_borders()

# Outra coisa interessante sobre o objeto moran_local
attr(x = moran_local, which = "quadr")

moran_local_mapa <- cbind(moran_local_mapa, 
                          attr(x = moran_local, which = "quadr")[1])

tm_shape(shp = moran_local_mapa) +
  tm_fill(col = "mean", palette = "-viridis") +
  tm_borders(col = "gray")



# 04) Clusterização LISA --------------------------------------------------

# O primeiro passo é o estabelecimento de um objeto que reservará espaços para 
# conter, no futuro, os quadrantes AA, AB, BA e BB:
quadrantes <- vector(mode = "numeric", length = nrow(moran_local))

quadrantes

# Criando um vetor que contenha o centro das observações da variável idh ao 
# redor de sua média:
centro_idh <- spatial_regiao_imediata@data$taxa - mean(spatial_regiao_imediata@data$taxa)

centro_idh


# Criando um vetor que contenha o centro dos valores da Estatística Moran Local 
# em torno de sua média:
centro_moran_local <- moran_local[,1] - mean(moran_local[,1])

centro_moran_local

# Criando um objeto que guarde a significância a ser adotada:
sig <- 0.05

# Enquadrando nossas observações em seus respectivos quadrantes:
quadrantes[centro_idh > 0 & centro_moran_local > 0] <- "HH"
quadrantes[centro_idh > 0 & centro_moran_local < 0] <- "HL"
quadrantes[centro_idh < 0 & centro_moran_local > 0] <- "LH"
quadrantes[centro_idh < 0 & centro_moran_local < 0] <- "LL"

quadrantes

# Ajustando a presença da observação em razão de sua significância estatística:
quadrantes[moran_local[,5] > sig] <- "Estatisticamente_não_significante"

quadrantes

# Juntando o objeto quadrantes ao objeto moran_local_mapa
moran_local_mapa@data["quadrantes"] <- factor(quadrantes)

# Plotando os quadrantes de forma espacial (versão 'default'):
tm_shape(shp = moran_local_mapa) +
  tm_polygons(col = "quadrantes", 
              pal = c(HH = "darkred",
                      HL = "red", 
                      LH = "lightblue", 
                      LL = "darkblue",
                      Estatisticamente_não_significante = "white")) +
  tm_borders()

# 05) Autocorrelação Local -  A Estatística G de Getis e Ord --------------

# listw_queen <- mat2listw(matrizW_queen, style="B")

# Calculando a Estatística G de Getis e Ord:
g_local <- localG(x = spatial_regiao_imediata@data$ocorrencias,
                  listw = listw_queen)

# Juntando as informações do objeto g_local ao nosso shapefile:
mapa_G <- cbind(spatial_regiao_imediata, as.matrix(g_local)) 

head(mapa_G@data)

# Renomeando a nova variável para facilitar:
mapa_G@data %>% 
  rename(estistica_g = 4) -> mapa_G@data

# Plotando a Estatística G de forma espacial:
tm_shape(mapa_G) + 
  tm_fill("estistica_g", 
          palette = "-viridis") + 
  tm_borders()

library(gtools)


# Mais uma vez, o erro a respeito da quantidade de cores. Vamos ajustar:
mapa_G@data <- mapa_G@data %>% 
  mutate(faixa_quantis = factor(quantcut(x = estistica_g, q = 8))) 

# Plotando a Estatística G de forma espacial (versão 'default'):
tm_shape(mapa_G) + 
  tm_fill("faixa_quantis", 
          palette = "-RdBu") + 
  tm_borders()

########################################################################################

### Chegue até aqui, só falta interpretar
#############################################

##########################################$$







tmap_mode("plot")

spatial_regiao_imediata  %>%  #filter(joined_semanas$SEM_ENT_UTI ==20)  %>% 
tm_shape() +
  tm_fill("ocorrencias",
          # style = "quantile",
          # n = 8,
          legend.hist = TRUE,
          palette = "viridis",
          popup.vars = c("rgi","nome_rgi","ocorrencias","SEM_ENT_UTI")
  ) +
  tm_layout(legend.outside = TRUE ) +
  # tm_fill("ocorrencias", popup.vars = c("rgi","nome_rgi","ocorrencias","SEM_ENT_UTI")) + 
  tm_borders("black", lwd = .5) +
  # tm_markers() +
  tm_facets(by = "SEM_ENT_UTI", ncol = 3) +
  # tm_facets(along = "SEM_ENT_UTI") +
  tm_layout(legend.outside.size = 0.2)

# joined_semanas %>% 
#   ggplot() +
#   geom_sf()
  # theme_bw()

######################################################
#                        ANIMADO                     # 
######################################################


# animado <- tm_shape(joined_semanas)  +
#   tm_fill("ocorrencias",
#             style = "quantile",
#             n = 8,
#             legend.hist = TRUE,
#             palette = "viridis"
#           ) +
#   tm_layout(legend.outside = TRUE ) +
#   # tm_fill("ocorrencias", popup.vars = c("name_muni","ocorrencias")) +
#   tm_borders("black", lwd = .5) +
#   tm_markers() +
#   # tm_facets(by = "SEM_ENT_UTI", ncol = 3) +
#   tm_facets(along = "SEM_ENT_UTI") +
#   tm_layout(legend.outside.size = 0.2)
# # + tm_markers(text="name_muni")
# 
# tmap_animation(  animado,
#                  delay = 20, width = 1200, height = 800,
#                  filename = "animado.mp4")

##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################










ocorrencias_com_sede <- merge( x = merged,
                               y = sedes_municipios_RJ,
                               by.x = "code_muni",
                               by.y = "code_muni"
                )

# tm_shape(ocorrencias_com_sede)  +
#   tm_fill("ocorrencias") +
#   tm_borders("black", lwd = .5) +
#   tm_markers()


ocorrencias_com_sede@data %>% 
  ggplot() +
  geom_histogram(aes(x = ocorrencias),
  fill = "deepskyblue4") +
  labs(x = "Eventos",
       y = "Frequencia")

# 
# tm_shape(shp = spatial_municipios_RJ) +
#   tm_fill(col = "code_muni", palette = "Blues") +
# tm_shape(shp = ocorrencias_com_sede) + 
#   tm_dots(col = "deepskyblue4", 
#           border.col = "black", 
#           size = 0.2, 
#           alpha = 0.8)


# Vamos tentar aplicar a função gBuffer():
ocorrencias_UTM <- spTransform(x = ocorrencias_com_sede,
                             CRSobj = CRS("+init=epsg:22523"))
buffer_ocorrencias <- gBuffer(spgeom = ocorrencias_UTM,
                            width = 1500,
                            byid = TRUE)

tm_shape(shp = buffer_ocorrencias) + 
  tm_borders(alpha = 0.5) 

tm_shape(shp = municipios_RJ) + 
  tm_fill(col = "code_muni", palette = "Blues") +
  tm_borders(alpha = 0.5) 
  
  
  tm_shape(shp = ocorrencias_com_sede) + 
  tm_dots(col = "ocorrencias", 
          size = 0.02) 




plot(ocorrencias_com_sede)

plot(spatial_municipios_RJ)



ocupacao %>%  filter(is.na(DT_1_DOSE)) %>% head()

# headoccupation <- head(ocupacao,100000)
  


# verificando a distribuição da variável dependente
# ggplot(ocupacao, aes(x = grouped_by_date$DT_NOTIFIC)) +
#   geom_density(aes(x = grouped_by_date$count), 
#                position = "identity", color = "black", size = 1) +
#   geom_histogram(aes(y = ..density..), color = "white", fill = "deepskyblue",
#                  bins = 30) +
#   theme_classic()


# 
# to_date(col("DT_NOTIFIC"),"MM-dd-yyyy").as("to_date")
# 
# glimpse(ocupacao)

# dplyr::show_query(ocupacao)


# ocupacao <- read_delim_arrow(
#   file,
#   delim = ";",
#   quote = "\"",
#   escape_double = TRUE,
#   escape_backslash = FALSE,
#   schema = NULL,
#   col_names= TRUE,
#   # col_types = schema( field("DT_NOTIFIC", date32(),locale(encoding = "ISO-8859-1")), field("SEM_NOT",int32())),
#   # col_types = schema( field("DT_NOTIFIC", string()), field("SEM_NOT",int32())),
#   # col_select = c("DT_NOTIFIC", "SEM_NOT"),
#   # col_types = schema(y = utf8()),
#   na = c("", "NA"),
#   quoted_na = TRUE,
#   skip_empty_rows = TRUE,
#   skip = 0L,
#   parse_options = NULL,
#   convert_options = NULL,
#   read_options = NULL,
#   as_data_frame = TRUE,
#   timestamp_parsers = NULL
# )





# verificando nossas variáveis e observações
# glimpse(ocupcao21)

# ajustando os tipos das variáveis Date




# verificando os missing values
ocupacao %>% filter(is.na(DT_NOTIFIC))

# levando o dataframe R para o cluster spark
dados_agrupados_voos_atrasados_spark <- copy_to(sc, 
                                                dados_agrupados_voos_atrasados_R, 
                                                "dados_agrupados_voos_atrasados_spark")

# verificando o dataframe spark
glimpse(dados_agrupados_voos_atrasados_spark)

# mostrando a lista de tabelas do cluster spark
src_tbls(sc)





glimpse(ocupacao)


# contagem de linhas do dataset
sdf_nrow(ocupacao)
# 
# headoccupation <- head(ocupacao,100)
# 
# # 
# headoccupation$DT_SEM_NOT <- as.Date(make_yearweek(year = 2021, week = headoccupation$SEM_NOT))
tshead <- ts(grouped_by_date$count,grouped_by_date$DT_NOTIFIC)
# make_yearweek(year = 2021, week = ocupacao$SEM_NOT[1:10])

estudante_escola$estudante <- as.factor(estudante_escola$estudante)
estudante_escola$escola <- as.factor(estudante_escola$escola)

# verificando a correlação das nossas variáveis numéricas
ggcorrplot(round(cor(select(estudante_escola, horas, desempenho, texp)), 1), lab = TRUE)

# verificando a distribuição da variável dependente
ggplot(estudante_escola, aes(x = desempenho)) +
  geom_density(aes(x = desempenho), 
               position = "identity", color = "black", size = 1) +
  geom_histogram(aes(y = ..density..), color = "white", fill = "deepskyblue",
                 bins = 30) +
  theme_classic()

# verificando a distribuição da variável dependente por escola
ggplot(estudante_escola, aes(x = desempenho)) +
  geom_density(aes(color = escola, fill = escola), 
               position = "identity", alpha = 0.3) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  theme_classic()

################################################################################
#                      PREPARAÇÃO DOS DADOS                                    #
################################################################################

# localizando a presença de valores missing no dataframe
sapply(estudante_escola, function(x) which(is.na(x)))

# contando a quantidade de valores missing no dataframe
sapply(estudante_escola, function(x) sum(is.na(x)))

# verificando os tipos das variáveis antes da modelagem
glimpse(estudante_escola)

################################################################################
#                      MODELAGEM                                               #
################################################################################
# Executar no terminal:
#  mlflow ui
# conectando ao mlflow para gerenciar todo o ciclo de vida do modelo
mlflow_set_tracking_uri('http://localhost:5000')

# criando o nosso primeiro experimento
mlflow_create_experiment('modelo_desempenho_estudante')

# acionando o experimento desejado
mlflow_set_experiment('modelo_desempenho_estudante')

################################################################################
#                  ESTIMAÇÃO DO MODELO NULO NO MLFLOW                          #
################################################################################

# a corrida é iniciada com a ativação da função "mlflow_start_run()"

# iniciando a primeira corrida do experimento
mlflow_start_run()

# definindo o nome da corrida
mlflow_set_tag("mlflow.runName", "Modelo linear nulo")

# adicionando os parâmetros do modelo [INPUTS]

# os parâmetros são definidos utilizando a função mlflow_log_param

mlflow_log_param("MODELO", "LINEAR")
mlflow_log_param("FORMULA", "desempenho ~ 1")

# estimando o modelo nulo
modelo_mlflow_estudante_escola_nulo <- lm(formula=desempenho ~ 1, 
                                          data=estudante_escola)

# colentando o output com as métricas do modelo
output_modelo <- summary(modelo_mlflow_estudante_escola_nulo)

# coletando os fittedvalues
y_hat_modelo_nulo <- predict(modelo_mlflow_estudante_escola_nulo, estudante_escola)

# armazenando as métricas da perfomance do modelo estimado
r2_modelo_nulo <- output_modelo$r.squared
r2_ajustado_modelo_nulo <- output_modelo$adj.r.squared
loglikelihood_modelo_nulo <- logLik(modelo_mlflow_estudante_escola_nulo)[1]

# adicionando as métricas do modelo [OUTPUT]

# adicionar as métricas da corrida no log do mlflow
mlflow_log_metric("R2", r2_modelo_nulo)
mlflow_log_metric("R2 ajustado", r2_ajustado_modelo_nulo)
mlflow_log_metric("Loglikelihood", loglikelihood_modelo_nulo)

# criando artefatos gráficos do modelo
ggplot(estudante_escola, 
       aes(x=desempenho, y=y_hat_modelo_nulo)) + 
  scale_colour_viridis_d() +
  geom_smooth(aes(x = desempenho, y = desempenho), method = "lm", 
              color = "gray44", size = 1.05,
              linetype = "longdash") +
  geom_point(color="red") +
  labs(x = "Fitted values",
       y = "Desempenho Escolar") +
  theme_bw()

# salvando o gráfico localmente para ser enviado como artefato para o mlflow
ggsave("modelo_nulo.png")

# adicionando o gráfico como artefato da corrida
mlflow_log_artifact("modelo_nulo.png")

# criando uma função customizada para fazer o predict do modelo
modelo_estudante_escola_nulo_encapsulado <- carrier::crate(
  ~ stats::predict(object=!!modelo_mlflow_estudante_escola_nulo, .x), 
  modelo_mlflow_estudante_escola_nulo)

# fazer o log do modelo gerado pela função customizada no mlflow
mlflow_log_model(modelo_estudante_escola_nulo_encapsulado, "modelo_estudante_escola_nulo_encapsulado")

# finalizando a corrida
mlflow_end_run()

################################################################################
#           ESTIMAÇÃO DO MODELO NULO ENCAPSULADO NO MLFLOW                     #
################################################################################

# fazendo o mesma corrida do modelo nulo, mas com o todo o script encapsulado

# MODELO LINEAR NULO - ENCAPSULADO
with(mlflow_start_run(), {
  
  # definindo o nome da corrida do experimento
  mlflow_set_tag("mlflow.runName", "Modelo linear nulo - encapsulado")
  
  # adicionar a fórmula utilizada como parâmetro no experimento
  mlflow_log_param("MODELO", "LINEAR")
  
  # adicionar a fórmula utilizada como parâmetro no experimento
  mlflow_log_param("FORMULA", "desempenho ~ 1")
  
  # modelagem dentro do encapsulamento do mlflow
  modelo_encapsulado <- lm(desempenho ~ 1,
                           data = estudante_escola)
  
  # sumário do modelo
  output_modelo <- summary(modelo_encapsulado)
  
  # valores fitted do modelo
  y_hat <- predict(modelo_encapsulado, estudante_escola)
  
  # armazenar o r2, r2 ajustado e loglik
  r2 <- output_modelo$r.squared 
  r2_ajustado <- output_modelo$adj.r.squared
  loglikelihood <- logLik(modelo_encapsulado)[1]
  
  # printar mensagens no log do mlflow
  message("R2: ", r2)
  message("R2 ajustado: ", r2_ajustado)
  message("Loglikelihood: ", loglikelihood)
  
  # artefatos do modelo
  ggplot(estudante_escola, 
         aes(x=desempenho, y=y_hat)) + 
    geom_smooth(aes(x = desempenho, y = desempenho), method = "lm", 
                color = "gray44", size = 1.05,
                linetype = "longdash") +
    geom_point(color="red") +
    labs(x = "Fitted values",
         y = "Desempenho Escolar") +
    theme_bw()
  
  # salvar o grafico localmente na maquina
  ggsave("grafico_fitted_vs_observado.png")
  
  mlflow_log_artifact("grafico_fitted_vs_observado.png")
  
  # inputar as métricas do run atual do mlflow
  mlflow_log_metric("R2", r2)
  mlflow_log_metric("R2 ajustado", r2_ajustado)
  mlflow_log_metric("Loglikelihood", loglikelihood)
  
  # criar uma função customizada que vai receber o modelo para fazer um futuro predict
  pacote_modelo_encapsulado <- carrier::crate(~ stats::predict(object=!!modelo_encapsulado, .x), 
                                              modelo_encapsulado)
  
  # fazer o log do modelo gerado pela função customizada no mlflow
  mlflow_log_model(pacote_modelo_encapsulado, "modelo_encapsulado_estudante_escola")
  
})

################################################################################
#           ESTIMAÇÃO DO MODELO OLS ENCAPSULADO NO MLFLOW                      #
################################################################################

# ENCAPSULANDO O MODELO LINEAR UTILIZANDO TXEP E HORAS COMO VARIAVEIS EXPLICATIVAS
with(mlflow_start_run(), {
  
  # definindo o nome da corrida do experimento
  mlflow_set_tag("mlflow.runName", "Modelo linear - texp + horas")
  
  # adicionar a fórmula utilizada como parâmetro no experimento
  mlflow_log_param("MODELO", "LINEAR")
  
  # adicionar a fórmula utilizada como parâmetro no experimento
  mlflow_log_param("FORMULA", "desempenho ~ texp + horas")
  
  # modelagem dentro do encapsulamento do mlflow
  modelo_encapsulado <- lm(desempenho ~ texp + horas,
                           data = estudante_escola)
  
  # sumário do modelo
  output_modelo <- summary(modelo_encapsulado)
  
  # valores fitted do modelo
  estudante_escola$y_hat <- predict(modelo_encapsulado, estudante_escola)
  
  # armazenar o r2 e r2 ajsutado
  r2 <- output_modelo$r.squared
  r2_ajustado <- output_modelo$adj.r.squared
  loglikelihood <- logLik(modelo_encapsulado)[1]
  
  # printar mensagens no log do mlflow
  message("R2: ", r2)
  message("R2 ajustado: ", r2_ajustado)
  message("Loglikelihood: ", loglikelihood)

  # artefatos do modelo
  ggplot(estudante_escola, 
         aes(x=desempenho, y=y_hat)) + 
    geom_smooth(aes(x = desempenho, y = desempenho), method = "lm", 
                color = "gray44", size = 1.05,
                linetype = "longdash") +
    geom_point() +
    labs(x = "Fitted values",
         y = "Desempenho Escolar") +
    theme_bw()
  
  # salvar o grafico localmente na maquina
  ggsave("grafico_fitted_vs_observado.png")
  
  mlflow_log_artifact("grafico_fitted_vs_observado.png")
  
  # inputar as métricas do run atual do mlflow
  mlflow_log_metric("R2", r2)
  mlflow_log_metric("R2 ajustado", r2_ajustado)
  mlflow_log_metric("Loglikelihood", loglikelihood)
  
  # criar uma função customizada que vai receber o modelo para fazer um futuro predict
  pacote_modelo_encapsulado <- carrier::crate(~ stats::predict(object=!!modelo_encapsulado, .x), 
                                              modelo_encapsulado)
  
  # fazer o log do modelo gerado pela função customizada no mlflow
  mlflow_log_model(pacote_modelo_encapsulado, "modelo_encapsulado_estudante_escola")
  
})

################################################################################
#           ESTIMAÇÃO DO MODELO HLM2 NULO ENCAPSULADO NO MLFLOW                #
################################################################################

# ENCAPSULANDO O MODELO HLM2 NULO
with(mlflow_start_run(), {
  
  # definindo o nome da corrida do experimento
  mlflow_set_tag("mlflow.runName", "Modelo nulo HLM2")
  
  # adicionar a fórmula utilizada como parâmetro no experimento
  mlflow_log_param("MODELO", "MULTINIVEL")
  
  # adicionar a fórmula utilizada como parâmetro no experimento
  mlflow_log_param("FORMULA", "desempenho ~ 1 + (1 | escola)")
  
  # modelagem dentro do encapsulamento do mlflow
  modelo_encapsulado <- lme(fixed = desempenho ~ 1, 
                            random = ~ 1 | escola,
                            data = estudante_escola,
                            method = "REML")

  # sumário do modelo
  output_modelo <- summary(modelo_encapsulado)
  
  # valores fitted do modelo
  estudante_escola$y_hat <- predict(modelo_encapsulado, estudante_escola)
  
  # armazenar o r2 e r2 ajsutado
  r2 <- output_modelo$r.squared
  r2_ajustado <- output_modelo$adj.r.squared
  loglikelihood <- logLik(modelo_encapsulado)[1]
  
  # printar mensagens no log do mlflow
  message("R2: ", r2)
  message("R2 ajustado: ", r2_ajustado)
  message("Loglikelihood: ", loglikelihood)
  
  # artefatos do modelo
  ggplot(estudante_escola, 
         aes(x=desempenho, y=y_hat)) + 
    geom_smooth(aes(x = desempenho, y = desempenho), method = "lm", 
                color = "gray44", size = 1.05,
                linetype = "longdash") +
    geom_point() +
    labs(x = "Fitted values",
         y = "Desempenho Escolar") +
    theme_bw()
  
  # salvar o grafico localmente na maquina
  ggsave("grafico_fitted_vs_observado.png")
  
  mlflow_log_artifact("grafico_fitted_vs_observado.png")
  
  # inputar as métricas do run atual do mlflow
  mlflow_log_metric("Loglikelihood", loglikelihood)
  
  # criar uma função customizada que vai receber o modelo para fazer um futuro predict
  pacote_modelo_encapsulado <- carrier::crate(~ stats::predict(object=!!modelo_encapsulado, .x, level = 0:1,), 
                                              modelo_encapsulado)
  
  # fazer o log do modelo gerado pela função customizada no mlflow
  mlflow_log_model(pacote_modelo_encapsulado, "modelo_encapsulado_estudante_escola")  

})

################################################################################
#             DEPLOYMENT DO MODELO PARA O ESTÁGIO DE ADAPTAÇÃO                 #
################################################################################

# CICLO DE VIDA DE UM MODELO
# -> ADAPTAÇÃO
# -> PRODUÇÃO
# -> ARQUIVADO

# consumindo o modelo em fase de adaptação
modelo_estagio_adaptacao_predict <- mlflow_load_model("models:/modelo_desempenho_estudante/staging")

# criando uma nova observação para fazer uma nova previsão
# uma observação com tempo de experiência do professor igual a 12 anos
# e horas de estudos igual a 20
novo_dado <- data.frame(texp=c(12),
                        horas=c(20))

# realizando um predict com o modelo através do mlflow
modelo_estagio_adaptacao_predict(novo_dado)

# alterando o estágio do modelo para a fase de produção

# tentando consumir novamente o modelo na fase de adaptação
modelo_estagio_adaptacao_predict <- mlflow_load_model("models:/modelo_desempenho_estudante/staging")

# consumindo o modelo em fase de produção
modelo_estagio_producao_predict <- mlflow_load_model("models:/modelo_desempenho_estudante/production")

# realizando um predict com o modelo em produção
modelo_estagio_producao_predict(novo_dado)

# alterando o estágio do modelo para a fase de arquivamento

# tentando consumir novamente o modelo na fase de produção
modelo_estagio_producao_predict <- mlflow_load_model("models:/modelo_desempenho_estudante/production")

# o modelo arquivado ainda pode ser carregado e consumido
modelo_estagio_arquivado_predict <- mlflow_load_model("models:/modelo_desempenho_estudante/archived")

# realizando um predict com o modelo arquivado
modelo_estagio_arquivado_predict(novo_dado)

# ciclo de vida lógico de um modelo
# ADAPTAÇÃO -> PRODUÇÃO -> ARQUIVADO

################################################################################
#  ESTIMAÇÃO DO MODELO COM INTERCEPTOS ALEATÓRIOS HLM2 ENCAPSULADO NO MLFLOW   #
################################################################################

# ENCAPSULANDO O MODELO COM INTERCEPTOS ALEATÓRIOS HLM2
with(mlflow_start_run(), {
  
  # definindo o nome da corrida do experimento
  mlflow_set_tag("mlflow.runName", "Modelo com interceptos aleatórios HLM2")
  
  # adicionar a fórmula utilizada como parâmetro no experimento
  mlflow_log_param("MODELO", "MULTINIVEL")
  
  # adicionar a fórmula utilizada como parâmetro no experimento
  mlflow_log_param("FORMULA", "desempenho ~ horas + (1 | escola)")
  
  # modelagem dentro do encapsulamento do mlflow
  modelo_encapsulado <- lme(fixed = desempenho ~ horas, 
                            random = ~ 1 | escola,
                            data = estudante_escola,
                            method = "REML")
  
  # sumário do modelo
  output_modelo <- summary(modelo_encapsulado)
  
  # valores fitted do modelo
  estudante_escola$y_hat <- predict(modelo_encapsulado, estudante_escola)
  
  # armazenar o r2 e r2 ajsutado
  r2 <- output_modelo$r.squared
  r2_ajustado <- output_modelo$adj.r.squared
  loglikelihood <- logLik(modelo_encapsulado)[1]
  
  # printar mensagens no log do mlflow
  message("R2: ", r2)
  message("R2 ajustado: ", r2_ajustado)
  message("Loglikelihood: ", loglikelihood)
  
  # artefatos do modelo
  ggplot(estudante_escola, 
         aes(x=desempenho, y=y_hat)) + 
    geom_smooth(aes(x = desempenho, y = desempenho), method = "lm", 
                color = "gray44", size = 1.05,
                linetype = "longdash") +
    geom_point() +
    labs(x = "Fitted values",
         y = "Desempenho Escolar") +
    theme_bw()
  
  # salvar o grafico localmente na maquina
  ggsave("grafico_fitted_vs_observado.png")
  
  mlflow_log_artifact("grafico_fitted_vs_observado.png")
  
  # inputar as métricas do run atual do mlflow
  mlflow_log_metric("Loglikelihood", loglikelihood)
  
  # criar uma função customizada que vai receber o modelo para fazer um futuro predict
  pacote_modelo_encapsulado <- carrier::crate(~ stats::predict(object=!!modelo_encapsulado, .x, level = 0:1), 
                                              modelo_encapsulado)
  
  # fazer o log do modelo gerado pela função customizada no mlflow
  mlflow_log_model(pacote_modelo_encapsulado, "modelo_encapsulado_estudante_escola")  
  
})

D################################################################################
#      ESTIMAÇÃO DO MODELO COM INTERCEPTOS E INCLINAÇÕES                       #
#          ALEATÓRIOS HLM2 ENCAPSULADO NO MLFLOW                               #
################################################################################

# ENCAPSULANDO O MODELO COM INTERCEPTOS E INCLINAÇÕES ALEATÓRIOS HLM2
with(mlflow_start_run(), {
  
  # definindo o nome da corrida do experimento
  mlflow_set_tag("mlflow.runName", "Modelo com interceptos e inclinações aleatórios HLM2")
  
  # adicionar a fórmula utilizada como parâmetro no experimento
  mlflow_log_param("MODELO", "MULTINIVEL")
  
  # adicionar a fórmula utilizada como parâmetro no experimento
  mlflow_log_param("FORMULA", "desempenho ~ horas + (horas | escola)")
  
  # modelagem dentro do encapsulamento do mlflow
  modelo_encapsulado <- lme(fixed = desempenho ~ horas, 
                            random = ~ horas | escola,
                            data = estudante_escola,
                            method = "REML")
  
  # sumário do modelo
  output_modelo <- summary(modelo_encapsulado)
  
  # valores fitted do modelo
  estudante_escola$y_hat <- predict(modelo_encapsulado, estudante_escola)
  
  # armazenar o r2 e r2 ajsutado
  r2 <- output_modelo$r.squared
  r2_ajustado <- output_modelo$adj.r.squared
  loglikelihood <- logLik(modelo_encapsulado)[1]
  
  # printar mensagens no log do mlflow
  message("R2: ", r2)
  message("R2 ajustado: ", r2_ajustado)
  message("Loglikelihood: ", loglikelihood)

  # artefatos do modelo
  ggplot(estudante_escola, 
         aes(x=desempenho, y=y_hat)) + 
    geom_smooth(aes(x = desempenho, y = desempenho), method = "lm", 
                color = "gray44", size = 1.05,
                linetype = "longdash") +
    geom_point(color="red") +
    labs(x = "Fitted values",
         y = "Desempenho Escolar") +
    theme_bw()
  
  # salvar o grafico localmente na maquina
  ggsave("grafico_fitted_vs_observado.png")
  
  mlflow_log_artifact("grafico_fitted_vs_observado.png")
  
  # inputar as métricas do run atual do mlflow
  mlflow_log_metric("Loglikelihood", loglikelihood)
  
  # criar uma função customizada que vai receber o modelo para fazer um futuro predict
  pacote_modelo_encapsulado <- carrier::crate(~ stats::predict(object=!!modelo_encapsulado, .x, level = 0:1), 
                                              modelo_encapsulado)
  
  # fazer o log do modelo gerado pela função customizada no mlflow
  mlflow_log_model(pacote_modelo_encapsulado, "modelo_encapsulado_estudante_escola")  
  
})

################################################################################
#           AVALIAÇÃO E DEPLOYMENT DO MELHOR MODELO                            #
################################################################################

# métrica utilizada: maior loglik

# coletando o modelo em fase de adaptação
modelo_estagio_adaptacao_predict <- mlflow_load_model("models:/modelo_desempenho_estudante/staging")

# coletando o modelo em fase de produção
modelo_estagio_produção_predict <- mlflow_load_model("models:/modelo_desempenho_estudante/production")

# verificando um novo dado
novo_dado <- data.frame(horas = 11,
                        texp = 3.6)

# realizando um predict no modelo de adaptação (OLS)
modelo_estagio_adaptacao_predict(novo_dado)

# verificando um novo dado para o modelo multinível
novo_dado <- data.frame(escola = "1",
                        horas = 11,
                        texp = 3.6)

# realizando um predict no modelo em produção (HLM2)
modelo_estagio_produção_predict(novo_dado)

################################################################################
#           DEPLOYMENT DE UM MODELO LOGÍSTICO BINÁRIO                          #
################################################################################

# carregando o dataset
chd_dados <- read.csv("CHD_preprocessed.csv")

# visualizando a base de dados
glimpse(chd_dados)

# ajustando as variáveis categóricas
chd_dados$TenYearCHD <- as.factor(chd_dados$TenYearCHD)
chd_dados$diabetes <- as.factor(chd_dados$diabetes)
chd_dados$prevalentHyp <- as.factor(chd_dados$prevalentHyp)
chd_dados$male <- as.factor(chd_dados$male)
chd_dados$prevalentStroke <- as.factor(chd_dados$prevalentStroke)
chd_dados$BPMeds <- as.factor(chd_dados$BPMeds)
chd_dados$currentSmoker <- as.factor(chd_dados$currentSmoker)

# visualizando a base de dados
glimpse(chd_dados)

# verificando as primeiras linhas
head(chd_dados)

# contando a quantidade de valores missing no dataframe
sapply(chd_dados, function(x) sum(is.na(x)))

################################################################################
#      ESTIMAÇÃO DO MODELO LOGISTICO INICIAL                                   #
################################################################################

# realizando um modelo inicial
modelo_inicial <- glm(TenYearCHD ~ age + education + cigsPerDay,
                      data = chd_dados,
                      family = binomial(link="logit"))

# visualizando o output do modelo
summary(modelo_inicial)

# coletando os fitted values
y_hat <- predict(modelo_inicial, newData=chd_dados, type="response")

# verificando a matriz de confusão com um cutoff de 0.5
cm <- confusionMatrix(data= as.factor(as.numeric(y_hat > 0.5)), reference = chd_dados$TenYearCHD, positive = "1")

# visualizando a matriz de confusão
print(cm)

# vamos utilizar a métrica da acurácia para comparar os modelos

# criando o nosso segundo experimento
mlflow_create_experiment('modelo_previsao_doenca')

# acionando o experimento desejado
mlflow_set_experiment('modelo_previsao_doenca')

# ENCAPSULANDO O MODELO LOGÍSTICO INICIAL
with(mlflow_start_run(), {
  
  # definindo o nome da corrida do experimento
  mlflow_set_tag("mlflow.runName", "Modelo logístico inicial")
  
  # adicionar a fórmula utilizada como parâmetro no experimento
  mlflow_log_param("MODELO", "LOGÍSTICO")
  
  # adicionar a fórmula utilizada como parâmetro no experimento
  mlflow_log_param("FORMULA", "TenYearCHD ~ male + age + cigsPerDay")
  
  # modelagem dentro do encapsulamento do mlflow
  modelo_encapsulado <-  glm(TenYearCHD ~ male + age + cigsPerDay,
                             data = chd_dados,
                             family = binomial(link="logit"))
  
  # sumário do modelo
  output_modelo <- summary(modelo_encapsulado)
  
  # valores fitted do modelo
  y_hat <- predict(modelo_encapsulado, newData=chd_dados, type="response")
  
  cm <- confusionMatrix(data= as.factor(as.numeric(y_hat>0.5)),
                        reference = chd_dados$TenYearCHD, positive = "1")
  
  # armazenar a acurácia do modelo
  acuracia <- cm$overall['Accuracy']
  
  # printar mensagens no log do mlflow
  message("Acurácia: ", acuracia)

  # inputar as métricas do run atual do mlflow
  mlflow_log_metric("Acurácia", acuracia)
  
  # criar uma função customizada que vai receber o modelo para fazer um futuro predict
  pacote_modelo_encapsulado <- carrier::crate(~ stats::predict(object=!!modelo_encapsulado, .x, type = "response"), 
                                              modelo_encapsulado)
  
  # fazer o log do modelo gerado pela função customizada no mlflow
  mlflow_log_model(pacote_modelo_encapsulado, "modelo_encapsulado_previsao_doenca")  
  
})

# ENCAPSULANDO O MODELO LOGÍSTICO FINAL
with(mlflow_start_run(), {
  
  # definindo o nome da corrida do experimento
  mlflow_set_tag("mlflow.runName", "Modelo logístico final")
  
  # adicionar a fórmula utilizada como parâmetro no experimento
  mlflow_log_param("MODELO", "LOGÍSTICO")
  
  # adicionar a fórmula utilizada como parâmetro no experimento
  mlflow_log_param("FORMULA", "TenYearCHD ~ male + age + cigsPerDay + diabetes + totChol + diaBP")
  
  # modelagem dentro do encapsulamento do mlflow
  modelo_encapsulado <-  glm(TenYearCHD ~ male + age + cigsPerDay + diabetes + totChol + diaBP,
                             data = chd_dados,
                             family = binomial(link="logit"))
  
  # sumário do modelo
  output_modelo <- summary(modelo_encapsulado)
  
  # valores fitted do modelo
  y_hat <- predict(modelo_encapsulado, newData=chd_dados, type="response")
  
  
  cm <- confusionMatrix(data= as.factor(as.numeric(y_hat>0.5)), reference = chd_dados$TenYearCHD, positive = "1")
  
  # armazenar a acurácia do modelo
  acuracia <- cm$overall['Accuracy']
  
  # printar mensagens no log do mlflow
  message("Acurácia: ", acuracia)
  
  # inputar as métricas do run atual do mlflow
  mlflow_log_metric("Acurácia", acuracia)
  
  # criar uma função customizada que vai receber o modelo para fazer um futuro predict
  pacote_modelo_encapsulado <- carrier::crate(~ stats::predict(object=!!modelo_encapsulado, .x, type = "response"), 
                                              modelo_encapsulado)
  
  # fazer o log do modelo gerado pela função customizada no mlflow
  mlflow_log_model(pacote_modelo_encapsulado, "modelo_encapsulado_previsao_doenca")  
  
})

# coletando o modelo em produção para realizar um predict
modelo_produção_predict <- mlflow_load_model("models:/modelo_previsao_doenca/production")

# criano uma nova observação para um homem com 40 anos e e que fuma 30 cigarros por dia
dado <- data.frame(male = as.factor(1),
                   age = 50,
                   cigsPerDay = 50)

# realizando o predict
modelo_produção_predict(dado)

################################################################################
#           CONSTRUÇÃO DA API PARA CONSUMO DO MODELO                           #
################################################################################

# ACESSAR SCRIPT api-rotas.R
