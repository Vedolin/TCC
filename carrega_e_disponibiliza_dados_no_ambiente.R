# Linha 200\
# Passos

#                    REGIOES IMEDIATAS            ####
## Obter regiões imediatas ####
### Obter dados de municípios que compõem cada região imediata ####

# ARTIGO!!!!
# Motivacao regiao imediata: 
# https://biblioteca.ibge.gov.br/index.php/biblioteca-catalogo?view=detalhes&id=2100600
# https://seer.ufu.br/index.php/hygeia/article/view/63335/35569

# https://www.ibge.gov.br/geociencias/organizacao-do-territorio/divisao-regional/15778-divisoes-regionais-do-brasil.html


system.time(
  
  source("./regiao_imediata.R")
)

# Talvez analisar as estatísticas das regiões imediatas. densidade populacional é legal ####
# Ligar as regiões imediatas a suas feições (mapas) ####


# VACINACAO ####

## Obter dados de vacinação e corrigir os outliers de data (do tipo, no sábado o número de vacinados *acumulado* é reportado como zero) ####
  # https://pkg.robjhyndman.com/forecast/reference/tsoutliers.html
## Disponibilizar dados espaciais de taxa de vacinação completa, por região imediata ####


system.time(
  
  source("./covid19-Datahub.R")
)

# Talvez analisar esses dados; máximo, mínimo, maior crescimento, maior estagnação? ####
# Preenche os valores faltantes com zero 




## Convertemos o formato long para wide, de modo que fique parecido com uma tabela ####
library(tidyr)
populacao_totalmente_vacinada_por_data_municipio <- pivot_wider(data = covid_vacinas, names_from = code_muni, values_from = people_fully_vaccinated, names_sort = TRUE)

### Adicionamos uma linha com data anterior ao primeiro item da série para funcioanr como uma semente de zeros ####
# Obtemos a data anterior à primeira amostra

data_anterior <- min(populacao_totalmente_vacinada_por_data_municipio$date)-1

# Criamos uma linha com uma lista de zeros para preencher os valores anteriores à série 
nova_linha <- c(as.character.Date(data_anterior), rep(0,ncol(populacao_totalmente_vacinada_por_data_municipio)-1))

populacao_totalmente_vacinada_por_data_municipio <- rbind(populacao_totalmente_vacinada_por_data_municipio,nova_linha)

## Ordenamos tabela por data, pois isso será importante para preenchermos os NAs repetindo o último valor visto ####
populacao_totalmente_vacinada_por_data_municipio <- populacao_totalmente_vacinada_por_data_municipio[order(populacao_totalmente_vacinada_por_data_municipio$date),]

library(zoo)


colunas_municipios <- colnames(populacao_totalmente_vacinada_por_data_municipio)[-1]

# ZOO =zoo(populacao_totalmente_vacinada_por_data_municipio[, colunas_municipios], order.by=as.Date(date))
# ZOO =zoo(populacao_totalmente_vacinada_por_data_municipio[, colunas_municipios])

ZOO =zoo(populacao_totalmente_vacinada_por_data_municipio[, colunas_municipios], order.by=as.Date(populacao_totalmente_vacinada_por_data_municipio$date))
zz <- ts(ZOO, frequency=1)
# plot (zz[,1:10])

fn_ts <- function(value_col) {
  na.locf(ts(value_col), na.rm = FALSE)
}

# fn_ts <- function(value_col) {
#   na.locf(zoo(,value_col), na.rm = FALSE)
# }

# População totalmente vacinada por data por muniicpio ####
populacao_totalmente_vacinada_por_data_municipio <- apply(populacao_totalmente_vacinada_por_data_municipio, 2,fn_ts)



# Testando Friburgo e Sao Paulo quanto a outliers.

code_muni_friburgo = 330340
code_muni_sampa = 355030

friburgo <- as.data.frame.ts(populacao_totalmente_vacinada_por_data_municipio[,as.character(code_muni_friburgo)],populacao_totalmente_vacinada_por_data_municipio[,1])
plot(friburgo)

sampa <- as.data.frame.ts(populacao_totalmente_vacinada_por_data_municipio[,as.character(code_muni_sampa)],populacao_totalmente_vacinada_por_data_municipio[,1])
plot(sampa)

# frisampa <- as.data.frame(
#   populacao_totalmente_vacinada_por_data_municipio[,as.character(code_muni_friburgo)],
#   populacao_totalmente_vacinada_por_data_municipio[,as.character(code_muni_sampa)],
#   populacao_totalmente_vacinada_por_data_municipio[,1]
#   )

frisampa <- as.data.frame(as.Date.character(populacao_totalmente_vacinada_por_data_municipio[,1]))
frisampa <- cbind(frisampa,as.data.frame(as.numeric(populacao_totalmente_vacinada_por_data_municipio[,as.character(code_muni_friburgo)])))
frisampa <- cbind(frisampa,as.data.frame(as.numeric(populacao_totalmente_vacinada_por_data_municipio[,as.character(code_muni_sampa)])))

# Agora que os NAs foram preenchidos adequadamente com zeros ####
## Converte dados para o formato long para fazer o join com o objeto espacial ####

tmp_df <- as.data.frame(populacao_totalmente_vacinada_por_data_municipio)

evolucao_vacina_municipio <- pivot_longer(data = tmp_df, cols=!date, names_to = "code_muni", values_to = "people_fully_vaccinated")
evolucao_vacina_municipio <- evolucao_vacina_municipio %>%
  mutate(
        date = as.Date.character(date),
         code_muni = as.integer(code_muni),
         people_fully_vaccinated = as.integer(people_fully_vaccinated)
        )

# Agora preciso da evolucao por regiao imediata ####
# Acrescento as regioes imediatas aos municipios

evolucao_vacina_municipio <- evolucao_vacina_municipio %>% 
  inner_join(composicao_por_municipios, by = join_by(code_muni == code_muni) ) %>% 
  inner_join(populacao_regiao_imediata, by = join_by(cod_rgi == cod_rgi))

  
evolucao_vacina_regiao_imediata <- evolucao_vacina_municipio %>%
  group_by(cod_rgi, nome_rgi, date, populacao_rgi) %>%
  summarize(
    populacao_totalmente_vacinada = sum(people_fully_vaccinated, na.rm = TRUE),
    .groups = 'drop'
  ) %>% 
  mutate(taxa_populacao_totalmente_vacinada = (populacao_totalmente_vacinada / populacao_rgi) * 100 ) 


# # Observando a evolução da vacinação por regiao imediata Sao Paulo e Friburgo  
# evolucao_regiao_sampa <- evolucao_vacina_regiao_imediata %>%
#                             filter(nome_rgi == 'São Paulo')
# evolucao_regiao_friburgo <- evolucao_vacina_regiao_imediata %>%
#   filter(nome_rgi == 'Nova Friburgo')
# 
# evolucao_regiao_frisampa <- rbind(evolucao_regiao_sampa, evolucao_regiao_friburgo)
# 
# 
# 
# 
# library(ggplot2)
# library(scales)
# library(plotly)
# ggplotly(
# ggplot(evolucao_regiao_frisampa) +
#  aes(x = date, y = taxa_populacao_totalmente_vacinada, colour = nome_rgi) +
#  geom_line() +
#  scale_color_hue(direction = 1) +
#  scale_x_date(date_breaks="1 month", date_labels="%W-%y", date_minor_breaks = "1 week") +
#  labs(x = "Data", y = "Percentual totalmente vacinados", 
#  title = "Taxa de população totalmente vacinada", subtitle = "Friburgo x São Paulo", color = "Região Imediata") +
#  ggthemes::theme_economist() +
#   theme(axis.text.x=element_text(angle=60, hjust=1)) 
# )

# 
# evolucao_vacina_estado <- evolucao_vacina_regiao_imediata %>%
#   group_by(grupo_rgi = substr(cod_rgi,0,2), date) %>%
#   summarize(
#     populacao_total = sum(populacao_rgi, na.rm = TRUE),
#     populacao_totalmente_vacinada = sum(populacao_totalmente_vacinada, na.rm = TRUE),
#     .groups = 'drop'
#   ) %>% 
#   mutate(taxa_populacao_totalmente_vacinada = (populacao_totalmente_vacinada / populacao_total) * 100 ) 
#   
# ggplotly(
#   ggplot(evolucao_vacina_estado) +
#     aes(x = date, y = taxa_populacao_totalmente_vacinada, colour = grupo_rgi) +
#     geom_line() +
#     scale_color_hue(direction = 1) +
#     scale_x_date(date_breaks="1 month", date_labels="%m-%y", date_minor_breaks = "1 week") +
#     labs(x = "Data", y = "Percentual totalmente vacinados", 
#          title = "Taxa de população totalmente vacinada", subtitle = "Por grupo de região imediata", color = "Região Imediata") +
#     ggthemes::theme_economist() +
#     theme(axis.text.x=element_text(angle=60, hjust=1)) 
# )

evolucao_vacina_regiao_imediata_SP_RJ <- evolucao_vacina_regiao_imediata %>%
  filter(substr(cod_rgi,0,2) %in% c('35','33')) %>% 
  filter(day(date)==1) 
  # filter(year(date) %in% c(2021, 2022, 2023))




spatial_vacinas_por_regiao_imediata <- regiao_imediata
spatial_vacinas_por_regiao_imediata@data$rgi <- as.numeric(spatial_vacinas_por_regiao_imediata@data$rgi)
sf_vacinas <- st_as_sf(x = spatial_vacinas_por_regiao_imediata,
                       crs = 4326) 
# evolucao_vacina_regiao_imediata_SP_RJ %>% select(date, cod_rgi, taxa_populacao_totalmente_vacinada)
joined_SP_RJ <-inner_join(sf_vacinas, evolucao_vacina_regiao_imediata_SP_RJ, by = join_by(rgi == cod_rgi)) %>% 
  dplyr::select(-nome_rgi.x, -nome_rgi.y, -populacao_totalmente_vacinada, -populacao_rgi)

vwide <- pivot_wider(data = joined_SP_RJ, names_from="date", values_from = "taxa_populacao_totalmente_vacinada")

st_write(vwide , "./joined_SP_RJ/joined_SP_RJ.shp")

joined_SP_RJ_wide <-inner_join(sf_vacinas, vwide, by = join_by(rgi == rgi))
st_write(joined_SP_RJ_wide , "./joined_SP_RJ/joined_SP_RJ.shp")

# st_write(joined_SP_RJ %>% select(date, rgi = rgi, taxa_populacao_totalmente_vacinada), "./joined_SP_RJ/joined_SP_RJ.shp")

gerar_mp4_taxa_de_vacinacao_animada <- FALSE

if (gerar_mp4_taxa_de_vacinacao_animada) {
  m <- tm_shape(joined_SP_RJ,  simplify = 0.5) +
    tm_fill(
      "taxa_populacao_totalmente_vacinada",
      id = "nome_rgi",
      style = "quantile",
      n = 8,
      # popup.vars = c("rgi","populacao_rgi","populacao_totalmente_vacinada","taxa_populacao_totalmente_vacinada","date"),
      palette = "viridis"
    ) +
    tm_facets(along = "date")
  
  
  tmap_mode("view")
  
  tmap_animation(m,
                 delay = 50,
                 outer.margins = 0,
                 filename = "evolucao_taxa_totalmente_vacinados_SP_RJ.mp4")
}



tm_view(text.size.variable = TRUE)

# tmap_mode("plot")
tmap_mode("view")

joined_SP_RJ %>%
  filter( date == "2022-06-01") %>%
  tm_shape(simplify = 0.5) +
  # tm_polygons(col = "taxa_populacao_totalmente_vacinada", id = "nome_rgi.x") +
  tm_polygons(col = "taxa_populacao_totalmente_vacinada",
              id = "nome_rgi",
              # style = "quantile",
              n = 8,
              popup.vars = c("rgi","populacao_rgi","populacao_totalmente_vacinada","taxa_populacao_totalmente_vacinada","date"),
              palette = "viridis"
  ) +
  tm_text("nome_rgi.x", size = "AREA", root = 5, remove.overlap = FALSE) 


# ts_friburgo <- zoo(friburgo)
# ts_friburgo <- zoo(friburgo$people_fully_vaccinated,friburgo$date)
# ts_friburgo <- na.locf(ts_friburgo, na.rm = FALSE)


# ts_sampa <- zoo(sampa$people_fully_vaccinated,sampa$date)
# ts_sampa <- na.locf(ts_sampa, na.rm = FALSE)











# Eventualmente plotar essas informações e análises no mapa ####

# Disponibiliza dados de *taxa* de internação por SRAG e tratar os outliers (como no exemplo do sabado) ####
# Talvez analisar esses dados; máximo, mínimo, maior crescimento, maior estagnação? ####
# Ligar as taxas de internação por SRAG às regiões imediatas ####
# Eventualmente plotar essas informações e análises no mapa ####

# Disponibiliza taxa de internação por SRAG por taxa de vacinação completa ####
# Obrigatoriamente analisar esses dados, pois são o alvo do trabalho; máximo, mínimo, maior crescimento, maior estagnação? ####

# Busca padrões espaciais ####
# As UFs têm comportamentos diferentes? ####
# Dentro das UFs há clusters? ####
# Sem dividir as UFs há clusters transestaduais? ####


