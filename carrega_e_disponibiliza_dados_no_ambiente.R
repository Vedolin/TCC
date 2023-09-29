# BANCOS DE DADOS: ####
# * taxa_internacao_por_100k_por_regiao_intermediaria_por_semana ####
# * evolucao_vacina_rgi_semana traz o total de vacinados, agrupado por rgi, ano e semana.
# -> evolucao_vacina_regiao_imediata tem a taxa da poplação totalmente vacinada dia a dia ####

# evolucao_vacina_regiao_imediata Contem todas as datas (sem buracos) ####
# evolucao_vacina_regiao_imediata_SP_RJ pegou os dias 01 de cada mes ####

# Correlação espaço-temporal entre vacinação contra SARS-CoV-2 e ocupação
# hospitalar por Síndrome Respiratória Aguda Grave no Brasil na pandemia de COVID-19

# Analise primaria comparando taxa de vacinacao com data de internacao
# Analise secundaria comparando taxa de vacinacao com data de entrada em UTI

# Objetivos a serem ajustados em funcao das perguntas acima


# Linha 483, exportei arquivo wide para shape
# Passos

#                    REGIOES IMEDIATAS            ####
## Obter regiões imediatas ####
### Obter dados de municípios que compõem cada região imediata ####
# Regiao imediata aqui -> https://github.com/ipeaGIT/geobr ####
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
# plot(friburgo)

sampa <- as.data.frame.ts(populacao_totalmente_vacinada_por_data_municipio[,as.character(code_muni_sampa)],populacao_totalmente_vacinada_por_data_municipio[,1])
# plot(sampa)

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

# evolucao_vacina_regiao_imediata Contem todas as datas (sem buracos) ####
# evolucao_vacina_regiao_imediata_SP_RJ pegou os dias 01 de cada mes ####

evolucao_vacina_regiao_imediata_SP_RJ <- evolucao_vacina_regiao_imediata %>%
  filter(substr(cod_rgi,0,2) %in% c('35','33')) %>% 
  filter(day(date)==1)  # pega apenas o dia 01 de cada mes
  # filter(year(date) %in% c(2021, 2022, 2023))

evolucao_vacina_rgi_semana <- evolucao_vacina_municipio %>%
  mutate(SEM_VACINA = isoweek(date),
         ANO_VACINA = year(date)
  ) %>% 
  group_by(cod_rgi, ANO_VACINA, SEM_VACINA) %>%
  summarize(
    populacao_totalmente_vacinada = sum(people_fully_vaccinated, na.rm = TRUE),
    .groups = 'drop'
  ) 

# evolucao_vacina_rgi_semana traz o total de vacinados, agrupado por rgi, ano e semana.


spatial_vacinas_por_regiao_imediata <- regiao_imediata
spatial_vacinas_por_regiao_imediata@data$rgi <- as.numeric(spatial_vacinas_por_regiao_imediata@data$rgi)
sf_vacinas <- st_as_sf(x = spatial_vacinas_por_regiao_imediata,
                       crs = 4326) 
# evolucao_vacina_regiao_imediata_SP_RJ %>% select(date, cod_rgi, taxa_populacao_totalmente_vacinada)
joined_SP_RJ <-inner_join(sf_vacinas, evolucao_vacina_regiao_imediata_SP_RJ, by = join_by(rgi == cod_rgi)) %>% 
  dplyr::select(-nome_rgi.x, -nome_rgi.y, -populacao_totalmente_vacinada, -populacao_rgi)

vwide <- pivot_wider(data = joined_SP_RJ, names_from="date", values_from = "taxa_populacao_totalmente_vacinada")

# CHECKCPOINT ####
# EXPORTAR TAXA DE VACINACAO PARA GEODA ####
# st_write(vwide , "./joined_SP_RJ/joined_SP_RJ.shp")



###############################################################
# AGORA CARREGAR OCUPACAO ####

system.time( 
  source("./setup_ambiente.R")
)

# Arquivo 08-TCC_taxa_ocupacao.R linha 532



arquivo = "./bases/SRAG_2021_a_2023/RESUMO_INTERNACOES.csv"
# arquivo = "./bases/SRAG_2021_a_2023/friburgo_INFLUD21-01-05-2023.csv"


ocupacao_carregada <- spark_read_csv(
  sc,
  delimiter = ";",
  name = "OCUPACAO_SRAG",
  path = arquivo,
  header = TRUE,
  infer_schema = TRUE
)

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
    # DT_NOTIFIC = to_date(DT_NOTIFIC, "d/M/y"),
    # DT_SIN_PRI = to_date(DT_SIN_PRI, "d/M/y"),
    # DT_NASC = to_date(DT_NASC, "d/M/y"),
    # DT_UT_DOSE = to_date(DT_UT_DOSE, "d/M/y"),
    # DT_VAC_MAE = to_date(DT_VAC_MAE, "d/M/y"),
    # DT_DOSEUNI = to_date(DT_DOSEUNI, "d/M/y"),
    # DT_1_DOSE = to_date(DT_1_DOSE, "d/M/y"),
    # DT_2_DOSE = to_date(DT_2_DOSE, "d/M/y"),
    # DT_ANTIVIR = to_date(DT_ANTIVIR, "d/M/y"),
    DT_INTERNA = to_date(DT_INTERNA, "d/M/y"),
    # DT_ENTUTI = to_date(DT_ENTUTI, "d/M/y"),
    # DT_SAIDUTI = to_date(DT_SAIDUTI, "d/M/y"),
    # DT_RAIOX = to_date(DT_RAIOX, "d/M/y"),
    # DT_EVOLUCA = to_date(DT_EVOLUCA, "d/M/y"),
    # DT_COLETA = to_date(DT_COLETA, "d/M/y"),
    # DT_PCR = to_date(DT_PCR, "d/M/y"),
    # DT_ENCERRA = to_date(DT_ENCERRA, "d/M/y"),
    # DT_DIGITA = to_date(DT_DIGITA, "d/M/y"),
    # DT_VGM = to_date(DT_VGM, "d/M/y"),
    # DT_TOMO = to_date(DT_TOMO, "d/M/y"),
    # DT_RES_AN = to_date(DT_RES_AN, "d/M/y"),
    # DT_CO_SOR = to_date(DT_CO_SOR, "d/M/y"),
    # DT_RES = to_date(DT_RES, "d/M/y"),
    # SEM_ENT_UTI = as.integer(date_format(to_date(DT_ENTUTI, "d/M/y"), "w")),
    # SEM_ENT_UTI = sql("CAST(CAST(unix_timestamp(DT_ENTUTI, 'MM/dd/yyyy') AS timestamp) AS date)")
    # SEM_ENT_UTI = isoweek(dmy(DT_ENTUTI)),
    # SEM_ENT_UTI =  dayofweek("DT_ENTUTI")
    # SEM_ENT_UTI = sql("weekofyear(DT_ENTUTI)")
    # SEM_ENT_UTI = sql("weekofyear(to_date(DT_ENTUTI, 'd/M/y'))"),
    # ANO_ENT_UTI = sql("year(to_date(DT_ENTUTI, 'd/M/y'))"),
    SEM_INTERNA = sql("weekofyear(to_date(DT_INTERNA, 'd/M/y'))"),
    ANO_INTERNA = sql("year(to_date(DT_INTERNA, 'd/M/y'))"),
    )

ocupacao <- ocupacao %>%
  filter(DT_INTERNA >= to_date("01/01/2021", "d/M/y")) %>% # Inicio dos dados de internacao
  filter(DT_INTERNA <= to_date("01/04/2023", "d/M/y")) # Ultimo dia da semana epidemiologica 13

# Acho que estou brigando demais com o spark
# Vou usar alguns comandos linux para ganhar tempo e assertividade
# Estou interessado em tres campos basicamente, e preciso criar a semana epidemiologica a partir da data da internacao por srag 
# pwd 
# cd '/home/administrator/Desktop/Guarda/personal/MBA/aulas/Big Data e Deployment de Modelos II/Arquivos e Scripts 28.04.2023/TCC/bases/SRAG_2021_a_2023'
# awk -F';' 'BEGIN { OFS=";" }  {print $26,$70,$77,$155}' INFLUD*.csv > RESUMO_INTERNACOES.csv
# "CO_MUN_RES";"DT_INTERNA";"DT_ENTUTI";"VACINA_COV"
# wc -l RESUMO_INTERNACOES.csv
# 2350258 RESUMO_INTERNACOES.csv

# Os valores válidos para o campo 4, VACINA_COV, são
# 1-Sim
# 2-Não
# 9-Ignorado 
# Informar se o paciente recebeu vacina COVID-19, após verificar a documentação

# Temos 445054 faltantes, e 569 inesperados.
# Considerando que o banco de dados prevê que esse dado pode ser "ignorado"
# vou considerá-lo assim para as análises
# Isso certamente é ruim, pois pode gerar um viés se não tratado adequadamente

# No entanto não esou buscando necessariamente se houve uma variação na taxa de vacinados que foram internados
# mas sim na taxa de internação em função da taxa de vacinação da população

# Nesse sentido estou olhando muito mais para o efeito em grande escala, não descartando um
# possível efeito de proteção de rebanho
# Amicus Plato, sed magis amica Veritas

# cut -d\; -f4 RESUMO_INTERNACOES.csv | sort | uniq -c 
# 445054 
# 4 ""
# 772302 1
# 830811 2
# 1 3
# 568 4
# 301515 9
# 3 "VACINA_COV"


# https://stackoverflow.com/questions/46086756/epi-week-query-method-for-sql-server
      
# SEMANA EPIDEMIOLOGICA ####
# http://www.portalsinan.saude.gov.br/calendario-epidemiologico?layout=edit&id=168

# NO coleta_ocupacao <- collect(ocupacao)

# NO rj_por_semana  <- coleta_ocupacao %>%
#   filter(SG_UF == 'RJ') %>% 
#   filter(!is.na(SEM_ENT_UTI))

# Remove filtro de vacina; não estou olhando apenas vacinados, e sim internações totais
# ocupacao <-ocupacao %>% filter(VACINA_COV==1)

por_municipio_semanas <- ocupacao %>%
  # filter(SG_UF == 'RJ') %>% 
  filter(!is.na(SEM_INTERNA)) %>% 
  group_by(CO_MUN_RES, ANO_INTERNA, SEM_INTERNA) %>%
  # group_by(CD_GEOCODI, SEM_ENT_UTI) %>%
  summarize(
    ocorrencias = n(),
    .groups = 'drop'
  )
#  NO coleta_por_municipio_semanas <- collect(por_municipio_semanas)

por_regiao_intermediaria_semanas <- ocupacao %>%
  # filter(SG_UF == 'RJ') %>% 
  filter(!is.na(SEM_INTERNA)) %>% 
  # mutate(rgi = as.numeric(substr(CD_GEOCODI,1,6))) %>%
  group_by(cod_rgi, ANO_INTERNA, SEM_INTERNA) %>%
  summarize(
    ocorrencias = n(),
    .groups = 'drop'
  )

por_semana <- ocupacao %>%
  # filter(SG_UF == 'RJ') %>% 
  filter(!is.na(SEM_INTERNA)) %>% 
  # mutate(rgi = as.numeric(substr(CD_GEOCODI,1,6))) %>%
  group_by(ANO_INTERNA, SEM_INTERNA) %>%
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

# Taxa de internação hospitalar:
# https://www.proadess.icict.fiocruz.br/index.php?pag=fic_r&cod=A51&tab=1

# Numerador: número de internações hospitalares de residentes pagas pelo SUS x 1.000. Denominador: população total residente, no período considerado.

coletado_R_semanas <- collect(por_municipio_semanas)
coletado_R_semanas <- coletado_R_semanas %>% inner_join(populacao_municipios, by  = join_by(CO_MUN_RES == code_muni))
coletado_R_semanas <- coletado_R_semanas %>% mutate(taxa_internacoes_srag_por_mil = (ocorrencias/populacao_municipio) * 1000)

coletado_R_regiao_intermediaria_semanas <- collect(por_regiao_intermediaria_semanas)
coletado_R_regiao_intermediaria_semanas <- coletado_R_regiao_intermediaria_semanas %>% inner_join(populacao_regiao_imediata, by = "cod_rgi")
coletado_R_regiao_intermediaria_semanas <- coletado_R_regiao_intermediaria_semanas %>% 
  mutate(taxa_internacoes_srag_por_mil = (ocorrencias/populacao_rgi) * 1000)
taxa_internacao_por_regiao_intermediaria_por_semana <- coletado_R_regiao_intermediaria_semanas



#####################
# ocupacao_por_regiao_imediata = 
vacina_internacao <-left_join(
  x = evolucao_vacina_rgi_semana %>% filter(SEM_VACINA != 53),
  y = taxa_internacao_por_regiao_intermediaria_por_semana,
  by = c("cod_rgi" = "cod_rgi", "ANO_VACINA"="ANO_INTERNA", "SEM_VACINA" = "SEM_INTERNA")
) %>% mutate(taxa_vacinacao = (populacao_totalmente_vacinada/populacao_rgi))
  


#####################
# coletado_R_regiao_intermediaria_semanas sumariza por cod_rgi, ano e weekofyear ####


# ATE AQUI ESTA BATENDO 

library('ISOweek')
library(stringr)

# fri <- coletado_R_regiao_intermediaria_semanas %>%  filter(cod_rgi == "330008") %>% select(ANO_INTERNA, SEM_INTERNA, taxa)
# 
# amostra <- coletado_R_regiao_intermediaria_semanas %>%  filter(cod_rgi %in% c("330008","430035")) %>% select(cod_rgi, ANO_INTERNA, SEM_INTERNA, taxa)

vacina_internacao <- vacina_internacao %>%  mutate(semana_iso = ISOweek2date(paste0(ANO_VACINA,"-W",str_pad(SEM_VACINA,2,pad = "0"),"-1")))
# z <- coletado_R_regiao_intermediaria_semanas %>%  mutate(semana_iso = ISOweek2date(paste0(ANO_INTERNA,"-W",str_pad(SEM_INTERNA,2,pad = "0"),"-1")))

# $$$ sf_vacina_internacao tem coordenadas, taxa de vacinacao e taxa de internacao <<<<< ####
sf_vacina_internacao <-inner_join(sf_vacinas, vacina_internacao, by = join_by(rgi == cod_rgi)) 

# pedacinho <- sf_vacina_internacao %>%  filter(rgi == 330001)
# st_write(pedacinho , "./taxa_internacao/taxa_internacao.shp", append=FALSE )

# Exporta mapa taxa internacoes por semana
pre_wide <- vacina_internacao %>% 
  select(cod_rgi,
         semana_iso,
         taxa_internacoes_srag_por_mil
         ) %>% 
  drop_na()

record_rgi_interna_max <- aggregate(taxa_internacoes_srag_por_mil ~ cod_rgi, pre_wide, max)
record_rgi_interna_min <- aggregate(taxa_internacoes_srag_por_mil ~ cod_rgi, pre_wide, min)
hist(record_rgi_interna_max$taxa_internacoes_srag_por_mil)
hist(record_rgi_interna_min$taxa_internacoes_srag_por_mil)


record_semana_interna_max <- aggregate(taxa_internacoes_srag_por_mil ~ semana_iso, pre_wide, max)
record_semana_interna_min <- aggregate(taxa_internacoes_srag_por_mil ~ semana_iso, pre_wide, min)
record_semana_interna_med <- aggregate(taxa_internacoes_srag_por_mil ~ semana_iso, pre_wide, mean)
record_semana_interna_max_min <- inner_join(record_semana_interna_max,record_semana_interna_min, by = join_by(semana_iso == semana_iso) )
plot(record_semana_interna_max)
plot(record_semana_interna_min)
plot(record_semana_interna_max_min)

# record_semana_interna_max_min <- zscore(record_semana_interna_max_min)

record_semana_interna_max_min$taxa_internacoes_srag_por_mil.x <- transform(record_semana_interna_max_min$taxa_internacoes_srag_por_mil.x, method="zscore")
record_semana_interna_max_min$taxa_internacoes_srag_por_mil.y <- transform(record_semana_interna_max_min$taxa_internacoes_srag_por_mil.y, method="zscore")

matplot(record_semana_interna_max_min, type = "b")
ts.plot(ts(record_semana_interna_max_min$taxa_internacoes_srag_por_mil.x), ts(record_semana_interna_max_min$taxa_internacoes_srag_por_mil.y), gpars = list(col = c("black", "red")))


# mmmm CUIDADO AQUI, NÃO TENHO CERTEZA SE FAZ SENTIDO ANALISAR A VARIACAO DA TAXA DE INTERNACAO ASSIM
lines_semana_interna <- inner_join(
  record_semana_interna_max %>% 
    mutate(
      internacoes_max = taxa_internacoes_srag_por_mil,
      anterior_internacoes_max = lag(taxa_internacoes_srag_por_mil),
      variacao_internacoes_max = internacoes_max - anterior_internacoes_max
           ),
  record_semana_interna_min %>%  mutate(taxa_internacoes_min = taxa_internacoes_srag_por_mil),
  by = c("semana_iso") )

lag()

ts.plot(ts(record_semana_interna_max_min$taxa_internacoes_srag_por_mil.x), ts(record_semana_interna_max_min$taxa_internacoes_srag_por_mil.y), gpars = list(col = c("black", "red")))

sf_long_internacao <-left_join(sf_vacinas, pre_wide, by = join_by(rgi == cod_rgi))

st_write(sf_vacinas , "./long/sf_vacinas.shp", append=FALSE )

write.csv(pre_wide,file='./long/sf_vacinas.csv',fileEncoding = "UTF-8")
+++++

st_write(sf_long_internacao , "./long/long_taxa_internacao.shp", append=FALSE )

# wide_internacao <- pivot_wider(data = pre_wide, names_from = "semana_iso", values_from = "taxa_internacoes_srag_por_mil" ,names_sort = TRUE)
wide_internacao <- pivot_wider(data = pre_wide, names_from = "semana_iso", values_from = "taxa_internacoes_srag_por_mil" ,names_sort = TRUE)
# Nomes no shapfile podem ter no maximo 10 caracteres
# estou removendo o hifen dos campos com as datas (como 2021-01-04) e incluindo um i na frente
# para representar as internacoes
wide_internacao <- wide_internacao %>% rename_at(vars(starts_with("20")), ~str_c("i",str_replace_all(.,"-",""))) 
sf_internacao <-left_join(sf_vacinas, wide_internacao, by = join_by(rgi == cod_rgi))


st_write(sf_internacao , "./evolucao/taxa_internacao.shp", append=FALSE )


# Exporta mapa taxa vacinacao por semana
pre_wide <- vacina_internacao %>% 
  select(cod_rgi,
         semana_iso,
         taxa_vacinacao
  ) %>% 
  drop_na()
wide_vacinacao <- pivot_wider(data = pre_wide, names_from = "semana_iso", values_from = "taxa_vacinacao", names_sort = TRUE)
wide_vacinacao <- wide_vacinacao %>% rename_at(vars(starts_with("20")), ~str_c("v",str_replace_all(.,"-","")))
sf_vacinacao <-left_join(sf_vacinas, wide_vacinacao, by = join_by(rgi == cod_rgi)) 
st_write(sf_vacinacao , "./evolucao/taxa_vacinacao.shp", append=FALSE )

wide_int_vac <- left_join(wide_internacao, wide_vacinacao, by = join_by(cod_rgi == cod_rgi)) 
sf_wide_int_vac <-left_join(sf_vacinas, wide_int_vac, by = join_by(rgi == cod_rgi))
st_write(sf_wide_int_vac , "./evolucao/internacao_vacinacao.shp", append=FALSE )

~----
tm_view(text.size.variable = TRUE)

# tmap_mode("plot")
tmap_mode("view")

sf_vacina_internacao %>%
  filter( semana_iso == "2022-04-04") %>%
  tm_shape(simplify = 0.5) +
  # tm_polygons(col = "taxa_populacao_totalmente_vacinada", id = "nome_rgi.x") +
  tm_polygons(col = "taxa_internacoes_srag_por_mil",
              # col = "taxa_internacao",
              id = "nome_rgi",
              # style = "quantile",
              n = 8,
              popup.vars = c("rgi","populacao_rgi","ocorrencias", "taxa_internacoes_srag_por_mil","taxa_vacinacao", "semana_iso"),
              # ,"populacao_totalmente_vacinada"
              palette = "viridis"
  ) +
  tm_text("nome_rgi", size = "taxa_internacoes_srag_por_mil", root = 5, remove.overlap = FALSE) 




###############################################################
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



