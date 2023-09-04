source("./setup_ambiente.R")
# source("./carrega_ocupacao_srag.R")


################################################################################
#               CARREGA OCUPACAO POR SRAG NO SPARK                             #
################################################################################

# Por exemplo, SRAG de 2021:
# path="./bases/SRAG_2021_a_2023/INFLUD21-01-05-2023.csv"

arquivo = "./bases/SRAG_2021_a_2023/INFLUD21-01-05-2023.csv"

ocupacao_carregada <- spark_read_csv(
  sc,
  delimiter = ";",
  name = "OCUPACAO_SRAG",
  path = arquivo,
  header = TRUE,
  infer_schema = TRUE
)

# Ajusta o formato das colunas de data
ocupacao <- ocupacao_carregada %>%
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
  )



################################################################################
# Fiz algumas tentativas de sumarizar todas as colunas de contagem por data,
# mas patinei. Deixando as tentativas aqui por enquanto

# por_data <- ocupacao %>%
#   group_by(DT_SIN_PRI) %>%
#   summarize(
#     count = n()
#     # , mean_dep_delay = mean(dep_delay, na.rm = FALSE)
#   )



# Isto é interessante, mas não é o que busco
# por_data <- ocupacao %>%
#   group_by(DT_SIN_PRI, DT_ENTUTI) %>%
#   tally()
  # summarise(across(everything(), n=n), .groups = 'drop')


# por_DT_SIN_PRI <- ocupacao %>%
#   group_by(DT_SIN_PRI) %>%
#   summarize(
#     count = n()
#   )
# 
# por_DT_1_DOSE <- ocupacao %>%
#   group_by(DT_1_DOSE) %>%
#   summarize(
#     count = n()
#   )
# 
# por_DT_2_DOSE <- ocupacao %>%
#   group_by(DT_2_DOSE) %>%
#   summarize(
#     count = n()
#   )
# 
# 
# 
# por_data <- ocupacao %>%
#   group_by(DT_SIN_PRI) %>%
#   add_count(DT_SIN_PRI)

# por_municipio_semanasintoma <- ocupacao %>%
#   group_by(CO_MUN_RES, SEM_PRI) %>%
#   summarize(
#     count = n(),
#     .groups = 'drop'
#   )

################################################################################


# contagem de linhas do dataset
#sdf_nrow(ocupacao)


# glimpse(ocupacao)

# Trabalhando amostras apenas com RJ

# por_municipio_entrada_uti <- ocupacao %>%
#   filter(SG_UF == 'RJ') %>% 
#   filter(!is.na(SEM_ENT_UTI)) %>% 
#   filter(SEM_ENT_UTI == 21) %>% # ******************************************
#   group_by(CO_MUN_RES, SEM_ENT_UTI) %>%
#   summarize(
#     ocorrencias = n(),
#     .groups = 'drop'
#   )
# 
# coletado_R <- collect(por_municipio_entrada_uti)
# 
# 
# ts_coletado_R <- ts(coletado_R)

# plot(ts_coletado_R)



# Prepara dados geoespaciais

# sedes_municipios <- read_municipal_seat()
# sedes_municipios_RJ <- sedes_municipios %>%
#                     filter(code_state == 33) %>% 
#                     mutate(code_muni = as.numeric(substr(code_muni,1,6))
#                     )
# 
# tmap_mode("view")
# tm_shape(sedes_municipios_RJ) +
#   tm_dots()
  # + tm_dots("name_muni")

# Não preciso baixar a todo momento
municipios <- read_municipality(code_muni = "RJ", year = 2020)
municipios_RJ <- municipios %>%
                 filter(code_state == 33) %>%
                 mutate(code_muni = as.numeric(substr(code_muni,1,6))
                  )



#####################################################################################
#######                        VIZINHANCA                           #################
#####################################################################################

# Estabelecendo vizinhanças por contiguidade, critério queen:
vizinhos_queen <- poly2nb(pl = spatial_municipios_RJ,
                          queen = TRUE,
                          row.names = spatial_municipios_RJ@data$name_muni)


# Informações relevantes sobre a vizinhança queen estabelecida:
summary(vizinhos_queen)



# Ok! Cadê a matriz W?
matrizW_queen <- nb2mat(neighbours = vizinhos_queen,
                        style = "B")

# Notaram o erro reportado pelo R? O erro diz, explicitamente, que há 
# observações sem uma vizinhança definida. Noutras palavras, há a ocorrência de 
# ilhas:
plot(spatial_municipios_RJ, border = "lightgray")
plot(vizinhos_queen, 
     coordinates(spatial_municipios_RJ), 
     add = TRUE, 
     col = "#33638DFF")

# No caso, a ilha diz respeito ao município de Ilhabela. Para contornar a
# situação, podemos utilizar o argumento zero.policy = TRUE.
matrizW_queen <- nb2mat(neighbours = vizinhos_queen,
                        style = "B",
                        zero.policy = TRUE)

# Para facilitar o estudo da nossa matriz W, podemos comandar:
colnames(matrizW_queen) <- spatial_municipios_RJ@data$NM_MUNICIP

View(matrizW_queen)



#####################################################################################
#######                        VIZINHANCA                           #################
#####################################################################################



# join municipios_RJ a coletado_R

# merged <- merge( x = spatial_municipios_RJ,
#                  y = coletado_R,
#                  by.x = "code_muni",
#                  by.y = "CO_MUN_RES"
#                 )
# 
# tmap_mode("view")
# tm_shape(merged)  +
#   tm_fill("ocorrencias") +
#   # tm_fill("ocorrencias", popup.vars = c("name_muni","ocorrencias")) +
#   tm_borders("black", lwd = .5) +
#   tm_markers()
# # + tm_markers(text="name_muni")
# 
# tmap_mode("plot")

##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################
##################################################################################################


por_municipio_semanas <- ocupacao %>%
  filter(SG_UF == 'RJ') %>% 
  filter(!is.na(SEM_ENT_UTI)) %>% 
  # filter(SEM_ENT_UTI == 21) %>% # ******************************************
  group_by(CO_MUN_RES, SEM_ENT_UTI) %>%
  summarize(
    ocorrencias = n(),
    .groups = 'drop'
  )


coletado_R_semanas <- collect(por_municipio_semanas)
#coletado_R_semanas <- coletado_R_semanas %>%  mutate(code_muni = CO_MUN_RES)

# merged_semanas <- merge( x = municipios_RJ,
#                  y = coletado_R_semanas[,c("SEM_ENT_UTI","ocorrencias")],
#                  by.x = "code_muni",
#                  by.y = "CO_MUN_RES"
# )


###************************************##############

coletado_R_semanas <- coletado_R_semanas %>%  rename(code_muni = CO_MUN_RES)

spatial_municipios_RJ  <- as_Spatial(municipios_RJ)

# Juntando a base de dados ao shapefile do Estado de RJ:
spatial_municipios_RJ@data %>% 
  # rename(code_muni = 1) %>%
  # mutate(code_muni = as.numeric(code_muni)) %>% 
  left_join(coletado_R_semanas, by = "code_muni") -> spatial_municipios_RJ@data

###************************************##############





joined_semanas <- left_join(
                    municipios_RJ,
                    coletado_R_semanas,
                    by= join_by(code_muni == CO_MUN_RES)
                  )

joined_semanas <-joined_semanas %>%
                  filter(SEM_ENT_UTI >=20) %>%
                  filter(SEM_ENT_UTI <=32)# Escolhi arbitrariamente para não esgotar o uso de memoria

tmap_mode("plot")

joined_semanas  %>%  filter(SEM_ENT_UTI ==20)  %>% 
tm_shape() +
  tm_fill("ocorrencias",
          # style = "quantile",
          # n = 8,
          legend.hist = TRUE,
          palette = "viridis"
  ) +
  tm_layout(legend.outside = TRUE ) +
  # tm_fill("ocorrencias", popup.vars = c("name_muni","ocorrencias")) + 
  tm_borders("black", lwd = .5) +
  # tm_markers() +
  # tm_facets(by = "SEM_ENT_UTI", ncol = 3) +
  # tm_facets(along = "SEM_ENT_UTI") +
  tm_layout(legend.outside.size = 0.2)

joined_semanas %>% 
  ggplot() +
  geom_sf()
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
