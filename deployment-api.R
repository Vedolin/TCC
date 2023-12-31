################################################################################
#               INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS               #
################################################################################

# função para instalar e carregar os pacotes necessários
instalar_carregar_pacotes <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# alterar caminho da variável de ambiente do python
# Sys.setenv(MLFLOW_BIN="./venv/Scripts/mlflow")
Sys.setenv(MLFLOW_BIN="/home/administrator/Desktop/Guarda/personal/MBA/aulas/Big Data e Deployment de Modelos II/Arquivos e Scripts 28.04.2023/deployment-modelos/venv/bin/mlflow")

# alterar caminho da variável de ambiente do python
# Sys.setenv(MLFLOW_PYTHON_BIN="./venv/Scripts/python")
Sys.setenv(MLFLOW_PYTHON_BIN="/home/administrator/Desktop/Guarda/personal/MBA/aulas/Big Data e Deployment de Modelos II/Arquivos e Scripts 28.04.2023/deployment-modelos/venv/bin/python")

# Sys.setenv(MLFLOW_BIN=system("which mlflow"))
# Sys.setenv(MLFLOW_PYTHON_BIN=system("which python"))

# lista de pacotes necessários
pacotes <- c("sparklyr", 
             "ggcorrplot",
             "dplyr", 
             "caret",
             "nlme",
             "ggplot2", 
             "carrier", 
             "mlflow", 
             "reticulate", 
             "stats", 
             "glue")

# instalar e carregar os pacotes
instalar_carregar_pacotes(pacotes)

################################################################################
#                       ENTENDIMENTO DO NEGÓCIO                                #
################################################################################

# Exemplo didático:
# Um município possui dados referentes a desempenho de alunos de algumas escolas
# em função de outras variáveis como tempo de experiência do professor e horas de
# estudo

# Objetivo:
# Desenvolver um modelo capaz de auxiliar na tomada de decisão referente a ações
# que podem ser criadas e monitoradas para melhorar o desempenho médio dos 
# estudantes das escolas da base disponível

# Apontamentos:
# 1. As escolas são de lugares distintos do município e de diversas classes sociais
#
# Variavél dependente: desempenho

################################################################################
#                      CONHECENDO NOSSA BASE DE DADOS                          #
################################################################################

# carregando a base de dados
estudante_escola <- read.csv("estudante_escola.csv")

# verificando nossas variáveis e observações
glimpse(estudante_escola)

# ajustando os tipos das variáveis categóricas
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
