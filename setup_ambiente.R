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
             "glue",
             "arrow",
             "tsibble",
             "sparklyr", 
             "nlme",
             "carrier",
             "epitools",
             "geobr",
             "sf",
              "SpatialEpi",
             "tmap",
             "rgeos",
             "spdep",
             "epitools",
             "covid19br",
             "COVID19"
)

# instalar e carregar os pacotes
instalar_carregar_pacotes(pacotes)




################################################################################
#                           SETUP DO AMBIENTE SPARK                            #
################################################################################

# alterar caminho da variável de ambiente do python
# Sys.setenv(MLFLOW_BIN="./venv/Scripts/mlflow")
Sys.setenv(MLFLOW_BIN="/home/administrator/Desktop/Guarda/personal/MBA/aulas/Big Data e Deployment de Modelos II/Arquivos e Scripts 28.04.2023/deployment-modelos/venv/bin/mlflow")

# alterar caminho da variável de ambiente do python
# Sys.setenv(MLFLOW_PYTHON_BIN="./venv/Scripts/python")
Sys.setenv(MLFLOW_PYTHON_BIN="/home/administrator/Desktop/Guarda/personal/MBA/aulas/Big Data e Deployment de Modelos II/Arquivos e Scripts 28.04.2023/deployment-modelos/venv/bin/python")

# Sys.setenv(MLFLOW_BIN=system("which mlflow"))
# Sys.setenv(MLFLOW_PYTHON_BIN=system("which python"))


# desconectar alguma conexão ativa com o spark
spark_disconnect_all()

# conectar ao cluster spark local
# options(sparklyr.log.console = TRUE) # Isto ajuda a investigar eventuais problemas com a conexão ao spark

config <- spark_config()
config$spark.sql.legacy.timeParserPolicy <- "LEGACY" # Devido a incompatibilidade de formato de data
sc <- spark_connect(master = "local",
                    config = config,
                    spark_home="~/Desktop/Guarda/personal/MBA/aulas/Big Data e Deployment de Modelos I/aula-big-data/spark/build/spark-3.4.0-bin-hadoop3")


# Apagar a tabela no spark caso necessário
# tbl_name <- "INFLUD21"
# DBI::dbGetQuery(sc, paste("DROP TABLE", tbl_name))








