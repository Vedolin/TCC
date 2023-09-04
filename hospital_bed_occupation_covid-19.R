# Studying covid hospitalization dataset
# https://opendatasus.saude.gov.br/dataset/registro-de-ocupacao-hospitalar-covid-19
# By 2022-06-28 there was no data dictionary provided

# https://www.youtube.com/watch?v=plWwstI2AEI
# https://mobilidadeativa.org.br/como-usar-as-bases-do-datasus/

# Parece que não vai dar para escapar do tabnet:
# http://tabnet.datasus.gov.br/cgi/tabcgi.exe?sih/cnv/nibr.def

# Informações coletadas e organizadas pela Fiocruz, inclusive com tutoriais de acesso
# https://pcdas.icict.fiocruz.br/tutoriais/tutorial-sih/


####################################################################################################
####################################################################################################
####################################################################################################

# Recomeçando em 2023-05-20
# OBTENÇÃO DE DADOS
# Obtendo bases de ocupação hospitalar por SRAG de 2021 a 2023 (apesar do nome do recurso) em
# https://dados.gov.br/dados/conjuntos-dados/srag-2021-e-2022
# INFLUD21-01-05-2023.csv
# INFLUD22-03-04-2023.csv
# INFLUD23-03-04-2023.csv





# esus-vepi.LeitoOcupacao_2020.csv
# esus-vepi.LeitoOcupacao_2021.csv
# esus-vepi.LeitoOcupacao_2021.csv

# E isto? https://www.gov.br/saude/pt-br/composicao/svsa/vigilancia-de-doencas-cronicas-nao-transmissiveis/sistemas-de-informacao-em-saude

library(tidyverse)
# library(esquisse)
# library(lubridate)

# https://arrow.apache.org/docs/r/
library(arrow)

# library(readr)

# file <- "bases/hospital_bed/esus-vepi.LeitoOcupacao_2022.csv"


file <- "./bases/SRAG_2021_a_2023/INFLUD21-01-05-2023.csv"

header <- read.delim(file=file,nrows=1, sep=";",header=TRUE)
print(header)


raw_occupation_2022 <- read_delim_arrow(
  file,
  delim = ";",
  quote = "\"",
  escape_double = TRUE,
  escape_backslash = FALSE,
  schema = NULL,
  col_names= TRUE,
  # col_types = schema( field("DT_NOTIFIC", date32(),locale(encoding = "ISO-8859-1")), field("SEM_NOT",int32())),
  # col_types = schema( field("DT_NOTIFIC", string()), field("SEM_NOT",int32())),
  # col_select = c("DT_NOTIFIC", "SEM_NOT"),
  # col_types = schema(y = utf8()),
  na = c("", "NA"),
  quoted_na = TRUE,
  skip_empty_rows = TRUE,
  skip = 0L,
  parse_options = NULL,
  convert_options = NULL,
  read_options = NULL,
  as_data_frame = TRUE,
  timestamp_parsers = NULL
)

str(raw_occupation_2022)
# glimpse(raw_occupation_2022)
dim(raw_occupation_2022)

source("explora_cnes.R")

# Removendo coluna anonima
occupation_2022 <- raw_occupation_2022 %>% dplyr::select("_id":"_updated_at")

# AGRUPAR POR E CONTAR POR MUNICIPIO PARA PLOTAR A CONTAGEM

occupation_por_municipio <- occupation_2022 %>% group_by(municipioNotificacao) %>%summarise(notificacoes_covid = sum(ocupacaoCovidUti))

# Inserindo a base de dados cnes ao nosso shapefile:
occupation_2022 %>% 
  left_join(cnes, by = c("cnes" = "CNES")) -> occupation_cnes

occupation_cnes <- occupation_cnes %>% filter(! is.na(munResLon))

sf_occupation_cnes <- st_as_sf(x = occupation_cnes,
                    coords = c("munResLon","munResLat"),
                    crs = 4326)

# Plotando o objeto sf_cnes de forma espacial:
tm_shape(shp = sf_occupation_cnes) + 
  tm_dots(size = 1)

# Visualizando o shapefile carregado:
tmap_mode("view")



# Carregando um shapefile de RJ:
shape_rj <- readOGR(dsn = "./shapes/RJ_Municipios_2021", 
                    layer = "RJ_Municipios_2021",
                    encoding = "UTF-8", 
                    use_iconv = TRUE)

# shape_rj %>% 
#   kable() %>%
#   kable_styling(bootstrap_options = "striped", 
#                 full_width = TRUE, 
#                 font_size = 12)

tm_shape(shp = shape_rj) +
  tm_borders()+
  tm_dots(col = "#39568CFF", size = 0.08)

tmap_mode("plot")
