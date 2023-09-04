options(scipen = 999) 

################################################################################
#               INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS               #
################################################################################

# função para instalar e carregar os pacotes necessários
instalar_carregar_pacotes <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# lista de pacotes necessários
pacotes <- c("COVID19", "dplyr", "lubridate", "arrow", "tsibble", "ggplot2", "plotly", "forecast","tidyr")

# instalar e carregar os pacotes
instalar_carregar_pacotes(pacotes)

################################################################################
#               EVITA REFAZER DOWNLOAD E PROCESSAMENTO DESNECESSARIAMENTE      #
# SE REFIZER TUDO O TEMPO E MEMÓRIA GASTOS SÃO MUITO GRANDES                   #
# user  system elapsed
# 40.415  17.078  71.562

usar_guardado_local <- TRUE

################################################################################

if (usar_guardado_local) {
  covid_vacinas <- read_parquet(file = "./populacao_totalmente_vacinada.parquet")
  # covid_vacinas <- covid_vacinas %>% dplyr::mutate(people_fully_vaccinated = replace_na(people_fully_vaccinated, 0))
} else {
  ################################################################################
  #                          BAIXA DADOS BRASIL POR MUNICPIO                     #
  ################################################################################
  
  # library(COVID19)
  downloaded_covid_vacinas <-
    covid19(country = c("Brazil"), level = 3)
  
  populacao_totalmente_vacinada <- downloaded_covid_vacinas %>%
    dplyr::select(
      date,
      people_fully_vaccinated,
      key_local
    ) %>%
    mutate (
      code_muni = as.numeric(substr(key_local, 1, 6)),
    ) %>%
    select(-key_local)
  
  # covid_vacinas <- downloaded_covid_vacinas %>%
  #   dplyr::select(
  #     date,
  #     confirmed,
  #     deaths,
  #     vaccines,
  #     people_vaccinated,
  #     people_fully_vaccinated,
  #     population,
  #     key_local
  #   ) %>%
  #   mutate (
  #     code_muni = as.numeric(substr(key_local, 1, 6)),
  #     # EPI_SEM_MEDIDA = as.integer(strftime(as.Date(date), format = "%V"))
  #     EPI_SEM_MEDIDA = epiweek(date),
  #     EPI_ANO_MEDIDA = epiyear(date)
  #   ) %>%
  #   select(-key_local)
  
  # write_parquet(
  #   downloaded_covid_vacinas,
  #   "./downloaded_covid_vacinas.parquet",
  #   chunk_size = NULL,
  #   version = "2.4",
  #   compression = "snappy",
  #   compression_level = NULL,
  #   use_dictionary = NULL,
  #   write_statistics = NULL,
  #   data_page_size = NULL,
  #   use_deprecated_int96_timestamps = FALSE,
  #   coerce_timestamps = NULL,
  #   allow_truncated_timestamps = FALSE
  # )
  
  
  write_parquet(
    populacao_totalmente_vacinada,
    "./populacao_totalmente_vacinada.parquet",
    chunk_size = NULL,
    version = "2.4",
    compression = "snappy",
    use_deprecated_int96_timestamps = FALSE,
    coerce_timestamps = NULL,
    allow_truncated_timestamps = FALSE
  )
  
  downloaded_covid_vacinas <- NULL
  populacao_totalmente_vacinada <- NULL
  
  populacao_totalmente_vacinada <- read_parquet(file = "./populacao_totalmente_vacinada.parquet")
  
}

# Taxa de mortalidade específica por causas selecionadas
# http://tabnet.datasus.gov.br/cgi/idb1997/mort/fqc09.htm#:~:text=Defini%C3%A7%C3%A3o%20%3A,em%20determinado%20local%20e%20per%C3%ADodo.

#     Nº de óbitos pela causa específica, em determinado local e período
#     -------------------------------------------------------------------  x100.000
#                População total do mesmo local e período

# 
# municipios <- c(330340, 312340, 355030)

# morte_vacina <- covid_vacinas %>%
#   filter(code_muni == 330340) %>%
#   filter(!is.na(people_fully_vaccinated )) %>% 
#   mutate(taxa = (deaths / confirmed)) %>% 
#   dplyr::select(date,code_muni, taxa)
# %>%
#   as_tsibble(
#     index = date,
#     key = taxa
#   )

# covid19 <- covid_vacinas %>%
#   filter(code_muni %in% municipios) %>%
#   filter(!is.na(people_fully_vaccinated )) %>% 
#   mutate(taxa_fully_vaccinated = (people_fully_vaccinated / population) * 100) %>% 
#   dplyr::select(date,code_muni, taxa_fully_vaccinated)


# visualizando a série com uma linha por code_muni
# ggplotly(
#   covid19 %>%
#     mutate(Data = date,
#            media_movel = ma(taxa_fully_vaccinated, order=14)) %>%
#     ggplot() +
#     # aes(x = Data , y = taxa, group = code_muni) +
#     geom_line(aes(x = Data, y = taxa_fully_vaccinated, group = code_muni, color = as.factor(code_muni))) +
#     # geom_line(aes(x = Data, y = media_movel, color = "Média Móvel"), size = 1) +
#     # ggplot(morte_vacina_2) +
#     labs(color = "Legenda:",
#          x = "Data",
#          y = "Percentual da população totalmente vacinada") +
#     scale_color_viridis_d() +
#     scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") +
#     theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
#           panel.background = element_rect(fill = "white", color = "black"),
#           panel.grid = element_line(color = "grey90"),
#           panel.border = element_rect(color = "black", fill = NA),
#           legend.position = "bottom")
# ) %>% layout(showlegend = TRUE,
#              legend = list(orientation = "h"))

# morte_vacina %>% 
#   mutate(Data = date,
#          media_movel = ma(taxa, order=14)) %>%
#   ggplot() +
#   geom_boxplot(aes(x = Data, y = taxa, color = "Por Dia")) +
#   labs(x = "Data",
#        y = "Comportamento da Covid-19") +
#   scale_color_viridis_d() +
#   scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
#         panel.background = element_rect(fill = "white", color = "black"),
#         panel.grid = element_line(color = "grey90"),
#         panel.border = element_rect(color = "black", fill = NA),
#         legend.position = "none")



# 
# 
# ggplot(covid19) +
#   aes(x = date, y = taxa_fully_vaccinated, colour = code_muni) +
#   geom_line() +
#   scale_color_viridis_d(option = "viridis", direction = 1) +
#   theme_minimal()
# 
# 
# morte_vacina_esquisse <- covid_vacinas %>%
#   filter(code_muni %in% municipios) %>%
#   filter(!is.na(people_fully_vaccinated )) %>% 
#   mutate(taxa = (deaths / confirmed))

# morte_vacina_2_spread <- spread(morte_vacina_2,code_muni,taxa)

# elaborando gráficos com o ggplot2


# # visualizando a série com as médias móveis
# ggplotly(
#   morte_vacina %>%
#     mutate(Data = date,
#            media_movel = ma(taxa, order=14)) %>%
#     ggplot() +
#     geom_line(aes(x = Data, y = taxa, color = "Por Dia")) +
#     geom_line(aes(x = Data, y = media_movel, color = "Média Móvel"), size = 1) +
#     # ggplot(morte_vacina_2) +
#     labs(color = "Legenda:",
#          x = "Data",
#          y = "Comportamento da Covid-19") +
#     scale_color_viridis_d() +
#     scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") +
#     theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
#           panel.background = element_rect(fill = "white", color = "black"),
#           panel.grid = element_line(color = "grey90"),
#           panel.border = element_rect(color = "black", fill = NA),
#           legend.position = "bottom")
# ) %>% layout(showlegend = TRUE,
#              legend = list(orientation = "h"))
# 
# 
# 
# 
# # visualizando a série com uma linha por code_muni
# ggplotly(
#   morte_vacina_2 %>%
#     mutate(Data = date,
#            media_movel = ma(taxa, order=14)) %>%
#     ggplot() +
#     # aes(x = Data , y = taxa, group = code_muni) +
#     geom_line(aes(x = Data, y = taxa, group = code_muni, color = as.factor(code_muni))) +
#     # geom_line(aes(x = Data, y = media_movel, color = "Média Móvel"), size = 1) +
#     # ggplot(morte_vacina_2) +
#     labs(color = "Legenda:",
#          x = "Data",
#          y = "Comportamento da Covid-19") +
#     scale_color_viridis_d() +
#     scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") +
#     theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
#           panel.background = element_rect(fill = "white", color = "black"),
#           panel.grid = element_line(color = "grey90"),
#           panel.border = element_rect(color = "black", fill = NA),
#           legend.position = "bottom")
# ) %>% layout(showlegend = TRUE,
#              legend = list(orientation = "h"))
# 

# morte_vacina %>% 
#   mutate(Data = date,
#          media_movel = ma(taxa, order=14)) %>%
#   ggplot() +
#   geom_boxplot(aes(x = Data, y = taxa, color = "Por Dia")) +
#   labs(x = "Data",
#        y = "Comportamento da Covid-19") +
#   scale_color_viridis_d() +
#   scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.4),
#         panel.background = element_rect(fill = "white", color = "black"),
#         panel.grid = element_line(color = "grey90"),
#         panel.border = element_rect(color = "black", fill = NA),
#         legend.position = "none")
