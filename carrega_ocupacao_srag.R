################################################################################
#                      DISPONIBILIZACAO DA OCUPACAO POR SRAG                   #
################################################################################

# Melhorar para receber parametros

carrega_ocupacao_srag <- function(){
    
arquivo = "./bases/SRAG_2021_a_2023/INFLUD21-01-05-2023.csv"

ocupacao_carregada <- spark_read_csv(sc,
                                     delimiter = ";",
                                     name = "OCUPACAO_SRAG", 
                                     path = arquivo, 
                                     header=TRUE, 
                                     infer_schema=TRUE)


# Ajusta o formato das colunas de data
ocupacao <- ocupacao_carregada %>% mutate(
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
}