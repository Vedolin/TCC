# Testa geostan
# Fazer source logo após 
# sf_vacina_internacao <-inner_join(sf_vacinas, vacina_internacao, by = join_by(rgi == cod_rgi)) 
# no carrega_e_disponibiliza_dados_no_ambiente.R

library(geostan)
library(ggplot2)
library(gridExtra)
data("georgia")

sf_use_s2(FALSE)

sf_amostra <- sf_vacina_internacao %>%
  # filter(rgi >= 330001) %>%
  # filter(rgi >  500000) %>%
  filter ( semana_iso == ymd('2022-09-12')) 
  # filter (semana_iso <= ymd('2022-12-31'))

sp_diag(sf_amostra$taxa_internacoes_srag_por_mil, sf_amostra, name = "Taxa internações SRAG por mil habitantes")
sp_diag(sf_amostra$taxa_vacinacao, sf_amostra, name = "Taxa vacinacao")

W <- shape2mat(sf_amostra, style = "W")
moran_plot(sf_amostra, W)

mc(sf_amostra$taxa_vacinacao, W)

