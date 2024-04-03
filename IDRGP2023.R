

# Intodução ---------------------------------------------------------------


## script para o monitoramento do IDRGP no exercício de 2023

## objetivo: levantar, tratar e analisar dados dos valores executados, nas regiões
## da cidade de são paulo a partir de uma cesta de desepesas selecionadas, que
## incluem o Programa de Metas 2021-2024 Versão Alteração Programática, uma lista
## de obras monitoradas e intervenções do Orçamento Cidadão


# Pacotes -----------------------------------------------------------------

install.packages("tidyverse")
library(tidyverse)

install.packages("readxl")
library(readxl)

install.packages("purrr")
library(purrr)

base::options(scipen = 99)



# Valores de referência IDRGP ---------------------------------------------


(ref_idrgp <- dplyr::tibble(sp_nome = c("Subprefeitura Capela do Socorro",
                                        "Subprefeitura M'Boi Mirim",
                                        "Subprefeitura Campo Limpo",
                                        "Subprefeitura São Mateus",
                                        "Subprefeitura Itaquera",
                                        "Subprefeitura Cidade Ademar",
                                        "Subprefeitura Freguesia/Brasilândia",
                                        "Subprefeitura São Miguel Paulista",
                                        "Subprefeitura Itaim Paulista",
                                        "Subprefeitura Pirituba/Jaraguá",
                                        "Subprefeitura Parelheiros",
                                        "Subprefeitura Jaçanã/Tremembé",
                                        "Subprefeitura Sapopemba",
                                        "Subprefeitura de Guaianases",
                                        "Subprefeitura Penha",
                                        "Subprefeitura Ipiranga",
                                        "Subprefeitura Cidade Tiradentes",
                                        "Subprefeitura Casa Verde/Cachoeirinha",
                                        "Subprefeitura Perus/Anhanguera",
                                        "Subprefeitura Butantã",
                                        "Subprefeitura Ermelino Matarazzo",
                                        "Subprefeitura de Vila Prudente",
                                        "Subprefeitura Sé",
                                        "Subprefeitura Vila Maria/Vila Guilherme",
                                        "Subprefeitura Aricanduva/Formosa/Carrão",
                                        "Subprefeitura Jabaquara",
                                        "Subprefeitura Mooca",
                                        "Subprefeitura Santana/Tucuruvi",
                                        "Subprefeitura Lapa",
                                        "Subprefeitura Santo Amaro",
                                        "Subprefeitura Vila Mariana",
                                        "Subprefeitura Pinheiros"),
  ref_idrgp = c(0.0708, 0.0706, 0.0616, 0.0511, 0.0487, 0.0483, 0.0456, 0.0419,
                0.0406, 0.0377, 0.0374, 0.0364, 0.0353, 0.0346, 0.0346, 0.0291,
                0.0278, 0.0270, 0.0258, 0.0250, 0.0210, 0.0183, 0.0179, 0.0167,
                0.0157, 0.0150, 0.0150, 0.0146, 0.0113, 0.0094, 0.0086, 0.0068)) %>% 
  arrange(desc(ref_idrgp)))


# Criar base de dados do PdM 2021-2024 ------------------------------------

(df_pdm1 <- readxl::read_excel("metas_alteracao_programatica.xlsx", sheet = "totais_metas", skip = 266) %>% 
   dplyr::select(-c("Diferença Soma dos valores proporcionais vs SMAE",
                    "Observações",
                    "...46",
                    "supra",
                    "...48",
                    "zero")))

## Regionalização pelo detalhamento da ação nas dotações utilizadas no pdm
(df_pdm1 <- df_pdm1 %>% 
    tidyr::pivot_longer(
      cols = c("Subprefeitura Aricanduva/Formosa/Carrão":"SE+SUPRACENTRO"),
      names_to = "sp_nome",
      values_to = "valor_da"))

base::sum(df_pdm1$valor_da, na.rm = TRUE)

df_pdm1 %>% 
  group_by(META) %>% 
  summarise(sum(unique(TOTAL_SMAE)))

## Regionalização pelo detalhamento da ação (exceto: metas de SMS e Meta 85, SMC, com informações diretas dos órgãos)
(df_pdm2 <- readxl::read_excel("metas_alteracao_programatica.xlsx", sheet = "totais_metas_inf_orgaos", skip = 266) %>% 
    dplyr::select(-c("Diferença Soma dos valores proporcionais vs SMAE", "Observações")) %>% 
    dplyr::rename("SE+SUPRACENTRO" = "SE+SUPRA_CENTRO"))

(df_pdm2 <- df_pdm2 %>% 
    tidyr::pivot_longer(
      cols = c("Subprefeitura Aricanduva/Formosa/Carrão":"SE+SUPRACENTRO"),
      names_to = "sp_nome",
      values_to = "valor_da_inf_orgao") %>% 
    dplyr::select(META, DOTACAO, sp_nome, valor_da_inf_orgao))

base::sum(df_pdm2$valor_da_inf_orgao, na.rm = TRUE)


## juntando df_pdm1 e df_pdm2
(df_pdm <- dplyr::full_join(df_pdm1, df_pdm2, by = c("META", "DOTACAO", "sp_nome")))

## checando warning message:
# Detected an unexpected many-to-many relationship between `x` and `y`.
# ℹ Row 8019 of `x` matches multiple rows in `y`.
# ℹ Row 2395 of `y` matches multiple rows in `x`

df_pdm[8019,]
df_pdm[2395,]


base::sum(df_pdm$valor_da, na.rm = TRUE)
base::sum(df_pdm$valor_da, na.rm = TRUE)-base::sum(df_pdm1$valor_da, na.rm = TRUE)
# Diferença de 55488642

base::sum(df_pdm$valor_da_inf_orgao, na.rm = TRUE)
base::sum(df_pdm$valor_da_inf_orgao, na.rm = TRUE)-base::sum(df_pdm2$valor_da_inf_orgao, na.rm = TRUE)
# Diferença de 55488642

### Necessário:
## remover duplicidade "Subprefeitura Sé" e "SE+SUBCENTRO"
## distribuir valores das supra regionais entre as subprefeituras de cada região
## acrescentar coluna com estimativa (TOTAL_SMAE/EXECUÇÃO FÍSICA)
## criar coluna com o código da ação orçamentária

(df_pdm <- df_pdm %>% 
  mutate("regiao" = case_when(
    sp_nome %in% c("Subprefeitura Aricanduva/Formosa/Carrão",
                   "Subprefeitura Cidade Tiradentes",
                   "Subprefeitura de Guaianases",
                   "Subprefeitura de Vila Prudente",
                   "Subprefeitura Ermelino Matarazzo",
                   "Subprefeitura Itaim Paulista",
                   "Subprefeitura Itaquera",
                   "Subprefeitura Mooca",
                   "Subprefeitura Penha",
                   "Subprefeitura São Mateus",
                   "Subprefeitura São Miguel Paulista",
                   "Subprefeitura Sapopemba",
                   "Supra Subprefeitura Leste") ~ "Leste",
    sp_nome %in% c("Subprefeitura Butantã",
                   "Subprefeitura Lapa",
                   "Subprefeitura Pinheiros",
                   "Supra Subprefeitura Oeste") ~ "Oeste",
    sp_nome %in% c("Subprefeitura Casa Verde/Cachoeirinha",
                   "Subprefeitura Freguesia/Brasilândia",
                   "Subprefeitura Jaçanã/Tremembé",
                   "Subprefeitura Perus/Anhanguera",
                   "Subprefeitura Pirituba/Jaraguá",
                   "Subprefeitura Santana/Tucuruvi",
                   "Subprefeitura Vila Maria/Vila Guilherme",
                   "Supra Subprefeitura Norte") ~ "Norte",
    sp_nome %in% c("Subprefeitura Sé",
                   "Supra Subprefeitura Centro",
                   "SE+SUPRACENTRO") ~ "Centro",
    TRUE ~ "Sul")))

df_pdm$valor_da_inf_orgao <- df_pdm$valor_da_inf_orgao %>% tidyr::replace_na(0) #substituir NA por 0

(df_exec_fisica <- dplyr::tibble(META = c(3, 4, 5, 7, 8, 9, 10, 12, 13, 14, 17, 25, 26, 27, 28, 36, 40, 42, 53, 54, 55, 59, 62, 72, 78, 79, 80, 81, 84),
                                resultado_2023 = c(10, 75, 3, 3, 1, 15, 1, 13373, 3942, 90429, 23, 4, 14, 12, 1, 376778, 325510, 1, 1, 1, 2, 3920, 2, 4, 2, 3, 554, 19, 3)))

(df_exec_reg <- readxl::read_excel("metas_alteracao_programatica.xlsx", sheet = "execucao_regionalizada", skip = 1))

df_exec_reg <- df_exec_reg %>% 
  rename("META" = "...1",
         "sp_nome" = "...2") %>% 
  mutate("exec_2023" = `2023`-`2022`) %>% 
  mutate("sp_nome" = case_when(
           sp_nome %in% "Aricanduva" ~ "Subprefeitura Aricanduva/Formosa/Carrão",
           sp_nome %in% "Butantã" ~ "Subprefeitura Butantã",
           sp_nome %in% "Campo Limpo" ~ "Subprefeitura Campo Limpo",
           sp_nome %in% "Capela do Socorro" ~ "Subprefeitura Capela do Socorro",
           sp_nome %in% "Casa Verde" ~ "Subprefeitura Casa Verde/Cachoeirinha",
           sp_nome %in% "Cidade Ademar" ~ "Subprefeitura Cidade Ademar",
           sp_nome %in% "Cidade Tiradentes" ~ "Subprefeitura Cidade Tiradentes",
           sp_nome %in% "Ermelino Matarazzo" ~ "Subprefeitura Ermelino Matarazzo",
           sp_nome %in% "Freguesia/Brasilândia" ~ "Subprefeitura Freguesia/Brasilândia",
           sp_nome %in% "Guaianases" ~ "Subprefeitura de Guaianases",
           sp_nome %in% "Ipiranga" ~ "Subprefeitura Ipiranga",
           sp_nome %in% "Itaim Paulista" ~ "Subprefeitura Itaim Paulista",
           sp_nome %in% "Itaquera" ~ "Subprefeitura Itaquera",
           sp_nome %in% "Jabaquara" ~ "Subprefeitura Jabaquara",
           sp_nome %in% "Jaçanã/Tremembé" ~ "Subprefeitura Jaçanã/Tremembé",
           sp_nome %in% "Lapa" ~ "Subprefeitura Lapa",
           sp_nome %in% "M'Boi Mirim" ~ "Subprefeitura M'Boi Mirim",
           sp_nome %in% "Mooca" ~ "Subprefeitura Mooca",
           sp_nome %in% "Parelheiros" ~ "Subprefeitura Parelheiros",
           sp_nome %in% "Penha" ~ "Subprefeitura Penha",
           sp_nome %in% "Perus/Anhanguera" ~ "Subprefeitura Perus/Anhanguera",
           sp_nome %in% "Pinheiros" ~ "Subprefeitura Pinheiros",
           sp_nome %in% "Pirituba/Jaraguá" ~ "Subprefeitura Pirituba/Jaraguá",
           sp_nome %in% "Santana/Tucuruvi" ~ "Subprefeitura Santana/Tucuruvi",
           sp_nome %in% "Santo Amaro" ~ "Subprefeitura Santo Amaro",
           sp_nome %in% "São Mateus" ~ "Subprefeitura São Mateus",
           sp_nome %in% "São Miguel Paulista" ~ "Subprefeitura São Miguel Paulista",
           sp_nome %in% "Sapopemba" ~ "Subprefeitura Sapopemba",
           sp_nome %in% "Sé" ~ "Subprefeitura Sé",
           sp_nome %in% "Vila Maria/Vila Guilherme" ~ "Subprefeitura Vila Maria/Vila Guilherme",
           sp_nome %in% "Vila Mariana" ~ "Subprefeitura Vila Mariana",
           TRUE ~ "Subprefeitura de Vila Prudente")) %>% 
  select(META, sp_nome, exec_2023)

(df_exec_reg <- df_exec_reg %>% 
  mutate("idrgp" = case_when(
    META %in% c(3, 4, 5, 7, 8, 9, 10, 12, 13, 14, 17, 25, 26, 27, 28, 36, 40, 42, 53, 54, 55, 59, 62, 72, 78, 79, 80, 81, 84) ~ "sim",
    TRUE ~ "nao"
  )) %>%
  filter(idrgp == "sim") %>% 
  select(META, sp_nome, exec_2023))

(df_pdm <- dplyr::full_join(df_pdm, df_exec_fisica, by = "META"))

(df_pdm <- dplyr::full_join(df_pdm, df_exec_reg, by = c("META", "sp_nome")))

df_pdm$exec_2023 <- df_pdm$exec_2023 %>% tidyr::replace_na(0) #substituir NA por 0

(df_pdm <- df_pdm %>% 
    mutate(estimativa = TOTAL_SMAE/resultado_2023*exec_2023))


# Estudo entre DA, informação do órgão e estimativa -----------------------

(estudo_pdm <- df_pdm %>% 
  group_by(META) %>% 
  summarise(total_smae = sum(unique(TOTAL_SMAE, na.rm = TRUE)),
            total_da = sum(unique(valor_da, na.rm = TRUE)),
            total_inf_orgao = sum(unique(valor_da_inf_orgao, na.rm = TRUE)),
            total_estimativa = mean(estimativa)))


sum(estudo_pdm$total_smae, na.rm = TRUE)
sum(estudo_pdm$total_da, na.rm = TRUE)
sum(estudo_pdm$total_inf_orgao, na.rm = TRUE)
sum(estudo_pdm$total_estimativa, na.rm = TRUE)

estudo_pdm %>% 
  mutate(liq_perc = total_inf_orgao/sum(total_inf_orgao, na.rm = TRUE))

# Base de dados Orçamento Cidadão -----------------------------------------

##
df_oc1 <- readxl::read_excel("Planilha_IDRGP_23_v2.xlsx")
df_oc <- df_oc1 %>% 
  filter(IDRGP == "Sim") %>% 
  select(`Nº Proposta`, DOTACAO, Secretaria, Subprefeitura, Liquidação)
df_oc<- df_oc %>% 
  rename("META"="Nº Proposta",
         sp_nome = Subprefeitura)
df_oc <- df_oc %>% 
  mutate(Liquidação = as.numeric(Liquidação))
df_oc <- df_oc %>% 
  mutate("sp_nome" = case_when(
    sp_nome %in% "Aricanduva/Formosa/Carrão" ~ "Subprefeitura Aricanduva/Formosa/Carrão",
    sp_nome %in% "Butantã" ~ "Subprefeitura Butantã",
    sp_nome %in% "Campo Limpo" ~ "Subprefeitura Campo Limpo",
    sp_nome %in% "Capela do Socorro" ~ "Subprefeitura Capela do Socorro",
    sp_nome %in% "Casa Verde" ~ "Subprefeitura Casa Verde/Cachoeirinha",
    sp_nome %in% "Cidade Ademar" ~ "Subprefeitura Cidade Ademar",
    sp_nome %in% "Cidade Tiradentes" ~ "Subprefeitura Cidade Tiradentes",
    sp_nome %in% "Ermelino Matarazzo" ~ "Subprefeitura Ermelino Matarazzo",
    sp_nome %in% "Freguesia/Brasilândia" ~ "Subprefeitura Freguesia/Brasilândia",
    sp_nome %in% "Guaianases" ~ "Subprefeitura de Guaianases",
    sp_nome %in% "Ipiranga" ~ "Subprefeitura Ipiranga",
    sp_nome %in% "Itaim Paulista" ~ "Subprefeitura Itaim Paulista",
    sp_nome %in% "Itaquera" ~ "Subprefeitura Itaquera",
    sp_nome %in% "Jabaquara" ~ "Subprefeitura Jabaquara",
    sp_nome %in% "Jaçanã/Tremembé" ~ "Subprefeitura Jaçanã/Tremembé",
    sp_nome %in% "Lapa" ~ "Subprefeitura Lapa",
    sp_nome %in% "M'Boi Mirim" ~ "Subprefeitura M'Boi Mirim",
    sp_nome %in% "Mooca" ~ "Subprefeitura Mooca",
    sp_nome %in% "Parelheiros" ~ "Subprefeitura Parelheiros",
    sp_nome %in% "Penha" ~ "Subprefeitura Penha",
    sp_nome %in% "Perus/Anhanguera" ~ "Subprefeitura Perus/Anhanguera",
    sp_nome %in% "Pinheiros" ~ "Subprefeitura Pinheiros",
    sp_nome %in% "Pirituba/Jaraguá" ~ "Subprefeitura Pirituba/Jaraguá",
    sp_nome %in% "Santana/Tucuruvi" ~ "Subprefeitura Santana/Tucuruvi",
    sp_nome %in% "Santo Amaro" ~ "Subprefeitura Santo Amaro",
    sp_nome %in% "São Mateus" ~ "Subprefeitura São Mateus",
    sp_nome %in% "São Miguel Paulista" ~ "Subprefeitura São Miguel Paulista",
    sp_nome %in% "Sapopemba" ~ "Subprefeitura Sapopemba",
    sp_nome %in% "Sé" ~ "Subprefeitura Sé",
    sp_nome %in% "Vila Maria/Vila Guilherme" ~ "Subprefeitura Vila Maria/Vila Guilherme",
    sp_nome %in% "Vila Mariana" ~ "Subprefeitura Vila Mariana",
    TRUE ~ "Subprefeitura de Vila Prudente")) 

df_oc <- df_oc %>% 
  mutate(acao_orc = stringr::str_sub(DOTACAO, start = 19, end = 23))

df_sub_oc <- df_oc %>% 
  group_by(sp_nome) %>% 
  summarise(total_oc = sum(Liquidação))
#Apenas 6 Subs

idrgp<- full_join(gp_idrgp, df_sub_oc, by = "sp_nome")
idrgp<- idrgp %>% 
  mutate(total_oc = ifelse( is.na(total_oc), 0, total_oc))
idrgp<- idrgp %>% 
  mutate(total_idrgp = total_oc+ valor_liq_2023,
         p_liq_23 = total_idrgp/sum(total_idrgp), 
         diferenca23= p_liq_23 - ref_idrgp)

##Gráficos Orçamento cidadão
#gráfico por órgão:
df_oc %>% 
  group_by(Secretaria) %>% 
  summarise(Liquidado_OC = sum(Liquidação)) %>% 
  ggplot() +
  geom_col(aes(x = Liquidado_OC,
               y = Secretaria),
           show.legend = TRUE) +
  theme_minimal()
  
#gráfico por Sub
df_oc %>% 
  group_by(sp_nome) %>% 
  summarise(Liquidado_OC = sum(Liquidação)) %>% 
  ggplot() +
  geom_col(aes(x = Liquidado_OC,
               y = sp_nome),
           show.legend = TRUE) +
  theme_minimal()

#gráfico por açãoorçamentária
df_oc %>% 
  group_by(acao_orc) %>% 
  summarise(Liquidado_OC = sum(Liquidação)) %>% 
  ggplot() +
  geom_col(aes(x = Liquidado_OC,
               y = acao_orc),
           show.legend = TRUE) +
  theme_minimal()
##



# Base de Dados Obras - UE ------------------------------------------------

##
##
##


# Gráfico do resultado do IDRGP2023 (Referência vs Liquidado) -------------

## Necessário criar df cols = c(sp_nome, referencia, pdm, obras, oc, total, diferenca)

(df_pdm <- df_pdm %>%
  filter(sp_nome != "Subprefeitura Sé"))

(df_pdm <- df_pdm %>% 
  mutate("sp_nome" = case_when(
    sp_nome %in% "SE+SUPRACENTRO" ~ "Subprefeitura Sé",
    TRUE ~ sp_nome)))

(gp_idrgp <- df_pdm %>% 
  group_by(sp_nome) %>%
  summarise(valor_liq_2023 = sum(unique(valor_da_inf_orgao), na.rm = TRUE)) %>% 
  mutate(perc_liq_2023 = valor_liq_2023/sum(valor_liq_2023, na.rm = TRUE)))

(gp_idrgp <- inner_join(gp_idrgp, ref_idrgp, by = "sp_nome"))

(gp_idrgp <- gp_idrgp %>% 
  mutate("diferenca" = perc_liq_2023 - ref_idrgp) %>% 
  print(n = 32))


gp_idrgp %>% 
  mutate("Status" = case_when(
    gp_idrgp$diferenca < 0 ~ "Execução MENOR do que a referência pelo IDRGP",
    gp_idrgp$diferenca >= 0 ~ "Execução MAIOR do que a referência pelo IDRGP"
  )) %>% 
  ggplot() +
  geom_col(aes(x = perc_liq_2023,
               y = forcats::fct_reorder(sp_nome, ref_idrgp),
               fill = Status),
           show.legend = TRUE) +
  scale_fill_manual(values = c("#0A447F", "orange")) +
  scale_x_continuous(name = NULL,
                     labels = scales::comma_format(big.mark = ".",
                                                   decimal.mark = ",")) +
  geom_point(mapping = aes(x = ref_idrgp,
                           y = sp_nome,
                           colour = "Referência pelo IDRGP"),
             show.legend = TRUE) +
  labs(title = "IDRGP 2023",
       y = "") +
  theme_minimal()+
  theme(legend.title = element_blank(),
        legend.position = "bottom")

# Gráfico da distância 1
sd(gp_idrgp$ref_idrgp)
# 0.01738926
se<-(sd(gp_idrgp$ref_idrgp)/sqrt(length(gp_idrgp$ref_idrgp)-1))
int<- 2*(sd(gp_idrgp$ref_idrgp)/sqrt(length(gp_idrgp$ref_idrgp)-1))
t.test(gp_idrgp$ref_idrgp , gp_idrgp$perc_liq_2023 , paired = T)
t.test(gp_idrgp$ref_idrgp, gp_idrgp$perc_liq_2023 )
## 95 percent confidence interval:
## -0.01019256  0.01606134
t.test(gp_idrgp$diferenca)
## -0.01587012  0.01000133

gp_idrgp %>% 
  mutate("Status" = case_when(
    gp_idrgp$diferenca < -0.01019256  ~ "Aquem da referência do IDRGP",
    gp_idrgp$diferenca >  0.01606134 ~ "Acima da referência do IDRGP"
  )) %>% 
  ggplot() +
  geom_col(aes(x = diferenca,
               y = sp_nome,
               fill = Status),
           show.legend = T) +
  scale_fill_manual(values = c("yellow", "red")) +
  labs(title = "IDRGP 2023",
       y = "") +
  theme_minimal()

# Gráfico da distância 2
gp_idrgp %>% 
  mutate(Status= if_else(
    gp_idrgp$diferenca >= -0.01019256 & gp_idrgp$diferenca <= 0.01606134, "Dentro da referência do IDRGP", "Fora da referência do IDRGP")
  ) %>% 
  ggplot() +
  geom_col(aes(x = diferenca,
               y = sp_nome,
               fill = Status),
           show.legend = T) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(title = "IDRGP 2023",
       y = "") +
  theme_minimal()


#mapa


library(sf)
#download.file("https://geosampa.prefeitura.sp.gov.br/PaginasPublicas/downloadIfr.aspx?orig=DownloadCamadas&arq=01_Limites%20Administrativos%5C%5CSubprefeituras%5C%5CShapefile%5C%5CSIRGAS_SHP_subprefeitura&arqTipo=Shapefile", destfile="SIRGAS_SHP_subprefeitura.zip")
sp <- read_sf("SIRGAS_SHP_subprefeitura//SIRGAS_SHP_subprefeitura_polygon.shp")


subprefeitura <- sp %>% 
    rename(sigla = sp_sigla,
           NOME=sp_nome) %>% 
    select(NOME, sigla, geometry)
subprefeitura<- subprefeitura %>% 
  mutate(sp_nome = case_when(
    sigla %in% "AF" ~ "Subprefeitura Aricanduva/Formosa/Carrão",
    sigla %in% "BT" ~ "Subprefeitura Butantã",
    sigla %in% "CL" ~ "Subprefeitura Campo Limpo",
    sigla %in% "CS" ~ "Subprefeitura Capela do Socorro",
    sigla %in% "CV" ~ "Subprefeitura Casa Verde/Cachoeirinha",
    sigla %in% "CAA" ~ "Subprefeitura Cidade Ademar",
    sigla %in% "CT" ~ "Subprefeitura Cidade Tiradentes",
    sigla %in% "EM" ~ "Subprefeitura Ermelino Matarazzo",
    sigla %in% "FO" ~ "Subprefeitura Freguesia/Brasilândia",
    sigla %in% "GU" ~ "Subprefeitura de Guaianases",
    sigla %in% "IP" ~ "Subprefeitura Ipiranga",
    sigla %in% "IP" ~ "Subprefeitura Itaim Paulista",
    sigla %in% "IT" ~ "Subprefeitura Itaquera",
    sigla %in% "JA" ~ "Subprefeitura Jabaquara",
    sigla %in% "JT" ~ "Subprefeitura Jaçanã/Tremembé",
    sigla %in% "LA" ~ "Subprefeitura Lapa",
    sigla %in% "MB" ~ "Subprefeitura M'Boi Mirim",
    sigla %in% "MO" ~ "Subprefeitura Mooca",
    sigla %in% "PA" ~ "Subprefeitura Parelheiros",
    sigla %in% "PE" ~ "Subprefeitura Penha",
    sigla %in% "PR" ~ "Subprefeitura Perus/Anhanguera",
    sigla %in% "PI" ~ "Subprefeitura Pinheiros",
    sigla %in% "PJ" ~ "Subprefeitura Pirituba/Jaraguá",
    sigla %in% "ST" ~ "Subprefeitura Santana/Tucuruvi",
    sigla %in% "SA" ~ "Subprefeitura Santo Amaro",
    sigla %in% "SM" ~ "Subprefeitura São Mateus",
    sigla %in% "MP" ~ "Subprefeitura São Miguel Paulista",
    sigla %in% "SB" ~ "Subprefeitura Sapopemba",
    sigla %in% "SE" ~ "Subprefeitura Sé",
    sigla %in% "MG" ~ "Subprefeitura Vila Maria/Vila Guilherme",
    sigla %in% "VM" ~ "Subprefeitura Vila Mariana",
    TRUE ~ "Subprefeitura de Vila Prudente"))

#teste do mapa com siglas
subprefeitura %>% 
  ggplot() +
  geom_sf() +
  geom_sf_text(aes(label = sigla), color = "black", size = 1.5, fontface = "bold")


#inserindo critérios para rotulação
gp_idrgp<- gp_idrgp %>% 
  mutate("Status" = case_when(
    gp_idrgp$diferenca <  -0.01587012 ~ "Aquem da referência do IDRGP",
    gp_idrgp$diferenca >  0.01000133 ~ "Acima da referência do IDRGP",
    TRUE ~ "Dentro da referência"))

#Juntanto DF
mapa_dif<- inner_join(subprefeitura, gp_idrgp, by="sp_nome")

# teste do mapa
mapa_dif %>% 
  ggplot()+
  geom_sf(aes(fill = diferenca, geometry = geometry)) +
  scale_fill_gradient(low="red",high="yellow") +
  geom_sf_text(aes(label = sigla), color = "white", size = 2, fontface = "bold") +
  theme_void() +
  labs(title = "Distribuição regional dos gastos do Programa de Metas 2023*",
       subtitle = "*Diferença entre valor de refencia do índice e o percentual liquidado do orçamento ",
       fill = "Diferença",
       caption = "Fonte: PMSP/SOF; SMADS; SME; SMS. Elaboração: SGM/SEPEP/CP.",
       x = NULL, y = NULL)

# mapa 2
mapa_dif %>% 
  ggplot()+
  geom_sf(aes(fill = Status)) +
  scale_fill_manual(values = c("yellow","red", "blue")) +
  geom_sf_text(aes(label = sigla), color = "white", size = 2, fontface = "bold") +
  theme_void() +
  labs(title = "Variação significativa do gasto do Programa de Metas 2023 da referência do IDRGP",
       subtitle = "t.test com intervalo de confiança de 95%",
       fill = "Aproximação do IDRGP",
       caption = "Fonte: PMSP/SOF; SMADS; SME; SMS. Elaboração: SGM/SEPEP/CP.",
       x = NULL, y = NULL)

#Mapa soma PDM +  OC
m_dif<- inner_join(subprefeitura, idrgp, by="sp_nome")
t.test(idrgp$diferenca23)
t.test(idrgp$p_liq_23, idrgp$ref_idrgp)

m_dif<- m_dif %>% 
  mutate("Status" = case_when(
    m_dif$diferenca23 < -0.01400318 ~ "Aquem da referência do IDRGP",
    m_dif$diferenca23 > 0.0140193 ~ "Acima da referência do IDRGP",
    TRUE ~ "Dentro da referência"))
m_dif %>% 
  ggplot()+
  geom_sf(aes(fill = Status)) +
  scale_fill_manual(values = c("yellow","red", "blue")) +
  geom_sf_text(aes(label = sigla), color = "white", size = 2, fontface = "bold") +
  theme_void() +
  labs(title = "Variação significativa do gasto de 2023 da referência do IDRGP",
       subtitle = "t.test com intervalo de confiança de 95%",
       fill = "Aproximação do IDRGP",
       caption = "Fonte: PMSP/SOF; SMADS; SME; SMS. Elaboração: SGM/SEPEP/CP.",
       x = NULL, y = NULL)

###Inserndo dados de 2022
idrgp22<- read_excel("consolidado_idrgp_2022.xlsx")
idrgp22<- idrgp22 %>% 
  mutate(tot22= sum(idrgp_2022),
         ref22= sum(valor_por_ano),
         p_liq_22 = ( liq_pdm +liq_obras + liq_oc )/tot22, 
         ref_idrgp22 = valor_por_ano/ref22,
         diferenca22= p_liq_22 - ref_idrgp22)
t.test(idrgp22$diferenca22)
t.test(idrgp22$p_liq_22, idrgp22$ref_idrgp22)
idrgp22<- idrgp22 %>% 
  mutate("Status" = case_when(
    idrgp22$diferenca22 <  - 0.01244564 ~ "Aquem da referência do IDRGP",
    idrgp22$diferenca22 >  0.01244564 ~ "Acima da referência do IDRGP",
    TRUE ~ "Dentro da referência"))
idrgp22<- idrgp22 %>% 
  rename(NOME = sp_nome)
idrgp22<- idrgp22 %>% 
  mutate(NOME = ifelse(NOME == "M'BOI MIRIM", "M BOI MIRIM", NOME))
map_22<- inner_join(idrgp22, subprefeitura, by = "NOME")
map_22 %>% 
  ggplot()+
  geom_sf(aes(fill = Status, geometry = geometry )) +
  scale_fill_manual(values = c("yellow","red", "blue")) +
  geom_sf_text(aes(label = sigla, geometry = geometry), color = "white", size = 2, fontface = "bold") +
  theme_void() +
  labs(title = "Variação significativa do gasto de 2022 da referência do IDRGP",
       subtitle = "t.test com intervalo de confiança de 95%",
       fill = "Aproximação do IDRGP",
       caption = "Fonte: PMSP/SOF; SMADS; SME; SMS. Elaboração: SGM/SEPEP/CP.",
       x = NULL, y = NULL)


### Soma dos gastos de 2022 e 2023
mapa_22e23<- inner_join( map_22, mapa_dif, by = c("NOME", "sigla", "geometry", "sp_nome"))
mapa_22e23<- mapa_22e23 %>% 
  mutate(liq_22e23 = idrgp_2022 + valor_liq_2023,
         p_liq_22e23 = liq_22e23/ sum(liq_22e23)) %>% 
  select(sigla, NOME, sp_nome, geometry, ref_idrgp, idrgp_2022, valor_liq_2023, liq_22e23, p_liq_22e23)
mapa_22e23<- mapa_22e23 %>% 
  mutate(diferenca = p_liq_22e23 - ref_idrgp)
t.test(mapa_22e23$diferenca)
mapa_22e23<- mapa_22e23 %>% 
  mutate("Status" = case_when(
    mapa_22e23$diferenca < -0.008045154 ~ "Aquem da referência do IDRGP",
    mapa_22e23$diferenca > 0.011301404 ~ "Acima da referência do IDRGP",
    TRUE ~ "Dentro da referência"))
mapa_22e23 %>% 
  ggplot()+
  geom_sf(aes(fill = Status, geometry = geometry )) +
  scale_fill_manual(values = c("yellow","red", "blue")) +
  geom_sf_text(aes(label = sigla, geometry = geometry), color = "white", size = 2, fontface = "bold") +
  theme_void() +
  labs(title = "Variação significativa do gasto no biênio 2022 e 2023 da referência do IDRGP",
       subtitle = "t.test com intervalo de confiança de 95%",
       fill = "Aproximação do IDRGP",
       caption = "Fonte: PMSP/SOF; SMADS; SME; SMS. Elaboração: SGM/SEPEP/CP.",
       x = NULL, y = NULL)


