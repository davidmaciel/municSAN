## code to prepare `bd` dataset goes here
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, janitor, PNADcIBGE, survey, naniar, daflateBR,
               lubridate, srvyr)


# munics ------------------------------------------------------------------

munic12 <- read.csv2("data-raw/sources/munic_san2012.CSV")
munic14 <- read.csv2("data-raw/sources/munic_san2014.CSV")
munic18 <- read.csv2("data-raw/sources/munic_san2018.CSV")


# dicionarios -------------------------------------------------------------

dic12 <- read.csv2("data-raw/sources/dic_2012.CSV")
dic14 <- read.csv2("data-raw/sources/dic_2014.CSV")
dic18 <- read.csv2("data-raw/sources/dic_2018.CSV")



# san em 2012 e 2021: características gerais ------------------------------


san18 <- munic18 %>%
  select("lei"= MSAN04, "cons" = MSAN05, "plano" = MSAN15,
         "caisan" = MSAN11,
         "re12mes" = MSAN07,
         "orca" = MSAN14,
         "mun" = Cod.Municipio,
         "consultivo" = MSAN061,
         "deliberativo" = MSAN062,
         "normativo" = MSAN063,
         "fiscalizador" = MSAN064) %>%
  mutate(ano = 2018,
         mun = as.character(mun) %>%
           str_remove( "\\d$") %>%
           as.integer())

san12 <- munic12 %>%
  select("lei" = A346, "cons" = A348, "plano" = A370,
         "caisan" = A357,
         "re12mes" = A355,
         "orca" = A356,
         "mun" = A1,
         "consultivo" = A351,
         "deliberativo" = A352,
         "normativo" = A353,
         "fiscalizador" = A354)%>%
  mutate(ano =2012)

san12_18 <- bind_rows(san12, san18) %>%
  mutate(
    lei = case_when(
      lei %in% c(
        "Não possui lei",
        "Não possui") ~ "Não",
      lei %in% c(
        "Possui lei municipal",
        "Possui lei",
        "A lei está em trâmite") ~ "Sim",
      lei %in% c(
        "Não informou",
        "Recusa") ~ "missing",
      T ~ lei
      ) %>% na_if("missing"),
    cons = na_if(cons, "Não informou") %>%
      na_if("Recusa"),
    plano = na_if(plano, "Não informou") %>%
      na_if("Recusa"),
    caisan = na_if(caisan, "Não informou") %>%
      na_if("Recusa"),
    re12mes = case_when(
      str_detect(re12mes, "\\d") ~ "Sim",
      re12mes == "Sim" ~ "Sim",
      re12mes == "Não" ~ "Não",
      T ~ "missing") %>% na_if("missing"),
    orca = na_if(orca, "Não aplicável") %>%
      na_if("-") %>% na_if("Recusa") %>%
      na_if("Não informou")) %>%
  naniar::replace_with_na_all(
    condition = ~.x %in% c("Não informou", "Recusa","-","Não aplicável")
  )


# renovação do plano ------------------------------------------------------


ren_plano <- san12_18 %>%
  select(mun, plano,ano) %>%
  filter(!is.na(plano)) %>%
  group_by(mun) %>%
  summarise(plano = str_c(plano, collapse = "-")) %>%
  group_by(plano) %>%
  count()


# conselhos que se tornaram apenas consultivos --------------------------------------------

san12_18 <- san12_18 %>%
  group_by(ano) %>%
  mutate(
    just_consult = if_else(
      consultivo == "Sim" &
        deliberativo == "Não" &
        normativo == "Não" &
        fiscalizador == "Não", "Sim", "Não"
    )
  )

mudanca_consult <- san12_18 %>%
  select(mun, just_consult, ano) %>%
  filter(!is.na(just_consult)) %>%
  group_by(mun) %>%
  summarise(just_consult = str_c(just_consult, collapse = "-")) %>%
  group_by(just_consult) %>%
  count()


# conselhor que perderam apoio orcamentario municipal ---------------------
muda_orca <- san12_18 %>%
  select(mun, orca,ano) %>%
  filter(!is.na(orca)) %>%
  group_by(mun) %>%
  summarise(orca = str_c(orca, collapse = "-")) %>%
  group_by(orca) %>%
  count()



