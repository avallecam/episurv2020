knitr::opts_chunk$set(echo = TRUE, eval = TRUE, fig.align = "center")
# after class, update 
# - unlock code_folding: hide 
# - change to echo = TRUE

klippy::klippy(position = c("top", "right"))

#> if(!require("remotes")) install.packages("remotes")
#> if(!require("tidyverse")) install.packages("tidyverse")
#> if(!require("lubridate")) install.packages("lubridate")
#> if(!require("compareGroups")) install.packages("compareGroups")
#> if(!require("rio")) install.packages("rio")
#> if(!require("naniar")) install.packages("naniar")
#> if(!require("incidence")) install.packages("incidence")
#> if(!require("epiR")) install.packages("epiR")
#> if(!require("epiDisplay")) install.packages("epiDisplay")
#> if(!require("epitidy")) remotes::install_github("avallecam/epitidy")
#> if(!require("incidenceflow")) remotes::install_github("avallecam/incidenceflow")
#> if(!require("epichannel")) remotes::install_github("avallecam/epichannel")

library(tidyverse)
library(lubridate)
library(compareGroups)

#> ruta_original <- "https://github.com/reconhub/learn/raw/master/static/data/linelist_20140701.xlsx"
#> rio::import(ruta_original)

# https://github.com/avallecam/epiapli2019/blob/master/01-epi_descriptiva.R
ruta_limpio <- "https://github.com/avallecam/epiapli2019/raw/master/data/casoslimpio_20190916.rds"
casos_limpio <- rio::import(file = ruta_limpio)
casos_limpio

casos_limpio %>% naniar::miss_var_summary()
casos_limpio %>% naniar::vis_miss()

casos_limpio %>% 
  ggplot(aes(x = date_of_onset)) +
  geom_histogram(binwidth = 7, color="white") +
  scale_x_date(date_breaks = "7 day", date_labels = "%b-%d") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

casos_limpio %>% 
  ggplot(aes(x = lon, y = lat, colour = date_of_onset)) +
  geom_point() +
  scale_color_gradient(low = "red", high = "yellow", trans = "date") +
  theme_bw()

casos_limpio %>% 
  select(outcome, gender, hospital) %>% 
  mutate(hospital = fct_infreq(hospital),
         outcome = fct_infreq(outcome)) %>% 
  compareGroups(~., data = .) %>% 
  createTable() %>% 
  export2md()

library(epitidy)

#> # dentro de R
#> if(!require("mosaicData")) install.packages("mosaicData")
#> ?mosaicData::Whickham

# https://github.com/avallecam/epiapli2019/blob/master/02-clean_db.R
smoke_limpio <- "https://github.com/avallecam/epiapli2019/raw/master/data/smokeclean_20190906.rds"
smoke_clean <- rio::import(file = smoke_limpio)
smoke_clean

tabla1 <- smoke_clean %>% 
  compareGroups(outcome~smoker+agegrp+age, data = ., byrow = T) %>% 
  createTable(show.all = T, sd.type = 2)

tabla1

#> tabla1 %>%
#>   export2xls("tabla/tabla02.xls")

# smoke_clean %>% glimpse()

#simple
simple_model <- glm(outcome_1 ~ smoker, 
                    data = smoke_clean, 
                    family = poisson(link = "log"))
epi_tidymodel_rr(simple_model)

#multiple: controlar por confusión
multiple_model <- glm(outcome_1 ~ smoker + age, 
                      data = smoke_clean, 
                      family = poisson(link = "log"))
epi_tidymodel_rr(multiple_model)

#> epi_tidymodel_rr(multiple_model) %>%
#>   writexl::write_xlsx("tabla/tabla03.xlsx")

#> # medidas de asociación ---------------------------------------------------
#> 
#> #epiR
#> library(epiR)
#> smoke_tabla1 <- with(smoke_clean,table(smoker_2,outcome_2)) %>% print()
#> epi.2by2(smoke_tabla1,method = "cohort.count")
#> 
#> #epiDisplay
#> library(epiDisplay)
#> smoke_tab2 <- with(smoke_clean,table(outcome,smoker)) %>% print()
#> cs(cctable = smoke_tab2)
#> 
#> # controlar por confusión -------------------------------------------------
#> 
#> #Mantel-Haenszel
#> smoke_tab3 <- with(smoke_clean,table(smoker_2,outcome_2,agegrp)) %>% print()
#> epi.2by2(smoke_tab3,method = "cohort.count")
#> mhor(mhtable=smoke_tab3,graph = F,design = "cohort")

library(outbreaks) #sample data
library(incidence) #core functions
library(incidenceflow)

ebola_sim$linelist %>% as_tibble()

dat <- ebola_sim$linelist$date_of_onset
enframe(dat,value = "date_of_onset") %>% select(date_of_onset)

i.7 <- incidence(dat, interval=7)
i.7

plot(i.7)

f1 <- fit(i.7[1:20])
f1

f2 <- fit_optim_split(i.7)
f2

f1 %>% tidy_incidence()

f1 %>% glance_incidence()

f2 %>% pluck("fit") %>% tidy_incidence()
f2 %>% pluck("fit") %>% glance_incidence()

incidence_purrr <- ebola_sim %>% 
  # extrae base
  pluck("linelist") %>% 
  # filtro explícito
  filter(date_of_onset<lubridate::ymd(20141007)) %>% 
  # estratifica por grupos
  group_by(gender) %>%
  # bases anidadas --- aquí se transforma la base!
  nest() %>% 
  # aplicar varias funciones del flujo
  # extraer fechas
  mutate(only_dates = map(.x = data,
                          .f = pull,
                          date_of_onset)) %>% 
  # configurar
  mutate(incidence_strata = map(.x = only_dates,
                                .f = incidence,
                                interval=7)) %>% 
  # ajustar
  mutate(strata_fit = map(.x = incidence_strata,
                          .f = fit)) %>% 
  # generar tabla tidy
  mutate(strata_fit_tidy = map(.x = strata_fit,
                               .f = tidy_incidence)) %>% 
  # generar tabla glance
  mutate(strata_fit_glance = map(.x = strata_fit,
                                 .f = glance_incidence)) %>% 
  
  # keep only the tibbles
  select(-data,-only_dates,-incidence_strata,-strata_fit)

# des-anidamos columna resultado de r y td
incidence_purrr %>% 
  unnest(cols = c(strata_fit_tidy))

# des-anidamos columna resultado de bondad de ajuste
incidence_purrr %>% 
  unnest(cols = c(strata_fit_glance))

#> #generar material para estudiantes
#> knitr::purl("0902-vigilancia-practica.Rmd",
#>             output = "0902-vigilancia-practica.R",
#>             documentation = 0)

#> 
