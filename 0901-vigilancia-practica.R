# instalar paquetes si se requiere ----------------------------------------

if(!require("remotes")) install.packages("remotes")
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("lubridate")) install.packages("lubridate")
if(!require("compareGroups")) install.packages("compareGroups")
if(!require("rio")) install.packages("rio")
if(!require("naniar")) install.packages("naniar")
if(!require("incidence")) install.packages("incidence")
if(!require("epiR")) install.packages("epiR")
if(!require("epiDisplay")) install.packages("epiDisplay")
if(!require("epitidy")) remotes::install_github("avallecam/epitidy")
if(!require("incidenceflow")) remotes::install_github("avallecam/incidenceflow")
if(!require("epichannel")) remotes::install_github("avallecam/epichannel")


# # 01 - tiempo espacio persona ------------------------------------
# ## _librerias a usar ---------------------------------

library(tidyverse)
library(lubridate)
library(compareGroups)

# ## _importar base de datos ---------------------------------

ruta_limpio <- "https://github.com/avallecam/epiapli2019/raw/master/data/casoslimpio_20190916.rds"
casos_limpio <- rio::import(file = ruta_limpio)
casos_limpio

casos_limpio %>% naniar::miss_var_summary()
casos_limpio %>% naniar::vis_miss()

# ## _distribución en tiempo ---------------------------------

casos_limpio %>%
  ggplot(aes(x = date_of_onset)) +
  geom_histogram(binwidth = 7, color="white") +
  scale_x_date(date_breaks = "7 day", date_labels = "%b-%d") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# ## _distribución en espacio ---------------------------------

casos_limpio %>%
  ggplot(aes(x = lon, y = lat, colour = date_of_onset)) +
  geom_point() +
  scale_color_gradient(low = "red", high = "yellow", trans = "date") +
  theme_bw()

# ## _tabla descriptiva ---------------------------------

casos_limpio %>%
  select(outcome, gender, hospital) %>%
  mutate(hospital = fct_infreq(hospital),
         outcome = fct_infreq(outcome)) %>%
  compareGroups(~., data = .) %>%
  createTable() %>%
  export2md()


# # 02 - factores asociados ------------------------------------
# ## _librerias a usar ---------------------------------

library(epitidy)

# ## _importar base de datos ---------------------------------

## # dentro de R
## if(!require("mosaicData")) install.packages("mosaicData")
## ?mosaicData::Whickham

smoke_limpio <- "https://github.com/avallecam/epiapli2019/raw/master/data/smokeclean_20190906.rds"
smoke_clean <- rio::import(file = smoke_limpio)
smoke_clean

# ## _tabla descriptiva ---------------------------------

tabla1 <- smoke_clean %>%
  compareGroups(outcome~smoker+agegrp+age, data = ., byrow = T) %>%
  createTable(show.all = T, sd.type = 2)

tabla1

## tabla1 %>%
##   export2xls("tabla/tabla02.xls")

# ## _medidas de asociación ---------------------------------

# ###__ modelo simple ---------------------------------

# smoke_clean %>% glimpse()

#simple
simple_model <- glm(outcome_1 ~ smoker,
                    data = smoke_clean,
                    family = poisson(link = "log"))
epi_tidymodel_rr(simple_model)



# ###__ modelo múltiple ---------------------------------

#multiple: controlar por confusión
multiple_model <- glm(outcome_1 ~ smoker + age,
                      data = smoke_clean,
                      family = poisson(link = "log"))
epi_tidymodel_rr(multiple_model)

## epi_tidymodel_rr(multiple_model) %>%
##   writexl::write_xlsx("tabla/tabla03.xlsx")

## # medidas de asociación ---------------------------------------------------
##
## #epiR
## library(epiR)
## smoke_tabla1 <- with(smoke_clean,table(smoker_2,outcome_2)) %>% print()
## epi.2by2(smoke_tabla1,method = "cohort.count")
##
## #epiDisplay
## library(epiDisplay)
## smoke_tab2 <- with(smoke_clean,table(outcome,smoker)) %>% print()
## cs(cctable = smoke_tab2)
##
## # controlar por confusión -------------------------------------------------
##
## #Mantel-Haenszel
## smoke_tab3 <- with(smoke_clean,table(smoker_2,outcome_2,agegrp)) %>% print()
## epi.2by2(smoke_tab3,method = "cohort.count")
## mhor(mhtable=smoke_tab3,graph = F,design = "cohort")


# # 03 - curva epidémica ------------------------------------
# ## _librerias a usar ---------------------------------

library(outbreaks) #sample data
library(incidence) #core functions
library(incidenceflow)

# ## _importar base de datos ---------------------------------

ebola_sim$linelist %>% as_tibble()

dat <- ebola_sim$linelist$date_of_onset
enframe(dat,value = "date_of_onset") %>% select(date_of_onset)

i.7 <- incidence(dat, interval=7)
i.7

plot(i.7)

# ## _curva ascendente ---------------------------------

f1 <- fit(i.7[1:20])
f1

# ## _curva descendente ---------------------------------

f2 <- fit_optim_split(i.7)
f2

# ## _generar tablas resumen ---------------------------------
# ###__ curva ascendente ---------------------------------

f1 %>% tidy_incidence()

f1 %>% glance_incidence()

# ###__ curva descendente ---------------------------------

f2 %>% pluck("fit") %>% tidy_incidence()

f2 %>% pluck("fit") %>% glance_incidence()

# # 04 - canal endémico ------------------------------------
# ## _librerias a usar ---------------------------------

library(epichannel)

# ## _importar base de datos ---------------------------------
# ###__ datos de enfermedad ---------------------------------

denv <-
  readr::read_csv("https://predict.cdc.gov/api/v1/attachments/dengue%20forecasting%20project/iquitos_training_data.csv") %>%
  mutate(year = lubridate::year(week_start_date),
         epiweek = lubridate::epiweek(week_start_date)) %>%
  mutate(adm="iquitos") %>%
  # cases per season - replace wiht a dummy year
  mutate(year = str_replace(season,"(.+)/(.+)","\\1") %>% as.double())

denv %>% glimpse()

# ###__ datos de población ---------------------------------

popdb <-
  readr::read_csv("https://predict.cdc.gov/api/v1/attachments/dengue%20forecasting%20project/iquitos_population_data.csv") %>%
  janitor::clean_names() %>%
  mutate(adm="iquitos")

popdb %>% glimpse()

## # denv %>% count(year,season,lag_year)
## # denv %>%
## #   ggplot(aes(x = week_start_date,y = total_cases)) +
## #   geom_col()
## # popdb %>% count(year)
## # denv %>% count(year)
## # denv %>% left_join(popdb)

# ## _adaptar bases de datos ---------------------------------

epi_adapted <-
  epi_adapt_timeserie(db_disease = denv,
                      db_population = popdb,
                      var_admx = adm,
                      var_year = year, # must be a common variable name between datasets
                      var_week = season_week,
                      # var_year = year,
                      # var_week = epiweek,
                      var_event_count = total_cases,
                      var_population = estimated_population)
epi_adapted

# ## _filtrar por años ---------------------------------

disease_now <- epi_adapted %>%
  filter(var_year==max(var_year))

disease_pre <- epi_adapted %>%
  filter(var_year!=max(var_year))

# ## _crear canal endémico ---------------------------------

disease_channel <-
  epi_create_channel(time_serie = disease_pre,
                     disease_name = "denv",
                     method = "gmean_1sd")
disease_channel

# ## _unir y graficar canal ---------------------------------

epi_join_channel(disease_channel = disease_channel,
                 disease_now = disease_now) %>%
  # ggplot
  epi_plot_channel() +
  labs(title = "Dengue virus Endemic Channel. Iquitos, Peru 2008/2009",
       caption = "Source: https://dengueforecasting.noaa.gov/",
       # x = "epiweeks",
       x = "Seasonal week",
       y = "Number of cases") +
  theme_bw()

