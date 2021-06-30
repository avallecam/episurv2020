knitr::opts_chunk$set(echo = TRUE, eval = TRUE, fig.align = "center")
# after class, update 
# - unlock code_folding: hide 
# - change to echo = TRUE

klippy::klippy(position = c("top", "right"))

library(tidyverse)
library(sf)

## install.packages("INLA", repos = "https://inla.r-inla-download.org/R/stable", dep = TRUE)

library(SpatialEpi)

# llamar a base de datos dentro de paquete
data(pennLC)

## # ¿qué tipo de objeto es?
## 
## 
## # identificar sus atributos
## attributes(pennLC)

## pennLC$data %>%
##   as_tibble() %>%
##   group_by(county) %>%
##   summarise(Y=sum(cases),
##             pop=sum(population)) %>%
##   ungroup()

## pennLC$smoking

## pennLC$spatial.polygon %>%
##               st_as_sf() %>%
##               as_tibble() %>%
##               rownames_to_column()

#outcome at county level + covariates (race,sex,age_strata)
d <- pennLC$data %>% 
  as_tibble() %>% 
  group_by(county) %>% 
  summarise(Y=sum(cases),
            pop=sum(population)) %>% 
  ungroup() %>% 
  
  #add exposure at county level
  left_join(pennLC$smoking) %>% 
  rownames_to_column() %>% 
  
  #add polygon to tibble
  left_join(pennLC$spatial.polygon %>% 
              st_as_sf() %>% 
              as_tibble() %>% 
              rownames_to_column()) 

d %>% 
  st_as_sf() %>% 
  st_transform(crs = 27700) %>% 
  ggplot(aes(fill=Y)) +
  geom_sf() +
  scale_fill_gradient(low = "white",high = "red") +
  guides(fill = guide_colorbar(reverse=F)) +
  theme_bw() +
  labs(title = "Observed cases",
       subtitle = "Political Map: Polygon boundaries for each county")

# Expected cases (using stratas!) ---------------------------------
e <- pennLC$data %>% 
  as_tibble() %>% 
  select(county,cases,population,race,gender,age)


## e %>% count(county) #67
## 
## e %>% count(race,gender,age) #16

population <- e$population
cases <- e$cases
n.strata <- 16 # = 2 races * 2 genders * 4 age bands
E <- expected(population, cases, n.strata)


## E

## e %>%
##   select(county) %>%
##   distinct() %>%
##   mutate(E=E)

## d %>%
##   left_join(
##     e %>%
##       select(county) %>%
##       distinct() %>%
##       mutate(E=E)
##   )

## d %>%
##   left_join(
##     e %>%
##       select(county) %>%
##       distinct() %>%
##       mutate(E=E)
##   ) %>%
##   mutate(SMR=Y/E)

map <- d %>% 
  left_join(
    e %>% 
      select(county) %>% 
      distinct() %>% 
      mutate(E=E)
  ) %>% 
  mutate(SMR=Y/E) %>% 
  # add data to map 
  st_as_sf() %>% 
  # transform to SpatialPolygonDataFrame
  as('Spatial')


# Mapping SMR ---------------------------------
map %>% 
  st_as_sf() %>% 
  st_transform(crs = 27700) %>% 
  ggplot(aes(fill=SMR)) +
  geom_sf() +
  scale_fill_gradient2(high = "#e34a33",low = "#2b8cbe",midpoint = 1) +
  guides(fill = guide_colorbar(reverse=F)) +
  theme_bw() +
  labs(title = "Standardize Mortality Ratio",
       subtitle = "Political Map: Polygon boundaries for each county")


# alternative - cartogram
map_carto <- map %>% 
  st_as_sf() %>% 
  st_transform(crs = 27700) %>% 
  cartogram::cartogram_cont("pop", itermax=5)

map_carto %>% 
  st_as_sf() %>% 
  ggplot(aes(fill=SMR)) +
  geom_sf() +
  scale_fill_gradient2(high = "#e34a33",low = "#2b8cbe",midpoint = 1) +
  guides(fill = guide_colorbar(reverse=F)) +
  theme_bw() +
  labs(title = "Standardize Mortality Ratio",
       subtitle = "Cartogram: polygon area proportional to the population size")


# Neighbourhood matrix ---------------------------------
library(spdep)
library(INLA)
nb <- poly2nb(map)
head(nb)
nb2INLA("map.adj", nb)
g <- inla.read.graph(filename = "map.adj")


# Inference using INLA ---------------------------------

#index both random effects
map$re_u <- 1:nrow(map@data) #spatial residual variation
map$re_v <- 1:nrow(map@data) #modeling unstructure noise

#create formula
#iid: independent and identically distributed
formula <- 
  Y ~ # outcome
  smoking + # exposure
  f(re_u, model = "besag", graph = g) + #random effect - spatial
  f(re_v, model = "iid") #random effect - noise

res <- inla(formula, 
            family = "poisson", 
            data = map@data, 
            E = E,
            control.predictor = list(compute = TRUE))


summary(res)

marginal <- inla.smarginal(res$marginals.fixed$smoking) %>% 
  data.frame()

ggplot(marginal, aes(x = x, y = y)) + 
  geom_line() +
  geom_vline(xintercept = 0, col = "blue") + 
  labs(x = expression(beta[1]), 
       y = "Density") +
  theme_bw()

head(res$summary.fitted.values)

map$RR <- res$summary.fitted.values[, "mean"]
map$LL <- res$summary.fitted.values[, "0.025quant"]
map$UL <- res$summary.fitted.values[, "0.975quant"]

map %>% 
  st_as_sf() %>% 
  st_transform(crs = 27700) %>% 
  ggplot(aes(fill=RR)) +
  geom_sf() +
  scale_fill_gradient2(high = "#e34a33",low = "#2b8cbe",midpoint = 1) +
  guides(fill = guide_colorbar(reverse=F)) +
  labs(fill = "linear\npredictor") +
  theme_bw()

map_carto <- map %>% 
  st_as_sf() %>% 
  st_transform(crs = 27700) %>% 
  cartogram::cartogram_cont("pop", itermax=5)

map_carto %>% 
  st_as_sf() %>% 
  ggplot(aes(fill=RR)) +
  geom_sf() +
  scale_fill_gradient2(high = "#e34a33",low = "#2b8cbe",midpoint = 1) +
  guides(fill = guide_colorbar(reverse=F)) +
  labs(fill = "linear\npredictor") +
  theme_bw()

## library(leaflet)
## 
## pal <- colorNumeric(palette = "YlOrRd", domain = map$RR)
## 
## labels <- sprintf("<strong> %s </strong> <br/> Observed: %s <br/> Expected: %s <br/>
##                   Smokers proportion: %s <br/>SMR: %s <br/>linear\npredictor: %s (%s, %s)",
##                   map$county,
##                   map$Y,
##                   round(map$E, 2),
##                   map$smoking,
##                   round(map$SMR, 2),
##                   round(map$RR, 2),
##                   round(map$LL, 2),
##                   round(map$UL, 2)) %>%
##   lapply(htmltools::HTML)
## 
## leaflet(map) %>%
##   addTiles() %>%
##     addPolygons(color = "grey",
##                 weight = 1,
##                 fillColor = ~pal(RR),
##                 fillOpacity = 0.5,
##                 highlightOptions = highlightOptions(weight = 4),
##                 label = labels,
##                 labelOptions = labelOptions(style = list("font-weight" = "normal",
##                                                          padding = "3px 8px"),
##                                             textsize = "15px", direction = "auto")) %>%
##   addLegend(pal = pal,
##             values = ~RR,
##             opacity = 0.5,
##             title = "linear<br>predictor",
##             position = "bottomright")

## #generar material para estudiantes
## knitr::purl("0903-vigilancia-practica.Rmd",
##             output = "0903-vigilancia-practica.R",
##             documentation = 0)

## 
