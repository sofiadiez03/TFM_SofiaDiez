library(cluster)
library(tidyr)
library(dplyr)
library(ggplot2)
library(restatapi) #cargar datos de eurostat


source("R/sim_fun_univ.R")   # for simulation
source("R/sq_dist.R")

############################## DATOS EUROSTAT ##################################
rd<-get_eurostat_raw("demo_fordagec",verbose=T)

age.valid <- setdiff(unique(rd$age) , c("TOTAL", "UNK", "Y10-14", "Y15-19", "Y20-24", "Y25-29", "Y30-34", "Y35-39", "Y40-44", "Y45-49"))
geo.valid <- setdiff(unique(rd$geo) , c("DE_TOT",  "FX", "EA19", "EU27_2007", "EU27_2020", "EU28","EFTA","EA20","EA21" ))  #FX:metropolitan France, #DE_TOT: total Germany (from before reunification)

country_list <- get_eurostat_codelist("geo",lang="en",cache=FALSE,verbose=TRUE)

codes <- which(country_list$code%in%geo.valid)
codes <-match(geo.valid,country_list$code)
countries <- country_list$name[codes]
countries[countries=="Türkiye"] <- "Turkey"
countries[countries=="Kosovo*"] <- "Kosovo"

count.names <- function(x) {
  countries[match(x,geo.valid)]
}

X0 <- rd %>% filter(age%in%age.valid, geo%in%geo.valid, ord_brth=="1") %>% dplyr::select(geo,time,age,values) %>%
  mutate(age=as.numeric(gsub("([YGE\\_])","",age)), country = count.names(geo)) %>% mutate(year=time) 
X.eust <- X0 %>% group_by(time,geo) %>%
  mutate(fert= as.numeric(values)/sum(as.numeric(values)) ) %>% filter(!is.na(fert)) %>% ungroup() %>%
  select(age, year, geo, country, fert) %>% filter(fert<1)  #this is in case in one country/year only GE50 age group is present and then all births are imputed to it

age.range <- range(X.eust$age)
# > age.range
# [1] 15 50



################################ DATOS HFD #####################################
X_all = read.table(file="asfrTRbo_read.txt", header = T)

#cohort representa el año de nacimiento de la madre para una edad dada (ej. 23) en un año dado (ej. 1984). En 1984 puede haber madres de 
# 23 nacidas en 1961 o en 1960 (que todavía no hayan cumplido años en el momento de dar a luz)
# nos quedamos con el año y sumamos sobre cohortes, y sólo elefimos ASFR1 (age specific fertility rate for at birth of the 1st child)
X <- X_all %>% group_by(Code, Year, Age) %>% summarise(fert=sum(ASFR1)) %>% ungroup()

unique(X$Year)
unique(X$Code)


# cambiamos los grupos de edad 12- y 55+ en 12 y 55  ("\\D" means all non-numerical characters)
X <- X %>% mutate(Age=as.numeric(gsub("\\D", "",Age)))
range(X$Age)
# > range(X$Age)
# [1] 12 55
# restringimos a los países válidos y cambiamos GBR_NP (UK-> GBR) y DEUTNP (Germany -> DEU) a los códigos oficiales y al rango de edad de Eurostat
geo.valid <- setdiff(unique(X$Code) , c("GBRTENW","GBR_NIR" ,"GBR_SCO","DEUTE"  ,"DEUTW" ))  #UK Wales, North Ir, Scotland, Germany East & West
X <- X %>% filter(Code%in%geo.valid) %>% mutate(Code=replace(Code, Code=="GBR_NP","GBR")) %>% 
  mutate(Code=replace(Code, Code=="DEUTNP","DEU"))
X <- X %>% mutate(Age=replace(Age, Age<=15,15)) %>% mutate(Age=replace(Age, Age>= 50,50)) %>% #llevamos 12-15 a 15, 50-55 a 50 y sumamos
  group_by(Code, Year, Age) %>% summarise(fert=sum(fert)) %>% ungroup()


#Equivalencia de códigos (Eurostat usa iso-2, HFD usa iso-3, pasar todo a iso-e)
library(countrycode)
codes.iso2<- function(x) {
  countrycode(x, origin = "iso3c", destination = "iso2c")
}
X.hfd <- X %>% mutate(geo = codes.iso2(Code)) %>% mutate(country = countrycode(geo, origin = 'iso2c', destination = 'country.name')) %>%
  mutate(age=Age) %>% mutate(year=Year) %>% select(age, year, geo, country, fert)




################################ DATOS HFC #####################################
X.hfc = read.table(file="HFC_ASFRstand_BO_clean.txt", header = T, sep=",")

# restringimos a los países válidos y cambiamos GBR_NP (UK-> GBR) y DEUTNP (Germany -> DEU) a los códigos oficiales y al rango de edad de Eurostat
geo.valid <- setdiff(gsub(" ","",unique(X.hfc$Country)) , c("GBRTENW","GBR_NIR" ,"GBR_SCO","DEUTE"  ,"DEUTW" ))  #UK Wales, North Ir, Scotland, Germany East & West


X.hfc <- X.hfc %>%  mutate(Country=gsub(" ","",Country)) %>% mutate(Country=replace(Country, Country=="GBR_NP","GBR")) %>% 
  mutate(Country=replace(Country, Country=="DEUTNP","DEU")) %>% filter(Country %in%geo.valid) 
unique(X.hfc$Country)

X.hfc <- X.hfc %>%  dplyr::select(Country, Age, Year1, ASFR1) %>%
  mutate(age=Age) %>% mutate(year=Year1) %>% mutate(fert=as.numeric(ASFR1)) %>% mutate(geo=codes.iso2(Country)) %>% 
  mutate(country = countrycode(geo, origin = 'iso2c', destination = 'country.name')) %>% 
  select(age, year, geo, country, fert)

#hay un NA, que se produce por el código SCG - The code was used for the State Union of Serbia and Montenegro from 2003 until the union peacefully dissolved in June 2006
#eliminamos esos datos
X.hfc <- X.hfc %>% filter(!is.na(country))
#también hay algunos NAs en los datos de fertilidad, que venían porque la variable ASFR1 aparecía como "." => los quitamos
X.hfc <- X.hfc %>% filter(!is.na(fert))

# range(X_HFC$age)
# [1] 14 50

X.hfc <- X.hfc %>% mutate(age=replace(age, age<=15,15)) %>% #llevamos 14-15 a 15y sumamos
  group_by(country, geo, year, age) %>% summarise(fert=sum(fert)) %>% ungroup() 


# Transformamos las curvas de HFD y HFC en densidades 
X.hfd.d <- X.hfd %>% group_by(year, country,geo) %>% mutate(fert = fert/pracma::trapz(age,fert)) %>% ungroup()
X.hfc.d <- X.hfc %>% group_by(year, country,geo) %>% mutate(fert = fert/pracma::trapz(age,fert)) %>% ungroup()

#### Combinamos los tres conjuntos de datos, cuando un país esté en las tres bases de datos, hacemos la media de los valores
X.all <- rbind(X.hfd.d, X.eust, X.hfc.d) %>% group_by(year, country,geo,age)  %>% summarize(fert = mean(fert)) %>% ungroup()

unique(X.all$country)
## Hay dos nombres distintos para Bosnia Herzegovina

unique(X.all$geo[X.all$country %in% c("Bosnia & Herzegovina", "Bosnia and Herzegovina")])
# [1] "BA"

X.all$country[ which(X.all$country=="Bosnia & Herzegovina")] <- "Bosnia and Herzegovina"

#luego vemos que Grecia a veces aparece con código GR y otras con código EL
X.all$geo[ which(X.all$geo=="EL")] <- "GR"
#y que UK a veces sale como UK y otras como GB
X.all$geo[ which(X.all$geo=="GB")] <- "UK"

#volvemos a hacer la media 
X.all <- X.all %>% group_by(year, country,geo,age)  %>% summarize(fert = mean(fert)) %>% ungroup()


X.all <- X.all[!is.na(X.all$fert), ] #### quitar NAs
X.all <- X.all %>% mutate(year=as.numeric(year)) ### year aparece como caracter



# All countries per year ----
#Plot all curves, by year (solo cada 5 años, para visualizar mejor)
ggplot(data=X.all %>% filter(year%in% seq(1935,2020,5)), aes(x=age, y=fert,group=country, colour=country)) +
  geom_line() + 
  facet_wrap(~year) +
  theme_light() +
  theme(plot.title = element_text(size = 20),axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),strip.text= element_text(size = 15)) +
  ggtitle("Age distribution at birth of first child")




selected_year <- 2020
X_2020 <- X.all %>% filter(year == selected_year)

count = unique(X_2020$country)
geo.codes = unique(X_2020$geo)

# Smoothing con fda para el año 2020 ----
basis <- fda::create.bspline.basis(norder=4, rangeval=unique(X_2020$age))
age = unique(X_2020$age)
age.sm = seq(min(age), max(age), length.out=200)

# Dataframe para guardar las curvas suavizadas de 2020
X_wide.sm <- data.frame(
  geo = rep(geo.codes, each=length(age.sm)), 
  country = rep(count, each=length(age.sm)), 
  age = age.sm, 
  fert_2020 = 0
)

par(mfrow=c(3,2))
for (i in count){
  data_pais <- X_2020 %>% filter(country == i) %>% na.exclude()
  
  # Graficamos la curva original
  plot(data_pais$age, data_pais$fert, type="l", main=paste("Original 2020:", i))
  

  fd <- fda::Data2fd(argvals=data_pais$age, y=as.matrix(data_pais$fert), basisobj=basis, lambda=0.5)
  dm.sm <- fda::eval.fd(age.sm, fd)

  # Graficamos la curva suavizada
  plot(age.sm, dm.sm, type="l", main=paste("Smoothed 2020:", i))
  
  M <- as.vector(dm.sm)
  
  # Correcciones de extremos
  M[is.na(M)] <- 0
  M[M < 0] <- 0
  
  X_wide.sm[X_wide.sm$country == i, "fert_2020"] <- M
}
par(mfrow=c(1,1))

# Volvemos a formato largo si es necesario para tus plots de ggplot
X.sm <- X_wide.sm 


# Depth-based estimation ----
omega = 0.1
alpha = 0.9
t <- unique(X_wide.sm$age)


data = matrix(X_wide.sm$fert_2020, nrow=length(count), byrow=T)

matplot(t, t(data), main="Smoothed curves year 2020", type="l", lty=1)

# Calculamos las matrices de distancias funcionales basadas en profundidad
DBest <- sq_dist(as.matrix(data), t, smooth=F, omega = omega, alpha = alpha)

df_siluetas <- data.frame()

for (d in 9:13) { # Iteramos sobre las 5 matrices de distancias
  D_dist <- as.dist(sqrt(DBest[[d]]))
  for (k in 2:10) {
    pam_fit <- pam(D_dist, k = k, diss = TRUE)
    sil_media <- pam_fit$silinfo$avg.width
    
    if (d == 9)  distance <- paste0("delta_1(L^2) omega = ", omega)
    if (d == 10) distance <- paste0("delta_1(delta_d) omega = ", omega, " alpha = ", alpha)
    if (d == 11) distance <- paste0("delta_1(delta_RelMS) omega = ", omega)
    if (d == 12) distance <- paste0("delta_2 omega = ", omega)
    if (d == 13) distance <- "delta_3 "
    
    df_siluetas <- rbind(df_siluetas, data.frame(
      Distance = distance,
      k = k,
      Silhouette = sil_media
    ))
  }
}

num_grupos_optimo <- df_siluetas %>%
  group_by(Distance) %>%
  summarise(
    Optimal_k = k[which.max(Silhouette)],
    Max_Silhouette = max(Silhouette)
  )

print(num_grupos_optimo)
# # A tibble: 5 × 3
# Distance                                   Optimal_k Max_Silhouette
# <chr>                                          <int>          <dbl>
# 1 "delta_1(L^2) omega = 0.1"                         2          0.459
# 2 "delta_1(delta_RelMS) omega = 0.1"                 2          0.458
# 3 "delta_1(delta_d) omega = 0.1 alpha = 0.9"         2          0.519
# 4 "delta_2 omega = 0.1"                              2          0.758
# 5 "delta_3 "                                         4          0.207



# Clustering delta_2 ----
d = 12 # delta_2
k = 2 

D_dist <- as.dist(sqrt(DBest[[d]]))
pam <- pam(D_dist, k = k, diss = TRUE)

#Group 1
count[pam$clustering==1]
# [1] "Albania"         "Austria"         "Belgium"         "Canada"          "Croatia"         "Cyprus"         
# [7] "Czechia"         "Denmark"         "Estonia"         "Finland"         "France"          "Georgia"        
# [13] "Greece"          "Iceland"         "Ireland"         "Italy"           "Japan"           "Latvia"         
# [19] "Lithuania"       "Luxembourg"      "Malta"           "Moldova"         "Netherlands"     "North Macedonia"
# [25] "Norway"          "Poland"          "Portugal"        "Russia"          "Serbia"          "Slovakia"       
# [31] "Slovenia"        "South Korea"     "Spain"           "Sweden"          "Switzerland"     "Taiwan"         
# [37] "Turkey"          "United Kingdom" 

matplot(t, t(data[pam$clustering==1,]), main="Smoothed curves year 2020, cluster 1", type="l",lty=1)

#Group 2
count[pam$clustering==2]
# "Bulgaria"      "Chile"         "Hungary"       "Mauritius"     "Romania"       "United States"

matplot(t, t(data[pam$clustering==2,]), main="Smoothed curves year 2020, cluster 2", type="l",lty=1)


