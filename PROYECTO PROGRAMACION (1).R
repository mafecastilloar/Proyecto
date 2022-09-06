#PROYECTO
#Federacion Nacional de Cafeteros de Colombia, estadısticas cafeteras:
#Integrantes: Natalia Ballesteros/ Santiago Silva/ Maria Fernanda Castillo 

#a) precios,  ́area y produccion del cafe
# b) exportaciones de cafe colombiano.
rm(list=ls())


install.packages("readxl")
install.packages("DataExplorer")
install.packages( "SmartEDA")
install.packages("openxlsx")
install.packages("tidyverse")
install.packages("psych")
library(DataExplorer)
library(SmartEDA)
library(readxl)
library(openxlsx)
library(tidyverse)
library(psych)  
library(dplyr)
library(priceR)

pid3<-datos <- read_excel("D:/Desktop/PROYECTO/datos.xlsx",
              sheet = "2. Precio Interno Mensual",range = "D6:E949")

exp2<-Exportaciones <- read_excel("D:/Desktop/PROYECTO/Exportaciones.xlsx", 
               sheet = "2. Total_Valor", range = "D7:E781")

prex<-datos <- read_excel("D:/Desktop/PROYECTO/datos.xlsx", 
              sheet = "4. Precio Ex_Dock Anual Civil",range = "D6:E115")

volexp<-Exportaciones <- read_excel("D:/Desktop/PROYECTO/Exportaciones.xlsx", 
              sheet = "1. Total_Volumen", range = "D7:E781")

view(prex)
view(pid3)
view(exp2)

str(pid3)
str(exp2)
dim(exp2)
dim(pid3)


#filtrar por decadas

#precios,area y produccion del cafe

decada1= pid3 %>% filter(Mes >="1944-01-01" & Mes<="1953-12-01")
decada2= pid3 %>% filter(Mes >="1954-01-01" & Mes<="1963-12-01")
decada3= pid3 %>% filter(Mes >="1964-01-01" & Mes<= "1973-12-01")
decada4= pid3 %>% filter(Mes >="1974-01-01" & Mes<="1983-12-01")
decada5= pid3 %>% filter(Mes >="1984-01-01" & Mes<="1993-12-01")
decada6= pid3 %>% filter(Mes >="1994-01-01" & Mes<= "2003-12-01")                         
decada7= pid3 %>% filter(Mes >="2004-01-01" & Mes<="2013-12-01")                                                               
decada8= pid3 %>% filter(Mes >="2014-01-01" & Mes<= "2022-07-01") #102

#añadimos la columna "decadas"
Decadas=c(rep(1,120),rep(2,120),rep(3,120),rep(4,120),rep(5,120),rep(6,120),rep(7,120),rep(8,103))
datos=data.frame(pid3,Decadas)
datos$Decadas=as.factor(datos$Decadas)
summary(datos)

#Exportaciones
decadae1= exp2 %>% filter(Mes>="1958-01-01" & Mes<="1967-12-01")
decadae2= exp2 %>% filter(Mes>="1968-01-01" & Mes<="1977-12-01")
decadae3= exp2 %>% filter(Mes>="1978-01-01" & Mes<="1987-12-01")
decadae4= exp2 %>% filter(Mes>="1988-01-01" & Mes<="1997-12-01")
decadae5= exp2 %>% filter(Mes>="1998-01-01" & Mes<="2007-12-01")
decadae6= exp2 %>% filter(Mes>="2008-01-01" & Mes<="2017-12-01")
decadae7= exp2 %>% filter(Mes>="2018-01-01" & Mes<="2022-06-01") ##53

#añadimos la columna "decadaE"
DecadasE=c(rep(1,120),rep(2,120),rep(3,120),rep(4,120),rep(5,120),rep(6,120),rep(7,54))
datosE=data.frame(exp2,DecadasE)
datosE$DecadasE=as.factor(datosE$DecadasE)
summary(datosE)

######Promedios y desviaciones estandar anuales o por decadadas.
 
# precios,area y produccion del cafe
#promedio por decadas

promedio1<-mean(decada1$`Precio interno`)
promedio1

promedio2<-mean(decada2$`Precio interno`)
promedio2

promedio3<-mean(decada3$`Precio interno`)
promedio3

promedio4<-mean(decada4$`Precio interno`)
promedio4

promedio5<-mean(decada5$`Precio interno`)
promedio5

promedio6<-mean(decada6$`Precio interno`)
promedio6

promedio7<-mean(decada7$`Precio interno`)
promedio7

promedio8<-mean(decada8$`Precio interno`)
promedio8

##promedio por decadas 
# exportaciones

promedioe1<-mean(decadae1$`Valor Nominal*`)
promedioe1

promedioe2<-mean(decadae2$`Valor Nominal*`)
promedioe2

promedioe3<-mean(decadae3$`Valor Nominal*`)
promedioe3

promedioe4<-mean(decadae4$`Valor Nominal*`)
promedioe4

promedioe5<-mean(decadae5$`Valor Nominal*`)
promedioe5

promedioe6<-mean(decadae6$`Valor Nominal*`)
promedioe6

promedioe7<-mean(decadae7$`Valor Nominal*`)#
promedioe7


#desviacion estandar por decadas
#precios,area y produccion del cafe


desviacionestandar1<-sd(decada1$`Precio interno`)
desviacionestandar1

desviacionestandar2<-sd(decada2$`Precio interno`)
desviacionestandar2

desviacionestandar3<-sd(decada3$`Precio interno`)
desviacionestandar3

desviacionestandar4<-sd(decada4$`Precio interno`)
desviacionestandar4

desviacionestandar5<-sd(decada5$`Precio interno`)
desviacionestandar5

desviacionestandar6<-sd(decada6$`Precio interno`)
desviacionestandar6

desviacionestandar7<-sd(decada7$`Precio interno`)
desviacionestandar7

desviacionestandar8<-sd(decada8$`Precio interno`)
desviacionestandar8

#desviacion estandar por decadas
# exportaciones

desviacionestandarE1<-sd(decadae1$`Valor Nominal*`)
desviacionestandarE1

desviacionestandarE2<-sd(decadae2$`Valor Nominal*`)
desviacionestandarE2

desviacionestandarE3<-sd(decadae3$`Valor Nominal*`)
desviacionestandarE3

desviacionestandarE4<-sd(decadae4$`Valor Nominal*`)
desviacionestandarE4

desviacionestandarE5<-sd(decadae5$`Valor Nominal*`)
desviacionestandarE5

desviacionestandarE6<-sd(decadae6$`Valor Nominal*`)
desviacionestandarE6

desviacionestandarE7<-sd(decadae7$`Valor Nominal*`)
desviacionestandarE7

#Otra forma de obtener promedios/desviación estandar
#tapply(variables, grupo, mean/sd)
tapply(datos$Precio.interno, datos$Decadas, mean)
tapply(datosE$Valor.Nominal.,datosE$DecadasE, mean)
tapply(datos$Precio.interno, datos$Decadas, sd)
tapply(datosE$Valor.Nominal., datosE$DecadasE, sd)

# resumen de datos:precios,area y produccion del cafe

resumen1=data.frame( cbind(
  decadas=c(1:8),
  promedio=c(promedio1,promedio2,promedio3,promedio4,promedio5,promedio6,promedio7,promedio8),
  desviacion_Estandar=c(desviacionestandar1,desviacionestandar2,desviacionestandar3,desviacionestandar4,desviacionestandar5,desviacionestandar6,desviacionestandar7,desviacionestandar8)
))

resumen1

#resumen de datos: Exportaciones

resumen2=data.frame( cbind(
  decadas=c(1:7),
  promedio=c(promedioe1,promedioe2,promedioe3,promedioe4,promedioe5,promedioe6,promedioe7),
  desviacion_Estandar=c(desviacionestandarE1,desviacionestandarE2,desviacionestandarE3,desviacionestandarE4,desviacionestandarE5,desviacionestandarE6,desviacionestandarE7)
))
resumen2

# 2.Gŕaficas de las variaciones de precio o produccíon a trav́es del tiempo.

# precios,area y produccion del cafe

 par(mfrow=c(2,1))
plot(datos$Mes,datos$Precio.interno, type="p", col=Decadas,
     main = "Variación del precio interno a traves del tiempo", ylab= "Precios", xlab= "Tiempo")

#Exportaciones

plot(datosE$Mes,datosE$Valor.Nominal., type="p", col=DecadasE,
     main = "Variación del precio de las exportaciones a traves del tiempo", ylab= "Precios", xlab= "Tiempo")



# 3.Proyecciones de precio o produccion utilizando regresion lineal

plot(volexp$MES~volexp$`Total Exportaciones`,
     main="Volumen de las exportaciones a traves del tiempo" ,xlab="años",
     ylab="Volumen de Exportacion")

regresion<- lm(volexp$MES~volexp$`Total Exportaciones`)
regresion


abline(regresion, col="red", lwd= 2) 

## 4.Graficas de precios estandarizados a precios de hoy (convirtiendo pesos de
## valor pasado a valor presente considerando la inflacion).

country<- "United States"
countries_dataframe<- show_countries()
inflation_dataframe<-retrieve_inflation_data(country)
adjust_for_inflation(prex[,2],1913,country = country, to_date = 2021,
                     inflation_dataframe = inflation_dataframe,
                     countries_dataframe = countries_dataframe)

Tiempo_Transcurrido<- NULL
Devaluacion_del_peso <-NULL
años <- 1913
for (i in 1:100) {
  Tiempo_Transcurrido[i]<- años
  Devaluacion_del_peso[i]<- adjust_for_inflation(prex[i,2],años,country = country,to_date = 2021,
                                                 inflation_dataframe = inflation_dataframe,
                                                 countries_dataframe = countries_dataframe)
  años<-años+1
  
}

plot(Tiempo_Transcurrido,Devaluacion_del_peso, type = "l", main= "Precios de las exportaciones teniendo en cuenta la inflacion ", xlab= "Años", ylab="Peso devaluado" )

##5. Pruebas estadısticas para comparar las medias de dos decadas
#por ejemplo (1991-2000 vs 2001-2010) de los precios del cafe
#utilizar precios estandarizados al valor presente.

Devaluacion_del_peso= as.numeric(Devaluacion_del_peso)


t.test(Devaluacion_del_peso[79:88],Devaluacion_del_peso[89:98], 
       alternative = "two.sided")

#### la diferencia en las medias es estadisticamente significativa de este modo 
###rechazo la hipotesis nula.






