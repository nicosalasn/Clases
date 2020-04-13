###Class 03 - Data Management & Visualization###
## Author: Esteban Lopez
## Course: Spatial Analytics 
## Program: Master in Business Analytics
## Institution: Universidad Adolfo Ibáñez


#---- Part 1: Data Management  -------------------

# Reading an exporting data

library(readxl) #Cargamos lector de excel
library(data.table) #cargamos data table

casos<-data.table(read_excel("~/Desktop/MBAn/Software tools for Big Data/Spatial Analytics/Clases/Class_02/2020-03-17-Casos-confirmados.xlsx",na = "—",trim_ws = TRUE,col_names = TRUE),stringsAsFactors = FALSE) #dato table del excel, los - del excel toman valor NA, las columnas toman nombre del excel por el true

casos<-casos[Región=="Metropolitana",] #tomamos la data table y lo filtramos por casos, región metropolitana

saveRDS(casos,"~/Desktop/MBAn/Software tools for Big Data/Spatial Analytics/Clases/Class_03/CasosCovid_RM.csv") #guarda un archivo de R

write.csv(casos,file = '~/Desktop/MBAn/Software tools for Big Data/Spatial Analytics/Clases/Class_03/CasosCovid_RM.csv',fileEncoding = 'UTF-8') #guarda como csv

writexl::write_xlsx

library(foreign) #carga foreign

write.dta

casosRM<-fread("~/Desktop/MBAn/Software tools for Big Data/Spatial Analytics/Clases/Class_03/CasosCovid_RM.csv",header = T, showProgress = T,data.table = T)

casosRM[,table(Sexo)]
casosRM[Sexo=="Fememino",Sexo:="Femenino"]


# Creating (factor) variables

class(casosRM$Sexo) #busca la clase de la variable sexo en los casos RM

casosRM[,Sexo:=factor(Sexo,nmax = 2)]

head(casosRM$Sexo) #me da los primeros 6 para la variable que pedimos
head(as.numeric(casosRM$Sexo)) #me da los primeros 6 para la variable sexo como numero, 1 mujer, 2 hombre

table(casosRM$Sexo) #contador de tabla para casos RM, por sexo
casosRM[,.N,by=.(Sexo)] #para casos RM, dime el numero para la variable sexo
casosRM[,.N,by=.(Sexo,`Centro de salud`)] #para casos RM, dime el numero para el sexo segun centro de salud

#Collapsing by Centro de Salud 

casosRM[,sum(`Casos confirmados`,na.rm = T),by=.(`Centro de salud`)][,V1/sum(V1)] #para casosRM remueve los NA, suma los casos confirmados por centro de salud y en base a eso, v1/sum(v1) donde v1 es el numero por centor de salud y sum el total 152 

# collapsing by average age

A<-casosRM[,.(AvAge=mean(Edad,na.rm = T)),by=.(`Centro de salud`)] #A nueva tabla con casos de Rm, con el promedio de la edad, removiendo NA, para cada centro de salud

B<-casosRM[,.(Total_centro=sum(`Casos confirmados`,na.rm = T)),by=.(`Centro de salud`)] #B nueva tabla con casos de Rm, con la suma de los casos confirmados removiendo NA, para cada centro de salud 

C<-casosRM[Sexo=="Femenino",.(Total_Centro_Mujeres=sum(`Casos confirmados`,na.rm = T)),by=.(`Centro de salud`)]#C nueva tabla para casos de Rm, con la suma de las mujeres confirmadas por centro de salud

D<-casosRM[Sexo=="Masculino",.(Total_Centro_Hombres=sum(`Casos confirmados`,na.rm = T)),by=.(`Centro de salud`)]#D nueva tabla con casos de Rm, con la suma de los hombres confirmados por centro de salud


#merging data sets

AB<-merge(A,B,by = "Centro de salud",all = T,sort = F) #Une A, con B en una sola tabla guiada por centro de salud, all T, no llena con datos x
ABC<-merge(AB,C,by = "Centro de salud",all = T,sort = F) #Une AB, con C en una sola tabla guiada por centro de salud, all T, no llena con datos x
ABCD<-merge(ABC,D,by = "Centro de salud",all = T,sort = F) #Une ABC, con D en una sola tabla guiada por centro de salud, all T, no llena con datos x

ABCD[,porc_mujeres:=Total_Centro_Mujeres/Total_centro] #CREA una nueva variable con el := que es el porcentaje de mujeres

# reshaping

E<-casosRM[,.(AvAge=mean(Edad,na.rm = T),`Casos confirmados`=sum(`Casos confirmados`,na.rm = T)),by=.(`Centro de salud`,Sexo)]#casos RM, les saca el promedio por edad, y suma los casos confirmados ordenando por centro de salud y sexo
G<-reshape(E,direction = 'wide',timevar = 'Sexo',v.names = c('AvAge','Casos confirmados'),idvar = 'Centro de salud') #separa por centro de salud, los casos confirmados y el promedio por sexo correspondiente

#---- Part 2: Visualization  -------------------

#Scatter plot
  #Base R 
plot(G$`Casos confirmados.Femenino`,G$`Casos confirmados.Masculino`) #abre un plot de G, respecto a los casos confirmados por sexo según eje
text(x =G$`Casos confirmados.Femenino`,y=G$`Casos confirmados.Masculino`, G$`Centro de salud`,cex=0.5) #le agrega al plot los textos según eje, con tamaño 0,5 el nombre de cada clinica que es lo último

#ggplot2
library(ggplot2)
p1<-ggplot(G,aes(x=`Casos confirmados.Femenino`,y=`Casos confirmados.Masculino`))+geom_point(aes(size=AvAge.Femenino,colour=AvAge.Masculino))+geom_text(aes(label=`Centro de salud`),size=2,check_overlap = T) #nuevo grafico según sexo para cada clínica
p1

#plotly
library(plotly)
ggplotly(p1) #una forma más linda de ver lo anterior

# other useful ways to show data

#high charter
# http://jkunst.com/highcharter/index.html


#---- Part 3: Intro to Mapping  -------------------
#install.packages("chilemapas")
library(chilemapas)
library(data.table)
library(ggplot2)

zonas_censo<-data.table(censo_2017_zonas,stringsAsFactors = F) #traemos la tabla para trabajarla con ese nombre de zonas_censo

poblacion_adulto_mayor_zonas<-zonas_censo[edad=="65 y mas",.(AdultosMayores=sum(poblacion)),by=.(geocodigo)] #filtramos adultos mayores de 65 para el censo según población y codico

zonas_valparaiso<-mapa_zonas[mapa_zonas$codigo_region=="05",] #nos ve las zonas y el codigo de región 05, que es valparaiso y nos da la info

zonas_valparaiso<-merge(zonas_valparaiso,codigos_territoriales[,c("codigo_comuna","nombre_comuna")],by="codigo_comuna",all.x=TRUE,sort=F)#merge zonas de valparaiso con su comuna respectiva y su codigo respectivo

zonas_valparaiso<-zonas_valparaiso[zonas_valparaiso$codigo_comuna%in%c("05101","05109"),] #filtra para esos codigos de comuna

zonas_valparaiso<-merge(zonas_valparaiso,poblacion_adulto_mayor_zonas,by="geocodigo",all.x=TRUE,sort=F) #cuenta los adultos mayores y los une para el geocodigo


#plotting
library(RColorBrewer)
paleta <- rev(brewer.pal(n = 9,name = "Reds"))


ggplot(zonas_valparaiso) +  #un mapa gigante de plot, sobre la población de adulto mayor
  geom_sf(aes(fill = AdultosMayores, geometry = geometry)) +
  scale_fill_gradientn(colours = rev(paleta), name = "Poblacion\nadulto mayor") +
  labs(title = "Poblacion de 65 años y más", subtitle = "Valparaíso y Viña del Mar") +
  theme_minimal(base_size = 11)

# creating a fake spatial distribution of adult population in space
zonas_valparaiso2<-cbind(zonas_valparaiso[,c("geocodigo","codigo_comuna","codigo_provincia","codigo_region","geometry")],"AdultosMayores"=sample(zonas_valparaiso$AdultosMayores,size = length(zonas_valparaiso$AdultosMayores)))


ggplot(zonas_valparaiso2) + #mismo para para el codigo anterior, el cual queda mucho más bonito y contempla todo
  geom_sf(aes(fill = AdultosMayores, geometry = geometry)) +
  scale_fill_gradientn(colours = rev(paleta), name = "Poblacion\nadulto mayor") +
  labs(title = "Poblacion de 65 años y más", subtitle = "Valparaíso y Viña del Mar") +
  theme_minimal(base_size = 13)

#comparing histograms of the same variable

hist(zonas_valparaiso$AdultosMayores,main = "Histograma Adultos Mayores Viña-Valpo")#muestra que ambos son iguales en un histograma de adultos mayores

hist(zonas_valparaiso2$AdultosMayores,main = "Histograma Adultos Mayores Viña-Valpo")
