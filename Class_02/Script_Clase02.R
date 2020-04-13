##########################################
## Class 02: Review and  Data Management
## Author: Esteban Lopez
## Course: Spatial Analytics 
## Program: Master in Business Analytics
## Institution: Universidad Adolfo Ibáñez
##########################################

#---- Part 1: Review  -------------------

#Estas son las cosas que me gustaría que les queden bien claras

### 1. Sintaxis básica

# Creación de Objetos

x<-NULL #objeto vacío
y<-c(TRUE,FALSE)#concatenar valores logicos
as.numeric(y) #convierta un objeto en numerico, si es que puede

A<-1 #creamos un objeto valor 1
years<-2010:2020 #objeto que tomara el rango completo del 2010 al 2020
tiktoc<-c("Que", "linda", "te ves", "limpiando", "Esperancita") #concatena los string entre parentesis, todos los elementos
class(tiktoc) #busca la clase del parentesis, caracter en este caso
year<- seq(2010,2020,by = 0.5)#secuencia de numeros de rango 2010 al 2020 con aumento de 0.5 cada vez

paste("Hola","Mundo",sep=" ") #Hola mundo separado con un espacio con la función sep

paste(tiktoc,collapse = " ") #collapse es lo mismo que el sep, pero cuando tomamos la función 

obj2<- as.numeric(c(1,2,3,4,"Esperancita")) #objeto 2 que contiene si es un numero cierta concatenación, el ultimo es NA, porque no es numero
is.na(obj2) #busca si hay na en el objeto 2


numeros_en_texto<-c("1","2","3") #nuevo objeto, reconoce estos caracteres como numeros
as.numeric(numeros_en_texto) #pregunta si son numeros y los muestra para el parentesis

m1<-matrix(1:4,2,2) #Nueva matriz de 1 a 4 de 2 x 2
m1%*%t(m1) #traspuesta de m1, multiplicada por la normal
diag(m1) #diagonal de la matriz
solve(m1)


a1<-array(1:12,dim = c(2,2,3)) #arreglo del 1 al 12, en dimensión 2x2 en 3 matrices

d1<-data.frame(m1) #crea un data frame con los datos de m1
data("quakes") # promise en el envirioment
d1<-data.frame(quakes) #crea un data frame con los datos de los temblores

ls() #muestra todo lo que tenemos en el ambiente (Environment)

l1<-list(Perrito=A,years,tiktoc,m1)  #crea unba lista con el contenido entre parentesis
A<-3L #la L hace que sea entero

# Manipulación de Objetos
A<-1L #la L hace que sea entero

class(A) #clase del objeto
typeof(A) #es más general, dice la clase del objeto

length(years) #largo de años del objeto
dim(m1) #dimensión de m1

object.size(d1)#el tamaño en bytes de d1

names(d1) #nombre de las columnas de d1
head(d1) #muestra los primeros 6 de d1
tail(d1) #muestra los ultimos 6 de d1

rm(A) #Remueve el objeto

#Bonus: como se borra todo?
rm(list=ls()) #BORRA TODO LO DEL ENVIRONMENT

# Indexación uso de los []

length(years) #largo de years
years[11] #posición 11 de largo

m1[1,2] #m1 posición 1,2 

dim(a1) #dimension de a1 , 2x2 3 veces

a1[2,1,3] #fila, columna,dimension en ese orden

class(a1) #clase del arreglo a1

l1[2] #l1 lista 1, años
l1[2][[1]][1:2] #years de l1 indice 1 y 2, el corchete 1 es referente a la fila

l1[[2]][3:5] # pedir el vector 2 de la lista que es el variable years, rango 3,4,5

l1$Perrito #llama la variable perrito y muestra su valor

d1[1,] #muesta la primera fila de d1 
d1[,1] #muestra la primera columna para todas las filas de d1
d1[,'lat'] #muestra la columna latitud para todas las filas de d1
d1$mag[seq(1,16,2)] #para d1 busca la variable magnitud con secuencia de 1 al 16 con incremento de 2 en 2
d1$lat[1:4] #para d1 busca la variable latitud y muestra del 1 al 4

d1[1:4,c('lat','long')] # del rango 1 al 4 muestrame latitud y longitud

d1$mag>5 #operacion logica que devuelve bool si la magnitud de mayores a 5, si se cumple o no
table(d1$mag>5) #computa en una tabla los valores de la operación anterior, COMO UN CONTADOR
d1[d1$mag>6,'stations'] #estaciones que han tenido magnitud mayores a 6
d1$dummy_5up<-as.numeric(d1$mag>5) #crea una nueva variable para ver si se cumple si la magnitud fue mayor a 5

# Distinguir entre funciones, objetos, números y sintaxis básica
# Funciones: palabra + () con argumentos separados por commas
# Objetos: palabras a la izquierda del signo <- 


#---- Part 2: Loops  -------------------

A<-2 #objeto valor 2

if(A==1){
  print("A es un objeto con un elemento numérico 1")
} else {
  print("A no es igual a 1, pero no se preocupe que lo hacemos")
  A<-1L
} #si se cumple la condición, pasa lo siguiente

A<-1 #valor 1 al objeto
class(A) #clase
typeof(A)#clse general

dim(A) #null porque no es array ni matriz
length(A) #largo de objeto

# For loop

for(i in 1:5){
  print(paste("Me le declaro a la ", i)) #EL PASTE ES PARA QUE TOME LA VARIABLE
  Sys.sleep(2) #DUERME 2 SEGUNDOS ANTES DE SEGUIR CORRIENDO, SE DUERME LA CONSOLA
  print("no mejor no... fail!")
  Sys.sleep(1)
} #LOOP de 1 a 5, que se recorre

i<-1
eps<-50/(i^2)
while(eps>0.001){
  eps<-50/(i^2)
  print(paste("eps value es still..", eps)) #CON EL PASTE TOMA LA VARIABLE
  i<-i+1
} #WHILE VA A RECORRER SIEMPRE QUE SE CUMPLA

#---- Part 3: Data Management ----
# Tres formas de trabajar con datos

### 1. R-Base 
#http://github.com/rstudio/cheatsheets/raw/master/base-r.pdf

quakes[quakes$mag>6,'mag'] #muestrame las magnitudes que sean mayores a 6

by(data = quakes$mag,INDICES = quakes$stations,FUN = mean) #para la data quakes, la magnitud va a ser indice y estaciones el promedio de las magnitudes
tapply(X = quakes$mag,INDEX = quakes$stations, FUN = mean) #promedia las magnitudes y los muestra en base a las estaciones

### 2. tydiverse 
#https://rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf
library(tidyverse)
#Cómo se instala el paquete si no lo tengo? Tank!!! ayudaaaa!

install.packages("tidyverse")

#install.packages("tydiverse")


quakes %>% 
  filter(mag>6) %>% 
  select(mag)  #con tydiverse buscame y filtrame las magnitudes mayores a 6

quakes %>% 
  group_by(stations) %>%
  summarise(mean(mag)) #agrupame el resumen del promedio de las magnitudes en base a las estaciones correspondientes


### 3. data.table (recommended in this course)
install.packages("data.table")
library(data.table)
#https://github.com/rstudio/cheatsheets/raw/master/datatable.pdf
#install.packages("data.table")
quakes<-data.table(quakes) #crea quakes como data table


quakes[quakes$mag>6,'mag'] #para quakes busca que se cumpla la condición cuando la variable mag es mayor a 6 y muestramelos en data table 

quakes[mag>6,.(mag)] #Para quakes, busca cundo se cumpla que magnitud mayor a 6, en base a mag... EL , ES PARA LA FUNCION Y EL . ES PARA SEPARAR HACIA ABAJO

quakes[,mean(mag),by=.(stations)]# para quakes busca el promedio de magnitud y ordenalo por estaciones

### Reading data from a file

library(readxl) #carga el abrir excel

casos<-data.table(read_excel("Class_02/2020-03-17-Casos-confirmados.xlsx",na = "—",trim_ws = TRUE,col_names = TRUE),stringsAsFactors = FALSE) #abreme el archivo, remplaza na por -, el trim elimina el espacio

casos<-casos[Región=="Metropolitana",] #reduce la base, solo muestra los de la región metropolitana

library(ggplot2) #carga gg plot, graficos

ggplot(casos[order(Edad,decreasing = T)],)+geom_bar(stat = 'identity' ,aes(x=`Centro de salud`, y=Edad/Edad, group=Sexo, fill=Edad)) + coord_flip()+ facet_wrap(~Sexo) 

casos[Sexo=="Fememino",Sexo:="Femenino"]

ggplot(casos[order(Edad,decreasing = T),])+geom_bar(stat = 'identity',aes(x=`Centro de salud` ,y=Edad/Edad,fill=Edad)) + coord_flip()+ facet_wrap(~Sexo) +labs(title = "Casos Confirmados por Sexo y Establecimiento",subtitle = "Región Metropolitana - 2020-03-17",caption = "Fuente: https://www.minsal.cl/nuevo-coronavirus-2019-ncov/casos-confirmados-en-chile-covid-19/")

