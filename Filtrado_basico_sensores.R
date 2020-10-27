library(dplyr)
library(ggplot2)
library(signal)

#Descargamos datos de sensores ambientales
#https://www.kaggle.com/garystafford/environmental-sensor-data-132k
datos = read.csv("iot_telemetry_data.csv")

#filtramos para quedarnos con un solo sensor, por coherencia de lecturas en el tiempo
datos_dev1<- dplyr::filter(datos, device == "b8:27:eb:bf:9d:51")

#cogemos un dato de cada 200, porque son series muy largas
datos_dev1_sub<- datos_dev1[seq(from=0, to=dim(datos_dev1)[1], by=200),]
summary(datos_dev1)

#ver que sensores tenemos para filtrar
plot(datos_dev1$smoke[1:500],col="blue", type = "l")
plot(datos_dev1$humidity[1:500],col="red", type = "l")
plot(datos_dev1$co[1:500],col="green", type = "l")

#implementar la funcion de ventana desplazante
mav <- function(x,n){stats::filter(x,rep(1/n,n), sides=2)}

#Usaremos también un Butterworth, de la libreria signal. Parametros: 
#orden del filtro, frecuencia del corte, tipo de filtro

#Probaremos humedad (menos ruido) y co

#HUMEDAD
mav_hum<- mav(datos_dev1$humidity[1:500],10)
plot(datos_dev1$humidity[1:500],col="red", type = "l")
lines(mav_hum,col="green")
#points(1:500, hempel_hum$y, col="pink", pch=20)



bfhum <- butter(2, 1/50, type="low")
#butter_hum <- filter(bfhum, datos_dev1$humidity[1:500])
butter_hum <- filtfilt(bfhum, datos_dev1$humidity[1:500])
points(1:500, butter_hum, col="black", pch=20)

#CO
mav_co<- mav(datos_dev1$co[1:500],10)
mav_co_2 <- mav(datos_dev1$co[1:500],20) #este pruebo con dos tamannos de ventana
plot(datos_dev1$co[1:500],col="red", type = "l")
lines(mav_co,col="green")
lines(mav_co_2, col="brown")
#points(1:500, hempel_co$y, col="red", pch=20)

bfco <- butter(2, 1/50, type="low")
butter_co <- filtfilt(bfco, datos_dev1$co[1:500])
#butter_co <- filter(bfco, datos_dev1$co[1:500])
points(1:500, butter_co, col="black", pch=20)

butter_co

