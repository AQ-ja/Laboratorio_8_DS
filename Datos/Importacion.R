# Loading
library("readxl")
library(ggplot2)

#--------------------------------------------------
# Analisis Exploratorio
#--------------------------------------------------

# xls files
data_impor <- read_excel("Importacion.xlsx")
View(data_impor)


#Clasifiacion de las variables
#Fecha = 
#Superior = 
# Regular = 
# Diesel  =
# Total_Gasolinas = 
# Total_Importaciones = 

hist(x = data_impor$Superior)
hist(x = data_impor$Regular)
hist(x = data_impor$Diesel)



# Mes con el cual se tiene mayor importacion de Gasolina
gas<-data_impor[order(-data_impor$Superior),]
super<- head(gas,10)
super<-super[,1:2]
View(super)


#Mes en el cual se tiene mayor importacion de Regular 
reg<-data_impor[order(-data_impor$Regular),]
regu<- head(reg,10)
regu<-regu[,1:2]
View(regu)

#Mes en el que se tiene mayor importacion de Disel 
diesel<-data_impor[order(-data_impor$Diesel),]
dies<- head(diesel, 10)
dies<-dies[,1:2]
View(dies)




def<-data.frame(Tipo=c("Gasolina","Regular","Diesel"), Media=c(mean(data_impor$Superior), mean(data_impor$Regular), mean(data_impor$Diesel)))

grafdef<-ggplot(data=def, aes(x=Tipo, y=Media, fill=Tipo)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=as.integer(Promedio)), vjust=1.6, color="blue",
            position = position_dodge(0.9), size=3.5)+
  labs(title="Promedio de importaciones por tipo de gasolina", y="Promedio")+
  theme(legend.position="none")



grafsup<-ggplot(data_impor, aes(x = format(data_impor$Fecha, format='%Y'), y = data_impor$Superior, fill =format(data_impor$Fecha, format='%Y') )) +
  geom_bar(stat = "identity",position=position_dodge())+labs(title="Importaciones por año de gasolina Superior", y="Importacion" , x='Año')+
  theme(legend.position="none")


#----------------------------------------------
# Series de tiempo y modelos 
#----------------------------------------------

#Librerias necesarias
library(forecast)
library(tseries)
library(fUnitRoots)
library(ggfortify)
library("readxl")

impor <- read_excel("Importacion.xlsx")

super<-impor[c('Fecha','Superior')]
regular<-impor[c('Fecha','Regular')]
diesel<-impor[c('Fecha','Diesel')]

# Para el combustible Superior 
super_ts <- ts( super$`Superior`, start = c(2001,1),frequency = 12)

# Inicio de la serie
start(super_ts)
#Final de la serie
end(super_ts)
#Frecuencia de la serie 
frequency(super_ts)

#Resultado en forma de grafica. 
plot(super_ts)
abline(reg=lm(super_ts~time(super_ts)), col=c("red"))

#Grafica para ver la estacionalidad 
plot(aggregate(super_ts,FUN=mean))
dec.Super<-decompose(super_ts)
plot(dec.Super)


####### Pasamos a ver la estimacion 
train <- head(super_ts, round(length(super_ts) * 0.7))
h <- length(super_ts) - length(train)
test <- tail(super_ts, h)

# se le aplica una transformacion logistica 
Superlog<-log(train)
plot(descompose(train))
plot(train)


#raices unitarias 
adfTest(train)
unitrootTest(train)

#Aplicando diferenciacion 
adfTest(diff(train))
unitrootTest(diff(train))



#grafico de autocorrelacion
acf(Superlog, 50)
#para una correlacion parcial 
pacf(Superlog, 50)

#Comprobar estacionalidad 
traines <- decompose(train)
plot(traines$seasonal)


#Para hacer el modelo ARIMA
fitArima<-arima(Superlog, order = c(1,2,2), seasonal =c(1,1,0))
fautoarima<-auto.arima(train)


#####PREDICCION 
auto.arima(super_ts)
fit <- arima(log(super_ts), c(0,1,1),seasonal = list(order = c(0,1,1), period = 12))
predi<-predict(fit, n.ahead = 3)
ts.plot(super_ts,2.718^pred$pred, log = "y", lty = c(1,3))
fit2 <- arima(log(super_ts), c(2, 1, 1),seasonal = list(order = c(0, 1, 0), period = 12))
forecastAP <- forecast(fit2, level = c(95), h = 3)
autoplot(forecastAP)



### Prohet 
library(prophet)
library(zoo)

df<-data.frame(ds=as.Date(as.yearmon(time(train))),y=as.matrix(train))
testdf<-data.frame(ds=as.Date(as.yearmon(time(test))),y=as.matrix(test) )
head(df)
fitProphet<-prophet(df,yearly.seasonality = T,weekly.seasonality = T)
future <- make_future_dataframe(fitProphet,periods = h,freq = "month", include_history = T)
p <- predict(fitProphet,future)
p<-p[,c("ds","yhat","yhat_lower","yhat_upper")]
plot(fitProphet,p)
pred<-tail(p,h)
pred$y<-testdf$y

ggplot(pred, aes(x=ds, y=yhat)) +
  geom_line(size=1, alpha=0.8) +
  geom_ribbon(aes(ymin=yhat_lower, ymax=yhat_upper), fill="blue", alpha=0.2) +
  geom_line(data=pred, aes(x=ds, y=y),color="red")










#Para el combustible Regular 

regular_ts <- ts( regular$`Regular`, start = c(2001,1),frequency = 12)

start(regular_ts)
end(regular_ts)
frequency(regular_ts)
plot(regular_ts)

abline(reg=lm(regular_ts~time(regular_ts)), col=c("red"))
plot(aggregate(regular_ts,FUN=mean))
dec.Regul<-decompose(regular_ts)
plot(dec.Regul)
plot(dec.Regul$seasonal)


# Para el combustible Diesel

diesel_ts <- ts( diesel$`Diesel`, start = c(2001,1),frequency = 12)

start(diesel_ts)
end(diesel_ts)
frequency(diesel_ts)
plot(diesel_ts)

abline(reg=lm(diesel_ts~time(diesel_ts)), col=c("red"))
plot(aggregate(diesel_ts,FUN=mean))
dec.Diesel<-decompose(diesel_ts)
plot(dec.Diesel)
plot(dec.Diesel$seasonal)






