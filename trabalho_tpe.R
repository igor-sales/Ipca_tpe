#Abrindo um banco
rm(list = ls())
setwd("C:\\Users\\Igor Salles\\Desktop")
base <- read.csv(file.choose(), sep = ";")

#########pacotes
install.packages("tseries")
install.packages("urca")
install.packages("forescat")
install.packages("rugarch")
install.packages("forecast")
install.packages("TSA")
install.packages("seas")
library(tseries)
library(urca)
library(tseries)
library(rugarch)
library(TSA)
library(forecast)
library(seas)

########## Serieeeee ##########
colnames(base)[4] <- "ipca"
ipca <- ts(base[, 4], start = c(1994, 1), frequency = 12)
ipca <- window(ipca, 2000, 2017)
plot(ipca, type = "l")

######## Decomposição da serie
# plot(stl(log(ipca),"periodic"))
x <- decompose(ipca)
plot(x)
par(mfrow = c(1, 2))
acf(ipca)
pacf(ipca)
#ou
forecast::ggtsdisplay(ipca, main = "Série da Dif.")

###Teste de deck-fuller
modelo <-
  ur.df(ipca, type = "none", selectlags = "AIC")#Teste sem intercepto e sem tendência
modelo2 <-
  ur.df(ipca, type = "drift", selectlags = "AIC") #Teste com intercepto e sem tendência
modelo3 <-
  ur.df(ipca, type = "trend", selectlags = "AIC") #Teste com intercepto e com tendência
summary(modelo)
summary(modelo2)
summary(modelo3)
### nota-se pelo teste de dick fuller que  não rejeita a hipotese nula
### logo ela é não estadcionária. vamos fazer a diferença.

### Acf e pacf - nota-se pelo grafico que fazer 1 diferença
dif_ipca = diff(ipca)
plot(dif_ipca, type = "l")

##ACF e pacf
par(mfrow = c(1, 2))
acf(dif_ipca)
pacf(dif_ipca)
#ou
forecast::ggtsdisplay(dif_ipca, main = "Série da Dif.")

# Fazendo novamente o teste de de ACF e PACF
modelo <-
  ur.df(diff(log(ipca)), type = "none", selectlags = "AIC")#Teste sem intercepto e sem tendência
modelo2 <-
  ur.df(diff(log(ipca)), type = "drift", selectlags = "AIC") #Teste com intercepto e sem tendência
modelo3 <-
  ur.df(diff(log(ipca)), type = "trend", selectlags = "AIC") #Teste com intercepto e com tendência
summary(modelo)
summary(modelo2)
summary(modelo3)

############## Verificiado sazionalidade##################

#c<-season(ipca)
#model<-lm(ipca~c-1)
#summary(model)
#xx<-length(ipca)
#xxx<-length(model$coefficients)
#a<-rep(model$coefficients,(floor(xx/xxx)))
#for (i in 1:(xx-(xxx*floor(xx/xxx))))
#a<-c(a,model$coefficients[i])
#deseason<-ipca-a
#acf(deseason)

##### Verificando qual o melhor medolo arima ####
modd <- mat.or.vec(9, 3)
k <- 1
for (i in 0:2) {
  for (j in 0:2) {
    xreg <- seasonaldummy(diff(ipca))
    modelo_arima <-
      arima(
        diff(ipca),
        order = c(i, 0, j),
        xreg = xreg,
        include.mean = TRUE,
        method = "ML"
      )
    modd[k, 1] <- paste(i, 0, j)
    modd[k, 2] <- AIC(modelo_arima)
    modd[k, 3] <- BIC(modelo_arima)
    k <- k + 1
  }
}
colnames(modd) <- c("Modelo Arima", "AIC", "BIC")
modd

###Alternativa
xreg <- seasonaldummy(diff(ipca))
modelo_arima <-
  arima(
    diff(ipca),
    order = c(1, 0, 0),
    xreg = xreg,
    include.mean = TRUE,
    method = "ML"
  )
#observa que o melhor modelo foi o modelo Ar(1) ou ARima(1,0,0) com sazonalidade


######### análise dos resíduos #############
########### comando do slide ########

Box.test(modelo_arima$residuals, lag = 10, type = "Ljung-Box")
qqnorm(modelo_arima$residuals)
qqline(modelo_arima$residuals, col = "red")
jarque.bera.test(modelo_arima$residuals)

####### Comando usando a função forecast
forecast::checkresiduals(modelo_arima[[1]], lag.max = 24, test = FALSE)

############### Previsão ##############
previsao <- forecast(modelo_arima[[1]], h = 10)

autoplot(previsao)

plot(forecast(modelo_arima[[1]], h = 4), include = 100)


modelo_arima_1 <-
  arima(
    diff(ipca),
    order = c(1, 0, 0),
    seasonal = c(),
    include.mean = TRUE,
    method = "ML"
  )
forecast(modelo_arima_1[[1]], h = 10)
