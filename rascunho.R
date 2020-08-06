rm(list = ls())

library(readr)
library(ggplot2)
library(dplyr)

PV <- read_csv2("dados/ResumoDescarga_T_15400000.txt", skip = 10) %>%
  mutate(Data = as.Date(Data, "%d/%m/%Y"),
         Hora = as.POSIXct(Hora, "%d/%m/%Y %H:%M:%S", tz = "GMT"))

X <- PV$Cota
Y <- PV$Vazao
fit <- lm(log(Y) ~ log(X/10))

plot(X/100, Y, bty = "n", pch = 20)

# Fitting the power law
# Note that the start argument requires a list with initial (rough!) estimates of the coefficients to be estimated
power.nls <- nls(Y ~ a * (X/100 - h0) ^ n,
                 start = list(a = fit$coefficients[1], n = fit$coefficients[2], h0 = -1))

library(minpack.lm)
power.nls <- nlsLM(Y ~ a * (X - h0) ^ n,
                   start = list(a = fit$coefficients[1], n = fit$coefficients[2], h0 = -1))

lines((0:20), 
      predict(power.nls, 
              newdata = data.frame("X" = (0:20)*100)), col = "red", lwd = 4)

summary(power.nls)$coefficients[1,1]
summary(power.nls)$coefficients[2,1]
summary(power.nls)$coefficients[3,1]

SF <- summary(power.nls)$coefficients
SF$coefficients

Manaus <- read.csv2("dados/Manaus.csv")
Manaus$data <- as.POSIXct(Manaus$data, "%d/%m/%Y %H:%M", tz = "GMT")
Manaus$DIA <- as.Date(Manaus$DIA, "%d/%m/%Y")

X <- Manaus$cota
Y <- Manaus$vazao

fit <- lm(log(Y) ~ log(X/10))

plot(X/100, Y, bty = "n", pch = 20)

# Fitting the power law
# Note that the start argument requires a list with initial (rough!) estimates of the coefficients to be estimated
power.nls <- nls(Y ~ a * (X/100 - h0) ^ n,
                 start = list(a = fit$coefficients[1], n = fit$coefficients[2], h0 = -1))

library(minpack.lm)
power.nls <- nlsLM(Y ~ a * (X - h0) ^ n,
                   start = list(a = fit$coefficients[1], n = fit$coefficients[2], h0 = -1))

lines((0:30), 
      predict(power.nls, 
              newdata = data.frame("X" = (0:30)*100)), col = "red", lwd = 4)






