library(DescTools)


Perfil <- read.csv2("dados/perfilManaus.csv")
names(Perfil) <- c("Dist", "Cota")
X <- Perfil$Dist
Perfil$Cota <- Perfil$Cota - 0.18353
Y <- Perfil$Cota
Y <- -Perfil$Cota + Perfil$Cota[1]

plot(X, Y, type = "l")
AUC(x = X, y = Y)


Cotas <- seq(10, 30, 0.5)
Areas <- array(NA, length(Cotas))
for(i in seq_along(Cotas)){
  
  X <- Perfil$Dist
  Y <- ifelse(-Perfil$Cota + Cotas[i] <= 0, 0, -Perfil$Cota + Cotas[i])
  
  Areas[i] <- AUC(x = X, y = Y)
}

X <- Cotas^2
fit <- lm(Areas ~ Cotas + X)
fit <- lm(Areas ~ Cotas)

plot(Cotas, Areas, pch = 20)
lines(Cotas, predict(fit, newdata = data.frame("Cotas" = Cotas, "X" = X)), col = 2)
text(16, 97000,
     paste0("Area = ", round(fit$coefficients[1]), " + ",
            round(fit$coefficients[2]), " * Cotas + ",
            round(fit$coefficients[3], 3), " * Cotas²"))

CotaArea <- data.frame("Cota" = Cotas, "Area" = Areas)
write.csv2(CotaArea, "CotaArea.csv", row.names = FALSE)


niveis <- read.csv2("dados/ttparicatubaManaus.csv")
niveis$X <- as.Date(niveis$X, "%d/%m/%Y")
names(niveis)[1] <- "Data"
head(niveis)
plot(niveis$TT_Prba-niveis$Manaus, type = "l")
x <- niveis$TT_Prba-niveis$Manaus
x <- x[complete.cases(niveis$TT_Prba)]
head(niveis)
library(forecast)
ACF <- Pacf(ts(x, start = c(2014, 274), frequency = 365.25)) 
auto.arima(ts(x, start = c(2014, 274), frequency = 365.25))

head(ACF)
?
format(as.Date("2014-10-01"), "%j")

length(niveis$Manaus[-1])
length(x)
plot(niveis$Manaus, x)
library(biwavelet)
biwavelet::wt(rbind(c(1:length(ts(x, start = c(2014, 274), frequency = 365.25)))),
              x)
