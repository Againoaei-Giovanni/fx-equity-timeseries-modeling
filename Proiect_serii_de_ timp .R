
# ============================================================

# Instalare pachete 
install.packages(c("tidyverse", "tseries", "urca", "forecast",
                   "lmtest", "aTSA", "ggplot2", "moments",
                   "rugarch", "FinTS", "vars", "tsDyn"), dependencies = TRUE)

# Curățare mediu
rm(list = ls())
graphics.off()

# Încărcare biblioteci
library(tidyverse)
library(tseries)
library(urca)
library(forecast)
library(lmtest)
library(aTSA)
library(ggplot2)
library(moments)
library(rugarch)
library(FinTS)
library(vars)
library(tsDyn)


# ============================================================
# CITIREA ȘI PREGĂTIREA DATELOR
# ============================================================

setwd("C:/Users/Giovanni/Desktop/serii de timp 1")
data <- read.csv("Date_serii_de_timp_curatate.csv")

# Conversie dată
data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
data <- data[order(data$Date), ]

# Serii temporale zilnice (frequency = 252 zile bursiere/an)
BET_ts <- ts(data$BET, frequency = 252)
FX_ts  <- ts(data$FX,  frequency = 252)

cat("Număr observații BET:", length(BET_ts), "\n")
cat("Număr observații FX: ", length(FX_ts),  "\n")
cat("Perioada: ", format(min(data$Date)), "–", format(max(data$Date)), "\n")


# ============================================================
# TASK 1 – ANALIZA PRELIMINARĂ
# ============================================================

# Graficul BET
ggplot(data, aes(x = Date, y = BET)) +
  geom_line(color = "steelblue", linewidth = 0.4) +
  labs(title = "Evolutia indicelui BET",
       x = "Data", y = "Valoare") +
  theme_minimal()

# Graficul FX
ggplot(data, aes(x = Date, y = FX)) +
  geom_line(color = "darkred", linewidth = 0.4) +
  labs(title = "Evolutia cursului de schimb RON/EUR",
       x = "Data", y = "RON/EUR") +
  theme_minimal()

# Grafic comparativ normalizat
data_norm <- data %>%
  mutate(BET_norm = BET / first(BET) * 100,
         FX_norm  = FX  / first(FX)  * 100) %>%
  pivot_longer(cols = c(BET_norm, FX_norm), names_to = "Serie", values_to = "Valoare")

ggplot(data_norm, aes(x = Date, y = Valoare, color = Serie)) +
  geom_line(linewidth = 0.4) +
  scale_color_manual(values = c("BET_norm" = "steelblue", "FX_norm" = "darkred"),
                     labels = c("BET (normalizat)", "FX (normalizat)")) +
  labs(title = "BET vs FX – evoluție normalizată (baza 100 = feb 2006)",
       x = "Data", y = "Indice (100 = start)", color = "") +
  theme_minimal()

# Descompunere STL
BET_stl <- stl(BET_ts, s.window = "periodic", robust = TRUE)
autoplot(BET_stl) + ggtitle("Descompunere STL – BET")

FX_stl <- stl(FX_ts, s.window = "periodic", robust = TRUE)
autoplot(FX_stl) + ggtitle("Descompunere STL – FX (RON/EUR)")

# ACF și PACF preliminare
ggtsdisplay(BET_ts, lag.max = 60, main = "BET: nivel – ACF și PACF")
ggtsdisplay(FX_ts,  lag.max = 60, main = "FX: nivel – ACF și PACF")


# ============================================================
# TASK 2 – ANALIZA DESCRIPTIVĂ
# ============================================================

# Statistici descriptive
desc_stats <- function(x, nume) {
  cat("\n====", nume, "====\n")
  cat("Media:          ", round(mean(x), 4), "\n")
  cat("Dev. standard:  ", round(sd(x), 4), "\n")
  cat("Minim:          ", round(min(x), 4), "\n")
  cat("Maxim:          ", round(max(x), 4), "\n")
  cat("Cuantila 25%:   ", round(quantile(x, 0.25), 4), "\n")
  cat("Mediana:        ", round(median(x), 4), "\n")
  cat("Cuantila 75%:   ", round(quantile(x, 0.75), 4), "\n")
  cat("Asimetrie:      ", round(skewness(x), 4), "\n")
  cat("Kurtoză:        ", round(kurtosis(x), 4), "\n")
}

desc_stats(data$BET, "BET")
desc_stats(data$FX,  "FX (RON/EUR)")

# Histograme
ggplot(data, aes(x = BET)) +
  geom_histogram(aes(y = after_stat(density)), bins = 60,
                 fill = "steelblue", color = "white", alpha = 0.7) +
  geom_density(color = "darkblue", linewidth = 1) +
  labs(title = "Distribuția BET", x = "Valoare", y = "Densitate") +
  theme_minimal()

ggplot(data, aes(x = FX)) +
  geom_histogram(aes(y = after_stat(density)), bins = 60,
                 fill = "darkred", color = "white", alpha = 0.7) +
  geom_density(color = "black", linewidth = 1) +
  labs(title = "Distribuția FX (RON/EUR)", x = "RON/EUR", y = "Densitate") +
  theme_minimal()

# Boxplot anual
ggplot(data %>% mutate(An = format(Date, "%Y")),
       aes(x = An, y = BET)) +
  geom_boxplot(fill = "steelblue", alpha = 0.6) +
  labs(title = "Boxplot anual – BET", x = "An", y = "Valoare") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data %>% mutate(An = format(Date, "%Y")),
       aes(x = An, y = FX)) +
  geom_boxplot(fill = "darkred", alpha = 0.6) +
  labs(title = "Boxplot anual – FX (RON/EUR)", x = "An", y = "RON/EUR") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Randamente logaritmice
data$rBET <- c(NA, diff(log(data$BET)))
data$rFX  <- c(NA, diff(log(data$FX)))

ggplot(data[-1,], aes(x = Date, y = rBET)) +
  geom_line(color = "steelblue", linewidth = 0.3) +
  labs(title = "Randamente logaritmice BET",
       x = "Data", y = "ln(BET_t / BET_{t-1})") +
  theme_minimal()

ggplot(data[-1,], aes(x = Date, y = rFX)) +
  geom_line(color = "darkred", linewidth = 0.3) +
  labs(title = "Randamente logaritmice FX",
       x = "Data", y = "ln(FX_t / FX_{t-1})") +
  theme_minimal()

desc_stats(na.omit(data$rBET), "Randamente log BET")
desc_stats(na.omit(data$rFX),  "Randamente log FX")


# ============================================================
# TASK 3 – TESTAREA STAȚIONARITĂȚII
# ============================================================

lBET <- log(data$BET)
lFX  <- log(data$FX)

lBET_ts <- ts(lBET, frequency = 252)
lFX_ts  <- ts(lFX,  frequency = 252)

rBET_ts <- ts(na.omit(data$rBET), frequency = 252)
rFX_ts  <- ts(na.omit(data$rFX),  frequency = 252)

# BET în log-nivel
cat("\n=== TESTE STAȚIONARITATE: log(BET) – NIVEL ===\n")
ggtsdisplay(lBET_ts, lag.max = 60, main = "log(BET) – nivel")
cat("\n-- ADF (trend) --\n")
summary(ur.df(lBET_ts, type = "trend", lags = 20, selectlags = "AIC"))
cat("\n-- PP (trend) --\n")
summary(ur.pp(lBET_ts, type = "Z-tau", model = "trend"))
cat("\n-- KPSS (tau) --\n")
summary(ur.kpss(lBET_ts, type = "tau"))

# BET în randamente
cat("\n=== TESTE STAȚIONARITATE: randamente log(BET) ===\n")
ggtsdisplay(rBET_ts, lag.max = 60, main = "Randamente log(BET)")
cat("\n-- ADF (none) --\n")
summary(ur.df(rBET_ts, type = "none", lags = 20, selectlags = "AIC"))
cat("\n-- PP (constant) --\n")
summary(ur.pp(rBET_ts, type = "Z-tau", model = "constant"))
cat("\n-- KPSS (mu) --\n")
summary(ur.kpss(rBET_ts, type = "mu"))

# FX în log-nivel
cat("\n=== TESTE STAȚIONARITATE: log(FX) – NIVEL ===\n")
ggtsdisplay(lFX_ts, lag.max = 60, main = "log(FX) – nivel")
cat("\n-- ADF (trend) --\n")
summary(ur.df(lFX_ts, type = "trend", lags = 20, selectlags = "AIC"))
cat("\n-- PP (trend) --\n")
summary(ur.pp(lFX_ts, type = "Z-tau", model = "trend"))
cat("\n-- KPSS (tau) --\n")
summary(ur.kpss(lFX_ts, type = "tau"))

# FX în randamente
cat("\n=== TESTE STAȚIONARITATE: randamente log(FX) ===\n")
ggtsdisplay(rFX_ts, lag.max = 60, main = "Randamente log(FX)")
cat("\n-- ADF (none) --\n")
summary(ur.df(rFX_ts, type = "none", lags = 20, selectlags = "AIC"))
cat("\n-- PP (constant) --\n")
summary(ur.pp(rFX_ts, type = "Z-tau", model = "constant"))
cat("\n-- KPSS (mu) --\n")
summary(ur.kpss(rFX_ts, type = "mu"))




# ============================================================
# TASK 4 – IDENTIFICAREA MODELULUI ARIMA
# ============================================================

# Împărțire train/test
n       <- length(rBET_ts)
n_test  <- 252
n_train <- n - n_test

rBET_train <- ts(rBET_ts[1:n_train], frequency = 252)
rBET_test  <- tail(rBET_ts, n_test)

rFX_train  <- ts(rFX_ts[1:n_train], frequency = 252)
rFX_test   <- tail(rFX_ts, n_test)

cat("Observații train:", n_train, "| Observații test:", n_test, "\n")

# Corelogramă BET
ggtsdisplay(rBET_train, lag.max = 60,
            main = "Randamente log(BET) – Train: ACF și PACF")

# Modele candidate BET
fit_BET_1 <- arima(rBET_train, order = c(1, 0, 1))
fit_BET_2 <- arima(rBET_train, order = c(1, 0, 0))
fit_BET_3 <- arima(rBET_train, order = c(0, 0, 1))
fit_BET_4 <- arima(rBET_train, order = c(2, 0, 2))
fit_BET_5 <- arima(rBET_train, order = c(3, 0, 0))

# Validare BET
cat("\n=== FIT BET ARIMA(1,0,1) ===\n")
summary(fit_BET_1); coeftest(fit_BET_1)
checkresiduals(fit_BET_1)
jarque.bera.test(residuals(fit_BET_1))
arch.test(fit_BET_1, output = TRUE)

cat("\n=== FIT BET ARIMA(1,0,0) ===\n")
summary(fit_BET_2); coeftest(fit_BET_2)
checkresiduals(fit_BET_2)
jarque.bera.test(residuals(fit_BET_2))
arch.test(fit_BET_2, output = TRUE)

cat("\n=== FIT BET ARIMA(0,0,1) ===\n")
summary(fit_BET_3); coeftest(fit_BET_3)
checkresiduals(fit_BET_3)
jarque.bera.test(residuals(fit_BET_3))
arch.test(fit_BET_3, output = TRUE)

cat("\n=== FIT BET ARIMA(2,0,2) ===\n")
summary(fit_BET_4); coeftest(fit_BET_4)
checkresiduals(fit_BET_4)
jarque.bera.test(residuals(fit_BET_4))
arch.test(fit_BET_4, output = TRUE)

cat("\n=== FIT BET ARIMA(3,0,0) ===\n")
summary(fit_BET_5); coeftest(fit_BET_5)
checkresiduals(fit_BET_5)
jarque.bera.test(residuals(fit_BET_5))
arch.test(fit_BET_5, output = TRUE)

# AIC comparativ BET
cat("\nAIC ARIMA BET:\n")
cat("ARIMA(1,0,1):", fit_BET_1$aic, "\n")
cat("ARIMA(1,0,0):", fit_BET_2$aic, "\n")
cat("ARIMA(0,0,1):", fit_BET_3$aic, "\n")
cat("ARIMA(2,0,2):", fit_BET_4$aic, "\n")
cat("ARIMA(3,0,0):", fit_BET_5$aic, "\n")

# Acuratețe pe setul de test BET
cat("\nAcuratețe test – ARIMA(1,0,1):\n")
forecast::accuracy(forecast::forecast(fit_BET_1, h = n_test), rBET_test)
cat("\nAcuratețe test – ARIMA(1,0,0):\n")
forecast::accuracy(forecast::forecast(fit_BET_2, h = n_test), rBET_test)
cat("\nAcuratețe test – ARIMA(0,0,1):\n")
forecast::accuracy(forecast::forecast(fit_BET_3, h = n_test), rBET_test)
cat("\nAcuratețe test – ARIMA(2,0,2):\n")
forecast::accuracy(forecast::forecast(fit_BET_4, h = n_test), rBET_test)
cat("\nAcuratețe test – ARIMA(3,0,0):\n")
forecast::accuracy(forecast::forecast(fit_BET_5, h = n_test), rBET_test)

# Model optim BET
best_BET <- fit_BET_4  

# Grafic prognoze BET
fc_BET <- forecast::forecast(best_BET, h = 60)
autoplot(fc_BET) +
  ggtitle("Previziuni ARIMA(2,0,2) – randamente log(BET)") +
  xlab("Timp") + ylab("Randament") +
  theme_minimal()

# Corelogramă FX
ggtsdisplay(rFX_train, lag.max = 60,
            main = "Randamente log(FX) – Train: ACF și PACF")

# Modele candidate FX
fit_FX_1 <- arima(rFX_train, order = c(1, 0, 1))
fit_FX_2 <- arima(rFX_train, order = c(1, 0, 0))
fit_FX_3 <- arima(rFX_train, order = c(0, 0, 1))

# Validare FX
cat("\n=== FIT FX ARIMA(1,0,1) ===\n")
summary(fit_FX_1); coeftest(fit_FX_1)
checkresiduals(fit_FX_1)
jarque.bera.test(residuals(fit_FX_1))
arch.test(fit_FX_1, output = TRUE)

cat("\n=== FIT FX ARIMA(1,0,0) ===\n")
summary(fit_FX_2); coeftest(fit_FX_2)
checkresiduals(fit_FX_2)
jarque.bera.test(residuals(fit_FX_2))
arch.test(fit_FX_2, output = TRUE)

cat("\n=== FIT FX ARIMA(0,0,1) ===\n")
summary(fit_FX_3); coeftest(fit_FX_3)
checkresiduals(fit_FX_3)
jarque.bera.test(residuals(fit_FX_3))
arch.test(fit_FX_3, output = TRUE)

# AIC comparativ FX
cat("\nAIC ARIMA FX:\n")
cat("ARIMA(1,0,1):", fit_FX_1$aic, "\n")
cat("ARIMA(1,0,0):", fit_FX_2$aic, "\n")
cat("ARIMA(0,0,1):", fit_FX_3$aic, "\n")

# Acuratețe pe setul de test FX
rFX_test <- tail(rFX_ts, n_test)
cat("\nAcuratețe test – ARIMA(1,0,1):\n")
forecast::accuracy(forecast::forecast(fit_FX_1, h = n_test), rFX_test)
cat("\nAcuratețe test – ARIMA(1,0,0):\n")
forecast::accuracy(forecast::forecast(fit_FX_2, h = n_test), rFX_test)
cat("\nAcuratețe test – ARIMA(0,0,1):\n")
forecast::accuracy(forecast::forecast(fit_FX_3, h = n_test), rFX_test)

# Model optim FX
best_FX <- fit_FX_3  

# Grafic prognoze FX
fc_FX <- forecast::forecast(best_FX, h = 60)
autoplot(fc_FX) +
  ggtitle("Previziuni ARIMA(0,0,1) – randamente log(FX)") +
  xlab("Timp") + ylab("Randament") +
  theme_minimal()


# ============================================================
# TASK 5 – SARIMA (verificare sezonalitate)
# ============================================================

# Verificare vizuală sezonalitate săptămânală
ggtsdisplay(rBET_train, lag.max = 60, main = "ACF/PACF randamente BET")
ggtsdisplay(rFX_train,  lag.max = 60, main = "ACF/PACF randamente FX")

# Test formal Kruskal-Wallis
data_no_na <- data[-1, ]
data_no_na$weekday <- weekdays(data_no_na$Date)

cat("\nTest Kruskal-Wallis BET:\n")
kruskal.test(rBET ~ weekday, data = data_no_na)

cat("\nTest Kruskal-Wallis FX:\n")
kruskal.test(rFX ~ weekday, data = data_no_na)




# ============================================================
# TASK 6 – ARCH/GARCH
# ============================================================

# Testul ARCH pe reziduurile modelelor optime
cat("\n=== TEST ARCH – reziduuri ARIMA(2,0,2) BET ===\n")
ArchTest(residuals(best_BET), lags = 12)

cat("\n=== TEST ARCH – reziduuri ARIMA(0,0,1) FX ===\n")
ArchTest(residuals(best_FX), lags = 12)

# Reziduuri la pătrat BET
res_BET  <- residuals(best_BET)
res2_BET <- res_BET^2

par(mfrow = c(2, 1))
plot(res_BET,  type = "l", main = "Reziduuri ARIMA(2,0,2) – BET", ylab = "ε_t")
plot(res2_BET, type = "l", main = "Reziduuri la pătrat – BET", ylab = "ε_t²")
par(mfrow = c(1, 1))
acf(res2_BET, lag.max = 60, main = "ACF reziduuri la pătrat – BET")

# Reziduuri la pătrat FX
res_FX  <- residuals(best_FX)
res2_FX <- res_FX^2

par(mfrow = c(2, 1))
plot(res_FX,  type = "l", main = "Reziduuri ARIMA(0,0,1) – FX", ylab = "ε_t")
plot(res2_FX, type = "l", main = "Reziduuri la pătrat – FX", ylab = "ε_t²")
par(mfrow = c(1, 1))
acf(res2_FX, lag.max = 60, main = "ACF reziduuri la pătrat – FX")

# Estimare GARCH(1,1) BET
spec_garch_BET <- ugarchspec(
  variance.model     = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model         = list(armaOrder = c(2, 2), include.mean = TRUE),
  distribution.model = "std"
)
fit_garch_BET <- ugarchfit(spec = spec_garch_BET, data = rBET_train)
cat("\n=== GARCH(1,1) – BET ===\n")
show(fit_garch_BET)

# Estimare GARCH(1,1) FX
spec_garch_FX <- ugarchspec(
  variance.model     = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model         = list(armaOrder = c(0, 1), include.mean = TRUE),
  distribution.model = "std"
)
fit_garch_FX <- ugarchfit(spec = spec_garch_FX, data = rFX_train)
cat("\n=== GARCH(1,1) – FX ===\n")
show(fit_garch_FX)

# Volatilitate condiționată BET
sigma_BET   <- sigma(fit_garch_BET)
dates_train <- data$Date[2:(n_train + 1)]

vol_BET <- data.frame(Date = dates_train, Volatilitate = as.numeric(sigma_BET))
ggplot(vol_BET, aes(x = Date, y = Volatilitate)) +
  geom_line(color = "steelblue", linewidth = 0.4) +
  labs(title = "Volatilitate condiționată GARCH(1,1) – BET",
       x = "Data", y = "Deviație standard condiționată") +
  theme_minimal()

# Volatilitate condiționată FX
sigma_FX <- sigma(fit_garch_FX)
vol_FX   <- data.frame(Date = dates_train, Volatilitate = as.numeric(sigma_FX))
ggplot(vol_FX, aes(x = Date, y = Volatilitate)) +
  geom_line(color = "darkred", linewidth = 0.4) +
  labs(title = "Volatilitate condiționată GARCH(1,1) – FX",
       x = "Data", y = "Deviație standard condiționată") +
  theme_minimal()


# ============================================================
# TASK 7 – COINTEGRARE
# ============================================================

# Pregătire date pentru cointegrare (log-nivel)
lBET_ts_full <- ts(log(data$BET), frequency = 252)
lFX_ts_full  <- ts(log(data$FX),  frequency = 252)

dset <- cbind(lBET_ts_full, lFX_ts_full)
colnames(dset) <- c("lBET", "lFX")

# Grafic comparativ
autoplot(dset) +
  ylab('') +
  ggtitle('Evoluția seriilor log(BET) și log(FX)') +
  theme_minimal()

# Selectia lagului optim
lagselect <- VARselect(dset, lag.max = 10, type = "const")
lagselect$selection

# Testul Johansen - Trace
cat("\n=== JOHANSEN TRACE ===\n")
ctest1 <- ca.jo(dset, type = "trace", ecdet = "const", K = 10)
summary(ctest1)

# Testul Johansen - MaxEigen
cat("\n=== JOHANSEN MAXEIGEN ===\n")
ctest2 <- ca.jo(dset, type = "eigen", ecdet = "const", K = 10)
summary(ctest2)




# ============================================================
# TASK 8 – MODELUL VAR
# ============================================================

# Estimare VAR cu lag = 10
var_model <- VAR(dset, p = 10, type = "const")
summary(var_model)

# Diagnosticare VAR
cat("\n=== TEST AUTOCORELARE SERIALĂ ===\n")
Serial_var <- serial.test(var_model, lags.pt = 10, type = "PT.asymptotic")
Serial_var

cat("\n=== TEST HETEROSCEDASTICITATE ===\n")
Arch_var <- arch.test(var_model, lags.multi = 15, multivariate.only = TRUE)
Arch_var

cat("\n=== TEST NORMALITATE ===\n")
Norm_var <- normality.test(var_model, multivariate.only = TRUE)
Norm_var

# IRF - Impuls Răspuns
irf1 <- irf(var_model, impulse = "lFX", response = "lBET",
            n.ahead = 20, boot = TRUE)
plot(irf1, ylab = "lBET", main = "Șoc în FX → răspuns BET")

irf2 <- irf(var_model, impulse = "lBET", response = "lFX",
            n.ahead = 20, boot = TRUE)
plot(irf2, ylab = "lFX", main = "Șoc în BET → răspuns FX")

# FEVD - Descompunerea varianței
fevd_var <- fevd(var_model, n.ahead = 10)
plot(fevd_var)

# ============================================================

