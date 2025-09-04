library(e1071)
library(distr)
library(knitr)
library(kableExtra)
library(ggplot2)

dane = read.csv("C:/Users/mikol/OneDrive/Desktop/ryzyko/amzn_us_d_2002.csv") # Amazon.com Inc (AMZN.US)
str(dane)

window_size <- 500
alpha <- 0.99

log_returns <- diff(log(dane$Zamkniecie)) # logarytmiczne stopy zwrotu
log_returns_strata <- -log_returns # strata

dane$Data=as.Date(dane$Data)
dates <- dane$Data[(window_size + 1):length(dane$Data)]
min(dane$Data)
max(dane$Data)
min(dates)
max(dates)
# Statystyki opisowe ------------------------------------------------------

# Usunięcie kolumny Data
dane_num <- dane[, !names(dane) %in% c("Data")]

# Funkcja do współczynnika zmienności
var_coef <- function(x) {
  if (mean(x, na.rm = TRUE) == 0) {
    return(NA)  # Unikanie dzielenia przez 0
  }
  return(sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100)
}

# Obliczanie statystyk
summary_stats <- data.frame(
  Zmienna = colnames(dane_num),
  Średnia = sapply(dane_num, function(x) mean(x, na.rm = TRUE)),
  Mediana = sapply(dane_num, function(x) median(x, na.rm = TRUE)),
  Min = sapply(dane_num, function(x) min(x, na.rm = TRUE)),
  Max = sapply(dane_num, function(x) max(x, na.rm = TRUE)),
  Odchylenie_standardowe = sapply(dane_num, function(x) sd(x, na.rm = TRUE)),
  Współczynnik_zmienności = sapply(dane_num, var_coef),
  Skośność = sapply(dane_num, function(x) skewness(x, na.rm = TRUE)),
  Kurtoza = sapply(dane_num, function(x) kurtosis(x, na.rm = TRUE))
)


# wykres wyceny i stóp zwrotu ---------------------------------------------


# Wykres wyceny
ggplot(dane, aes(x = as.Date(Data), y = Zamkniecie)) +
  geom_line(color = "blue") +
  labs(x = "Data", y = "Cena zamknięcia", title = "Wycena akcji Apple (AAPL)") +
  scale_x_date(breaks = seq(as.Date("2007-01-03"), as.Date("2024-12-31"), by = "year"),
               labels = format(seq(as.Date("2007-01-03"), as.Date("2024-12-31"), by = "year"), "%Y")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Wykres logarytmicznych stóp zwrotu
dane_log_returns <- data.frame(Data = dane$Data[2:length(dane$Data)], LogReturns = log_returns)
ggplot(dane_log_returns, aes(x = as.Date(Data), y = LogReturns)) +
  geom_line(color = "red") +
  labs(x = "Data", y = "Logarytmiczne stopy zwrotu", title = "Logarytmiczne stopy zwrotu Apple (AAPL)") +
  scale_x_date(breaks = seq(as.Date("2007-01-03"), as.Date("2024-12-31"), by = "year"),
               labels = format(seq(as.Date("2007-01-03"), as.Date("2024-12-31"), by = "year"), "%Y")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# VaR ---------------------------------------------------------------------


# Historyczna -------------------------------------------------------------

compute_VaR_ES <- function(returns_window, alpha) {
  VaR <- quantile(returns_window, alpha)
  ES <- mean(returns_window[returns_window > VaR])
  return(list(VaR = VaR, ES = ES))
}

# Pętla przesuwająca okno
VaR_values <- numeric()
ES_values <- numeric()

# Obliczenie VaR i ES dla okna
for (i in 1:(length(log_returns_strata) - window_size + 1)) {
  window <- log_returns_strata[i:(i + window_size - 1)]
  res <- compute_VaR_ES(window, alpha)
  VaR_values <- c(VaR_values, res$VaR)
  ES_values <- c(ES_values, res$ES)
}

# Wykres VaR i ES
plot(as.Date(dates), VaR_values, type = "l", col = "red", 
     xlab = "Data", ylab = "Wartość", main = "99% VaR i ES - Metoda Historyczna",
     ylim = range(VaR_values, ES_values))
lines(as.Date(dates), ES_values, col = "blue")
legend("topright", legend=c("VaR", "ES"), fill=c("red", "blue"), cex=0.7)



# Historyczna z wagami ----------------------------------------------------

# obliczenia VaR i ES metodą historyczną z wagami
compute_weighted_VaR_ES <- function(returns_window, alpha) {
  n <- length(returns_window)
  weights <- (alpha^(n:1 - 1) * (1 - alpha)) / (1 - alpha^n)#wagi
  sorted_returns <- sort(returns_window)# Sortowanie stóp zwrotu
  sorted_weights <- weights[order(returns_window)]# Przypisanie wag do posortowanych stóp zwrotu
  cumulative_weights <- cumsum(sorted_weights)# Obliczenie skumulowanych wag
  # indeksu VaR (pierwsza wartość, gdzie skumulowana waga >= alpha)
  VaR_index <- which(cumulative_weights >= alpha)[1]
  VaR <- sorted_returns[VaR_index]# VaR
  #ES
  ES <- sum(sorted_returns[VaR_index:length(sorted_returns)] * 
              sorted_weights[VaR_index:length(sorted_weights)]) / 
    sum(sorted_weights[VaR_index:length(sorted_weights)])
  
  return(list(VaR = VaR, ES = ES))
}

# Pętla przesuwająca okno
weighted_VaR_values <- numeric()
weighted_ES_values <- numeric()

for (i in 1:(length(log_returns_strata) - window_size + 1)) {
  window <- log_returns_strata[i:(i + window_size - 1)]
  res <- compute_weighted_VaR_ES(window, alpha)
  weighted_VaR_values <- c(weighted_VaR_values, res$VaR)
  weighted_ES_values <- c(weighted_ES_values, res$ES)
}

plot(as.Date(dates), weighted_VaR_values, type = "l", col = "red", lty = 1,
     xlab = "Data", ylab = "Wartość", main = "99% VaR i ES - Metoda Historyczna z Wagami",
     ylim = range(weighted_VaR_values, weighted_ES_values))
lines(as.Date(dates), weighted_ES_values, col = "blue", lty = 1)
legend("topright", legend=c("VaR", "ES"), fill=c("red", "blue"), cex=0.7)


# EWMA --------------------------------------------------------------------

q <- 0.99 

# Funkcja do obliczania zmienności EWMA
ewma_volatility <- function(returns, lambda) {
  n <- length(returns)
  sigma_squared <- numeric(n)
  sigma_squared[1] <- var(returns)
  for (t in 2:n) {
    sigma_squared[t] <- lambda * sigma_squared[t - 1] + (1 - lambda) * returns[t - 1]^2
  }
  return(sqrt(sigma_squared))
}

lambda <- 0.94  # Współczynnik wygładzania w modelu EWMA

# Obliczenie zmienności EWMA dla całego szeregu
zmewma <- ewma_volatility(log_returns_strata, lambda)

ewma_VaR_values <- numeric(length(log_returns_strata) - window_size + 1)
ewma_ES_values <- numeric(length(log_returns_strata) - window_size + 1)

for (i in 1:(length(log_returns_strata) - window_size + 1)) {
  window <- log_returns_strata[i:(i + window_size - 1)]
  
  # Skalowanie zwrotów zgodnie z metodą EWMA
  scenariuszEwma <- window * zmewma[i + window_size - 1] / zmewma[i:(i + window_size - 1)]
  
  # Obliczenie VaR i ES 
  VaR_scaled <- quantile(scenariuszEwma, alpha)
  ES_scaled <- mean(scenariuszEwma[scenariuszEwma > VaR_scaled])
  
  ewma_VaR_values[i] <- VaR_scaled
  ewma_ES_values[i] <- ES_scaled
}

plot(as.Date(dates), ewma_VaR_values, type = "l", col = "red", lty = 1,
     xlab = "Data", ylab = "Wartość", main = "99% VaR i ES - Metoda EWMA",
     ylim = range(ewma_VaR_values, ewma_ES_values)) 
lines(as.Date(dates), ewma_ES_values, col = "blue", lty = 1)  
legend("topright", legend = c("VaR", "ES"), fill = c("red", "blue"), cex = 0.7)

# Testy wsteczne ----------------------------------------------------------

# Test Kupca --------------------------------------------------------------
tabela_test_kupiec = data.frame("odrzucone" = rep(1,3),
                                "nieodrzucone" = rep(1,3),
                                row.names = c("Historyczna", "Hist. z wagami",
                                              "EWMA"))

test_kupca <- function(VaR_hits, alpha) {
  n <- length(VaR_hits)  # Liczba dni
  x <- sum(VaR_hits)     # Liczba przekroczeń (1 = przekroczenie, 0 = brak)
  pi_hat <- x / n        # Szacowane prawdopodobieństwo przekroczeń
  
  L0 <- x * log(alpha) + (n - x) * log(1 - alpha)# Funkcja wiarygodności w H0
  L1 <- x * log(pi_hat) + (n - x) * log(1 - pi_hat)# Funkcja wiarygodności w H1
  LR_uc <- -2 * (L0 - L1)# Statystyka ilorazu wiarygodności
  p_value <- 1 - pchisq(LR_uc, df = 1)# P-wartość testu
  
  list(
    exceptions = x,
    LR_uc = LR_uc,
    p_value = p_value)
}

#metoda historyczna
temp_hist=c()
for(i in 1:(length(log_returns_strata) - window_size+1)){
  Var_hit_hist=ifelse(log_returns_strata[i:(i+window_size-1)] > VaR_values[i], 1, 0)
  temp_hist[i]=ifelse(test_kupca(Var_hit_hist, 0.01)$p_value < 0.05, 1, 0)
}

tabela_test_kupiec[1,1]=sum(temp_hist, na.rm=T)
tabela_test_kupiec[1,2]=length(log_returns_strata)-499 - sum(temp_hist, na.rm=T)

#metoda historyczna z wagami
temp_wagi=c()
for(i in 1:(length(log_returns_strata) - window_size+1)){
  Var_hit_wagi=ifelse(log_returns_strata[i:(i+window_size-1)] > weighted_VaR_values[i], 1, 0)
  temp_wagi[i]=ifelse(test_kupca(Var_hit_wagi, 0.01)$p_value < 0.05, 1, 0)
}

tabela_test_kupiec[2,1]=sum(temp_wagi, na.rm=T)
tabela_test_kupiec[2,2]=length(log_returns_strata)-499 - sum(temp_wagi, na.rm=T)

#metoda ewma
temp_ewma=c()
for(i in 1:(length(log_returns_strata) - window_size+1)){
  Var_hit_ewma=ifelse(log_returns_strata[i:(i+window_size-1)] > ewma_VaR_values[i], 1, 0)
  temp_ewma[i]=ifelse(test_kupca(Var_hit_ewma, 0.01)$p_value < 0.05, 1, 0)
}

tabela_test_kupiec[3,1]=sum(temp_ewma, na.rm=T)
tabela_test_kupiec[3,2]=length(log_returns_strata)-499 - sum(temp_ewma, na.rm=T)

kable(tabela_test_kupiec)


# test świateł ------------------------------------------------------------

tabela_test_swiatla = data.frame("Zielone" = rep(1,3),
                                 "Żółte" = rep(1,3),
                                 "Czerwone" = rep(1,3),
                                 row.names = c("Historyczna", "Hist. z wagami",
                                               "EWMA"))

test_swiatel <- function(VaR_hits) {
  
  n <- length(VaR_hits)
  exceptions <- sum(VaR_hits)    
  
  green_limit <- qbinom(c(0.001,0.95), n, 1 - alpha)
  yellow_limit <- qbinom(c(0.95,0.999), n, 1 - alpha)
  
  if (exceptions <= green_limit[2]) {
    zone <- "Green"
  } else if (exceptions <= yellow_limit[2]) {
    zone <- "Yellow"
  } else {
    zone <- "Red"
  }
  
  list(
    exceptions = exceptions,
    green_limit = green_limit,
    zone = zone
  )
}

#metoda historyczna
temp_hist=c()
for(i in 1:(length(log_returns_strata) - window_size +1)){
  Var_hit_hist=ifelse(log_returns_strata[i:(i+499)] > VaR_values[i], 1, 0)
  temp_hist[i]=ifelse(test_swiatel(Var_hit_hist)$zone == "Green", "Zielone", 
                      ifelse(test_swiatel(Var_hit_hist)$zone == "Yellow",
                             "Żółte", "Czerwone"))
}

tab_hist = table(temp_hist)
tabela_test_swiatla[1,1]=ifelse(is.na(tab_hist["Zielone"]), 0, tab_hist["Zielone"])
tabela_test_swiatla[1,2]=ifelse(is.na(tab_hist["Żółte"]), 0, tab_hist["Żółte"])
tabela_test_swiatla[1,3]=ifelse(is.na(tab_hist["Czerwone"]), 0, tab_hist["Czerwone"])



#metoda hist z wagami
temp_wagi=c()
for(i in 1:(length(log_returns_strata) - window_size+1)){
  Var_hit_wagi=ifelse(log_returns_strata[i:(i+499)] > weighted_VaR_values[i], 1, 0)
  temp_wagi[i]=ifelse(test_swiatel(Var_hit_wagi)$zone == "Green", "Zielone", 
                      ifelse(test_swiatel(Var_hit_wagi)$zone == "Yellow",
                             "Żółte", "Czerwone"))
}
tab_wagi = table(temp_wagi)
tabela_test_swiatla[2,1]=ifelse(is.na(tab_wagi["Zielone"]), 0, tab_wagi["Zielone"])
tabela_test_swiatla[2,2]=ifelse(is.na(tab_wagi["Żółte"]), 0, tab_wagi["Żółte"])
tabela_test_swiatla[2,3]=ifelse(is.na(tab_wagi["Czerwone"]), 0, tab_wagi["Czerwone"])

# metoda ewma
temp_ewma=c()
for(i in 1:(length(log_returns_strata) - window_size+1)){
  Var_hit_ewma=ifelse(log_returns_strata[i:(i+499)] > ewma_VaR_values[i], 1, 0)
  temp_ewma[i]=ifelse(test_swiatel(Var_hit_ewma)$zone == "Green", "Zielone", 
                      ifelse(test_swiatel(Var_hit_ewma)$zone == "Yellow",
                             "Żółte", "Czerwone"))
}
tab_ewma = table(temp_ewma)
tabela_test_swiatla[3,1]=ifelse(is.na(tab_ewma["Zielone"]), 0, tab_ewma["Zielone"])
tabela_test_swiatla[3,2]=ifelse(is.na(tab_ewma["Żółte"]), 0, tab_ewma["Żółte"])
tabela_test_swiatla[3,3]=ifelse(is.na(tab_ewma["Czerwone"]), 0, tab_ewma["Czerwone"])

kable(tabela_test_swiatla)

# Test Christoffersona ----------------------------------------------------

tabela_test_christoff = data.frame("odrzucone" = rep(1,3),
                            "nieodrzucone" = rep(1,3),
                            row.names = c("Historyczna", "Hist. z wagami",
                                          "EWMA"))

test_christoffersona <- function(VaR_hits) {
  # Budowanie macierzy przejść
  transitions <- table(factor(VaR_hits[-length(VaR_hits)], levels = c(0, 1)), 
                       factor(VaR_hits[-1], levels = c(0, 1)))  
  # Liczby przejść
  n00 <- transitions[1, 1]  
  n01 <- transitions[1, 2]  
  n10 <- transitions[2, 1]  
  n11 <- transitions[2, 2] 
  
  # Prawdopodobieństwa przejść
  p0 <- ifelse(n00 + n01 > 0, n01 / (n00 + n01), 0)
  p1 <- ifelse(n10 + n11 > 0, n11 / (n10 + n11), 0)
  pi_hat <- ifelse(n00 + n01 + n10 + n11 > 0, (n01 + n11) / (n00 + n01 + n10 + n11), 0)
  
  # Zmiana dla przypadków równych zero
  p0 <- max(p0, 0.000001)
  p1 <- max(p1, 0.000001)
  pi_hat <- max(pi_hat, 0.000001)
  
  # Statystyka log-likelihood ratio
  L_ind <- n00 * log(1 - p0) + n01 * log(p0) +
    n10 * log(1 - p1) + n11 * log(p1)
  L_un <- (n00 + n10) * log(1 - pi_hat) + (n01 + n11) * log(pi_hat)
  LR_ind <- -2 * (L_un - L_ind)

  p_value <- 1 - pchisq(LR_ind, df = 1)
  
  list(
    LR_ind = LR_ind,
    p_value = p_value
  )
}

#metoda historyczna
temp_hist=c()
for(i in 1:(length(log_returns_strata) - window_size+1)){
  Var_hit_hist=ifelse(log_returns_strata[i:(i+499)] > VaR_values[i], 1, 0)
  temp_hist[i]=ifelse(test_christoffersona(Var_hit_hist)$p_value < 0.05, 1, 0)
}

tabela_test_christoff[1,1]=sum(temp_hist, na.rm=T)
tabela_test_christoff[1,2]=length(log_returns_strata)-499 - sum(temp_hist, na.rm=T)


#metoda hist z wagami
temp_wagi=c()
for(i in 1:(length(log_returns_strata) - window_size+1)){
  Var_hit_wagi=ifelse(log_returns_strata[i:(i+499)] > weighted_VaR_values[i], 1, 0)
  temp_wagi[i]=ifelse(test_christoffersona(Var_hit_wagi)$p_value < 0.05, 1, 0)
}
tabela_test_christoff[2,1]=sum(temp_wagi)
tabela_test_christoff[2,2]=length(log_returns_strata)-499 - sum(temp_wagi)

#metoda ewma
temp_ewma=c()
for(i in 1:(length(log_returns_strata) - window_size+1)){
  Var_hit_ewma=ifelse(log_returns_strata[i:(i+499)] > ewma_VaR_values[i], 1, 0)
  temp_ewma[i]=ifelse(test_christoffersona(Var_hit_ewma)$p_value < 0.05, 1, 0)
}
tabela_test_christoff[3,1]=sum(temp_ewma, na.rm=T)
tabela_test_christoff[3,2]=length(log_returns_strata)-499 - sum(temp_ewma, na.rm=T)

kable(tabela_test_christoff)


