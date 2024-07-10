# Tobit Analysis
# Between Necessities and Luxuries:
# Household Education Expenditures in Mexico

# Pedro I. Rosas-Medina
# FLACSO Mexico
# June 23, 2023

# ----- Packages -----
library(tidyverse)
library(tidyr)
library(stats)
library(VGAM)
library(AER)
library(stats4)
library(texreg)
# ----- Data Frames -----
# Working Directory Path
# Sustituyase la direccion del folder donde se encuentre su base de datos
setwd("")

# Main Data Frame
load(file = "ch.RData")
options(scipen=0)

# ----- Data Cleaning for Analysis -----
colnames(ch)
ch <- ch %>%
  filter(edad_jefe >= 25 & edad_jefe <= 65,
         ing_cor > 0)
dx1quan <- quantile(ch$ing_cor,
                    c(0.05, 0.95))
ch <- ch[ch$ing_cor > dx1quan[1] & ch$ing_cor < dx1quan[2] , ]

# ----- Tobit Regressions: First Equation -----
# --- Tobit Model 1-2
## Modelo 1 usando libreria VGAM
colnames(ch)
m1 <- vglm(log_educa ~ log_ing_perc + log_tamhog + edad_jefe + sexo_jefe2 +
             group_educa,
           data = ch,
           family = VGAM::tobit(Lower = 0),
           trace = TRUE)
summary(m1)

## Modelo 1 usando libreria AER
m1_alt = AER::tobit(log_educa ~ log_ing_perc + log_tamhog + edad_jefe + 
                      sexo_jefe2 + group_educa,
                    data = ch,
                    left = 0,
                    right = Inf)
summary(m1_alt)

# --- Tobit Model 2
## Modelo 2 usando libreria VGAM
m2 <- vglm(log_educa ~ log_inperc2 + log_tamhog + edad_jefe + sexo_jefe2 +
             group_educa,
           data = ch,
           family = VGAM::tobit(Lower = 0),
           trace = TRUE)
summary(m2)

## Modelo 2 usando libreria AER
m2_alt = AER::tobit(log_educa ~ log_inperc2 + log_tamhog + edad_jefe + 
                      sexo_jefe2 + group_educa,
                    data = ch,
                    left = 0,
                    robust = TRUE,
                    right = Inf)
summary(m2_alt)

# ----- Tobit Regressions: Second Equation -----
# --- Tobit Model 3-6
## Modelo 3 usando libreria VGAM
m3_h <- vglm(log_educa ~ log_ing_perc + log_tamhog + mal_pres + mal_prim + 
               mal_secu + mal_prepa + mal_sup + edad_jefe + sexo_jefe2 + 
               group_educa,
             data = ch,
             family = VGAM::tobit(Lower = 0),
             trace = TRUE)
summary(m3_h)

## Modelo 3 usando libreria AER
m3_halt = AER::tobit(log_educa ~ log_ing_perc + log_tamhog + mal_pres + 
                       mal_prim + mal_secu + mal_prepa + mal_sup + edad_jefe + 
                       sexo_jefe2 + group_educa,
                     data = ch,
                     left = 0,
                     robust = TRUE,
                     right = Inf)
summary(m3_halt)

## Modelo 4 usando libreria VGAM
m32_h <- vglm(log_educa ~ log_inperc2 + log_tamhog + mal_pres + mal_prim + 
                mal_secu + mal_prepa + mal_sup + edad_jefe + sexo_jefe2 + 
                group_educa,
              data = ch,
              family = VGAM::tobit(Lower = 0),
              trace = TRUE)
summary(m32_h)

## Modelo 4 usando libreria AER
m32_halt = AER::tobit(log_educa ~ log_inperc2 + log_tamhog + mal_pres + 
                        mal_prim + mal_secu + mal_prepa + mal_sup + edad_jefe + 
                        sexo_jefe2 + group_educa,
                      data = ch,
                      left = 0,
                      robust = TRUE,
                      right = Inf)
summary(m32_halt)

## Modelo 5 usando libreria VGAM
m3_m <- vglm(log_educa ~ log_ing_perc + log_tamhog + wom_pres + wom_prim + 
               wom_secu + wom_prepa + edad_jefe + sexo_jefe2 + 
               group_educa,
             data = ch,
             family = VGAM::tobit(Lower = 0),
             trace = TRUE)
summary(m3_m)

## Modelo 5 usando libreria AER
m3_malt = AER::tobit(log_educa ~ log_ing_perc + log_tamhog + wom_pres + 
                       wom_prim + wom_secu + wom_prepa + edad_jefe + 
                       sexo_jefe2 + group_educa,
                     data = ch,
                     left = 0,
                     robust = TRUE,
                     right = Inf)
summary(m3_malt)

## Modelo 6 usando libreria VGAM
m32_m <- vglm(log_educa ~ log_inperc2 + log_tamhog + wom_pres + wom_prim + 
                wom_secu + wom_prepa + edad_jefe + sexo_jefe2 + 
                group_educa,
              data = ch,
              family = VGAM::tobit(Lower = 0),
              trace = TRUE)
summary(m32_m)

## Modelo 6 usando libreria AER
m32_malt = AER::tobit(log_educa ~ log_inperc2 + log_tamhog + wom_pres + 
                        wom_prim + wom_secu + wom_prepa + edad_jefe + 
                        sexo_jefe2 + group_educa,
                      data = ch,
                      left = 0,
                      robust = TRUE,
                      right = Inf)
summary(m32_malt)

# ----- Estadisticos de resumen -----
# Modelo 1 (resultados de resumen)
ctable1 <- coef(summary(m1))
pvals1 <- 2 * pt(abs(ctable1[ , "z value"]),
                 df.residual(m1),
                 lower.tail = FALSE)
cbind(ctable1, pvals1)

# Modelo 2 (resultados de resumen)
ctable2 <- coef(summary(m2))
pvals2 <- 2 * pt(abs(ctable2[ , "z value"]),
                 df.residual(m2),
                 lower.tail = FALSE)
cbind(ctable2, pvals2)

# Modelo 3 (resultados de resumen)
ctable3 <- coef(summary(m3_h))
pvals3 <- 2 * pt(abs(ctable3[ , "z value"]),
                 df.residual(m3_h),
                 lower.tail = FALSE)
cbind(ctable3, pvals3)

# Modelo 4 (resultados de resumen)
ctable4 <- coef(summary(m32_h))
pvals4 <- 2 * pt(abs(ctable4[ , "z value"]),
                 df.residual(m32_h),
                 lower.tail = FALSE)
cbind(ctable4, pvals4)

# Modelo 5 (resultados de resumen)
ctable5 <- coef(summary(m3_m))
pvals5 <- 2 * pt(abs(ctable5[ , "z value"]),
                 df.residual(m3_m),
                 lower.tail = FALSE)
cbind(ctable5, pvals5)

# Modelo 6 (resultados de resumen)
ctable6 <- coef(summary(m32_m))
pvals6 <- 2 * pt(abs(ctable6[ , "z value"]),
                 df.residual(m32_m),
                 lower.tail = FALSE)
cbind(ctable6, pvals6)

# ----- Supuestos -----
# Modelo 1 (supuestos)
b1 <- coef(m1)
se1 <- sqrt(diag(vcov(m1)))
cbind(LL1 = b1 - qnorm(0.975) * se1,
      UL1 = b1 + qnorm(0.975) * se1)
ch$yhat1 <- fitted(m1)[ , 1]
ch$rr1 <- resid(m1, type = "response")
ch$rp1 <- resid(m1, type = "pearson")[ , 1]

# Modelo 2 (supuestos)
b2 <- coef(m2)
se2 <- sqrt(diag(vcov(m2)))
cbind(LL2 = b2 - qnorm(0.975) * se2,
      UL2 = b2 + qnorm(0.975) * se2)
ch$yhat2 <- fitted(m2)[ , 1]
ch$rr2 <- resid(m2, type = "response")
ch$rp2 <- resid(m2, type = "pearson")[ , 1]

# Modelo 3 (supuestos)
b3 <- coef(m3_h)
se3 <- sqrt(diag(vcov(m3_h)))
cbind(LL3 = b3 - qnorm(0.975) * se3,
      UL3 = b3 + qnorm(0.975) * se3)
ch$yhat3 <- fitted(m3_h)[ , 1]
ch$rr3 <- resid(m3_h, type = "response")
ch$rp3 <- resid(m3_h, type = "pearson")[ , 1]

# Modelo 4 (supuestos)
b4 <- coef(m32_h)
se4 <- sqrt(diag(vcov(m32_h)))
cbind(LL2 = b4 - qnorm(0.975) * se4,
      UL2 = b4 + qnorm(0.975) * se4)
ch$yhat4 <- fitted(m32_h)[ , 1]
ch$rr4 <- resid(m32_h, type = "response")
ch$rp4 <- resid(m32_h, type = "pearson")[ , 1]

# Modelo 5 (supuestos)
b5 <- coef(m3_m)
se5 <- sqrt(diag(vcov(m3_m)))
cbind(LL5 = b5 - qnorm(0.975) * se5,
      UL5 = b5 + qnorm(0.975) * se5)
ch$yhat5 <- fitted(m3_m)[ , 1]
ch$rr5 <- resid(m3_m, type = "response")
ch$rp5 <- resid(m3_m, type = "pearson")[ , 1]

# Modelo 6 (supuestos)
b6 <- coef(m32_m)
se6 <- sqrt(diag(vcov(m32_m)))
cbind(LL6 = b6 - qnorm(0.975) * se6,
      UL6 = b6 + qnorm(0.975) * se6)
ch$yhat6 <- fitted(m32_m)[ , 1]
ch$rr6 <- resid(m32_m, type = "response")
ch$rp6 <- resid(m32_m, type = "pearson")[ , 1]

# ----- Curvas de Engel -----
# Elasticidades
ch$e1 <- (0.84867 / ch$yhat1) + 1 # Modelo 1
ch$e2 <- (0.42434 / ch$yhat2) + 1 # Modelo 2
ch$e3 <- (0.84231 / ch$yhat3) + 1 # Modelo 3
ch$e4 <- (0.42116 / ch$yhat4) + 1 # Modelo 4
ch$e5 <- (1.09655 / ch$yhat5) + 1 # Modelo 5
ch$e6 <- (0.54828 / ch$yhat6) + 1 # Modelo 5

# Elasticidad en puntos de la distribucion del gasto educativo del hogar
dx2quan <- quantile(ch$yhat1, 
                    c(.75, .80, .85,
                      .90, .95, .99)) # Modelo 1
dx3quan <- quantile(ch$yhat2, 
                    c(.75, .80, .85,
                      .90, .95, .99)) # Modelo 2
dx4quan <- quantile(ch$yhat3, 
                    c(.75, .80, .85,
                      .90, .95, .99)) # Modelo 3
dx5quan <- quantile(ch$yhat4, 
                    c(.75, .80, .85,
                      .90, .95, .99)) # Modelo 4
dx6quan <- quantile(ch$yhat5, 
                    c(.75, .80, .85,
                      .90, .95, .99)) # Modelo 5
dx7quan <- quantile(ch$yhat6, 
                    c(.75, .80, .85,
                      .90, .95, .99)) # Modelo 6
dx2quan
dx3quan
dx4quan
dx5quan
dx6quan
dx7quan

# Calculo
(0.84867 / 0.446) + 1
(0.84867 / 0.955) + 1
(0.84867 / 1.523) + 1
(0.84867 / 2.229) + 1
(0.84867 / 3.273) + 1
(0.84867 / 5.190) + 1 # Modelo 1

(0.42434 / 0.446) + 1
(0.42434 / 0.955) + 1
(0.42434 / 1.523) + 1
(0.42434 / 2.229) + 1
(0.42434 / 3.273) + 1
(0.42434 / 5.190) + 1 # Modelo 2

(0.84231 / 0.849) + 1
(0.84231 / 1.452) + 1
(0.84231 / 2.167) + 1
(0.84231 / 3.035) + 1
(0.84231 / 4.335) + 1
(0.84231 / 6.751) + 1 # Modelo 3

(0.42116 / 0.849) + 1
(0.42116 / 1.452) + 1
(0.42116 / 2.167) + 1
(0.42116 / 3.035) + 1
(0.42116 / 4.335) + 1
(0.42116 / 6.751) + 1 # Modelo 4

(1.09655 / 0.692) + 1
(1.09655 / 1.297) + 1
(1.09655 / 1.983) + 1
(1.09655 / 2.833) + 1
(1.09655 / 4.045) + 1
(1.09655 / 6.207) + 1 # Modelo 5

(0.54828 / 0.692) + 1
(0.54828 / 1.297) + 1
(0.54828 / 1.983) + 1
(0.54828 / 2.833) + 1
(0.54828 / 4.045) + 1
(0.54828 / 6.207) + 1 # Modelo 6