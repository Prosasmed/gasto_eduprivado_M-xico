# Tablas

# Entre necesidades y lujos:
# El gasto educativo de los hogares en Mexico

# Pedro I. Rosas-Medina
# FLACSO Mexico
# Agisti 28, 2023

# ----- Packages -----
library(tidyverse)
library(dplyr)
library(plyr)
library(stats)
library(ggpubr)
library(MASS)
library(psych)

# ----- Data Frames -----
# Working Directory Path
# Sustituyase la direccion del folder donde se encuentre su base de datos
setwd("")

# Main Data Frame
load(file = "ch.RData")
options(scipen=999)

# ----- Descriptive Statistics -----
# --- Table 1 (Full Sample)
colnames(ch)
ch_d1 <- ch %>%
  dplyr::select(tot_integ, hombres, mujeres, ing_cor, ing_perc, gasto_mon,
                gast_perc, educacion, av_gastos)
table_1a <- psych::describe(ch_d1)
colnames(table_1a)
table_1a <- table_1a %>%
  dplyr::select(n, min, max, mean, median, sd)
View(table_1a)

# --- Table 1 (Reduced Sample)
ch2 <- ch %>%
  filter(edad_jefe >= 25 & edad_jefe <= 65,
         ing_cor > 0)
dx1quan <- quantile(ch2$ing_cor,
                    c(0.05, 0.95))
ch2 <- ch2[ch2$ing_cor > dx1quan[1] & ch2$ing_cor < dx1quan[2] , ]
ch_d2 <- ch2 %>%
  dplyr::select(tot_integ, hombres, mujeres, ing_cor, ing_perc, gasto_mon,
                gast_perc, educacion, av_gastos)
table_1b <- psych::describe(ch_d2)
table_1b <- table_1b %>%
  dplyr::select(n, min, max, mean, median, sd)
View(table_1b)

# --- Table 2
# Factor variables
table2 <- table(ch$tip_loc, ch$sexo_jefe, ch$group_educa)
ftable(table2)

# --- Table 3
table3 <- ch %>%
  dplyr::group_by(zone, tip_loc, sexo_jefe) %>%
  dplyr::summarise(Mean = mean(educacion, na.rm = TRUE),
                   Mean2 = mean(av_gastos, na.rm = TRUE))
View(table3)