# Figuras

# Entre necesidades y lujos:
# El gasto educativo de los hogares en Mexico

# Pedro I. Rosas-Medina
# FLACSO Mexico
# Agisti 28, 2023

# ----- Packages -----
library(dplyr)
library(stats)
library(ggpubr)
library(plyr)
library(VGAM)
library(stats4)
library(graphics)
library(ggplot2)
library(grDevices)
library(gridExtra)
library(jtools)
library(scales)
library(cowplot)

# ----- Data Frames -----
# Working Directory Path
# Sustituyase por el propio donde se encuentre su base de datos
setwd()

# Main Data Frame
load(file = "ch.RData")

# Average expenditures (simpler visualization)
gasto_prom <- read.csv("gasto_prom.csv")

# ----- Graphs. Figure 1 -----

# Panel A: Household Expenditure on Education (Average)
colnames(gasto_prom) # To remember the name of the variables

p1 <- gasto_prom %>%
  ggplot(aes(x = reorder(abv_entidad, prom_gastoeduca),
             y = prom_gastoeduca)) +
  geom_bar(position = "dodge",
           stat = "summary",
           fun = "mean",
           fill = "lightslategrey") +
  labs(x = "", y = "") +
  theme_apa() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1,
                                   family = "Times New Roman",
                                   color = "black"),
        axis.text.y = element_text(size = 10,
                                   family = "Times New Roman",
                                   color = "black"))
p1

# Panel B: Average ratio of educational expenditures over household expenditures
colnames(ch) # To remember the name of the variables

p2 <- ch %>%
  ggplot(aes(x = reorder(abv_ent, av_gastos),
             y = av_gastos)) +
  geom_bar(position = "dodge",
           stat = "summary",
           fun = "mean",
           fill = "lightslategrey") +
  labs(x = "", y = "") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_apa() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 7.5, angle = 45, hjust = 1,
                                   family = "Times New Roman",
                                   color = "black"),
        axis.text.y = element_text(size = 10,
                                   family = "Times New Roman",
                                   color = "black"))
p2

# Panel C: Ratio of total educational expenditures of households over total ex-
# penditures
colnames(gasto_prom)

p3 <- gasto_prom %>%
  ggplot(aes(x = reorder(abv_entidad, av_gastos1),
             y = av_gastos1)) +
  geom_bar(position = "dodge",
           stat = "summary",
           fun = "mean",
           fill = "lightslategrey") +
  labs(x = "", y = "") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_apa() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 7.5, angle = 45, hjust = 1,
                                   family = "Times New Roman",
                                   color = "black"),
        axis.text.y = element_text(size = 10,
                                   family = "Times New Roman",
                                   color = "black"))
p3

# --- Plots in a 2 * 2 grid
panel <- arrangeGrob(p1,
                     p2, p3,
                     ncol = 2, nrow = 2,
                     layout_matrix = rbind(c(1,1), c(2,3)))
panel1 <- as_ggplot(panel) +
  draw_plot_label(label = c("A", "B", "C"),
                  size = 10,
                  x = c(0, 0, 0.5),
                  y = c(1, 0.523, 0.5),
                  family = "Times New Roman")
panel1

# Figure 1 in the article
# Sustituyase la direccion a un folder donde guste guardar las figuras
# La direccion del folder va previo a la /
png(filename = "/figure1.png",
    width = 22.94,
    height = 13.61,
    units = "cm",
    res = 500)
plot(panel1)
dev.off()

# ----- Graphs. Figure 2 -----
# Five-year age group
ch2 <- ch %>%
  dplyr::filter(educacion > 1) %>%
  mutate(group_edad = case_when(edad_jefe >= 14 &
                                  edad_jefe <= 19 ~ 1,
                                edad_jefe >= 20 &
                                  edad_jefe <= 24 ~ 2,
                                edad_jefe >= 25 & 
                                  edad_jefe <= 29 ~ 3,
                                edad_jefe >= 30 &
                                  edad_jefe <= 34 ~ 4,
                                edad_jefe >= 35 &
                                  edad_jefe <= 39 ~ 5,
                                edad_jefe >= 40 &
                                  edad_jefe <= 44 ~ 6,
                                edad_jefe >= 45 &
                                  edad_jefe <= 49 ~ 7,
                                edad_jefe >= 50 &
                                  edad_jefe <= 54 ~ 8,
                                edad_jefe >= 55 &
                                  edad_jefe <= 59 ~ 9,
                                edad_jefe >= 60 ~ 10))
ch2$group_educa <- factor(ch2$group_educa, levels = c("No Education",
                                                      "Basic",
                                                      "Second.Non-Tertiary", 
                                                      "Tertiary"))

# Panel A: Household expenditure on education by age-group and sex of the house-
# hold head
p4 <- ch2 %>%
  ggplot(aes(x = factor(group_edad),
             y = educacion,
             fill = sexo_jefe2)) +
  geom_bar(stat = "summary",
           position = "dodge",
           fun = "mean") +
  scale_x_discrete(labels = c("1" = "14-19", "2" = "20-24", "3" = "25-29",
                              "4" = "30-34", "5" = "35-39", "6" = "40-44",
                              "7" = "45-49", "8" = "50-54", "9" = "55-59",
                              "10" = "60-More")) +
  scale_fill_manual(values = c("gray12", "lightslategrey"),
                    labels = c("Hombres", "Mujeres")) +
  theme_apa() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1,
                                   family = "Times New Roman",
                                   color = "black"),
        axis.text.y = element_text(size = 10,
                                   family = "Times New Roman",
                                   color = "black"),
        legend.text = element_text(size = 10,
                                   family = "Times New Roman"),
        legend.position = "bottom",
        legend.background = element_rect(linetype = "solid",
                                         colour = "black",
                                         linewidth = 0.35))
p4

# Panel B: Average ratio of educational expenditures over household expenditures
# by age group and sex of the household head
p5 <- ch2 %>%
  ggplot(aes(x = factor(group_edad),
             y = av_gastos,
             fill = sexo_jefe2)) +
  geom_bar(stat = "summary",
           fun = "mean",
           position = "dodge") +
  scale_x_discrete(labels = c("1" = "14-19", "2" = "20-24", "3" = "25-29",
                              "4" = "30-34", "5" = "35-39", "6" = "40-44",
                              "7" = "45-49", "8" = "50-54", "9" = "55-59",
                              "10" = "60-More")) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(values = c("gray12", "lightslategrey"),
                    labels = c("Hombres", "Mujeres")) +
  theme_apa() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1,
                                   family = "Times New Roman",
                                   color = "black"),
        axis.text.y = element_text(size = 10,
                                   family = "Times New Roman",
                                   color = "black"),
        legend.text = element_text(size = 10,
                                   family = "Times New Roman"),
        legend.position = "bottom",
        legend.background = element_rect(linetype = "solid", color = "black",
                                         linewidth = 0.35))
p5

# Panel C: Household expenditure on education by educational background and sex
# of the household head
p6 <- ch2 %>%
  ggplot(aes(x = group_educa,
             y = educacion,
             fill = sexo_jefe2)) +
  geom_bar(stat = "summary",
           fun = "mean",
           position = "dodge") +
  scale_fill_manual(values = c("gray12", "lightslategrey"),
                    labels = c("Hombres", "Mujeres")) +
  scale_x_discrete(labels = c("No Education" = "Sin\nEducación",
                              "Basic" = "Básica",
                              "Second.Non-Tertiary" = "Media\nSuperior",
                              "Tertiary" = "Superior")) +
  theme_apa() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 10, family = "Times New Roman",
                                   color = "black"),
        axis.text.y = element_text(size = 10, family = "Times New Roman",
                                   color = "black"),
        legend.text = element_text(size = 10, family = "Times New Roman",
                                   color = "black"),
        legend.position = "bottom",
        legend.background = element_rect(linetype = "solid", color = "black",
                                         linewidth = 0.35))
p6

# Panel D: Average ratio of educational expenditures over household expenditures
# by educational background and sex of the household head
p7 <- ch2 %>%
  ggplot(aes(x = group_educa,
             y = av_gastos,
             fill = sexo_jefe2)) +
  geom_bar(stat = "summary",
           fun = "mean",
           position = "dodge") +
  scale_fill_manual(values = c("gray12", "lightslategrey"),
                    labels = c("Hombres", "Mujeres")) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_x_discrete(labels = c("No Education" = "Sin\nEducación",
                              "Basic" = "Básica",
                              "Second.Non-Tertiary" = "Media\nSuperior",
                              "Tertiary" = "Superior")) +
  theme_apa() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 10, family = "Times New Roman",
                                   color = "black"),
        axis.text.y = element_text(size = 10, family = "Times New Roman",
                                   color = "black"),
        legend.text = element_text(size = 10, family = "Times New Roman",
                                   color = "black"),
        legend.position = "bottom",
        legend.background = element_rect(linetype = "solid", color = "black",
                                         linewidth = 0.35))
p7

# --- Plots in a 2 * 2 grid
panel2 <- ggarrange(p4, p5, p6, p7,
                    labels = c("A", "B", "C", "D"),
                    ncol = 2, nrow = 2,
                    label.x = c(0.025, 0.025, 0.025, 0.025),
                    label.y = c(1, 1, 0.99, 0.99),
                    vjust = c(1.5, 1.5, 1.5, 1.5),
                    font.label = list(size = 10, face = "bold",
                                      family = "Times New Roman"),
                    common.legend = TRUE,
                    legend = "bottom")
panel2

# Figure 2 in the article
# Sustituyase la direccion a un folder donde guste guardar las figuras
# La direccion del folder va previo a la /
png(filename = "/figure2.png",
    width = 22.94,
    height = 13.61,
    units = "cm",
    res = 500)
plot(panel2)
dev.off()

# ----- Graphs. Figure 3 -----
# Household expenditure on education histogram
p8 <- ch %>%
  ggplot(aes(x = log_educa)) +
  geom_histogram(binwidth = 1,
                 color = "black",
                 fill = "gray90",
                 alpha = 0.9) +
  labs(x = "Gasto educativo por hogar (Log)") +
  scale_y_continuous(labels = label_number(scale = 1e-3)) +
  theme_apa() +
  theme(axis.title.x = element_text(size = 12, family = "Times New Roman",
                                    color = "black"),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 11, family = "Times New Roman",
                                   color = "black"),
        axis.text.y = element_text(size = 11, family = "Times New Roman",
                                   color = "black"))
p8

# Figure 3 in the article
# Sustituyase la direccion a un folder donde guste guardar las figuras
# La direccion del folder va previo a la /
png(filename = "/figure3.png",
    width = 15.82,
    height = 10.54,
    units = "cm",
    res = 500)
plot(p8)
dev.off()

# ----- Graphs. Figure 4 -----
# Data frames with elasticities (easier visualization)
engel <- data.frame(
  Quantiles = c(75, 75, 75, 75, 75, 75, 75,
                80, 80, 80, 80, 80, 80, 80,
                85, 85, 85, 85, 85, 85, 85,
                90, 90, 90, 90, 90, 90, 90,
                95, 95, 95, 95, 95, 95, 95,
                99, 99, 99, 99, 99, 99, 99),
  elast = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4", "Modelo 5", "Modelo 6", "Constante (1)",
            "Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4", "Modelo 5", "Modelo 6", "Constante (1)",
            "Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4", "Modelo 5", "Modelo 6", "Constante (1)",
            "Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4", "Modelo 5", "Modelo 6", "Constante (1)",
            "Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4", "Modelo 5", "Modelo 6", "Constante (1)",
            "Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4", "Modelo 5", "Modelo 6", "Constante (1)"),
  values = c(2.9, 1.95, 1.99, 1.5, 2.58, 1.79, 1,
             1.89, 1.44, 1.58, 1.29, 1.85, 1.42, 1,
             1.56, 1.28, 1.39, 1.19, 1.55, 1.28, 1,
             1.38, 1.19, 1.28, 1.14, 1.39, 1.19, 1,
             1.26, 1.13, 1.19, 1.1, 1.27, 1.14, 1,
             1.16, 1.08, 1.12, 1.06, 1.18, 1.09, 1))

View(engel)

# Elasticidades de los modelos 1-6 + constante en 1
p9 <- engel %>%
  ggplot(aes(x = Quantiles,
             y = values,
             group = elast)) +
  geom_line(aes(linetype = elast)) +
  geom_point(aes(shape = elast),
             size = 2) +
  scale_linetype_manual(values = c("dotted", "solid", "dashed", "longdash",
                                   "twodash", "dotdash", "dotdash"),
                        labels = c("Constante = 1", "Modelo 1", "Modelo 2",
                                   "Modelo 3", "Modelo 4", "Modelo 5",
                                   "Modelo 6")) +
  scale_shape_manual(values = c(15, 16, 17, 18, 19, 13, 12),
                     labels = c("Constante = 1", "Modelo 1", "Modelo 2",
                                "Modelo 3", "Modelo 4", "Modelo 5",
                                "Modelo 6")) +
  labs(x = "Percentiles") +
  scale_x_continuous(limits = c(75, 99),
                     breaks = c(75, 80, 85, 90, 95, 99)) +
  theme_apa() +
  theme(axis.title.x = element_text(size = 12,
                                    family = "Times New Roman"),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 11,
                                   family = "Times New Roman",
                                   color = "black"),
        axis.text.y = element_text(size = 11,
                                   family = "Times New Roman",
                                   color = "black"),
        legend.position = c(0.88, 0.65),
        legend.text = element_text(size = 8,
                                   family = "Times New Roman"),
        legend.background = element_rect(linetype = "solid",
                                         colour = "black",
                                         linewidth = 0.35))
p9

# Figure 4 in the article
# Sustituyase la direccion a un folder donde guste guardar las figuras
# La direccion del folder va previo a la /
png(filename = "/figure4.png",
    width = 15.82,
    height = 10.54,
    units = "cm",
    res = 500)
plot(p9)
dev.off()
