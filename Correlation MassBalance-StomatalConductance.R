library(readr)
library(ggpubr)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(dplyr)
library(agricolae)
library(readxl)
library(rstatix)
library(ggbreak)
library(scales)

#Physiological data --> stomatal conductance ####
physio <- read_excel("DATA_Physiological_Parameters.xlsx", sheet = "1")
name <- "Physiological parameters"
physio <- physio[,-c(5,2,3)]

colnames(physio) <- c("Treatment", "gs")


## melt & group & remove missing values ####
physio1 <- melt(physio, id=c("Treatment"))
physio2 <- physio1[physio1$`value`>0,]

physioRoot <- physio2[-c(1,3,4,8,12,13,15,19,20,23,25,26,27,14,10),-1]
physioShoot <- physio2[-c(1,3,4,8,12,13,15,19,20,23,25,26,27,14),-1]

#Mass Balance data ####
mbalance <- read.csv("DATA_STD_MassBalance_Reuptake.csv", sep=";",
                  header=T)
mbalance <- mbalance[mbalance$Time==17,]


RootLow <- mbalance[mbalance$Species_Tissue=="50microM_TR",]
RootHigh <- mbalance[mbalance$Species_Tissue=="500microM_TR",]
ShootLow <- mbalance[mbalance$Species_Tissue=="50microM_TS",]
ShootHigh <- mbalance[mbalance$Species_Tissue=="500microM_TS",]

RootLow <- cbind(RootLow, physioRoot)
RootHigh <- cbind(RootHigh, physioRoot)
ShootLow <- cbind(ShootLow, physioShoot)
ShootHigh <- cbind(ShootHigh, physioShoot)

RootLow_Pears <- cor.test(RootLow$Value, RootLow$value, 
                method = "pearson")

RootHigh_Pears <- cor.test(RootHigh$Value, RootHigh$value, 
                method = "pearson")

ShootLow_Pears <- cor.test(ShootLow$Value, ShootLow$value, 
                method = "pearson")

ShootHigh_Pears <- cor.test(ShootHigh$Value, ShootHigh$value, 
                method = "pearson")


RootLow_Corr <- ggscatter(RootLow, x = "Value", y = "value", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Mass Balance", ylab = "Stomatal conductance")

RootHigh_Corr <- ggscatter(RootHigh, x = "Value", y = "value", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Mass Balance", ylab = "Stomatal conductance")

ShootLow_Corr <- ggscatter(ShootLow, x = "Value", y = "value", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Mass Balance", ylab = "Stomatal conductance")

ShootHigh_Corr <- ggscatter(ShootHigh, x = "Value", y = "value", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Mass Balance", ylab = "Stomatal conductance")

Corr <- ggarrange(RootLow_Corr, RootHigh_Corr, ShootLow_Corr, ShootHigh_Corr, 
                  labels = c("50microM_TR", "500microM_TR", "50microM_TS", "500microM_TS"),
                  ncol = 2, nrow = 2)


ggsave(filename = "Correlation MassBalance_StomatalConductance.pdf", plot = last_plot(), dpi = 600, units = "cm", width = 80, height = 80, scale = 0.3)


#write.table(Summary_table, "13C_summary_statistics.csv", quote = FALSE, sep = ";")