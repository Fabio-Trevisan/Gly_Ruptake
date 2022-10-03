library(readr)
library(ggpubr)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(plyr) 
library(dplyr)
library(readxl)


#SideBySide Boxlot ####

table <- read.csv("DATA_MassBalance_Reuptake.csv", sep=";",
                   header=T)


table <- table %>% drop_na(Value)


my_order<- c("C", "Fe", "P")
f1 <- ggplot(table, aes(x = as.factor(Time), y = Value, fill = Treatment))+  
  stat_boxplot(geom="errorbar")+
  geom_boxplot()+
  theme_bw()+ 
  scale_fill_manual(values=c("grey77","darkorange2", "skyblue3"))

f1 + facet_wrap(~Species_Tissue, scales="free", ncol = 2)+
  ylab("Delta 13C") + 
  xlab("Time (Days)")

ggsave(filename = "MassBalance_SideBySide_Boxplot.pdf", plot = last_plot(), dpi = 600, units = "cm", width = 70, height = 60, scale = 0.5)
