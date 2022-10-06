library(readr)
library(ggpubr)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(plyr) 
library(dplyr)
library(readxl)
library(scales)


table <- read.csv("DATA_13C_Reuptake.csv", sep=";",
                   header=T)


table <- table %>% drop_na(Value)


#SideBySide Boxlot ####
my_order<- c("C", "Fe", "P")
f1 <- ggplot(table, aes(x = as.factor(Time), y = Value, fill = Treatment))+  
  stat_boxplot(geom="errorbar")+
  geom_boxplot()+
  theme_bw()+ 
  scale_fill_manual(values=c("grey77","darkorange2", "skyblue3"))
  
f1 + facet_wrap(~Species_Tissue, scales="free", ncol = 2)+
  ylab("Delta 13C") + 
  xlab("Time (Days)")

ggsave(filename = "13C_SideBySide_Boxplot.pdf", plot = last_plot(), dpi = 600, units = "cm", width = 70, height = 80, scale = 0.5)


#SideBySide Boxlot + trendline ####
trendline <- geom_smooth(aes(group = Treatment, color = Treatment, fill = Treatment), method=lm, alpha = 0.1, linetype="dashed") 

f1 <- ggplot(table, aes(x = factor(Time), y = Value, fill = Treatment)) +  
  trendline +
  stat_boxplot(geom="errorbar") +
  geom_boxplot() +
  theme_bw() + 
  scale_fill_manual(values=c("grey77","darkorange2", "skyblue3")) +
  scale_color_manual(values=c("grey77","darkorange2", "skyblue3")) +
  scale_y_continuous(labels = scientific)

f2 <- f1 + facet_wrap(~Species_Tissue, scales="free", ncol = 2)+
  ylab("Delta 13C") + 
  xlab("Time (Days)")
f2

ggsave(filename = "13C_SideBySide_Boxplot_2.pdf", plot = last_plot(), dpi = 600, units = "cm", width = 70, height = 60, scale = 0.5)
