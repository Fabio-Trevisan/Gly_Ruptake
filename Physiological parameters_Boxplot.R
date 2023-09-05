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



#read csv & rename columns ####
table <- read_excel("DATA_Physiological_Parameters.xlsx", sheet = "1")
name <- "Physiological parameters"
table <- table[,-5]

colnames(table) <- c("Treatment", "E", "A", "gs")



#melt & group & remove missing values ####
table1 <- melt(table, id=c("Treatment"))
table_1_sum<-table1 %>% 
  group_by(variable)
Table_2_sum <- table_1_sum %>% drop_na(value)
Table_2_sum <- table_1_sum[table_1_sum$`value`>0,]


df <- table
df$A_mol <- df$A/10^6
df <- df[-3]
df <- melt(df, id=c("Treatment"))
df_sum<-df %>% 
  group_by(variable)
df_sum2 <- df_sum %>% drop_na(value)
df_sum2 <- df_sum[df_sum$`value`>0,]



#boxplots & facet wrap ####
my_order<- c("C", "Fe", "P")
ggplot(Table_2_sum, aes(x= Treatment, value, fill=Treatment))+  
  stat_boxplot(geom="errorbar", width=0.2)+
  geom_boxplot(width=0.5)+ 
  labs(caption ="(based on ADC instruement. Fabio Trevisan)")+ 
  theme(legend.position = "NONE") + theme_bw() +
  facet_wrap(~variable, scales="free", nrow = 2) + ylab("mol m-²  s-1") + xlab("Treatments") +
  scale_x_discrete(limits=my_order) +
  scale_fill_manual(values=c("grey77","darkorange2", "skyblue3"))

ggsave(filename = paste(name, ".pdf", sep = ""), plot = last_plot(), dpi = 600, units = "cm", width = 60, height = 50, scale = 0.5)



#side by side boxplots ####
my_order<- c("A_mol", "E", "gs")
f1 <- ggplot(df_sum2, aes(x = variable, y = value, fill=Treatment))+  
  stat_boxplot(geom="errorbar", width=0.2, position = position_dodge(width = 0.5))+
  geom_boxplot(width=0.5)+ 
  theme(legend.position = "NONE") + theme_bw() + 
  scale_fill_manual(values=c("grey77","darkorange2", "skyblue3")) +
  scale_x_discrete(limits=my_order) +
  ylab("mol m-²  s-1") + xlab("Physiological parameters") 
  
f2 <- f1 + 
  scale_y_break(c(0.04,0.08),  scales = 1) +
  scale_y_break(c(0.0028,0.0000025),  scales = 1) +
  scale_y_continuous(breaks = seq(0, 1 , 0.1), labels = scientific) 
f2
ggsave(filename = paste(name, "_1.pdf", sep = ""), plot = f2, dpi = 600, units = "cm", width = 60, height = 50, scale = 0.5)

f3 <- f1 + 
  scale_y_break(c(0.04,0.08),  scales = 1) +
  scale_y_break(c(0.0028,0.0000025),  scales = 1) +
  scale_y_continuous(breaks = seq(0, 0.04 , 0.01), labels = scientific) 
f3
ggsave(filename = paste(name, "_2.pdf", sep = ""), plot = f3, dpi = 600, units = "cm", width = 60, height = 50, scale = 0.5)

f4 <- f1 + 
  scale_y_break(c(0.04,0.08),  scales = 1) +
  scale_y_break(c(0.0028,0.0000025),  scales = 1) +
  scale_y_continuous(breaks = seq(0, 0.0000025 , 0.0000005), labels = scientific) 
f4
ggsave(filename = paste(name, "_3.pdf", sep = ""), plot = f4, dpi = 600, units = "cm", width = 60, height = 50, scale = 0.5)



#statistics ####
colnames(Table_2_sum) <- c("Treatment", "Variable", "Value") 

##Assumptions ####
## 1. Homogeneity of variances
##Treatment*Time
Table_2_sum$Treatment <- factor(Table_2_sum$Treatment)
Table_2_sum$Variable <- factor(Table_2_sum$Variable)

L_test <- Table_2_sum %>%
  group_by(Variable) %>%
  levene_test(Value ~ Treatment)
View(L_test)
write.table(L_test, file = "Physiological_Parameters_Levene_test_results.csv", quote = FALSE, sep = ";")

##2. Normality
##Shapiro-Wilk test for all single treatments
SW_test <- Table_2_sum %>%
  group_by(Treatment, Variable) %>%
  shapiro_test(Value)
View(SW_test)
write.table(SW_test, file = "Physiological_Parameters_ShapiroWilk_test_results.csv", quote = FALSE, sep = ";")

##3. Indipendency
#Data are indepent by experimental design!



##anova for tukey ####
OneWay_Anova_Boxplot <- lapply(split(Table_2_sum, Table_2_sum[["Variable"]]), function(i){ 
  aov(Value ~ Treatment, data = i)
})

##anova for print
OneWay_Anova_Boxplot2 <- lapply(split(Table_2_sum, Table_2_sum[["Variable"]]), function(i){ 
  anova(lm(Value ~ Treatment, data = i))
})

sink(paste(name, "OneWay_Anova_Boxplot.csv", sep = "_"))
OneWay_Anova_Boxplot2
sink(NULL)

##Tukey ####
##HSD complete
HSD_Boxplot <- lapply(names(OneWay_Anova_Boxplot), function(i){ 
  HSD.test(OneWay_Anova_Boxplot[[i]], "Treatment")
})
names(HSD_Boxplot) <- names(OneWay_Anova_Boxplot)

##HSD groups only
HSD_Boxplot_groups <- lapply(names(OneWay_Anova_Boxplot), function(i){
  as.data.frame(HSD_Boxplot[[i]][["groups"]])
})
names(HSD_Boxplot_groups) <- names(OneWay_Anova_Boxplot)

sink(paste(name, "HSD_Boxplot.csv", sep = "_"))
HSD_Boxplot_groups
sink(NULL)