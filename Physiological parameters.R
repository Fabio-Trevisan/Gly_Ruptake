library(readr)
library(ggpubr)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(dplyr)
library(agricolae)
library(readxl)
library(rstatix)




#read csv & rename columns ####
table <- read_excel("DATA_Physiological_Parameters.xlsx", sheet = "1")
name <- "Physiological_Parameters"




colnames(table) <- c("Treatment", "E", "A", "gs", "Normalized gs")



#melt & group & remove missing values ####
table1 <- melt(table, id=c("Treatment"))

table_1_sum<-table1 %>% 
  group_by(variable)

Table_2_sum <- table_1_sum %>% drop_na(value)


#boxplots & facet wrap ####
my_order<- c("C", "Fe", "P")
ggplot(Table_2_sum, aes(x= Treatment, value, fill=Treatment))+  
  stat_boxplot(geom="errorbar", width=0.2)+
  geom_boxplot(width=0.5)+ 
  labs(caption ="(based on ADC instruement. Fabio Trevisan)")+ 
  theme(legend.position = "NONE") + theme_bw() +
  facet_wrap(~variable, scales="free") + ylab(name) + xlab("Treatments") +
  scale_x_discrete(limits=my_order) +
  scale_fill_manual(values=c("grey77","darkorange2", "skyblue3"))

ggsave(filename = paste(name, ".pdf", sep = ""), plot = last_plot(), dpi = 600, units = "cm", width = 60, height = 50, scale = 0.5)



#statistics ####
colnames(Table_2_sum) <- c("Treatment", "Variable", "Value") 

#Assumptions 
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
Data are indepent by experimental design!



##anova for tukey
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

#Tukey
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