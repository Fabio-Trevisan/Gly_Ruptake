library(readr)
library(ggpubr)
library(tidyverse)
library(plyr)
library(dplyr)
library(rstatix)
library(purrr)
library(agricolae)

#ICP statistics ####
#Read CSV ####
table <- read.csv("DATA_Gly-Reuptake_ICP_He.csv", sep=";",
                  header=T)


#Assumptions ####
## 1. Homogeneity of variances
##Treatment*Time
Levene_test <- lapply(split(table, table$Tissue), function(i){
  levene_test(ppb ~ Treatment * Element, data = i)
})

##2. Normality
##Shapiro-Wilk test for all single treatments
SW_test <- table %>%
  group_by(Element, Treatment, Tissue) %>%
  shapiro_test(ppb)
View(SW_test)
write.table(SW_test, file = "ICP_ShapiroWilk_test_results.csv", quote = FALSE, sep = ";")

##3. Indipendency
#Data are indepent by experimental design!


#1way ANOVA ####
###create Subsets according to Tissue 
vector_Tissue <- c("R", "S")

Subsets <- lapply(vector_Tissue, function(i){ 
  i <- subset(table, Tissue == i)
})

names(Subsets) <- vector_Tissue

##Treatment.for tukey
OneWay_Anova_Tr <- lapply(vector_Tissue, function(m){
  lapply(split(Subsets[[m]], Subsets[[m]][["Element"]]), function(i){ 
    aov(ppb ~ Treatment, data = i)
  })
})
names(OneWay_Anova_Tr) <- vector_Tissue


##Treatment.for print
OneWay_Anova_Tr2 <- lapply(vector_Tissue, function(m){
  lapply(split(Subsets[[m]], Subsets[[m]][["Element"]]), function(i){ 
    anova(lm(ppb ~ Treatment, data = i))
  })
})
names(OneWay_Anova_Tr2) <- vector_Tissue


##OneWayAnova save
sink("ICP_OneWayAnova_Results_Tr.csv")
OneWay_Anova_Tr2 
sink(NULL)


#Tukey as post hoc test ####
##Treatment
HSD_Tr <- lapply(vector_Tissue, function(m){
  lapply(names(OneWay_Anova_Tr[[m]]), function(i){ 
    HSD.test(OneWay_Anova_Tr[[m]][[i]], "Treatment")
  })
})
names(HSD_Tr) <- vector_Tissue
for(i in vector_Tissue) {
  list <- names(OneWay_Anova_Tr[[i]]) 
  names(HSD_Tr[[i]]) <- list
}


##HSD_test save
##Treatment
HSD_Tr_groups <- lapply(vector_Tissue, function(i){
  lapply(names(OneWay_Anova_Tr[[i]]), function(m){
    as.data.frame(HSD_Tr[[i]][[m]][["groups"]])
  })
})
names(HSD_Tr_groups) <- vector_Tissue
for(i in vector_Tissue) {
  list <- names(OneWay_Anova_Tr[[i]]) 
  names(HSD_Tr_groups[[i]]) <- list
}
sink("ICP_HSD_Tr.csv")
HSD_Tr_groups 
sink(NULL)

