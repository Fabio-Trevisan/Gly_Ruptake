library(readr)
library(ggpubr)
library(tidyverse)
library(plyr)
library(dplyr)
library(rstatix)
library(purrr)
library(agricolae)


#Fresh weight statistics (extended)####

#for Time ####
#Read CSV ####
table <- read.csv("DATA_weight.csv", sep=";",
                  header=T)

vector_Species_Tissue <- c("0microM_TR",
                           "0microM_TS",
                           "50microM_TR",
                           "50microM_TS",
                           "500microM_TR",
                           "500microM_TS")

Summary_table <- ddply(table, c("Treatment", "Time", "Species_Tissue"), summarise,
                       N    = sum(!is.na(Value)),
                       mean = mean(Value, na.rm=TRUE),
                       sd   = sd(Value, na.rm=TRUE),
                       se   = sd / sqrt(N))
Summary_table
write.table(Summary_table, "Weight_summary_statistics.csv", quote = FALSE, sep = ";")



#create Subsets according to Species_Tissue ####
Subsets <- lapply(vector_Species_Tissue, function(i){ 
  i <- subset(table, Species_Tissue == i)
})

names(Subsets) <- vector_Species_Tissue



#transform variable to factor ####
table$Time <- factor(table$Time)

Subsets[["0microM_TR"]][["Time"]] <- factor(Subsets[["0microM_TR"]][["Time"]])
Subsets[["0microM_TS"]][["Time"]] <- factor(Subsets[["0microM_TS"]][["Time"]])
Subsets[["50microM_TR"]][["Time"]] <- factor(Subsets[["50microM_TR"]][["Time"]])
Subsets[["50microM_TS"]][["Time"]] <- factor(Subsets[["50microM_TS"]][["Time"]])
Subsets[["500microM_TR"]][["Time"]] <- factor(Subsets[["500microM_TR"]][["Time"]])
Subsets[["500microM_TS"]][["Time"]] <- factor(Subsets[["500microM_TS"]][["Time"]])


#Assumptions ####
## 1. Homogeneity of variances
##Treatment*Time
Levene_test <- lapply(split(table, table$Species_Tissue), function(i){
  levene_test(Value ~ Treatment * Time, data = i)
})

##2. Normality
##Shapiro-Wilk test for all single treatments
SW_test <- table %>%
  group_by(Time, Treatment, Species_Tissue) %>%
  shapiro_test(Value)
View(SW_test)
write.table(SW_test, file = "Weight_ShapiroWilk_test_results.csv", quote = FALSE, sep = ";")

##3. Indipendency
#Data are indepent by experimental design!



#2way ANOVA ####
##Multiple Species_Tissue
TwoWay_Anova <- lapply(split(table, table$Species_Tissue), function(i){
  anova(lm(Value ~ Treatment * Time, data = i))
})
write.table(TwoWay_Anova, file = "Weight_TwoWay_Anova_results.csv", quote = FALSE, sep = ";")

sink("Weight_TwoWay_Anova_results2.csv")
TwoWay_Anova
sink(NULL)


#1way ANOVA ####
##Time.for tukey
OneWay_Anova_Ti <- lapply(vector_Species_Tissue, function(m){
  lapply(split(Subsets[[m]], Subsets[[m]][["Treatment"]]), function(i){ 
    aov(Value ~ Time, data = i)
  })
})
names(OneWay_Anova_Ti) <- vector_Species_Tissue

##Time.for print
OneWay_Anova_Ti2 <- lapply(vector_Species_Tissue, function(m){
  lapply(split(Subsets[[m]], Subsets[[m]][["Treatment"]]), function(i){ 
    anova(lm(Value ~ Time, data = i))
  })
})
names(OneWay_Anova_Ti2) <- vector_Species_Tissue

##OneWayAnova save
sink("Weight_OneWayAnova_Results_Ti.csv")
OneWay_Anova_Ti2 
sink(NULL)



#Tukey as post hoc test ####
##Time
HSD_Ti <- lapply(vector_Species_Tissue, function(m){
  lapply(names(OneWay_Anova_Ti[[m]]), function(i){ 
    HSD.test(OneWay_Anova_Ti[[m]][[i]], "Time")
  })
})
names(HSD_Ti) <- vector_Species_Tissue
for(i in vector_Species_Tissue) {
  list <- names(OneWay_Anova_Ti[[i]]) 
  names(HSD_Ti[[i]]) <- list
}

##HSD_test save
##Time
HSD_Ti_groups <- lapply(vector_Species_Tissue, function(i){
  lapply(names(OneWay_Anova_Ti[[i]]), function(m){
    as.data.frame(HSD_Ti[[i]][[m]][["groups"]])
  })
})
names(HSD_Ti_groups) <- vector_Species_Tissue
for(i in vector_Species_Tissue) {
  list <- names(OneWay_Anova_Ti[[i]]) 
  names(HSD_Ti_groups[[i]]) <- list
}
sink("Weight_HSD_Ti.csv")
HSD_Ti_groups 
sink(NULL)




#for Treatment ####
#1way ANOVA ####
##Treatment.for tukey
OneWay_Anova_Tr <- lapply(vector_Species_Tissue, function(m){
  lapply(split(Subsets[[m]], Subsets[[m]][["Time"]]), function(i){ 
    aov(Value ~ Treatment, data = i)
  })
})
names(OneWay_Anova_Tr) <- vector_Species_Tissue


##Treatment.for print
OneWay_Anova_Tr2 <- lapply(vector_Species_Tissue, function(m){
  lapply(split(Subsets[[m]], Subsets[[m]][["Time"]]), function(i){ 
    anova(lm(Value ~ Treatment, data = i))
  })
})
names(OneWay_Anova_Tr2) <- vector_Species_Tissue


##OneWayAnova save
sink("Weight_OneWayAnova_Results_Tr.csv")
OneWay_Anova_Tr2 
sink(NULL)



#Tukey as post hoc test ####
##Treatment
HSD_Tr <- lapply(vector_Species_Tissue, function(m){
  lapply(names(OneWay_Anova_Tr[[m]]), function(i){ 
    HSD.test(OneWay_Anova_Tr[[m]][[i]], "Treatment")
  })
})
names(HSD_Tr) <- vector_Species_Tissue
for(i in vector_Species_Tissue) {
  list <- names(OneWay_Anova_Tr[[i]]) 
  names(HSD_Tr[[i]]) <- list
}


##HSD_test save
##Treatment
HSD_Tr_groups <- lapply(vector_Species_Tissue, function(i){
  lapply(names(OneWay_Anova_Tr[[i]]), function(m){
    as.data.frame(HSD_Tr[[i]][[m]][["groups"]])
  })
})
names(HSD_Tr_groups) <- vector_Species_Tissue
for(i in vector_Species_Tissue) {
  list <- names(OneWay_Anova_Tr[[i]]) 
  names(HSD_Tr_groups[[i]]) <- list
}
sink("Weight_HSD_Tr.csv")
HSD_Tr_groups 
sink(NULL)

