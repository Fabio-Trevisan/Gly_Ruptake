library(plyr) 
library(dplyr)
library(reshape2)
library(caret)
library(ggplot2)


#Read csv ####
table <- read.csv("DATA_Gly-Reuptake_ICP_He.csv", sep=";",
                  header=T)


#Re-arrange table and mean value calculation ####
table2 <- melt(data = table, 
              id.vars = c("Tissue", "Treatment"), 
              variable.name = "Element", 
              value.name = "ppb"
               )

table3 <- dcast(table2, 
                Tissue + Treatment ~Element, 
                mean,
                value.var = "ppb") 


#Normalization and centration for each element####
#between 0 and 1 (according to range)
process1 <- preProcess(table3, method=c("range"))
norm_scale1 <- predict(process1, table3)

norm_scale <- melt(norm_scale1, id = c("Tissue","Treatment"))

ggplot(norm_scale, aes(Treatment, variable, size=value, colour = Treatment)) +
  geom_point() + 
  facet_wrap(~Tissue, ncol = 2)


ggsave(filename = "ICP_Bubbleplot.pdf", 
       plot = last_plot(), 
       dpi = 600, 
       units = "cm", 
       width = 80, 
       height = 70, 
       scale = 0.2)

