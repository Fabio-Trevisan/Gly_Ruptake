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

#summary table ####
Summary_table <- ddply(table2, c("Treatment", "Element", "Tissue"), summarise,
                       N    = sum(!is.na(ppb)),
                       mean = mean(ppb, na.rm=TRUE),
                       sd   = sd(ppb, na.rm=TRUE),
                       se   = sd / sqrt(N))

ICP_table <- Summary_table[,-6]
ICP_table$mean <- round(ICP_table$mean, 3)
ICP_table$se <- round(ICP_table$se, 3)
ICP_table$Concentration <- paste(ICP_table$mean, ICP_table$se, sep = " ± ")
ICP_table <- ICP_table[,-c(5,6)]

ICP_table_2 <- dcast(ICP_table, Tissue+Treatment+N~Element, value.var = "Concentration") 


write.table(ICP_table_2, file = "ICP_table_Glycine.csv", quote = FALSE, sep = ";")


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

