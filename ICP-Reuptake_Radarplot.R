library(plyr) 
library(dplyr)
library(reshape2)
library(caret)
library(ggplot2)
library(ggiraphExtra)
library(scales)

#Read csv ####
table <- read.csv("DATA_Gly-Reuptake_ICP_He.csv", sep=";",
                  header=T)


#Re-arrange table and mean value calculation ####
table2 <- dcast(table, 
                Tissue + Treatment ~Element, 
                mean,
                value.var = "ppb")

#creare due subset per micro- e macro-nutrienti ####
table_Micro <- dcast(table, 
                Tissue + Treatment ~Element, 
                mean,  
                subset = .(Abbundance=="Micro"), 
                value.var = "ppb")

table_Macro <- dcast(table, 
                     Tissue + Treatment ~Element, 
                     mean,  
                     subset = .(Abbundance=="Macro"), 
                     value.var = "ppb")


#Normalization and centration ####
#between 0 and 1 (according to range)
process1 <- preProcess(table2, method=c("range"))
norm_scale1 <- predict(process1, table2)

#or scalate e centrate sullo zero (standardizzate)
process2 <- preProcess(table2, method=c("center", "scale"))
norm_scale2 <- predict(process2, table2)


#Summary table ####
Summary_table <- ddply(table, c("Element", "Mode","Treatment","Abbundance"), summarise,
                       N    = sum(!is.na(ppb)),
                       mean = mean(ppb, na.rm=TRUE),
                       sd   = sd(ppb, na.rm=TRUE),
                       se   = sd / sqrt(N))
Summary_table



#Radarplot ####
f2 <- ggRadar(data = norm_scale1, 
             alpha = 0.1,
             size = 2,
             colour = "blue",
             mapping = aes(colour = Treatment, facet = Tissue)
             )

#or
#Micro + Macro 
f2 <- ggRadar(data = table2, 
              alpha = 0.1,
              size = 2,
              mapping = aes(colour = Treatment, facet = Tissue)
              )
f2
ggsave(filename = "ICP_Radarplot.pdf", 
       plot = last_plot(), 
       dpi = 600, 
       units = "cm", 
       width = 70, 
       height = 45, 
       scale = 0.3)

#Micro
fMicro <- ggRadar(data = table_Micro, 
              alpha = 0.1,
              size = 2,
              mapping = aes(colour = Treatment, facet = Tissue)
              )
fMicro
ggsave(filename = "ICP_Radarplot_Micro.pdf", 
       plot = last_plot(), 
       dpi = 600, 
       units = "cm", 
       width = 70, 
       height = 45, 
       scale = 0.3)

#Macro
fMacro <- ggRadar(data = table_Macro, 
              alpha = 0.1,
              size = 2,
              mapping = aes(colour = Treatment, facet = Tissue)
              )
fMacro
ggsave(filename = "ICP_Radarplot_Macro.pdf", 
                      plot = last_plot(), 
                      dpi = 600, 
                      units = "cm", 
                      width = 70, 
                      height = 45, 
                      scale = 0.3)


