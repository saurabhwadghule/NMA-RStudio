library(netmeta)
library(dplyr)
library(tidyverse)
library(dplyr)
library(readxl)


#-----------------------------------
#PFS in OP
enma <- read_excel("C:/Users/Saurabh Wadghule/OneDrive - Innomagine Consulting Private Limited/Desktop/Master project excelfiles/NMA/enma.xlsx", 
                   range = "A1:G5")
View(enma)

attach(enma)
net1 <- netmeta(TE, seTE, 
                treat1,
                treat2,
                plastic = FALSE,
                Study,
                sm = "HR",
                data = enma,
                common = FALSE,
                reference.group = "Placebo")

netgraph(net1,
         points = TRUE,
         points.max = 1,
         points.min = 4,
         plastic = FALSE,
         scale = 0.7,
         thickness = "number.of.studies",
         number.of.studies = TRUE,
         pch.points = 16,
         col.points = "RED",
         cex.number.of.studies = 0.8,
         cex = 1.2,
         offset = 0.14,
         seq = "optimal")

forest(net1, 
       pooled = ifelse(net1$random, 3, 1),
       sortvar = -sucra,
       drop.reference.group = TRUE,
       smlab = "PFS in Overall Population",
       label.left = "Favours Intervention",
       leftlabs = "Compared with Placebo",
       rightcols = c("effect", "ci"),
       rightlabs = c("HR", "95% CI"),
       xlim = c(0.2,1),
       small.values = "desirable",
       digits.prop = 4)

netrank(net1, method = "SUCRA")

netleague(net1,
          seq = c("O", "N", "R", "P"))

netleague(net1,
          seq = c("O", "N", "R", "P"),
          path = "C:\\Users\\Saurabh Wadghule\\Downloads\\SUCRA.xlsx",
          overwrite = TRUE, 
          digits = 2)


#-----------------------------------------
#OS in OP
enma1 <- read_excel("C:/Users/Saurabh Wadghule/OneDrive - Innomagine Consulting Private Limited/Desktop/Master project excelfiles/NMA/enma.xlsx", 
                   sheet = "OS in OP")
View(enma1)

attach(enma1)
net2 <- netmeta(TE, seTE, 
                treat1,
                treat2,
                plastic = FALSE,
                Study,
                sm = "HR",
                data = enma1,
                common = FALSE,
                reference.group = "Placebo")

netgraph(net2,
         points = TRUE,
         points.max = 1,
         points.min = 4,
         plastic = FALSE,
         scale = 0.7,
         thickness = "number.of.studies",
         number.of.studies = TRUE,
         pch.points = 16,
         col.points = "RED",
         cex.number.of.studies = 0.8,
         cex = 1.2,
         offset = 0.14)

forest(net2, 
       pooled = ifelse(net1$random, 3, 1),
       sortvar = -sucra,
       drop.reference.group = TRUE,
       smlab = "OS in Overall Population",
       label.left = "Favours Intervention",
       label.right = "Favours placebo",
       leftlabs = "Compared with Placebo",
       rightlabs = c("HR", "95% CI"),
       rightcols = c("effect", "ci"),
       xlim = c(0.5,2),
       small.values = "desirable",
       digits.prop = 4)

netrank(net2, method = "SUCRA")

netleague(net2,
          path = "C:\\Users\\Saurabh Wadghule\\Downloads\\SUCRA.xlsx",
          overwrite = TRUE, 
          digits = 2)


#---------------------------------------
#Pfs in BRCAm
enma2 <- read_excel("C:/Users/Saurabh Wadghule/OneDrive - Innomagine Consulting Private Limited/Desktop/Master project excelfiles/NMA/enma.xlsx", 
                    sheet = "PFS in BRCAm")

attach(enma1)
net3 <- netmeta(TE, seTE, 
                treat1,
                treat2,
                plastic = FALSE,
                Study,
                sm = "HR",
                data = enma2,
                common = FALSE,
                reference.group = "Placebo")

netgraph(net3,
         points = TRUE,
         points.max = 1,
         points.min = 4,
         plastic = FALSE,
         scale = 0.7,
         thickness = "number.of.studies",
         number.of.studies = TRUE,
         pch.points = 16,
         col.points = "RED",
         cex.number.of.studies = 0.8,
         cex = 1.2,
         offset = 0.14,
         seq = "optimal")

forest(net3, 
       pooled = ifelse(net1$random, 3, 1),
       sortvar = -sucra,
       drop.reference.group = TRUE,
       smlab = "PFS in BRCAm patients",
       label.left = "Favours Intervention",
       leftlabs = "Compared with Placebo",
       rightlabs = c("HR", "95% CI"),
       rightcols = c("effect", "ci"),
       xlim = c(0.2,1),
       small.values = "desirable",
       digits.prop = 4)

netrank(net3, method = "SUCRA")

netleague(net3,
          seq = c("O", "N", "R", "P"),
          path = "C:\\Users\\Saurabh Wadghule\\Downloads\\SUCRA.xlsx",
          overwrite = TRUE, 
          digits = 2)


#---------------------------------------------------
#Pfs in HRd
enma3 <- read_excel("C:/Users/Saurabh Wadghule/OneDrive - Innomagine Consulting Private Limited/Desktop/Master project excelfiles/NMA/enma.xlsx", 
                    sheet = "PFS in HRd")

attach(enma3)
net4 <- netmeta(TE, seTE, 
                treat1,
                treat2,
                plastic = FALSE,
                Study,
                sm = "HR",
                data = enma3,
                common = FALSE,
                reference.group = "Placebo")

netgraph(net4,
         rotate = 120,
         points = TRUE,
         points.max = 1,
         points.min = 4,
         plastic = FALSE,
         scale = 0.7,
         thickness = "number.of.studies",
         number.of.studies = TRUE,
         pch.points = 16,
         col.points = "RED",
         cex.number.of.studies = 0.8,
         cex = 1.2,
         offset = 0.14,
         seq = "optimal")

forest(net4,
       pooled = ifelse(net1$random, 3, 1),
       sortvar = -sucra,
       drop.reference.group = TRUE,
       smlab = "PFS in HRd positive patients",
       label.left = "Favours Intervention",
       leftlabs = "Compared with Placebo",
       rightlabs = c("HR", "95% CI"),
       rightcols = c("effect", "ci"),
       xlim = c(0.2,1),
       small.values = "desirable",
       digits.prop = 4)

netrank(net4, method = "SUCRA")

netleague(net4,
          seq = c("N", "R", "P"),
          path = "C:\\Users\\Saurabh Wadghule\\Downloads\\SUCRA.xlsx",
          overwrite = TRUE, 
          digits = 2)
       