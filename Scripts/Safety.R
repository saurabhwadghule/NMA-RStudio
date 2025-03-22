library(netmeta)
library(tidyverse)
library(readxl)
safety <- read_excel("C:/Users/Saurabh Wadghule/OneDrive - Innomagine Consulting Private Limited/Desktop/Master project excelfiles/NMA/safety.xlsx", 
                     sheet = "Sheet2")

attach(safety)


#generate TE, and seTE
saf <- pairwise(treat = list(treat, Placebo), 
                event = list(event1, event2),
                n = list(n1, n2),
                data = safety,
                sm = "RR")

#---------------------------------
#any grade 3/4 AE
#---------------------------------
anyAE <- saf %>% 
  filter(outcome == "Any AE")

#plot forest plot
plot1 <- netmeta(anyAE, common = FALSE, reference.group = "Placebo")


forest(plot1,
       sortvar = -sucra,
       drop.reference.group = TRUE,
       smlab = "Any Grade 3 or 4 AE",
       label.right = "  Favours Placebo",
       leftlabs = "Compared with Placebo",
       rightcols = c("effect", "ci"),
       rightlabs = c("HR", "95% CI"),
       xlim = c(1, 6),
       small.values = "desirable",
       digits.prop = 4)

netrank(plot1)
netleague(plot1,
          seq = c("O", "N", "R", "P"))




#---------------------------------
#Anemia [grade 3/4]
#---------------------------------
anemia <- saf %>% 
  filter(outcome == "Anemia")

#plot forest plot
plot2 <- netmeta(anemia, common = FALSE, reference.group = "Placebo")


forest(plot2,
       sortvar = -sucra,
       drop.reference.group = TRUE,
       smlab = "Anemia (Grade 3 or 4)",
       label.right = "  Favours Placebo",
       leftlabs = "Compared with Placebo",
       rightcols = c("effect", "ci"),
       rightlabs = c("HR", "95% CI"),
       xlim = c(1, 100),
       small.values = "desirable",
       digits.prop = 4)

netrank(plot2)
netleague(plot2,
          seq = c("O", "N", "R", "P"))



#---------------------------------
#Neutropenia [grade 3/4]
#---------------------------------
Neutropenia <- saf %>% 
  filter(outcome == "Neutropenia")

#plot forest plot
plot3 <- netmeta(Neutropenia, common = FALSE, reference.group = "Placebo")


forest(plot3,
       sortvar = -sucra,
       drop.reference.group = TRUE,
       smlab = "Neutropenia (Grade 3 or 4)",
       label.right = "  Favours Placebo",
       leftlabs = "Compared with Placebo",
       rightcols = c("effect", "ci"),
       rightlabs = c("HR", "95% CI"),
       xlim = c(0.5, 50),
       small.values = "desirable",
       digits.prop = 4)

netrank(plot3)
netleague(plot3,
          seq = c("O", "N", "R", "P"))


#---------------------------------
#Thrombocytopenia [grade 3/4]
#---------------------------------
Thrombocytopenia <- saf %>% 
  filter(outcome == "Thrombocytopenia")

#plot forest plot
plot4 <- netmeta(Thrombocytopenia,
                 subset = Thrombocytopenia$study %in% c("SOLO-1", "ATHENA-MONO", "PRIME"),
                 common = FALSE, reference.group = "Placebo")


forest(plot4,
       sortvar = -sucra,
       drop.reference.group = TRUE,
       smlab = "  Thrombocytopenia (Grade 3 or 4)",
       label.right = "  Favours Placebo",
       label.left = "  Favours Intervention",
       leftlabs = " Compared with Placebo   ",
       rightcols = c("effect", "ci"),
       rightlabs = c("
        HR", "95% CI"),
       xlim = c(0.2,50),
       small.values = "desirable",
       digits.prop = 4)

netrank(plot4)
netleague(plot4,
          seq = c("O", "N", "R", "P"))
