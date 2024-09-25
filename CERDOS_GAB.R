### GAB 2024

library(Rmisc)
library(plyr)
library(glmmTMB)
library(readxl)
library(DHARMa)
library(emmeans)
library(ggplot2)
library(psych)



gab <- read_excel("BD.xlsx")
gab

# Chi2

library(MASS)        
print(str(gab))

gab$IFIFinal<-as.factor(gab$IFIFinal)
gab$Granja<-as.factor(gab$Granja)
data = data.frame(gab$Granja,gab$IFIfinal)

# Create a contingency table with the needed variables.           
data_bis = table(gab$Granja,gab$IFIFinal) 

print(data_bis)

# applying chisq.test() function
print(chisq.test(data_bis))

# GLM - Bernoulli

#INTERCEPT: CORTE OTRO
gab$Granja<-factor(gab$Granja, levels =c("2", "1"))

m1a_bernoulli <- glmmTMB(IFIFinal ~ Granja, data = gab, family = binomial())

summary (m1a_bernoulli)
drop1(m1a_bernoulli, test= "Chisq")

#comparaciones cria

options(emmeans= list(emmeans = list(infer = c(TRUE, FALSE)),
                      contrast = list(infer = c(TRUE, TRUE))))

emmeans(m1a_bernoulli,
        list(pairwise ~ Granja),
        type = "response")
