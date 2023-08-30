### GLM with effect

library(tidyverse)
library(tidylog)
library(rcompanion)
library(tidyr)

outdir <- "Figures/"

## data 
data <- read.csv("/Users/katieirving/Library/CloudStorage/OneDrive-SharedLibraries-SCCWRP/SCCWRP Risk Assessment - General/Shellfish Tox Datasets for Katie/AquaticOrganisms_Clean_final_ToMEx1.csv")
names(dataR)
unique(data$lvl2)
## filter mollusc and only neccessary columns

data <- data %>%
  filter(organism.group == "Mollusca") %>%
  select(source, species, effect, dose.particles.mL.master, lvl1, lvl2, lvl3)


# Reproduction ------------------------------------------------------------
names(dataR)
head(dataR)
dataR <- data %>%
  filter(lvl2 == "reproduction") %>%
  mutate(effectCon = ifelse(effect == "Y", 1, 0), dose.particles.L = dose.particles.mL.master*1000) 

?as.factor
str(dataR)
reprMod <- glm(effectCon~dose.particles.L, family=binomial(link="logit"), data=dataR)
summary(reprMod)
rsq <- nagelkerke(reprMod)
rsq$Pseudo.R.squared.for.model.vs.null[3] ##  0.300336

predRepro <- predict(reprMod, newdata=dataR, type="response")

dataR <- cbind(dataR, predRepro) 
dataR


r1 <- ggplot(dataR, aes(x=dose.particles.L, y=predRepro))+
  geom_line()+
  theme(strip.background = element_blank(),
        strip.text.y = element_blank()) +
  geom_vline(xintercept = 14300000000,  linetype="dashed", linewidth=0.5, color = "grey50") +
  geom_vline(xintercept = 143000000000,  linetype="dashed", linewidth=0.5, color = "grey50") +
  scale_y_continuous(limits=c(0,1))+
  theme_minimal()+
  theme(text = element_text(size=15),axis.text.x = element_text(angle = 60,  vjust = 0.5, hjust=0.5)) +
  labs(
       x = "Dose: Particles per L",
       y = "Probability of Reproductive Effect") #+ theme_bw(base_size = 15)
r1

out.filename <- paste0("Figures/04_reproduction_dose_rep.jpg")
ggsave(r1, file = out.filename, dpi=300, height=4, width=6)

# Mortality ------------------------------------------------------------
names(dataM)
head(dataM)
dataM <- data %>%
  filter(lvl2 == "mortality") %>%
  mutate(effectCon = ifelse(effect == "Y", 1, 0), dose.particles.L = dose.particles.mL.master*1000)


MortMod <- glm(effectCon~dose.particles.L, family=binomial(link="logit"), data=dataM)

rsq <- nagelkerke(MortMod)
rsq$Pseudo.R.squared.for.model.vs.null[3] ##   0.364904

predMort <- predict(MortMod, newdata=dataM, type="response")

dataM <- cbind(dataM, predMort) 
dataM


m1 <- ggplot(dataM, aes(x=dose.particles.L, y=predMort))+
  geom_line()+
  theme(strip.background = element_blank(),
        strip.text.y = element_blank()) +
  geom_vline(xintercept = 1,  linetype="dashed", linewidth=0.5, color = "grey50") +
  geom_vline(xintercept = 3,  linetype="dashed", linewidth=0.5, color = "grey50") +
  scale_y_continuous(limits=c(0,1))+
  theme_minimal()+
  theme(text = element_text(size=15),axis.text.x = element_text(angle = 60,  vjust = 0.5, hjust=0.5)) +
  labs(
    x = "Dose: Particles per L",
    y = "Probability of Increased Mortality") #+ theme_bw(base_size = 15)
m1

out.filename <- paste0("Figures/04_mortality_dose_rep.jpg")
ggsave(m1, file = out.filename, dpi=300, height=4, width=6)

