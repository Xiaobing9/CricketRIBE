#for a review: use a glm and include sex as a factor. 

library(tidyverse)
library(lme4)
library(emmeans)
library(effects)
library(car)
library(ggpubr)
###### Experiment 1 ######
#this assumes that the file is in the same directory as the data 

data1 <- read.csv("Exp1Tidy.csv")

#question: is there a difference when crickets live with irradiated crickets? 
head(data1)

data1$Group <- relevel(as.factor(data1$Group), "Non-Cohabitated")
levels(data1$Group)
data1$Sex <- as.factor(data1$Sex)

with(data1, table(Sex, Group))


#now we want to fit the model for this. 
#creating a linear model testing the effects of group and sex on weight
mod1 <- lm(Weight ~ Group*Sex, data = data1)

summary(mod1)
anova(mod1)
plot(mod1)


plot(emmeans(mod1, ~ Group, by = "Sex"))


ggplot(pee, aes(x = Sex, y = emmean, col = Group)) + 
  geom_point(position = position_dodge (width = 1)) + 
  geom_crossbar(aes(ymax = upper.CL, ymin = lower.CL), 
                position = position_dodge (width = 1),
                width = 0.7,
                fatten = 1) + 
  scale_colour_manual(values=c("black", "darkgrey")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  ylab("Maturation Weight (mg)")

ggplot(peem, aes(x = Sex, y = emmean, col = Group)) + 
  geom_point(position = position_dodge (width = 1)) + 
  geom_crossbar(aes(ymax = upper.CL, ymin = lower.CL), 
                position = position_dodge (width = 1),
                width = 0.7,
                fatten = 1) + 
  scale_colour_manual(values=c("black", "darkgrey")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  ylab("Maturation Weight (mg)")

ggplot(peef, aes(x = Sex, y = emmean, col = Group)) + 
  geom_point(position = position_dodge (width = 1)) + 
  geom_crossbar(aes(ymax = upper.CL, ymin = lower.CL), 
                position = position_dodge (width = 1),
                width = 0.7,
                fatten = 1) + 
  scale_colour_manual(values=c("black", "darkgrey")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  ylab("Maturation Weight (mg)")

#compare_means(Mat.Time ~ Group, method = "anova", data = data1)

#but is there a change for time to maturation?
mod2 <- lm(Mat.Time ~ Group*Sex, data = data1)

summary(mod2)
anova(mod2)

plot(mod2)

plot(emmeans(mod2, ~ Group, by = "Sex"))

ggplot(poo, aes(x = Sex, y = emmean, col = Group)) + 
  geom_point(position = position_dodge (width = 1)) + 
  geom_crossbar(aes(ymax = upper.CL, ymin = lower.CL), 
                position = position_dodge (width = 1),
                width = 0.7,
                fatten = 1) + 
  scale_colour_manual(values=c("black", "darkgrey")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  ylab("Maturation Time (day)")

ggplot(poom, aes(x = Sex, y = emmean, col = Group)) + 
  geom_point(position = position_dodge (width = 1)) + 
  geom_crossbar(aes(ymax = upper.CL, ymin = lower.CL), 
                position = position_dodge (width = 1),
                width = 0.7,
                fatten = 1) + 
  scale_colour_manual(values=c("black", "darkgrey")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  ylab("Maturation Time (day)")

ggplot(poof, aes(x = Sex, y = emmean, col = Group)) + 
  geom_point(position = position_dodge (width = 1)) + 
  geom_crossbar(aes(ymax = upper.CL, ymin = lower.CL), 
                position = position_dodge (width = 1),
                width = 0.7,
                fatten = 1) + 
  scale_colour_manual(values=c("black", "darkgrey")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  ylab("Maturation Time (day)")

#and growth rate?
mod3 <- lm(Growth.Rate ~ Group*Sex, data = data1)

plot(mod3)

summary(mod3)
anova(mod3)

pairs(emmeans(mod3,~ Group, by = "Sex"))

plot(emmeans(mod3, ~ Group, by = "Sex"))

#95% CI

ggplot(crap, aes(x = Sex, y = emmean, col = Group)) + 
  geom_point(position = position_dodge (width = 1)) + 
  geom_crossbar(aes(ymax = upper.CL, ymin = lower.CL), 
                position = position_dodge (width = 1),
                width = 0.7,
                fatten = 1) + 
  scale_colour_manual(values=c("black", "darkgrey")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  #stat_compare_means(method = "wilcox.test") +
  ylab("Growth Rate (mg/day)")
ggplot(crapm, aes(x = Sex, y = emmean, col = Group)) + 
  geom_point(position = position_dodge (width = 1)) + 
  geom_crossbar(aes(ymax = upper.CL, ymin = lower.CL), 
                position = position_dodge (width = 1),
                width = 0.7,
                fatten = 1) + 
  scale_colour_manual(values=c("black", "darkgrey")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  #stat_compare_means(method = "wilcox.test") +
  ylab("Growth Rate (mg/day")

ggplot(crapf, aes(x = Sex, y = emmean, col = Group)) + 
  geom_point(position = position_dodge (width = 1)) + 
  geom_crossbar(aes(ymax = upper.CL, ymin = lower.CL), 
                position = position_dodge (width = 1),
                width = 0.7,
                fatten = 1) + 
  scale_colour_manual(values=c("black", "darkgrey")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  #stat_compare_means(method = "wilcox.test") +
  ylab("Growth Rate (mg/day")


#plotting it out 
#Plotting least square means calculating model with effects of group and sex using emmeans
crap <- data.frame(emmeans(mod3, ~ Group, by = "Sex"))
poo <- data.frame(emmeans(mod2, ~ Group, by = "Sex"))
pee <- data.frame(emmeans(mod1, ~ Group, by = "Sex"))
#Computing contrast in estimated marignal means between groups within sex
pairs(emmeans(mod3, ~ Group, by = "Sex"))
pairs(emmeans(mod2, ~ Group, by = "Sex"))
pairs(emmeans(mod1, ~ Group, by = "Sex"))
#computing contrast within groups
pairs(emmeans(mod3, ~ Sex, by = "Group"))
pairs(emmeans(mod2, ~ Sex, by = "Group"))
pairs(emmeans(mod1, ~ Sex, by = "Group"))


my_comparisons <- list( c("Group", "Sex"))

crapf <-filter(crap, Sex == "F")
crapm <-filter(crap, Sex == "M")

poof <- filter(poo, Sex == "F")
poom <- filter(poo, Sex == "M")

peef <- filter(pee, Sex == "F")
peem <- filter(pee, Sex == "M")

ggplot(crap, aes(x = Sex, y = emmean, col = Group)) + 
  geom_point(position = position_dodge (width = 0.3)) + 
  #95% CI
  geom_crossbar(aes(ymax = upper.CL, ymin = lower.CL), 
                position = position_dodge (width = 0.3),
                width = 0.2,
                fatten = 1) + 
  scale_colour_manual(values=c("black", "darkgrey")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  ylab("Growth Rate (mg/day")


ggplot(poo, aes(x = Sex, y = emmean, col = Group)) + 
  #geom_point(position = position_dodge (width = 0.3)) + 
  geom_crossbar(aes(ymax = upper.CL, ymin = lower.CL), 
                position = position_dodge (width = 0.3),
                width = 0.2,
                fatten = 1) + 
  scale_colour_manual(values=c("black", "darkgrey")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  ylab("Maturation Time (day)")


ggplot(pee, aes(x = Sex, y = emmean, col = Group)) + 
  geom_point(position = position_dodge (width = 0.3)) + 
  geom_crossbar(aes(ymax = upper.CL, ymin = lower.CL), 
                position = position_dodge (width = 0.3),
                width = 0.2,
                fatten = 1) + 
  scale_colour_manual(values=c("black", "darkgrey")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  ylab("Maturation Weight (mg)")

#######relationship between mat time and weight ######

ggplot(data1, aes(x = Mat.Time, y = Weight, col = Sex)) + 
  geom_point() + 
  geom_smooth(method = lm)

#some stats 
#this looks wild because so far from 0 
data1$mat.timeC <- data1$Mat.Time - mean(data1$Mat.Time)

mod4 <- lm(Weight ~ mat.timeC*Sex*Group, dat = data1)

plot(mod4)
summary(mod4)
anova(mod4)

plot(allEffects(mod4))


trash <- data.frame(predictorEffect("mat.timeC", mod4))

#the ribbon is wild. must fix
ggplot(trash, aes(x = mat.timeC, y = fit, col = Sex, linetype = Group)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) 

#Experiment 2 data
ggplot(data2, aes(x = Mat.Time, y = Weight, col = Sex)) + 
  geom_point() + 
  geom_smooth(method = lm)

data2$mat.timeC <- data2$Mat.Time - mean(data2$Mat.Time)
uh <- lm(Weight ~ mat.timeC*Sex*Group, dat = data2)
Anova(uh)
plot(allEffects(uh))


############Experiment 2 Combined?##############

data2 <- read.csv("Exp2Tidy.csv")

head(data2)
str(data2)

#getting dose and timeing sepearate. 

data2$timeing <- ifelse(grepl("^New", data2$Group), "New", "Previous")

##data2$Group <- sub("Previous-23.2-", "Previous-23.2", data2$Group)
data2$dose <- str_sub(data2$Group, -4, -1)

str(data2)

with(data2, table(Sex, Group))

#Comparisions with random effect of group
data2$timeing <- relevel(as.factor(data2$timeing) , "New")
data2$dose <- relevel(as.factor(data2$dose) , "Sham")


#same questions as before. lmer -> linear model with random effect
#random efect accounts for the cohort diffrences. 
#We want to account for groups, but we don't care, doesn't estimate mean, only variance
mod5 <- lmer(Weight ~ dose*Sex + (1|timeing), data = data2)

plot(mod5)

summary(mod5)
Anova(mod5)

plot(emmeans(mod5, ~dose, by = "Sex"))

pairs(emmeans(mod5, ~dose, by = "Sex"))
pairs(emmeans(mod5, ~Sex, by = "dose"))


we1 <- data.frame(emmeans(mod5, ~ dose, by = "Sex"))

#by sex
ggplot(we1, aes(x = dose, y = emmean, col = Sex)) + 
  #geom_point(position = position_dodge (width = 0.3)) + 
  geom_crossbar(aes(ymax = upper.CL, ymin = lower.CL), 
                position = position_dodge (width = 0.3),
                width = 0.2,
                fatten = 1) + 
  scale_colour_manual(values=c("darkgrey", "black")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  ylab("Maturation Weight (mg)")

#by dose
ggplot(we1, aes(x = Sex, y = emmean, col = dose)) + 
  #geom_point(position = position_dodge (width = 0.3)) + 
  geom_crossbar(aes(ymax = upper.CL, ymin = lower.CL), 
                position = position_dodge (width = 0.3),
                width = 0.2,
                fatten = 1) + 
  scale_colour_manual(values=c("black", "darkgrey", "lightgrey")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  ylab("Maturation Weight (mg)")

mod6 <- lmer(Mat.Time ~ dose*Sex + (1|timeing), data = data2)

plot(mod6)

summary(mod6)
Anova(mod6)

plot(emmeans(mod6, ~dose, by = "Sex"))

pairs(emmeans(mod6, ~dose, by = "Sex"))

mt1 <- data.frame(emmeans(mod6, ~ dose, by = "Sex"))

ggplot(mt1, aes(x = dose, y = emmean, col = Sex)) + 
  #geom_point(position = position_dodge (width = 0.3)) + 
  geom_crossbar(aes(ymax = upper.CL, ymin = lower.CL), 
                position = position_dodge (width = 0.3),
                width = 0.2,
                fatten = 1) + 
  scale_colour_manual(values=c("darkgrey", "black")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  ylab("Maturation Time (Day)")

ggplot(mt1, aes(x = Sex, y = emmean, col = dose)) + 
  #geom_point(position = position_dodge (width = 0.3)) + 
  geom_crossbar(aes(ymax = upper.CL, ymin = lower.CL), 
                position = position_dodge (width = 0.3),
                width = 0.2,
                fatten = 1) + 
  scale_colour_manual(values=c( "black", "darkgrey", "lightgrey")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  ylab("Maturation Time (day)")


mod7 <- lmer(Growth.Rate ~ dose*Sex + (1|timeing), data = data2)

plot(mod7)

summary(mod7)
Anova(mod7)

plot(emmeans(mod7, ~dose, by = "Sex"))

pairs(emmeans(mod7, ~dose, by = "Sex"))


gr1 <- data.frame(emmeans(mod7, ~ dose, by = "Sex"))

ggplot(gr1, aes(x = dose, y = emmean, col = Sex)) + 
  #geom_point(position = position_dodge (width = 0.3)) + 
  geom_crossbar(aes(ymax = upper.CL, ymin = lower.CL), 
                position = position_dodge (width = 0.3),
                width = 0.2,
                fatten = 1) + 
  scale_colour_manual(values=c("darkgrey", "black")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  ylab("Growth Rate (mg/day))")


ggplot(gr1, aes(x = Sex, y = emmean, col = dose)) + 
  #geom_point(position = position_dodge (width = 0.3)) + 
  geom_crossbar(aes(ymax = upper.CL, ymin = lower.CL), 
                position = position_dodge (width = 0.3),
                width = 0.2,
                fatten = 1) + 
  scale_colour_manual(values=c("black", "darkgrey", "lightgrey")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  ylab("Growth Rate (mg/day))")

###########Experiment 2 separated##########
data3 <- read.csv("3.csv")
data3$Group <- relevel(as.factor(data3$Group), "New-Sham")


mod8 <- lm(Weight ~ Group*Sex, data = data3)

plot(mod8)

summary(mod8)
Anova(mod8)

plot(emmeans(mod8, ~ Group, by = "Sex"))

pairs(emmeans(mod8, ~ Group, by = "Sex"))




mod9 <- lm(Mat.Time ~ Group*Sex, data = data3)

plot(mod9)

summary(mod9)
Anova(mod9)


plot(emmeans(mod9, ~ Group, by = "Sex"))

pairs(emmeans(mod9, ~ Group, by = "Sex"))



mod10 <- lm(Growth.Rate ~ Group*Sex, data = data3)

plot(mod10)

summary(mod10)
Anova(mod10)


plot(emmeans(mod10, ~ Group, by = "Sex"))

pairs(emmeans(mod10, ~ Group, by = "Sex"))




data4 <- read.csv("4.csv")
data4$Group <- relevel(as.factor(data4$Group), "Previous-Sham")

mod11 <- lm(Weight ~ Group*Sex, data = data4)

plot(mod11)

summary(mod11)
Anova(mod11)


plot(emmeans(mod11, ~ Group, by = "Sex"))

pairs(emmeans(mod11, ~ Group, by = "Sex"))


mod12 <- lm(Mat.Time ~ Group*Sex, data = data4)

plot(mod12)

summary(mod12)
Anova(mod12)


plot(emmeans(mod12, ~ Group, by = "Sex"))

pairs(emmeans(mod12, ~ Group, by = "Sex"))

mod13 <- lm(Growth.Rate ~ Group*Sex, data = data4)

plot(mod13)

summary(mod13)
Anova(mod13)


plot(emmeans(mod13, ~ Group, by = "Sex"))

pairs(emmeans(mod13, ~ Group, by = "Sex"))


#########Experiment 2 as is##########
data2 <- read.csv("Exp2Tidy.csv")
data2$Group <- relevel(as.factor(data2$Group), "New-Sham")

mod14 <- lm(Weight ~ Group*Sex, data = data2)

plot(mod14)

summary(mod14)
Anova(mod14)

plot(emmeans(mod14, ~ Group, by = "Sex"))

pairs(emmeans(mod14, ~ Group, by = "Sex"))
pairs(emmeans(mod14, ~ Sex, by = "Group"))

#Corrected using Tukey HSD 
we <- data.frame(emmeans(mod14, ~ Group, by = "Sex"))
wef <- filter(we, Sex == "F")
wem <- filter(we, Sex == "M")

ggplot(wem, aes(x = Group, y = emmean, col = Group)) + 
  geom_point(position = position_dodge (width = 1)) + 
  geom_crossbar(aes(ymax = upper.CL, ymin = lower.CL), 
                position = position_dodge (width = 1),
                width = 0.7,
                fatten = 1,
  ) + 
  scale_colour_manual(values=c("black", "grey69", "grey60", "grey55", "grey30")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = "none") +
  ylab("Maturation Weight (mg)")+
  xlab("Males")

ggplot(wef, aes(x = Group, y = emmean, col = Group)) + 
  geom_point(position = position_dodge (width = 1)) + 
  geom_crossbar(aes(ymax = upper.CL, ymin = lower.CL), 
                position = position_dodge (width = 1),
                width = 0.7,
                fatten = 1,
  ) + 
  scale_colour_manual(values=c("black", "grey69", "grey60", "grey55", "grey30")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = "none") +
  ylab("Maturation Weight (mg)")+
  xlab("Females")



mod15 <- lm(Mat.Time ~ Group*Sex, data = data2)

plot(mod15)

summary(mod15)
Anova(mod15)

plot(emmeans(mod15, ~ Group, by = "Sex"))

pairs(emmeans(mod15, ~ Group, by = "Sex"))
pairs(emmeans(mod15, ~ Sex, by = "Group"))

mt <- data.frame(emmeans(mod15, ~ Group, by = "Sex"))
mtf <- filter(mt, Sex == "F")
mtm <- filter(mt, Sex == "M")

ggplot(mtm, aes(x = Group, y = emmean, col = Group)) + 
  geom_point(position = position_dodge (width = 1)) + 
  geom_crossbar(aes(ymax = upper.CL, ymin = lower.CL), 
                position = position_dodge (width = 1),
                width = 0.7,
                fatten = 1,
  ) + 
  scale_colour_manual(values=c("black", "grey69", "grey60", "grey55", "grey30")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = "none") +
  ylab("Maturation Time (days)")+
  xlab("Males")

ggplot(mtf, aes(x = Group, y = emmean, col = Group)) + 
  geom_point(position = position_dodge (width = 1)) + 
  geom_crossbar(aes(ymax = upper.CL, ymin = lower.CL), 
                position = position_dodge (width = 1),
                width = 0.7,
                fatten = 1,
  ) + 
  scale_colour_manual(values=c("black", "grey69", "grey60", "grey55", "grey30")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = "none") +
  ylab("Maturation Time (days)") +
  xlab("Females")

ggplot(mt, aes(x = Sex, y = emmean, col = Group)) + 
  geom_point(position = position_dodge (width = 1)) + 
  geom_crossbar(aes(ymax = upper.CL, ymin = lower.CL), 
                position = position_dodge (width = 1),
                width = 0.7,
                fatten = 1,
  ) + 
  scale_colour_manual(values=c("black", "grey69", "grey60", "grey55", "grey30")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  ylab("Maturation Time (days)")+



mod16 <- lm(Growth.Rate ~ Group*Sex, data = data2)

plot(mod16)

summary(mod16)
Anova(mod16)

plot(emmeans(mod16, ~ Group, by = "Sex"))

pairs(emmeans(mod16, ~ Group, by = "Sex"))
pairs(emmeans(mod16, ~ Sex, by = "Group"))

gr <- data.frame(emmeans(mod16, ~ Group, by = "Sex"))

grf <- filter(gr, Sex == "F")
grm <- filter(gr, Sex == "M")

ggplot(grm, aes(x = Group, y = emmean, col = Group)) + 
  geom_point(position = position_dodge (width = 1)) + 
  geom_crossbar(aes(ymax = upper.CL, ymin = lower.CL), 
                position = position_dodge (width = 1),
                width = 0.7,
                fatten = 1,
  ) + 
  scale_colour_manual(values=c("black", "grey69", "grey60", "grey55", "grey30")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = "none") +
  ylab("Growth Rate (mg/day)") +
  xlab("Males")

ggplot(grf, aes(x = Group, y = emmean, col = Group)) + 
  geom_point(position = position_dodge (width = 1)) + 
  geom_crossbar(aes(ymax = upper.CL, ymin = lower.CL), 
                position = position_dodge (width = 1),
                width = 0.7,
                fatten = 1,
                
  ) + 
  scale_colour_manual(values=c("black", "grey69", "grey60", "grey55", "grey30")) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = "none") +
  ylab("Growth Rate (mg/day)") +
  xlab("Females")
