library(openxlsx)

###########################
ind_dat <-read_excel("02_Individuals_2020_AS .xlsx")
str(ind_dat)
ind_dat$management <- as.factor(ind_dat$management)
str(ind_dat)
names(ind_dat)
####

model1 <- lm(Leave.Nb ~ management + area + exposition + slope + soil_depth + soil_water + PAR_above + PAR_ground + HL_cover + moss_cover + soil_cover + vegHeigth_max + vegHeigth_90, data = ind_dat)
summary(model1)
plot(model1)


# glm....

library(MASS)
model2 <- glm(Leave.Nb ~ management + area + exposition + slope + soil_depth + soil_water + PAR_above + PAR_ground + HL_cover + moss_cover + soil_cover + vegHeigth_max + vegHeigth_90, data = ind_dat)
summary(model2)
plot(model2)

model3 <- glm(Leave.Nb ~ management + area + slope + soil_depth + soil_water + PAR_above + PAR_ground + HL_cover + moss_cover + soil_cover + vegHeigth_max + vegHeigth_90, data = ind_dat)
anova(model2, model3)
summary(model3)

model4 <- glm(Leave.Nb ~ management + area + slope + soil_depth + soil_water + PAR_above + PAR_ground + HL_cover + soil_cover + vegHeigth_max + vegHeigth_90, data = ind_dat)
anova(model3, model4)
summary(model4)

model5 <- glm(Leave.Nb ~ management + area + slope + soil_depth + PAR_above + PAR_ground + HL_cover + soil_cover + vegHeigth_max + vegHeigth_90, data = ind_dat)
anova(model4, model5)
summary(model5)

model6 <- glm(Leave.Nb ~ management + area + slope + soil_depth + PAR_above + PAR_ground + HL_cover + soil_cover + vegHeigth_90, data = ind_dat)
anova(model5, model6)
summary(model6)

model7 <- glm(Leave.Nb ~ management + area + soil_depth + PAR_above + PAR_ground + HL_cover + soil_cover + vegHeigth_90, data = ind_dat)
anova(model6, model7)
summary(model7)

model8 <- glm(Leave.Nb ~ management + area + PAR_above + PAR_ground + HL_cover + soil_cover + vegHeigth_90, data = ind_dat)
anova(model7, model8)
summary(model8)

model9 <- glm(Leave.Nb ~ management + PAR_above + PAR_ground + HL_cover + soil_cover + vegHeigth_90, data = ind_dat)
anova(model8, model9)
summary(model9)

model10 <- glm(Leave.Nb ~ management + PAR_ground + HL_cover + soil_cover + vegHeigth_90, data = ind_dat)
anova(model9, model10)
summary(model10)

model11 <- glm(Leave.Nb ~ management +HL_cover + soil_cover + vegHeigth_90, data = ind_dat)
anova(model10, model11)
summary(model11)

model12 <- glm(Leave.Nb ~ management +HL_cover + soil_cover, data = ind_dat)
anova(model11, model12)
summary(model12)

model13 <- glm(Leave.Nb ~ management +HL_cover, data = ind_dat)
anova(model12, model13)
summary(model13)


#######
#### plot Number of Leaves explained by management

boxplot(Leave.Nb ~ management, data = ind_dat, col = "red")

# are there significat differences between management types?
install.packages('emmeans')
library(emmeans)
#differnce function of paris
diff <- emmeans(model13, ~ management)
pairs(diff)

# how to get the significance letters for the graph?
install.packages('multcomp')
library(multcomp)
library(multcompView)
#compact letter display
cld(diff, Letters = letters)
#
boxplot(Leave.Nb ~ management, data = ind_dat, col = "red", xlab = "Management type", ylab = "Nb of Leaves", ylim = c(3.0, 6.8))
text(1, 6.7, "a")
text(2, 6.7, "ab")
text(3, 6.7, "ab")
text(4, 6.7, "b")

#### plot Number of Leaves explained by HL cover
library(ggplot2)
ggplot(data = ind_dat, aes(x = HL_cover, y = Leave.Nb))+ 
  geom_point(color = "black", alpha = 0.25, shape = 16, size = 1) +
  geom_smooth(method = MASS::glm.nb, formula = y ~ x, color = "darkgreen", se = TRUE, fill = "green", alpha = 0.35) +
  labs(x = "Percentage of HL cover", y = "Number of Leaves") +
  theme_bw()


