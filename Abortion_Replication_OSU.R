#Load Data and Libraries for Analysis

df <- read.csv(file.choose())
install.packages("e1071")
install.packages("lmtest")
install.packages("interactions")
install.packages("ggplot2")
installed.packages("dplyr")
library(e1071)
library(lmtest)
library(interactions)
library(ggplot2)
library(dplyr)
sum(duplicated(df))
colSums(is.na(df))

#Remove Missing Data

df <- df[-c(1, 334, 36, 40, 138, 139, 140, 141, 142, 144, 145, 180, 205, 221, 231, 246, 249, 250, 260, 275, 314, 337, 351, 353, 365, 401, 436, 443, 445, 461, 472, 475, 476, 496, 497, 498, 499, 505, 507, 508, 530, 538, 558, 562),]
colSums(is.na(df))

#Remove Failed Attention Check

gimmedacount <- table(df$attn3)
print(gimmedacount)
df_filtered <- df[df$attn3 == 4,]
gimmedacount <- table(df_filtered$attn3)
print(gimmedacount)

#Reverse Coding
#STMO

df_filtered$soi_3.1_reversed <- 10 - df_filtered$soi_3.1
gimmedacount <- table(df_filtered$soi_3.1_reversed)
print(gimmedacount)
gimmedacount <- table(df_filtered$soi_3.1)
print(gimmedacount)

#Abortion Opps

df_filtered$Q9_1_reversed <- 8 - df_filtered$Q9_1
gimmedacount <- table(df_filtered$Q9_1_reversed)
print(gimmedacount)
gimmedacount <- table(df_filtered$Q9_1)
print(gimmedacount)
df_filtered$Q9_3_reversed <- 8 - df_filtered$Q9_3
gimmedacount <- table(df_filtered$Q9_3_reversed)
print(gimmedacount)
gimmedacount <- table(df_filtered$Q9_3)
print(gimmedacount)

#Birth Control Opps

df_filtered$Q10_1_reversed <- 8 - df_filtered$Q10_1
gimmedacount <- table(df_filtered$Q10_1_reversed)
print(gimmedacount)
gimmedacount <- table(df_filtered$Q10_1)
print(gimmedacount)
df_filtered$Q10_3_reversed <- 8 - df_filtered$Q10_3
gimmedacount <- table(df_filtered$Q10_3_reversed)
print(gimmedacount)
gimmedacount <- table(df_filtered$Q10_3)
print(gimmedacount)

#Mean Composites
#Deterrence Beliefs (only items from Study 1)

df_filtered$Deterrence_mean_composite <- rowMeans(df_filtered[, c("abban", "abban.0", "Q4")], na.rm = TRUE)
gimmedacount <- table(df_filtered$Deterrence_mean_composite)
print(gimmedacount)

#Deterrence Beliefs (all items)

df_filtered$Deter_all_composite <- rowMeans(df_filtered[, c("abban", "abban.0", "Q4", "Q6", "Q7", "Q8")], na.rm = TRUE)
gimmedacount <- table(df_filtered$Deter_all_composite)
print(gimmedacount)

#STMO (only items from Study 1)

df_filtered$STMO_subOne_mean_composite <- rowMeans(df_filtered[, c("soi_1.1", "soi_2.1", "soi_3.1_reversed")], na.rm = TRUE)
gimmedacount <- table(df_filtered$STMO_subOne_mean_composite)
print(gimmedacount)

#SOI (all items)

df_filtered$SOI_composite <- rowMeans(df_filtered[, c("soi_1", "soi_2", "soi_3", "soi_1.0", "soi_2.0", "soi_3.0", "soi_1.1", "soi_2.1", "soi_3.1_reversed")], na.rm = TRUE)
gimmedacount <- table(df_filtered$SOI_composite)
print(gimmedacount)

#Abortion Opps (only items from Study 1)

df_filtered$AbortionOpps_mean_composite <- rowMeans(df_filtered[, c("Q9_1_reversed", "Q9_2", "Q9_3_reversed", "Q9_4")], na.rm = TRUE)
gimmedacount <- table(df_filtered$AbortionOpps_mean_composite)
print(gimmedacount)

#Birth Control Opps

df_filtered$BirthConOpps_mean_composite <- rowMeans(df_filtered[, c("Q10_1_reversed", "Q10_2", "Q10_3_reversed", "Q10_4")], na.rm = TRUE)
gimmedacount <- table(df_filtered$BirthConOpps_mean_composite)
print(gimmedacount)


#Religiosity

df_filtered$Religious_composite <- rowMeans(df_filtered[, c("relig_1", "relig_2")], na.rm = TRUE)
gimmedacount <- table(df_filtered$Religious_composite)
print(gimmedacount)

#Gender (dummy)

df_filtered$sex.dummy <- ifelse(df_filtered$sex == "1", 0, ifelse(df_filtered$sex == "2", 1, NA))

#Normality

skewness(df_filtered$Deterrence_mean_composite)
skewness(df_filtered$STMO_subOne_mean_composite)
skewness(df_filtered$AbortionOpps_mean_composite)
kurtosis(df_filtered$Deterrence_mean_composite)
kurtosis(df_filtered$STMO_subOne_mean_composite)
kurtosis(df_filtered$AbortionOpps_mean_composite)

#Scatter Plots

plot(df_filtered$Deterrence_mean_composite, df_filtered$AbortionOpps_mean_composite)
lines(lowess(df_filtered$Deterrence_mean_composite, df_filtered$AbortionOpps_mean_composite), col = "red")
plot(df_filtered$STMO_subOne_mean_composite, df_filtered$AbortionOpps_mean_composite)
lines(lowess(df_filtered$STMO_subOne_mean_composite, df_filtered$AbortionOpps_mean_composite), col = "red")


#STMO Only Model
model <- lm(AbortionOpps_mean_composite ~ STMO_subOne_mean_composite, data = df_filtered)
summary(model)

#Interaction Model 1: Deterrence Beliefs and STMO (only Study 1 items) on Abortion Opps
model <- lm(AbortionOpps_mean_composite ~ STMO_subOne_mean_composite * Deterrence_mean_composite, data = df_filtered)
summary(model)

#SIMPLE SLOPES TEST AND PLOT
# Categorize STMO and Deterrence Beliefs
mean_STMO <- mean(df_filtered$STMO_subOne_mean_composite, na.rm = TRUE)
sd_STMO <- sd(df_filtered$STMO_subOne_mean_composite, na.rm = TRUE)
df_filtered$STMO_cat <- ifelse(df_filtered$STMO_subOne_mean_composite < (mean_STMO - sd_STMO), "Low", 
                      ifelse(df_filtered$STMO_subOne_mean_composite > (mean_STMO + sd_STMO), "High", NA))

mean_det <- mean(df_filtered$Deterrence_mean_composite, na.rm = TRUE)
sd_det <- sd(df_filtered$Deterrence_mean_composite, na.rm = TRUE)
df_filtered$Deterrence_cat <- ifelse(df_filtered$Deterrence_mean_composite < (mean_det - sd_det), "Low", 
                            ifelse(df_filtered$Deterrence_mean_composite > (mean_det + sd_det), "High", NA))

# Convert to factors
df_filtered$STMO_cat <- factor(df_filtered$STMO_cat, levels = c("High", "Low"))
df_filtered$Deterrence_cat <- factor(df_filtered$Deterrence_cat, levels = c("Low", "High"))

# Run the model
model <- lm(AbortionOpps_mean_composite ~ STMO_cat * Deterrence_cat, data = df_filtered)
summary(model)

# Use interact_plot from the 'interactions' package to generate the interaction plot
interaction_plot <- interact_plot(model, pred = "STMO_cat", modx = "Deterrence_cat", 
                                  plot.points = TRUE, modx.labels = c("Low Deterrence", "High Deterrence"), 
                                  x.label = "Short-Term Mating Orientation (STMO)", 
                                  y.label = "Opposition to Abortion")

# Show the plot
print(interaction_plot)

# Calculate means for the categories
mean_values <- aggregate(AbortionOpps_mean_composite ~ STMO_cat + Deterrence_cat, data = df, FUN = mean)
print(mean_values)

#Model 1: Gender Only

model_withCovariates <- lm(AbortionOpps_mean_composite ~ sex.dummy, data = df_filtered)
summary(model_withCovariates)

#Model 1: Gender and STMO

model_withCovariates <- lm(AbortionOpps_mean_composite ~ STMO_subOne_mean_composite + sex.dummy, data = df_filtered)
summary(model_withCovariates)

#Model 2: AbortionOpps_No Covariates (now with SOI and all deterrence beliefs)

model <- lm(AbortionOpps_mean_composite ~ SOI_composite * Deter_all_composite, data = df_filtered)
summary(model)

#Model 3: Birth Con Opps_No Covariates

model <- lm(BirthConOpps_mean_composite ~ SOI_composite * Deter_all_composite, data = df_filtered)
summary(model)



