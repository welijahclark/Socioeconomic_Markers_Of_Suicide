#######################################
#W. Elijah Clark STA 5167 Project Code#
#######################################

#Note: Some Code Discrepancies have occurred with summary statements in between the presentation and final paper.

#All Libraries Used
#########
library(ggplot2)
library(readr)
library(MASS)
library(dplyr)
library(car)
library(multcompView)
library(Rfit)
library(NSM3)
library(corrr)
library(ggcorrplot)
library(factoextra)
library(data.table)
library(GLDEX)
#########

#Custom Functions Used
#########
ihs <- function(x) {
  y <- log(x + sqrt(x ^ 2 + 1))
  return(y)
}
#########

#Data Import
#########
library(readr)
Death <- read_csv("All Academic Files/All Graduate Dox/FSU Statistics/Grad School Course Files/Statistics in Applications 2/Project/New_Project_Post_Withdraw/suicide_rates_1990-2022.csv")

#Removing Duplicates and NAs
library(dplyr)
Death %>% distinct()
Death <- na.omit(Death)

#Old Imputation Method with aggregate() (Did not impute all semi-duplicate rows.)
#unalived1 <- aggregate(Death$SuicideCount,by=list(RegionName=Death$RegionName, CountryName=Death$CountryName, Year=Death$Year, Sex=Death$Sex, AgeGroup=Death$AgeGroup, CauseSpecificDeathPercentage=Death$CauseSpecificDeathPercentage, DeathRatePer100K=Death$DeathRatePer100K, Population=Death$Population, GDP=Death$GDP, GDPPerCapita=Death$GDPPerCapita, GrossNationalIncome=Death$GrossNationalIncome, GNIPerCapita=Death$GNIPerCapita, InflationRate=Death$InflationRate, EmploymentPopulationRatio=Death$EmploymentPopulationRatio),FUN=mean)
#unalived2 <- aggregate(unalived1$CauseSpecificDeathPercentage,by=list(RegionName=unalived1$RegionName, CountryName=unalived1$CountryName, Year=unalived1$Year, Sex=unalived1$Sex, AgeGroup=unalived1$AgeGroup, SuicideCount=unalived1$x, DeathRatePer100K=unalived1$DeathRatePer100K, Population=unalived1$Population, GDP=unalived1$GDP, GDPPerCapita=unalived1$GDPPerCapita, GrossNationalIncome=unalived1$GrossNationalIncome, GNIPerCapita=unalived1$GNIPerCapita, InflationRate=unalived1$InflationRate, EmploymentPopulationRatio=unalived1$EmploymentPopulationRatio),FUN=mean)
#unalived3 <- aggregate(unalived2$DeathRatePer100K,by=list(RegionName=unalived2$RegionName, CountryName=unalived2$CountryName, Year=unalived2$Year, Sex=unalived2$Sex, AgeGroup=unalived2$AgeGroup, SuicideCount=unalived2$SuicideCount, CauseSpecificDeathPercentage=unalived2$x, Population=unalived2$Population, GDP=unalived2$GDP, GDPPerCapita=unalived2$GDPPerCapita, GrossNationalIncome=unalived2$GrossNationalIncome, GNIPerCapita=unalived2$GNIPerCapita, InflationRate=unalived2$InflationRate, EmploymentPopulationRatio=unalived2$EmploymentPopulationRatio),FUN=mean)
#unalived4 <- na.omit(unalived3)
#unalived <- unalived4

#Newer Data Cleaning (Imputes all rows with dplyr)
#Note: One time I ran this removed Unknown genders.
#Re-running it again with post-presentation feedback somehow keeps that factor.
#I do not think I changed anything, so I do not know why that happened.
Death <- Death %>%
  group_by(
    RegionName,
    CountryName,
    Year,
    Sex,
    AgeGroup,
    Population,
    GDP,
    GDPPerCapita,
    GrossNationalIncome,
    GNIPerCapita,
    InflationRate,
    EmploymentPopulationRatio
  ) %>%
  summarise(
    SuicideCount = mean(SuicideCount),
    CauseSpecificDeathPercentage = mean(CauseSpecificDeathPercentage),
    DeathRatePer100K = mean(DeathRatePer100K),
    .groups = "drop"
  )

#How to test if the dplyr data cleaning worked:
Death %>%
  filter(
    CountryName == "United States of America",
    AgeGroup == "25-34 years",
    Year == 2020,
    Sex == "Male"
  ) %>%
  nrow()

#Natural Zero Death versus Removing Zero Deaths
#Remaking the data frame for natural zeros versus removing zeros

#Scenario 1: Zero deaths assumed to be naturally occurring values
unalived <- Death

#Variables Declared Version 1: Imputed
#########
RegionCode <- unalived$RegionCode
Factored_RegionCode <- factor(RegionCode)
RegionName <- unalived$RegionName
Factored_RegionName <- factor(RegionName)
Year <- unalived$Year
Factored_Year <- factor(Year)
Gender <- unalived$Sex
Factored_Gender <- unalived$Sex
AgeGroup <- unalived$AgeGroup
Factored_AgeGroup <- unalived$AgeGroup
Unalived_Count <- unalived$SuicideCount
Unalived_Percentage <- unalived$CauseSpecificDeathPercentage
Unalived_100K <- unalived$DeathRatePer100K
Population <- unalived$Population
GDP <- unalived$GDP
GDP_Capita <- unalived$GDPPerCapita
GNI <- unalived$GrossNationalIncome
GNI_Capita <- unalived$GNIPerCapita
Inflation <- unalived$InflationRate
Employment <- unalived$EmploymentPopulationRatio

#Transformed Variables
float_UnalivedCount <- Unalived_Count+.0001
float_UnalivedPercent <- Unalived_Percentage+.0001
float_Unalived100K <- Unalived_100K+.0001
log_floatUnaliveCount <- log(float_UnalivedCount)
log_floatUnalivePercent <- log(float_UnalivedPercent)
log_floatUnalive100K <- log(float_Unalived100K)
ihs_UnalivedCount <- ihs(Unalived_Count)
ihs_UnalivedPercent <- ihs(Unalived_Percentage)
ihs_Unalived100K <- ihs(Unalived_100K)
log_population <- log(Population)
log_GDP <- log(GDP)
log_GDPCapita <- log(GDP_Capita)
log_GNI <- log(GNI)
log_GNICapita <- log(GNI)
#########

#Summary Statistics
#######
fivenum(Unalived_Count)
boxplot(fivenum(Unalived_Count))
fivenum(Unalived_Percentage)
boxplot(fivenum(Unalived_Percentage))
fivenum(Unalived_100K)
boxplot(fivenum(Unalived_100K))
fivenum(log_floatUnaliveCount)
boxplot(fivenum(log_floatUnaliveCount))
#######

#Some QQ Plots
########
qqPlot(Unalived_Count) + title("QQPlot of untransformed Suicide Deaths")
qqPlot(log_floatUnaliveCount) + title("QQPlot of log Suicide Deaths")
qqPlot(ihs_UnalivedCount) + title("QQPlot of IHS Suicide Deaths")
########


#PCA and Corr Matrix for Scenario 1
#######
#Different Data Frame and Correlation Matrix
testDF1 <- data.frame(Unalived_Count,
                      Unalived_Percentage,
                      Unalived_100K,
                      Population,
                      GDP,
                      GDP_Capita,
                      GNI,
                      GNI_Capita,
                      Inflation,
                      Employment)
                      
data_normalized <- scale(testDF1)
corr_matrix <- cor(testDF1)
print(corr_matrix)
ggcorrplot(corr_matrix) 

#PCA Stuff
data.pca <- princomp(corr_matrix)
summary(data.pca)

data.pca$loadings[, 1:2]
fviz_eig(data.pca, barfill = "thistle2", barcolor = "thistle", addlabels = TRUE)

#Note: cos2 entails qualities of representation
fviz_pca_var(data.pca,  col.var = "cos2",
             gradient.cols = c("midnightblue", "aquamarine", "coral"),
             repel = TRUE)


#Corr Matrix with Transformed Variables
testDF2 <- data.frame(ihs_UnalivedCount,
                      ihs_UnalivedPercent,
                      ihs_Unalived100K,
                      Population,
                      log_GDP,
                      log_GDPCapita,
                      log_GNI,
                      log_GNICapita,
                      Inflation,
                      Employment)

data_normalized2 <- scale(testDF2)
corr_matrix2 <- cor(testDF2)
ggcorrplot(corr_matrix2)


#######



#Attempted Box Cox Transforms
#######
library(MASS)
boxcox(lm(float_UnalivedCount ~ Population)) + title("Lambda for Death Count & Population")
boxcox(lm(GDP ~ Population)) + title("Lambda for GDP & Population")
boxcox(lm(GNI ~ Population)) + title("Lambda for GNI & Population")

boxcox(lm(Population ~ float_UnalivedCount)) + title("Lambda for Death Count & Population")
boxcox(lm(Population ~ GDP)) + title("Lambda for GDP & Population")
boxcox(lm(Population ~ GNI)) + title("Lambda for GNI & Population")

b <- boxcox(lm(GNI ~ Population))
# Exact lambda
#Note: DO NOT CHANGE FORMULA BELOW. Replacing y and x with real object names BREAKS this.
exact_lambda <- b$x[which.max(b$y)]
print(exact_lambda)

b2 <- boxcox(lm(GDP ~ Population))
# Exact lambda
#Note: DO NOT CHANGE FORMULA BELOW. Replacing y and x with real object names BREAKS this.
exact_lambda <- b2$x[which.max(b2$y)]
print(exact_lambda)

b3 <- boxcox(lm(float_UnalivedCount ~ Population))
# Exact lambda
#Note: DO NOT CHANGE FORMULA BELOW. Replacing y and x with real object names BREAKS this.
exact_lambda <- b3$x[which.max(b3$y)]
print(exact_lambda)
#######

#Step-wise Regressions: Unalived Counts
#######
#Log Transforms

#GDP with Years
stepwise_1 <- lm(log_floatUnaliveCount ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + Factored_Year + log_population + log_GDP + Inflation + Employment)
stepwise_1 <- step(stepwise_1, direction = "both")
summary(stepwise_1)

#GDP without Years
stepwise_2 <- lm(log_floatUnaliveCount ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + log_population + log_GDP + Inflation + Employment)
stepwise_2 <- step(stepwise_2, direction = "both")
summary(stepwise_2)

#GNI with Years
stepwise_3 <- lm(log_floatUnaliveCount ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + Factored_Year + log_population + log_GNI + Inflation + Employment)
stepwise_3 <- step(stepwise_3, direction = "both")
summary(stepwise_3)

#GNI without Years
stepwise_4 <- lm(log_floatUnaliveCount ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + log_population + log_GNI + Inflation + Employment)
stepwise_4 <- step(stepwise_4, direction = "both")
summary(stepwise_4)


#GDP/Capita with Years
stepwise_5 <- lm(log_floatUnaliveCount ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + Factored_Year + log_GDPCapita + Inflation + Employment)
stepwise_5 <- step(stepwise_5, direction = "both")
summary(stepwise_5)

#GDP/Capita without Years
stepwise_6 <- lm(log_floatUnaliveCount ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + log_GDPCapita + Inflation + Employment)
stepwise_6 <- step(stepwise_6, direction = "both")
summary(stepwise_6)

#GNI/Capita with Years
stepwise_7 <- lm(log_floatUnaliveCount ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + Factored_Year + log_GNICapita + Inflation + Employment)
stepwise_7 <- step(stepwise_7, direction = "both")
summary(stepwise_7)

#GNI/Capita without Years
stepwise_8 <- lm(log_floatUnaliveCount ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + log_GNICapita + Inflation + Employment)
stepwise_8 <- step(stepwise_8, direction = "both")
summary(stepwise_8)

#IHS Transforms

#GDP with Years
stepwise_9 <- lm(ihs_UnalivedCount ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + Factored_Year + log_population + log_GDP + Inflation + Employment)
stepwise_9 <- step(stepwise_9, direction = "both")
summary(stepwise_9)

#GDP without Years
#This appears to be one of the two best models
stepwise_10 <- lm(ihs_UnalivedCount ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + log_population + log_GDP + Inflation + Employment)
stepwise_10 <- step(stepwise_10, direction = "both")
summary(stepwise_10)

#GNI with Years
stepwise_11 <- lm(ihs_UnalivedCount ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + Factored_Year + log_population + log_GNI + Inflation + Employment)
stepwise_11 <- step(stepwise_11, direction = "both")
summary(stepwise_11)

#GNI without Years
#This appears to be the other of the two best models
stepwise_12 <- lm(ihs_UnalivedCount ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + log_population + log_GNI + Inflation + Employment)
stepwise_12 <- step(stepwise_12, direction = "both")
summary(stepwise_12)


#GDP/Capita with Years
stepwise_13 <- lm(ihs_UnalivedCount ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + Factored_Year + log_GDPCapita + Inflation + Employment)
stepwise_13 <- step(stepwise_13, direction = "both")
summary(stepwise_13)

#GDP/Capita without Years
stepwise_14 <- lm(ihs_UnalivedCount ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + log_GDPCapita + Inflation + Employment)
stepwise_14 <- step(stepwise_14, direction = "both")
summary(stepwise_14)

#GNI/Capita with Years
stepwise_15 <- lm(ihs_UnalivedCount ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + Factored_Year + log_GNICapita + Inflation + Employment)
stepwise_15 <- step(stepwise_15, direction = "both")
summary(stepwise_15)

#GNI/Capita without Years
stepwise_16 <- lm(ihs_UnalivedCount ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + log_GNICapita + Inflation + Employment)
stepwise_16 <- step(stepwise_16, direction = "both")
summary(stepwise_8)

#######

#Step-wise Regressions: Unalived Percentages
#######
#Log Transforms

#GDP with Years
stepwise_17 <- lm(log_floatUnalivePercent ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + Factored_Year + log_population + log_GDP + Inflation + Employment)
stepwise_17 <- step(stepwise_17, direction = "both")
summary(stepwise_17)

#GDP without Years
stepwise_18 <- lm(log_floatUnalivePercent ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + log_population + log_GDP + Inflation + Employment)
stepwise_18 <- step(stepwise_18, direction = "both")
summary(stepwise_18)

#GNI with Years
stepwise_19 <- lm(log_floatUnalivePercent ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + Factored_Year + log_population + log_GNI + Inflation + Employment)
stepwise_19 <- step(stepwise_19, direction = "both")
summary(stepwise_19)

#GNI without Years
stepwise_20 <- lm(log_floatUnalivePercent ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + log_population + log_GNI + Inflation + Employment)
stepwise_20 <- step(stepwise_20, direction = "both")
summary(stepwise_20)


#GDP/Capita with Years
stepwise_21 <- lm(log_floatUnalivePercent ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + Factored_Year + log_GDPCapita + Inflation + Employment)
stepwise_21 <- step(stepwise_21, direction = "both")
summary(stepwise_21)

#GDP/Capita without Years
stepwise_22 <- lm(log_floatUnalivePercent ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + log_GDPCapita + Inflation + Employment)
stepwise_22 <- step(stepwise_22, direction = "both")
summary(stepwise_22)

#GNI/Capita with Years
stepwise_23 <- lm(log_floatUnalivePercent ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + Factored_Year + log_GNICapita + Inflation + Employment)
stepwise_23 <- step(stepwise_23, direction = "both")
summary(stepwise_23)

#GNI/Capita without Years
stepwise_24 <- lm(log_floatUnalivePercent ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + log_GNICapita + Inflation + Employment)
stepwise_24 <- step(stepwise_24, direction = "both")
summary(stepwise_24)

#IHS Transforms

#GDP with Years
stepwise_25 <- lm(ihs_UnalivedPercent ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + Factored_Year + log_population + log_GDP + Inflation + Employment)
stepwise_25 <- step(stepwise_25, direction = "both")
summary(stepwise_25)

#GDP without Years
stepwise_26 <- lm(ihs_UnalivedPercent ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + log_population + log_GDP + Inflation + Employment)
stepwise_26 <- step(stepwise_26, direction = "both")
summary(stepwise_26)

#GNI with Years
stepwise_27 <- lm(ihs_UnalivedPercent ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + Factored_Year + log_population + log_GNI + Inflation + Employment)
stepwise_27 <- step(stepwise_27, direction = "both")
summary(stepwise_27)

#GNI without Years
stepwise_28 <- lm(ihs_UnalivedPercent ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + log_population + log_GNI + Inflation + Employment)
stepwise_28 <- step(stepwise_28, direction = "both")
summary(stepwise_28)


#GDP/Capita with Years
stepwise_29 <- lm(ihs_UnalivedPercent ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + Factored_Year + log_GDPCapita + Inflation + Employment)
stepwise_29 <- step(stepwise_29, direction = "both")
summary(stepwise_29)

#GDP/Capita without Years
stepwise_30 <- lm(ihs_UnalivedPercent ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + log_GDPCapita + Inflation + Employment)
stepwise_30 <- step(stepwise_30, direction = "both")
summary(stepwise_30)

#GNI/Capita with Years
stepwise_31 <- lm(ihs_UnalivedPercent ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + Factored_Year + log_GNICapita + Inflation + Employment)
stepwise_31 <- step(stepwise_31, direction = "both")
summary(stepwise_31)

#GNI/Capita without Years
stepwise_32 <- lm(ihs_UnalivedPercent ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + log_GNICapita + Inflation + Employment)
stepwise_32 <- step(stepwise_32, direction = "both")
summary(stepwise_32)
#######

#Step-wise Regressions: Unalived 100LK
#######
#Log Transforms

#GDP with Years
stepwise_33 <- lm(log_floatUnalive100K ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + Factored_Year + log_population + log_GDP + Inflation + Employment)
stepwise_33 <- step(stepwise_33, direction = "both")
summary(stepwise_33)

#GDP without Years
stepwise_34 <- lm(log_floatUnalive100K ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + log_population + log_GDP + Inflation + Employment)
stepwise_34 <- step(stepwise_34, direction = "both")
summary(stepwise_34)

#GNI with Years
stepwise_35 <- lm(log_floatUnalive100K ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + Factored_Year + log_population + log_GNI + Inflation + Employment)
stepwise_35 <- step(stepwise_35, direction = "both")
summary(stepwise_35)

#GNI without Years
stepwise_36 <- lm(log_floatUnalive100K ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + log_population + log_GNI + Inflation + Employment)
stepwise_36 <- step(stepwise_36, direction = "both")
summary(stepwise_36)


#GDP/Capita with Years
stepwise_37 <- lm(log_floatUnalive100K ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + Factored_Year + log_GDPCapita + Inflation + Employment)
stepwise_37 <- step(stepwise_37, direction = "both")
summary(stepwise_37)

#GDP/Capita without Years
stepwise_38 <- lm(log_floatUnalive100K ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + log_GDPCapita + Inflation + Employment)
stepwise_38 <- step(stepwise_38, direction = "both")
summary(stepwise_38)

#GNI/Capita with Years
stepwise_39 <- lm(log_floatUnalived100K ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + Factored_Year + log_GNICapita + Inflation + Employment)
stepwise_39 <- step(stepwise_39, direction = "both")
summary(stepwise_39)

#GNI/Capita without Years
stepwise_40 <- lm(log_floatUnalived100K ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + log_GNICapita + Inflation + Employment)
stepwise_40 <- step(stepwise_40, direction = "both")
summary(stepwise_40)

#IHS Transforms

#GDP with Years
stepwise_41 <- lm(ihs_Unalived100K ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + Factored_Year + log_population + log_GDP + Inflation + Employment)
stepwise_41 <- step(stepwise_41, direction = "both")
summary(stepwise_41)

#GDP without Years
stepwise_42 <- lm(ihs_Unalived100K ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + log_population + log_GDP + Inflation + Employment)
stepwise_42 <- step(stepwise_42, direction = "both")
summary(stepwise_42)

#GNI with Years
stepwise_43 <- lm(ihs_Unalived100K ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + Factored_Year + log_population + log_GNI + Inflation + Employment)
stepwise_43 <- step(stepwise_43, direction = "both")
summary(stepwise_43)

#GNI without Years
stepwise_44 <- lm(ihs_Unalived100K ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + log_population + log_GNI + Inflation + Employment)
stepwise_44 <- step(stepwise_44, direction = "both")
summary(stepwise_44)


#GDP/Capita with Years
stepwise_45 <- lm(ihs_Unalive100K ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + Factored_Year + log_GDPCapita + Inflation + Employment)
stepwise_45 <- step(stepwise_45, direction = "both")
summary(stepwise_45)

#GDP/Capita without Years
stepwise_46 <- lm(ihs_Unalive100K ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + log_GDPCapita + Inflation + Employment)
stepwise_46 <- step(stepwise_46, direction = "both")
summary(stepwise_46)

#GNI/Capita with Years
stepwise_47 <- lm(ihs_Unalive100K ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + Factored_Year + log_GNICapita + Inflation + Employment)
stepwise_47 <- step(stepwise_47, direction = "both")
summary(stepwise_47)

#GNI/Capita without Years
stepwise_48 <- lm(ihs_Unalive100K ~ Factored_AgeGroup + Factored_Gender + Factored_RegionName + log_GNICapita + Inflation + Employment)
stepwise_48 <- step(stepwise_48, direction = "both")
summary(stepwise_48)

#######

#All ANOVAS
#Rudimentary One-Way ANOVA plots for visual comparisons: Unalived_Count
#######
#Gender
ggplot(unalived, aes(Gender, Unalived_Count)) + geom_boxplot(fill="thistle", color="thistle4") +theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
ggplot(unalived, aes(Gender, log_floatUnaliveCount)) + geom_boxplot(fill="thistle", color="thistle4") +theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
ggplot(unalived, aes(Gender, ihs_UnalivedCount)) + geom_boxplot(fill="thistle", color="thistle4") +theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

#AgeGroup
ggplot(unalived, aes(AgeGroup, Unalived_Count)) + geom_boxplot(fill="thistle", color="thistle4") +theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
ggplot(unalived, aes(AgeGroup, log_floatUnaliveCount)) + geom_boxplot(fill="thistle", color="thistle4") +theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
ggplot(unalived, aes(AgeGroup, ihs_UnalivedCount)) + geom_boxplot(fill="thistle", color="thistle4") +theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())


#RegionName
ggplot(unalived, aes(RegionName, Unalived_Count)) + geom_boxplot(fill="thistle", color="thistle4")
ggplot(unalived, aes(RegionName, log_floatUnaliveCount)) + geom_boxplot(fill="thistle", color="thistle4")
ggplot(unalived, aes(RegionName, ihs_UnalivedCount)) + geom_boxplot(fill="thistle", color="thistle4")

#######

#Actual ANOVA Models

#One Way ANOVAs
######
#Untransformed
#Age
age_deathcount <- lm(aov(Unalived_Count ~ Factored_AgeGroup))
summary(age_deathcount)
TukeyHSD(age_deathcount)

#Gender
gender_deathcount <- aov(Unalived_Count ~ Gender)
summary(gender_deathcount)
TukeyHSD(gender_deathcount)

#Region
region_deathcount <- aov(Unalived_Count ~ RegionName)
summary(region_deathcount)
TukeyHSD(region_deathcount)

#Log Transforms
age_deathcount2 <- aov(log_floatUnaliveCount ~ Factored_AgeGroup)
summary(age_deathcount2)
TukeyHSD(age_deathcount2)

#Gender
gender_deathcount2 <- aov(log_floatUnaliveCount ~ Gender)
summary(gender_deathcount2)
TukeyHSD(gender_deathcount2)

#Region
region_deathcount2 <- aov(log_floatUnaliveCount ~ RegionName)
summary(region_deathcount2)
TukeyHSD(region_deathcount2)

#IHS Transforms
age_deathcount3 <- aov(ihs_UnalivedCount ~ Factored_AgeGroup)
age_deathcount3_2 <- lm(aov(ihs_UnalivedCount ~ Factored_AgeGroup))
summary(age_deathcount3)
summary(age_deathcount3_2)
TukeyHSD(age_deathcount3)

#Gender
gender_deathcount3 <- aov(ihs_UnalivedCount ~ Gender)
summary(gender_deathcount3)
TukeyHSD(gender_deathcount3)

#Region
region_deathcount3 <- aov(ihs_UnalivedCount ~ RegionName)
summary(region_deathcount3)
TukeyHSD(region_deathcount3)
#####

#Two-Way Models
#####
#Age and Gender 
agegender_deathcount <- aov(Unalived_Count ~ Factored_AgeGroup * Gender)
summary(agegender_deathcount)
TukeyHSD(agegender_deathcount)

#Gender and Region
genderregion_deathcount <- aov(Unalived_Count ~ Gender *Factored_RegionName)
summary(genderregion_deathcount)
TukeyHSD(genderregion_deathcount)

#Region and Age
regionage_deathcount <- aov(Unalived_Count ~ Factored_RegionName * Factored_AgeGroup)
summary(regionage_deathcount)
TukeyHSD(regionage_deathcount)

#Log Transforms
#Age and Gender
logagegender_deathcount <- aov(log_floatUnaliveCount ~ Factored_AgeGroup * Gender)
summary(logagegender_deathcount)
TukeyHSD(logagegender_deathcount)

#Gender and Region
loggenderregion_deathcount <- aov(log_floatUnaliveCount ~ Gender *Factored_RegionName)
summary(loggenderregion_deathcount)
TukeyHSD(loggenderregion_deathcount)

#Region and Age
logregionage_deathcount <- aov(log_floatUnaliveCount ~ Factored_RegionName * Factored_AgeGroup)
summary(logregionage_deathcount)
TukeyHSD(logregionage_deathcount)


#IHS Transforms

#Age and Gender
ihsagegender_deathcount <- aov(ihs_UnalivedCount ~ Factored_AgeGroup * Gender)
summary(ihsagegender_deathcount)
TukeyHSD(ihsagegender_deathcount)

#Gender and Region
ihsgenderregion_deathcount <- aov(ihs_UnalivedCount ~ Gender *Factored_RegionName)
summary(ihsgenderregion_deathcount)
TukeyHSD(ihsgenderregion_deathcount)

#Region and Age
ihsregionage_deathcount <- aov(ihs_UnalivedCount ~ Factored_RegionName * Factored_AgeGroup)
summary(ihsregionage_deathcount)
TukeyHSD(ihsregionage_deathcount)
#####


#Three-Way Model:
#####
#Untransformed
RegionAgeGend_deathcount <- aov(Unalived_Count ~ Factored_RegionName * Factored_Gender * Factored_AgeGroup)
summary(RegionAgeGend_deathcount)
TukeyHSD(RegionAgeGend_deathcount)

#Log
Log_RegionAgeGend_deathcount <- aov(log_floatUnaliveCount ~ Factored_RegionName * Factored_Gender * Factored_AgeGroup)
summary(Log_RegionAgeGend_deathcount)
TukeyHSD(Log_RegionAgeGend_deathcount)

#IHS
IHS_RegionAgeGend_deathcount <- aov(ihs_UnalivedCount ~ Factored_RegionName * Factored_Gender * Factored_AgeGroup)
summary(IHS_RegionAgeGend_deathcount)
TukeyHSD(IHS_RegionAgeGend_deathcount)

######


#Time Series Attempts
######

#United States
#Male 25-34
US_Deaths <- unalived[unalived$CountryName %in% c("United States of America"),]
US_Deaths_Male <- US_Deaths[US_Deaths$Sex %in% c("Male"),]
US_Deaths_Male_2534 <- US_Deaths_Male[US_Deaths_Male$AgeGroup %in% c("25-34 years"),]

USM2534Deaths <- US_Deaths_Male_2534$SuicideCount
IHS_USM2534Deaths <- ihs(USM2534Deaths)

UStimeM2534 <- ts(IHS_USM2534Deaths)
plot1 <- plot.ts(UStimeM2534) + title("Time Series of US Male 25-34 IHS SuicideCounts")


#Female 25-34
US_Deaths <- unalived[unalived$CountryName %in% c("United States of America"),]
US_Deaths_Female <- US_Deaths[US_Deaths$Sex %in% c("Female"),]
US_Deaths_Female_2534 <- US_Deaths_Female[US_Deaths_Female$AgeGroup %in% c("25-34 years"),]

USF2534Deaths <- US_Deaths_Female_2534$SuicideCount
IHS_USF2534Deaths <- ihs(USF2534Deaths)

UStimeF2534 <- ts(IHS_USF2534Deaths)
plot2<- plot.ts(UStimeF2534) + title("Time Series of US Female 25-34 IHS SuicideCounts")

#Male 15-24
US_Deaths <- unalived[unalived$CountryName %in% c("United States of America"),]
US_Deaths_Male <- US_Deaths[US_Deaths$Sex %in% c("Male"),]
US_Deaths_Male_1524 <- US_Deaths_Male[US_Deaths_Male$AgeGroup %in% c("15-24 years"),]

USM1524Deaths <- US_Deaths_Male_1524$SuicideCount
IHS_USM1524Deaths <- ihs(USM1524Deaths)

UStimeM1524 <- ts(IHS_USM1524Deaths)
plot3 <- plot.ts(UStimeM1524) + title("Time Series of US Male 15-24 IHS SuicideCounts")


#Female 15-24
US_Deaths <- unalived[unalived$CountryName %in% c("United States of America"),]
US_Deaths_Female <- US_Deaths[US_Deaths$Sex %in% c("Female"),]
US_Deaths_Female_1524 <- US_Deaths_Female[US_Deaths_Female$AgeGroup %in% c("15-24 years"),]

USF1524Deaths <- US_Deaths_Female_1524$SuicideCount
IHS_USF1524Deaths <- ihs(USF1524Deaths)

UStimeF1524 <- ts(IHS_USF1524Deaths)
plot4 <- plot.ts(UStimeF1524) + title("Time Series of US Female 15-24 IHS SuicideCounts")

#"Unknown" 25-34
US_Deaths <- unalived[unalived$CountryName %in% c("United States of America"),]
US_Deaths_Unknown <- US_Deaths[US_Deaths$Sex %in% c("Unknown"),]
US_Deaths_Unknown_2534 <- US_Deaths_Unknown[US_Deaths_Unknown$AgeGroup %in% c("25-34 years"),]

USU2534Deaths <- US_Deaths_Unknown_2534$SuicideCount
IHS_USU2534Deaths <- ihs(USU2534Deaths)

UStimeU2534 <- ts(IHS_USU2534Deaths)
plot5<- plot.ts(UStimeU2534) + title("Time Series of US Unknown 25-34 IHS SuicideCounts")


#Russia
Russian_Deaths <- unalived[unalived$CountryName %in% c("Russian Federation"),]
Russian_Deaths_Male <- Russian_Deaths[Russian_Deaths$Sex %in% c("Male"),]
Russian_Deaths_Male_2534 <- Russian_Deaths_Male[Russian_Deaths_Male$AgeGroup %in% c("25-34 years"),]


RussianM2534Deaths <- Russian_Deaths_Male_2534$SuicideCount
IHS_RussianM2534Deaths <- ihs(RussianM2534Deaths)

Russiantime <- ts(IHS_RussianM2534Deaths)
plot.ts(Russiantime) + title("Time Series of Russian Male 25-34 IHS SuicideCounts")



#UK+Ireland
UK_Deaths <- unalived[unalived$CountryName %in% c("United Kingdom of Great Britain and Northern Ireland"),]
UK_Deaths_Female <- UK_Deaths[UK_Deaths$Sex %in% c("Female"),]
UK_Deaths_Female_3554 <- UK_Deaths_Female[UK_Deaths_Female$AgeGroup %in% c("35-54 years"),]


UKF3554Deaths <- UK_Deaths_Female_3554$SuicideCount
IHS_UKF3554Deaths <- ihs(UKF3554Deaths)

UKtime <- ts(IHS_UKF3554Deaths)
plot.ts(UKtime) + title("Time Series of UK Female 35-54 IHS SuicideCounts")


#Mexico
Mexico_Deaths <- unalived[unalived$CountryName %in% c("Mexico"),]
Mexico_Deaths_Female <- Mexico_Deaths[Mexico_Deaths$Sex %in% c("Female"),]
Mexico_Deaths_Female_1524 <- Mexico_Deaths_Female[Mexico_Deaths_Female$AgeGroup %in% c("15-24 years"),]


MexicoF1524Deaths <- Mexico_Deaths_Female_1524$SuicideCount
IHS_MexicoF1524Deaths <- ihs(MexicoF1524Deaths)

Mexicotime <- ts(IHS_MexicoF1524Deaths)
plot.ts(Mexicotime) + title("Time Series of Mexico Female 15-24 IHS SuicideCounts")




#Scenario 2: Zero deaths assumed to be underreports or nonreports
library(dplyr)
unalived2 <- filter(unalived, SuicideCount != 0)

#Variables Declared from unalived2
#########
RegionCode2 <- unalived2$RegionCode
Factored_RegionCode2 <- factor(RegionCode2)
RegionName2 <- unalived2$RegionName
Factored_RegionName2 <- factor(RegionName2)
CountryName2 <- unalived2$CountryName
Factored_CountryName2 <- factor(CountryName2)
Year2 <- unalived2$Year
Factored_Year2 <- factor(Year2)
Gender2 <- unalived2$Sex
Factored_Gender2 <- unalived2$Sex
AgeGroup2 <- unalived2$AgeGroup
Factored_AgeGroup2 <- unalived2$AgeGroup
Unalived_Count2 <- unalived2$SuicideCount
Unalived_Percentage2 <- unalived2$CauseSpecificDeathPercentage
Unalived_100K2 <- unalived2$DeathRatePer100K
Population2 <- unalived2$Population
GDP2 <- unalived2$GDP
GDP_Capita2 <- unalived2$GDPPerCapita
GNI2 <- unalived2$GrossNationalIncome
GNI_Capita2 <- unalived2$GNIPerCapita
Inflation2 <- unalived2$InflationRate
Employment2 <- unalived2$EmploymentPopulationRatio

#Transformed Variables (Note: IHS not necessary in this version.)
log_UnaliveCount2 <- log(Unalived_Count2)
log_UnalivePercent2 <- log(Unalived_Percentage2)
log_Unalive100K2 <- log(Unalived_100K2)
log_population2 <- log(Population2)
log_GDP2 <- log(GDP2)
log_GDPCapita2 <- log(GDP_Capita2)
log_GNI2 <- log(GNI2)
log_GNICapita2 <- log(GNI2)


#Summary Statistics
#######
fivenum(Unalived_Count2)
boxplot(fivenum(Unalived_Count2))
fivenum(Unalived_Percentage2)
boxplot(fivenum(Unalived_Percentage2))
fivenum(Unalived_100K2)
boxplot(fivenum(Unalived_100K2))
fivenum(log_UnaliveCount)
boxplot(fivenum(log_UnaliveCount))
#######


#PCA and Corr Matrix for Scenario 2
#######
#Different Data Frame and Correlation Matrix
testDF3 <- data.frame(Unalived_Count2,
                      Unalived_Percentage2,
                      Unalived_100K2,
                      Population2,
                      GDP2,
                      GDP_Capita2,
                      GNI2,
                      GNI_Capita2,
                      Inflation2,
                      Employment2)

data_normalized <- scale(testDF3)
corr_matri3 <- cor(testDF3)
print(corr_matri3)
ggcorrplot(corr_matri3) 

#PCA Stuff
data.pc3 <- princomp(corr_matri3)
summary(data.pca3)

data.pca3$loadings[, 1:2]
fviz_eig(data.pca3, barfill = "thistle2", barcolor = "thistle", addlabels = TRUE)

#Note: cos2 entails qualities of representation
fviz_pca_var(data.pca3,  col.var = "cos2",
             gradient.cols = c("midnightblue", "aquamarine", "coral"),
             repel = TRUE)


#Corr Matrix with Transformed Variables
testDF4 <- data.frame(log_UnaliveCount2,
                      log_UnalivePercent2,
                      log_Unalive100K2,
                      log_population2,
                      log_GDP2,
                      log_GDPCapita2,
                      log_GNI2,
                      log_GNICapita2,
                      Inflation2,
                      Employment2)

data_normalized4 <- scale(testDF4)
corr_matrix4 <- cor(testDF4)
ggcorrplot(corr_matrix4)
#######

#Regression Model for Scenario2
#Based on better models from S1
stepwise_s2 <- lm(log_UnaliveCount2 ~ Factored_AgeGroup2 + Factored_Gender2 + Factored_RegionName2 + log_GDP2 + log_population2 + Inflation2 + Employment2)
stepwise_s2 <- step(stepwise_s2, direction = "both")
summary(stepwise_s2)
