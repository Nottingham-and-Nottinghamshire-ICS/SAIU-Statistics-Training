#============================================================================
# packages
if (!require("epitools")){ install.packages("epitools")}
if (!require("dplyr")){ install.packages("dplyr")}
if (!require("ggplot2")){ install.packages("ggplot2")}
if (!require("summarytools")){ install.packages("summarytools")}

if (!require("readr")){ install.packages("readr")}
if (!require("DescTools")){ install.packages("DescTools")} # Cochran-Armitage test for trend
if (!require("gmodels")){ install.packages("gmodels")}


library(readr)
library(ggplot2)
library(epitools)
library(summarytools)
library(DescTools)
library(gmodels)
library(stargazer)
library(readxl)


#==========================================================================
# CATEGORICAL DATA

# categorical outcome data analysis - binary
  
#=============================================================================
# Analysis Proportions
#=============================================================================

data <- read_csv("subset_dataset.csv")

#data <- read_csv("sample_data3.csv")

#============================================================================
# Suppose we want to test the hypothesis that gender is associated with
# diagnosis of CHD. Specifically, we are interested in determining whether 
# men are at a higher risk for being diagnosed with CHD than women (this should be our "test" hypothesis)


data <- data  %>% filter(sex == "Male"| sex == "Female")

data <- data %>% 
  mutate(chd2 = case_when( 
    chd ==  1 ~ "With CHD",
    chd  == 0 ~ "Without CHD"))
#=================================================================================
# PROPORTIONS
#=================================================================================
# visualize the  number and proportions with/without chd by exposure (males and females)

chd_prop <- data  %>%
  group_by(sex) %>%
  group_by(chd2, .add= "TRUE")%>%
  summarize(N = n()) %>%   
  mutate(freq = N / sum(N),
                   pct = round((freq*100), 2),
                  pct = paste0(pct,"%"))
chd_prop

# quick visualization
ggplot(chd_prop, aes(x= sex, y=pct, fill = sex))+
  geom_bar(stat = "identity",width = 0.5) + facet_wrap( ~ chd2)

# The mosaic plot is another graphical technique for showing the association between two categorical variables

# create a contingency table for CHD by gender using the table function (first parameter is row, 2nd parameter is column(outcome) variable)

table <- table(data$sex, data$chd)

association.table <- data.frame("No CHD"=c(25577,22778),CHD=c(1071,572), row.names=c("Female","Male"))


mosaicplot(association.table, color = c("darkred", "gold"), xlab ="Sex", ylab = "Proportion")



# calculate percentages
table2 <- round(prop.table((table), margin=1) *100,2) # for row marginals

table2
# prop.table((table), margin=2)  # for columns marginals

data$chd <- as.factor(data$chd)

data$sex <- as.factor(data$sex)

summarytools::ctable(data$sex, data$chd,prop = 'n', totals = TRUE)

# The prop test simply test whether the proportions are different
# it doesn't provide any information about the direction, i.e which one is higher or lower


# syntax below 
prop.test(x = c(no_male_chd, no_female_chd), n = c(Total_males, Total_females))


res <- prop.test(x = c(572, 1071), n = c(23350, 26648))
# Printing the results
res

# printing the p-value
res$p.value

# printing the mean
res$estimate

#printing the confidence interval
res$conf.int

# The test returns the following important statistics - pearson chi-square test statistics, p value, 95% CI

# if you want to test whether the observed proportion of males with CHD is less than the observed proportion of females with CHD, type this:

prop.test(x = c(572, 1071), n = c(23350, 26648),
          alternative = "less")

# if you want to test whether the observed proportion of males with CHD is greater than the observed proportion of females with CHD, type this:


prop.test(x = c(572, 1071), n = c(23350, 26648),
          alternative = "greater")


#================================================================================
# ODDS RATIO
#===============================================================================

# One of the ways to measure the strength of the association between two categorical variables is an odds ratio.

# To obtain crude odds ratio, either a 2x2 table can be used or a one predictor logistic regression model can be fitted
# or use the command fisher.test()



# methods 2: Using the contingency table to extract the elements individually

# specify outcome in 2nd part of the table parameter, in this case, chd
table <- table(data$sex, data$chd)

table 

#summarytools::ctable(data$sex, data$chd,prop = 'r',totals=TRUE)

a <- table[2, 1]
b <- table[2, 2]
c <- table[1,1]
d <- table[1,2]

# Calculate the odds ratio | Females to male
odds_ratio <- (b * c) / (a * d) 

# Print the odds ratio
print(odds_ratio)

# methods 2: Using a single predictor model

# Fit model
fit1 <- glm(chd ~ sex, data=data, family=binomial)
summary(fit1)

coef(fit1) %>% exp()

# There is a statistically significant difference in the odds ratio, p value  =  2.2e-16
# Males  have a 60% lower risk of being diagnosed with CHD compared to females

confint(fit1) %>% exp()

# method 3. Using fisher test

fisher_test = fisher.test(table)
fisher_test


# Chi-square test of independence in R
# The chi-square test evaluates whether there is a significant association between the categories of the two variables.

Chi_test <- chisq.test(table(data$sex, data$chd))
Chi_test

# Observed counts
Chi_test$observed


# Fisher exact test


fisher_test <- fisher.test(table(data$sex, data$chd))

fisher_test


# To perform a squared trend test (also known as a chi-squared test for trend or Cochran-Armitage test for trend

# Is there any significant linear trend between chd and levels on obesity?

table3 <- table(data$chd,data$latestbmi)

CochranArmitageTest(table3)










            
            



