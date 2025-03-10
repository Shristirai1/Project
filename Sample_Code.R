##CODING SAMPLE
##Shristi Rai

install.packages("tidyverse")
library(tidyverse)
##Part I: Health Saving Experiment

rosca120123 <- read.csv("rosca120123.csv")

## 1. 

##H0: There is no difference in the investment of health products between the locked box and safe box respondents 
##H1: There is a difference in the investment of health products between the locked box and safe box respondents

tapply(rosca120123$fol2_amtinvest, rosca120123$treatment, mean, na.rm = T)

boxplot(rosca120123$fol2_amtinvest ~ rosca120123$treatment, 
        main = "catregories of treatment", 
        ylab= "Investment Levels", 
        xlab= "Treatment", 
        ylim= c(0, 1750))
        
## Results:The boxplot shows that the mean investment level in the locked box group is lower than the mean investment level in the secure box group. 
##Furthermore, the locked box group shows a lower standard deviation, indicating that investment amounts are less variable within this group. 
##It may be concluded that the experiment indicated a statistically significant difference in health product investment between the two groups. 
##The safe box group, in particular, has a higher mean investment level.
##As a result, the experiment provides support to the alternative hypothesis (H1), 
##implying that getting a safe box rather than a locked box has a significant influence on spending in health items.

## 2  

##H0: There is no difference between investment among male and female in health products
##H1: There is a difference between investment among male and female in health products 

boxplot(rosca120123$fol2_amtinvest ~ rosca120123$bg_female, 
        main = "Investment by Gender",
        ylab = "Health Products",
        xlab = "Gender",
        ylim = c(0,2500))

tapply(rosca120123$fol2_amtinvest, rosca120123$bg_female, mean, na.rm = T)

## 0= Male, 1= Female

t.test(rosca120123$fol2_amtinvest[rosca120123$bg_female == 0],
       rosca120123$fol2_amtinvest[rosca120123$bg_female == 1])

##Results: There are gender differences in the distribution of investment to health products. 
##The t-test results show a statistically significant difference in average expenditure on health goods between female and male individuals. 
##The graph shows that the average investment among female participants exceeds that of their male counterparts. 
##On average, female participants are more likely than male participants to invest in health products.

## 3.

##H0: Recieving a safebox has an effect on female investment on health products copmared to receiving a lockbox.
##H1:Receiving a safebox does not have any effect on female investement compared to receiving a lockbox

library(dplyr)
library(ggplot2)

data <- read.csv("rosca120123.csv")

data <- data %>%
  filter(treatment != "control" & fol2_amtinvest != 0)

summary_stats <- data %>%
  filter(bg_female == 1) %>%
  group_by(treatment) %>%
  summarise(mean_investment = mean(fol2_amtinvest),
            sd_investment = sd(fol2_amtinvest),
            n = n())

anova_result <- aov(fol2_amtinvest ~ treatment + bg_female, data = data)
summary(anova_result)

female_investment_plot <- ggplot(summary_stats, aes(x = treatment, y = mean_investment, fill = treatment)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_investment - sd_investment, ymax = mean_investment + sd_investment),
                width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Comparison of Female Investment in Health Products",
       x = "Treatment",
       y = "Average Investment") +
  theme_minimal()

print(female_investment_plot)

##Results: There is a divergence in the effects of the treatments based on the observed impact of the treatments (safebox and locked box) and the variable indicating whether the subject is female. 
##The ANOVA results lead us to reject the null hypothesis, implying that there is no significant difference in female investment when the effects of the safebox and the locked box are compared. 
##The female investment plot, which shows that the average investment for both safebox and locked box treatments is substantially comparable, supports this result. 
##Receiving either a safebox or a locked box does not appear to have a substantial impact on female investment in health items.

## 4. 

library(lme4)

lmer_model <- lmer(fol2_amtinvest ~ treatment * bg_female + (1 | treatment), data = data)
summary(lmer_model)

##Results: the variable "treatment" receives more considerable support than "gender." 
##"treatment" variable outperforms "gender," indicating a statistically significant positive influence on the dependent variable
##the absence of statistical significance in the interaction effect shows that the effects of "treatment" and "gender" are constant across levels of the dependent variable

##Part II: Contact Theory Experiment 

##  1.

library(dplyr)
gay_marriage_data <- read.csv("gay.csv")

gay_marriage_data <- mutate(gay_marriage_data, ssm_cat = ifelse(ssm <= 2, 1,
                                                                ifelse(ssm == 3, 2, 3)))

category_counts <- table(gay_marriage_data$ssm_cat)

category_percentages <- round(100 * category_counts / sum(category_counts), 1)
barplot(category_percentages, main = "Distribution of support for Gay Marriage",
        xlab = "Support Category", ylab = "Percentage", 
        col = c("red", "blue", "green"), legend = rownames(category_percentages))

## 2. 

##Hypotheses

##H0= Same sex marriage script does not increase support for gay marriage
##H1= Same sex marriage script increases support for gay marriage

##H0= Gay Canvasser does not increase support for gay marriage
##H1= Gay canvasser increases support for gay marriage 

library(dplyr)
library(tidyverse)

data <- read_csv("gay.csv")

gay_marriage_data %>%
  group_by(treatment) %>%
  summarize(percentage_support = mean(ssm %in% c("4","5")) * 100)

## Results: The same-sex marriage script by a gay canvasser did not significantly increase support for gay marriage when compared to using the script by a ga canvasser. 
##Nonetheless, in comparison to the gay canvasser, the gay canvasser significantly increases support for gay marriage.

## 3

gay_marriage_data %>%
  mutate(diff_support = mean(ssm) - mean(ssm[study == "1 No Contact"])) %>%
  select(treatment, study, diff_support)

summary_stats <- gay_marriage_data %>%
  group_by(treatment, study) %>%
  summarize(avg_support = mean(ssm))

ggplot(summary_stats, aes(x = treatment, y = avg_support, fill = as.factor(study))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Support for Gay Marriage by Treatment and Study",
       x = "Treatment",
       y = "Average Support") +
  theme_minimal()

##Results:Including a gay canvasser in a recycling script or a same-sex marriage screenplay successfully increases support for gay marriage. 
##This effect is more noticeable in the recycling script than in the same-sex marriage script.
## The significance of the message surpasses that of the messenger. 
##This is evident in the greater difference in support for gay marriage with a gay canvasser in the same-sex marriage script compared to the control group. 


## 4

support_by_study <- gay_marriage_data %>%
  group_by(study) %>%
  summarize(avg_support = mean(ssm, na.rm = TRUE))

ggplot(support_by_study, aes(x = factor(study), y = avg_support, fill = factor(study))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Support for Gay Marriage",
       x = "Study",
       y = "Average Support") +
  theme_minimal()

aov(ssm ~ as.factor(wave), data = gay_marriage_data)

boxplot(ssm ~ as.factor(wave), data = gay_marriage_data, main = "Gay Marriage Support Over Time")

##Results: The ANOVA results show a significant shift in support for gay marriage across time periods (F(4, 475) = 31.37, p 0.001). 
##This suggests a constant and significant increase in support for gay marriage throughout the years. 
##The boxplot emphasizes this growing support, which is visible in the plot's rightward tendency over time.
