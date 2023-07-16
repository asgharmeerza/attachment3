#Housekeeping
rm(list=ls())
library(tidyverse)
library(haven)
library(dplyr)
library(ggplot2)

#Importing HSE 2018 Data

Data <- read_dta("C:/Users/cmp22aam/Desktop/Attachment 3/UKDA-8649-stata/stata/stata13/hse_2018_eul_15082022.dta")
View(Data)

#1. Defining the population

#Do you have asthma?
#Labels:
# value            label
#    -9          Refused
#    -8       Don't know
#    -1   Not applicable
#     1   Current asthma
#     2      Past asthma
#     3 Never had asthma

table(Data$asthma)

#Recode to Asthmatic and Non-asthmatic
Data$asthmatic <- NA #removing missing data
Data$asthmatic[Data$asthma == 1] <- "Asthma"
Data$asthmatic[Data$asthma > 1]  <- "No Asthma"
table(Data$asthmatic)

#Recode Age
Data$age <- NA #removing missing data
Data$age[Data$Ag015g4 == 1] <- "2-4"
Data$age[Data$Ag015g4 == 2]  <- "5-10"
Data$age[Data$Ag015g4 == 3]  <- "11-15"
table(Data$age)
table(Data$asthmatic, Data$age)

#Defining the Exposure - Second hand smoke (passive smoking)
table(Data$adultsmoke)
Data$exposed <- NA #removing missing data
Data$exposed[Data$adultsmoke == 0] <- "No Passive Smoking in Home"
Data$exposed[Data$adultsmoke == 1]  <- "Passive Smoking in Home"
table(Data$exposed)

#Control/Uncontrolled
Data$control <- NA #removing missing data
Data$control[Data$control2 == 0] <- "Controlled"
Data$control[Data$control2 == 1]  <- "Uncontrolled"
table(Data$control)

#Sex
Data$sex <- NA #removing missing data (100)
Data$sex[Data$Sex == 1] <- "Male"
Data$sex[Data$Sex == 2]  <- "Female"
table(Data$sex)

#Wheeze
table(Data$TweWz)
Data$wheeze <- NA #removing missing data (100)
Data$wheeze[Data$TweWz == 1] <- "Last 12 months"
Data$wheeze[Data$TweWz == 2]  <- "Not in last 12 months"
table(Data$wheeze)

#QIMD
table(Data$qimd)
Data$deprivation <- NA #removing missing data (100)
Data$deprivation[Data$qimd == 1] <- "0.48 to 8.37"
Data$deprivation[Data$qimd == 2]  <- "8.37 to 13.92"
Data$deprivation[Data$qimd == 3]  <- " 13.92 to 21.43"
Data$deprivation[Data$qimd == 4]  <- " 21.43 to 33.88 "
Data$deprivation[Data$qimd == 5]  <- "33.88 to 92.60"
table(Data$deprivation)

df = data.frame(Data$asthmatic,
                Data$age,
                Data$control,
                Data$exposed,
                Data$sex,
                Data$wheeze,
                Data$deprivation)


population <- na.omit(df)
view(population)

# install.packages("gtsummary")
library(gtsummary)
tbl_summary(population)


#2. Graphs


#Asthma by Exposure
brks <- c(0, 0.25, 0.5, 0.75, 1)
ggplot(population, aes(x = Data.exposed, fill = Data.asthmatic)) +
  geom_bar(position = "fill") +
  geom_text( aes(label=signif((100 * ..count..) / tapply(..count.., ..x.., sum)[as.character(..x..)], digits=3)), stat="count", position=position_fill(vjust=0.5)) +
  ylab("Percentage") + xlab("Exposure")+ ggtitle("Percentage of Asthmatics by Exposure") +
  scale_y_continuous(breaks = brks, labels = scales::percent(brks)) +
  guides(fill=guide_legend(title = "Asthmatic")) +
  coord_flip() +
  labs(caption="Source : HSE 2018
       n=169")

### Chi squared test for Association ###
chi <- table(population$Data.asthmatic, population$Data.exposed)
chisq.test(chi,correct=FALSE)
#X-squared = 1.4905, df = 1, p-value = 0.2221


#Control by Exposure
brks <- c(0, 0.25, 0.5, 0.75, 1)
ggplot(population, aes(x = Data.exposed, fill = Data.control)) +
  geom_bar(position = "fill") +
  geom_text( aes(label=signif((100 * ..count..) / tapply(..count.., ..x.., sum)[as.character(..x..)], digits=3)), stat="count", position=position_fill(vjust=0.5)) +
  ylab("Percentage") + xlab("Exposure")+ ggtitle("Percentage of Asthma Control by Exposure") +
  scale_y_continuous(breaks = brks, labels = scales::percent(brks)) +
  guides(fill=guide_legend(title = "Asthma Control")) +
  coord_flip() +
  labs(caption="Source : HSE 2018
       n=169")

#Wheeze by Exposure
brks <- c(0, 0.25, 0.5, 0.75, 1)
ggplot(population, aes(x = Data.exposed, fill = Data.wheeze)) +
  geom_bar(position = "fill") +
  geom_text( aes(label=signif((100 * ..count..) / tapply(..count.., ..x.., sum)[as.character(..x..)], digits=3)), stat="count", position=position_fill(vjust=0.5)) +
  ylab("Percentage") + xlab("Exposure")+ ggtitle("Percentage of Exacerbations by Exposure") +
  scale_y_continuous(breaks = brks, labels = scales::percent(brks)) +
  guides(fill=guide_legend(title = "Exacerbations")) +
  coord_flip() +
  labs(caption="Source : HSE 2018
       n=169")



### Chi squared test for Association ###
chi2 <- table(population$Data.exposed, population$Data.wheeze)
chisq.test(chi2,correct=FALSE)
#X-squared = 1.9126, df = 1, p-value = 0.1667

#3. Calculate Risk

Risk_Exposed <- 2.55
Risk_Unexposed <- 1.00

population$RR <- ifelse(population$Data.exposed != "No Passive Smoking in Home", Risk_Exposed, 2.55)
population$RR <- ifelse(population$Data.exposed == "Passive Smoking in Home", Risk_Exposed, 1.00)
view(population)

table(population$Data.asthmatic,population$Data.exposed)
#               No Passive Smoking in Home Passive Smoking in Home
#Controlled                           75                      10
#Uncontrolled                         74                      10

#Total                                149                     20

Rate_Uncontrolled_Asthma_Pre <- 84/169
Rate_Uncontrolled_Asthma_Pre

Average_Risk_Pre <- (149*1 + 20*2.55)/169
Average_Risk_Pre


#4. Modelling the effect of an intervention

#Effect of a complex intervention
#combining personalised feedback on home air quality,
#behavioural support and nicotine replacement therapy
#for temporary abstinence over 3 Months

Attempt_Rate <- 0.436
Success_Rate <- 0.230

Intervention_Effect <- Attempt_Rate*Success_Rate
Intervention_Effect #0.10028
#There is a 10% reduction in exposure to SHS as a result of the intervention
#after 3 months


#Post-Intervention
#               No Passive Smoking in Home Passive Smoking in Home
#Total                                151                     18
151*1
18*2.55

Average_Risk_Post <- (151*1 + 18*2.55)/169
Average_Risk_Post

Proportional_effect <- Weighted_average_risk_population_PI/Weighted_average_risk_population
Proportional_effect
#of unctrolled asthma

Proportion_Effect <- Average_Risk_Post/Average_Risk_Pre
Proportion_Effect

Rate_Uncontrolled_Asthma_Post <- Rate_Uncontrolled_Asthma_Pre * Proportion_Effect
Rate_Uncontrolled_Asthma_Post

No_Uncontrontrolled_Post <- 169*Rate_Uncontrolled_Asthma_Post
No_Uncontrontrolled_Post

Reduction_Uncontrolled <- 84 - No_Uncontrontrolled_Post
Reduction_Uncontrolled

#5. Cost of Uncontrolled Asthma

Cost_Controlled <- 0
Cost_Uncontrolled <- 2912

population$Control_Cost <- ifelse(population$Data.control != "Controlled", Cost_Uncontrolled, 2912)
population$Control_Cost <- ifelse(population$Data.control == "Uncontrolled", Cost_Uncontrolled, 0)
view(population)


Expected_Uncontrolled_Cost <- Reduction_Uncontrolled * Cost_Uncontrolled
Expected_Uncontrolled_Cost

Intervention_Cost <- 328*20
Intervention_Cost

Net_Benefit <- Intervention_Cost - Expected_Uncontrolled_Cost
Net_Benefit

Net_Benefit_Per_Case <- Net_Benefit/Reduction_Uncontrolled
Net_Benefit_Per_Case
