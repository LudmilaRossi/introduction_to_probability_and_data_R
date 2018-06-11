#------------------------------------------------------------------------------------------------------------
# Description:   Course Introduction to Probability and Data at Duke University - Coursera.org
#                Final Project Week 5 / Behavioral Risk Factor Surveillance System
#
# Dataset:       brfss2013: https://d3c33hcgiwev3.cloudfront.net/_384b2d9eda4b29131fb681b243a7767d_brfss2013.RData?Expires=1464220800&Signature=QjjfkYt-C-wTRcwEvnuyFDvphasfoKEAkMo-088OscAJxIV7NbLUqOmkJZBXsVLUuJmJZmatWsi7bOoG9WMkp18BK4bGVvEpAjHyar9fOMmT9TcnFax5m2Dj8nJZzqox4IV20XOtnijNbzwQH4N8yd7CtbIc1tNTgeo8mq5ezjQ_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A
# Information:   Download Link: http://www.cdc.gov/brfss/annual_data/annual_2013.html    
#
# Author:        Ludmila Rossi
# Date:          11.06.2018
#
#------------------------------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------------------------
# Install / Load Libraries
#------------------------------------------------------------------------------------------------------------
install.packages('ggplot2')    
install.packages('plyr')  
install.packages('dplyr') 
install.packages('gridExtra')
install.packages('ggmap')
install.packages('knitr') 
install.packages('gtable') 
install.packages('grid')
install.packages('scales')

library('ggplot2')    # library to create plots
library('plyr')       # data manipulation
library('dplyr')      # ""
library('gridExtra')  # supports the layout functionality with ggplot
library('ggmap')      # visualize maps 
library('knitr')      # required to apply knitr options 
library('gtable')     # used to overlay plots in ggplot
library('grid')       # ""
library('scales')       # ""

# apply general knitr options
#opts_chunk$set(echo = TRUE,fig.align='center')
knitr::opts_chunk$set(comment=NA, fig.align='center')

#------------------------------------------------------------------------------------------------------------
# Download and extract Data and load file
#------------------------------------------------------------------------------------------------------------

# Load data and set global knitr options 
load(file = 'brfss2013.RData')

# Size of the dataset
dim(brfss2013)

# verify types and summary of each variable 
str(brfss2013)
summary(brfss2013)

# Retrieve only the required columns
Col <- c('X_state', 'X_bmi5', 'X_bmi5cat', 'genhlth', 'hlthpln1', 'exerany2', 'X_frtlt1', 'X_veglt1', 'X_pacat1', 'exract11', 'X_age80', 'weight2', 'height3', 'income2', 'educa')
datAnalysis <- brfss2013[, Col]
str(datAnalysis)

# Show histogram and boxplot to verify the distribution 
hist(datAnalysis$X_bmi5, breaks = 100, col = 'violet')
boxplot(datAnalysis$X_bmi5, main = "Boxplot")

# Calculate the mean 
mean(datAnalysis$X_bmi5, na.rm = TRUE)

# Calculate the median
median(datAnalysis$X_bmi5, na.rm = TRUE)

#------------------------------------------------------------------------------------------------------------
# Research questions
#------------------------------------------------------------------------------------------------------------

# 1) Is there a correlation between access to "general health" and physical exercise in the US adult population?
# 2) Is there an association between BMI and the quantity of total physical activity per week for males and females?
# 3) Is there a relation between physical activity, work status and income level in the US adult population?
# 4) Related to the previous research question, it could also discover if there is any correlation between income levels and the state of general health. 
#    In addition, it can be analyzed if there are notable differences between genres. 

#------------------------------------------------------------------------------------------------------------
# Create the data subsets for research question 1 by providing only the required columns
#------------------------------------------------------------------------------------------------------------

# Dataset for research question 1
Col <- c('X_state', 'genhlth', 'hlthpln1', 'X_bmi5cat', 'X_bmi5', 'exerany2')
data.rsq <- brfss2013[, Col]

#------------------------------------------------------------------------------------------------------------
# Data preprocessing
#------------------------------------------------------------------------------------------------------------

USPop2013 <- 315091138

# Remove rows with missing data
data.rsq <- data.rsq[complete.cases(data.rsq),]

# evaluate the percentage of the US population based on bmi Category
bmi.cat <- plyr::count(data.rsq, 'X_bmi5cat')
bmi.cat <- bmi.cat[-5,]
bmi.cat$percent <- prop.table(bmi.cat$freq)
bmi.cat$population <- lapply(bmi.cat$percent, function(x) {
  USPop2013 * x
})

#------------------------------------------------------------------------------------------------------------
# Plots design
#------------------------------------------------------------------------------------------------------------

ggplot(bmi.cat, aes(x = factor(X_bmi5cat), y = percent, fill = percent, alpha = 0.8)) +
  geom_bar(stat = 'identity') +
  xlab('BMI Category') + 
  ylab('Percentage of US population') +
  ggtitle('Distribution of US population in regards to \nBMI category') +
  geom_text(aes(label = round(percent * 100, 2)), vjust = 1.6, color = 'black', position = position_dodge(0.9), size = 3)

# Relation between health care and general condition 
mosaicplot( ~ genhlth + hlthpln1, data = data.rsq, xlab = 'General health', ylab = 'Health care plan', color = c('darkblue', 'purple'), 
            main = 'General health vs helth care plan')

# Relation between exercise and general condition 
mosaicplot( ~ genhlth + exerany2, data = data.rsq, xlab = 'General health', ylab = 'Physical excercise', color = c('darkblue', 'purple'), 
            main = 'General health vs physical excercise')

#------------------------------------------------------------------------------------------------------------
# Research question 2 - Association between BMI and quantity of total physical activity per week and per gender
#------------------------------------------------------------------------------------------------------------

brfss2013_pt_bmi <- brfss2013 %>% filter(pa1min_ != "NA") %>% filter(X_bmi5 != "NA") %>% mutate(bmi = X_bmi5 / 100)

ggplot(brfss2013_pt_bmi, aes(x = bmi, y = log(pa1min_ + 1), colour = sex)) + 
  geom_point(shape = 19, alpha = 1/4) + 
  geom_smooth(method = lm, se = FALSE) + 
  scale_colour_manual(name = 'Genre', breaks = c('Male', 'Female'), values = c('#07004D', '#FF81E4')) + 
  xlab('BMI') + 
  ylab('Minutes of physical training per week')

brfss2013_pt_bmi %>% group_by(sex) %>% summarise(corr_bmi_phys_act = cor(bmi, pa1min_))

#------------------------------------------------------------------------------------------------------------
# Research question 3 - Relation between physical activity, work status and income level
#------------------------------------------------------------------------------------------------------------

# genhlth: General Health
# income2: Income Level
# Remove all NA data.
physAct_rm.na <- filter(brfss2013, !is.na(exerany2), !is.na(income2), !is.na(employ1))

# Find porportion that does any exercise grouped by income.
physAct_income <- physAct_rm.na %>% group_by(income2) %>% summarise(prop_exer = sum(exerany2 == "Yes") / n())

# Print result
physAct_income

# In this Plot it replace spaces with line breaks in income2 names.
levels(physAct_income$income2) <- gsub(" ", "\n", levels(physAct_income$income2))
ggplot(physAct_income, aes(income2, prop_exer)) +
  geom_point(aes(income2, prop_exer)) +
  labs(title='Percentage of people that exercised in last 30 days vs. income', x='Income', y='Percentage who exercises')

# Find percentage of people who does any exercise grouped by employment status.
physAct_employ <- physAct_rm.na %>% group_by(employ1) %>% summarise(prop_exer = sum(exerany2 == "Yes") / n())

# Print.
physAct_employ

# Plot
levels(physAct_employ$employ1) <- gsub(" ", "\n", levels(physAct_employ$employ1))
ggplot(physAct_employ, aes(employ1, prop_exer)) +
  geom_point(aes(employ1, prop_exer)) +
  labs(title='Percentage of people that exercised in last 30 days vs. employment status',
       x='Employment status', y='Percentage who exercises')

#------------------------------------------------------------------------------------------------------------
# Research question 4 - Correlation between income and general health. Differences between genres.
#------------------------------------------------------------------------------------------------------------

# Query the relevant variables
inc_hea_gen <- select(brfss2013, genhlth ,income2) %>% 
  filter(!is.na(genhlth), !is.na(income2))

# Present totals of variables being analayzed
inc_hea_gen %>% group_by(genhlth) %>%    summarise(count=n())

inc_hea_gen %>% group_by(income2) %>%   summarise(count=n())

ggplot(data = inc_hea_gen, aes(x = genhlth , y = income2)) +
  geom_count(color = 'purple') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab('General Health') +
  ylab ('Income Level')
