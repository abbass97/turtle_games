## LSE Data Analytics Online Career Accelerator 
# Turtle Games Data Analysis:  Advanced Analytics for Organisational Impact


################################################################################


## Exploratory data analysis will be performed, and findings will be presented 
## through statistical analysis and by plotting various graphs.
  

# Approach:

## 1. Load, clean and wrangle the data using R. 
## Use summary statistics and groupings if required to sense-check and gain 
## insights into the data. Make sure to use different visualisations such as 
## scatter plots, histograms, and box plots to learn more about the data set. 
## Explore the data and comment on the insights gained from 
## your exploratory data analysis. For example, outliers, missing values, 
## and distribution of data. Also make sure to comment on initial patterns and 
## distributions or behaviour that may be of interest to the business.
## Use the cleaned version of the data.  
## Drop ‘product’, 'review' and ‘summary’ columns. The dataset that was used had
## already had the names of the columns ‘remuneration’ and 
## ‘spending_score’ changed to salary and spending respectively. Import the 
## turtle_review.csv data file, import all the required libraries for the 
## analysis and view the data. View the head the data. Create a summary of the 
## new data frame.


## 2. Present findings and recommendations to the technical and business users.

###############################################################################


# 1. Load and Explore the data

# Prepare workstation and install important packages and libraries such as 
## the tidyverse, skimr, psych and moments packages to read, explore, 
## analyse, manipulate, wrangle, visualise and explore data.


# Import the necessary libraries. Create insightful summaries of data set.
install.packages('tidyverse')
install.packages("skimr")
install.packages("psych")
install.packages("moments")
install.packages("DataExplorer")

# load the libraries
library(tidyverse)
library(skimr)
library(psych)
library(moments)
library(DataExplorer)
library(tibble)


# Set your working directory.
setwd(dir='D:/Course 3- Advanced Analytics for Organisational Impact/assignment_files_new/For Portfolio')

# Import the data set.
games <- read.csv('D:/Course 3- Advanced Analytics for Organisational Impact/assignment_files_new/For Portfolio/clean_reviews.csv', header=T)
games<- read.csv(file.choose(), header=T)

# Return the dimensions of your data frame.
dim(games)
# Print the data frame.
View(games)
View(games[1:50, ])
head(games)
as_tibble(games)

# Return the structure of the data frame.
str(games)


## The dataframe consists of 4 numerical variables and 5 categorical ones. There
## are 2000 rows and 9 columns.

###############################################################################


# 2. Data wrangling & exploring.

## Remove irrelevant columns - review and summary 
library(dplyr)
edited_games <- select(games,  -review, -summary)


## Sense-check the new data frame
dim(edited_games)
# The new dataframe has 6 columns and 2000 rows.
as_tibble(edited_games)

# Check for missing values
sum(is.na(edited_games))
# There are no missing values.

## Summarise the new data frame.
head(edited_games)

#Print the summary staistics of the data set.
summary(edited_games)
# The loyalty variable has values ranging between 25 and 6847 with a median of 
# 1276, suggestive of outliers. 

# Create a data profiling report in html.
DataExplorer::create_report(edited_games)

################################################################################


# 3. Perform EDA by creating tables and visualisations

##  Visualisation of the data by creating Boxplots and Histograms.

## Boxplots of Age, Salary, Spending, product and loyalty.
boxplot(edited_games$age, main = "Boxplot of Age")
boxplot(edited_games$salary, main = "Boxplot of Salary")
boxplot(edited_games$spending, main = "Boxplot of Spending")
boxplot(edited_games$loyalty, main = "Boxplot of Loyalty") # potential outliers
boxplot(edited_games$product, main = "Boxplot of Product")

# Create a histogram of 'Age' column
library(ggplot2)

ggplot(edited_games, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "white") +
  labs(title = "Histogram of Age", x = "Age", y = "Count")


# Create a histogram of 'Product' column
ggplot(edited_games, aes(x = product)) +
  geom_histogram(binwidth = 200, fill = "pink", color = "white") +
  labs(title = "Histogram of Product", x = "Product", y = "Count")

# Create a histogram of 'Spending' column
ggplot(games, aes(x = spending)) +
  geom_histogram(binwidth = 5, fill = "lightcoral", color = "white") +
  labs(title = "Histogram of Spending Score", x = "Spending Score", y = "Count")

# Create a histogram of 'loyalty' column
ggplot(edited_games, aes(x = loyalty)) +
  geom_histogram(binwidth = 200, fill = "yellow", color = "white") +
  labs(title = "Histogram of Loyalty", x = "Loyalty", y = "Count")


# Create histograms for all numerical columns.
plot_histogram(edited_games)

################################################################################


# Calculate the Summary Statistics of Loyalty Points for all customers.
# Summarise the mean, median, standard deviation, minimum and maximum values will
# be calculated  on the column loyalty.


summarise(
  edited_games,
  mean_loyalty = mean(loyalty, na.rm = TRUE),
  median_loyalty = median(loyalty, na.rm = TRUE),
  max_loyalty = max(loyalty, na.rm = TRUE),
  min_loyalty = min(loyalty, na.rm = TRUE),
  sd_loyalty = sd(loyalty, na.rm = TRUE)
)
# The summary of this statistics is,
#  mean_loyalty median_loyalty max_loyalty min_loyalty sd_loyalty
#<dbl>          <dbl>       <int>       <int>      <dbl>
#  1        1578.           1276        6847          25      1283.


# Loyalty points summary by gender.
edited_games %>%
  group_by(gender) %>%
  summarise(
    mean_loyalty = mean(loyalty, na.rm = TRUE),
    median_loyalty = median(loyalty, na.rm = TRUE),
    max_loyalty = max(loyalty, na.rm = TRUE),
    min_loyalty = min(loyalty, na.rm = TRUE),
    sd_loyalty = sd(loyalty, na.rm = TRUE),
    .groups = "drop"
  )

# A tibble: 2 × 6
# gender mean_loyalty median_loyalty max_loyalty min_loyalty sd_loyalty
#  <chr>         <dbl>          <dbl>       <int>       <int>      <dbl>
#  1 Female        1601.           1281        6847          30      1251.
#  2 Male          1549.           1248        6208          25      1323.

# Average Loyalty points by Gender
library(ggplot2)

edited_games %>%
  group_by(gender) %>%
  summarise(mean_loyalty = mean(loyalty, na.rm = TRUE)) %>%
  ggplot(aes(x = gender, y = mean_loyalty, fill = gender)) +
  geom_col() +
  labs(title = "Average Loyalty by Gender", x = "Gender", y = "Mean Loyalty") +
  theme_minimal()

# Average Loyalty points by Age
library(ggplot2)

edited_games %>%
  group_by(age) %>%
  summarise(mean_loyalty = mean(loyalty, na.rm = TRUE)) %>%
  ggplot(aes(x = age, y = mean_loyalty, fill = age)) +
  geom_col() +
  labs(title = "Average Loyalty by Age", x = "Age", y = "Mean Loyalty") +
  theme_minimal()

# Average Loyalty points by Education
library(ggplot2)

edited_games %>%
  group_by(education) %>%
  summarise(mean_loyalty = mean(loyalty, na.rm = TRUE)) %>%
  ggplot(aes(x = education, y = mean_loyalty, fill = education)) +
  geom_col() +
  labs(title = "Average Loyalty by Education", x = "Education", y = "Mean Loyalty") +
  theme_minimal()


# Average Loyalty points by Salary
library(ggplot2)

edited_games %>%
  group_by(salary) %>%
  summarise(mean_loyalty = mean(loyalty, na.rm = TRUE)) %>%
  ggplot(aes(x = salary, y = mean_loyalty, fill = salary)) +
  geom_col() +
  labs(title = "Average Loyalty by Salary", x = "Salary", y = "Mean Loyalty") +
  theme_minimal()

# Average Loyalty points by Spending
library(ggplot2)

edited_games %>%
  group_by(spending) %>%
  summarise(mean_loyalty = mean(loyalty, na.rm = TRUE)) %>%
  ggplot(aes(x = spending, y = mean_loyalty, fill = spending)) +
  geom_col() +
  labs(title = "Average Loyalty by Spending", x = "Spending", y = "Mean Loyalty") +
  theme_minimal()

# Average Loyalty points by Product
library(ggplot2)

edited_games %>%
  group_by(product) %>%
  summarise(mean_loyalty = mean(loyalty, na.rm = TRUE)) %>%
  ggplot(aes(x = product, y = mean_loyalty, fill = product)) +
  geom_col() +
  labs(title = "Average Loyalty by Product", x = "Product", y = "Mean Loyalty") +
  theme_minimal()
###############################################################################

# Using Aggregate() to compute the mean of loyalty points based on spending
# score, salary, gender, education and age.
# # Specify the function as aggregate(), the numeric variable (loyalty),
# the grouping variable (spending), the data source (game_sales), and the mean 
# as function. The additional grouping variable (salary, age, education and
# gender) will also be aggregated.


# Average loyalty points by (spending)
spend_loyalty <- aggregate(loyalty ~ spending, edited_games, mean)
# print out average loyalty points per spending score in decreasing order 
sorted_spend_loyalty <- spend_loyalty[order(-spend_loyalty$loyalty),]
sorted_spend_loyalty
# Count the total number of spending score values based on loyalty points. 
#count(sorted_spend_loyalty)
nrow(sorted_spend_loyalty)
# The output shows that the highest mean value for loyalty point is 4925.76190 
# with the spending score of 91. The lowest mean value for loyalty points is 
## 31.30769 with the spending score of 3. THe pattern folows as the hihger the 
## spending score the higher the loyalty point.

library(ggplot2)

ggplot(sorted_spend_loyalty, aes(x = reorder(as.factor(spending), -loyalty), y = loyalty)) +
  geom_col(fill = "steelblue") +
  labs(title = "Average Loyalty by Spending", x = "Spending", y = "Average Loyalty") +
  theme_minimal()


# Average loyalty points based on grouping variable (gender)
gender_loyalty <- aggregate(loyalty ~gender, edited_games, mean)

# print out average loyalty points per gender in decreasing order 
sorted_gender_loyalty <- gender_loyalty[order(-gender_loyalty$loyalty),]
sorted_gender_loyalty
# Count the total number of spending score values based on loyalty points. 
#count(sorted_spend_loyalty)
nrow(sorted_gender_loyalty)

# More loyalty points are earned by females than males.
## 1 Female 1601.167
## Male 1548.588

library(ggplot2)

ggplot(sorted_gender_loyalty, aes(x = reorder(as.factor(gender), -loyalty), y = loyalty)) +
  geom_col(fill = "steelblue") +
  labs(title = "Average Loyalty by Gender", x = "Gender", y = "Average Loyalty") +
  theme_minimal()


# Average loyalty points based on grouping variable (education)
education_loyalty <- aggregate(loyalty ~education, edited_games, mean)
# print out average loyalty points per gender in decreasing order 
sorted_education_loyalty <- education_loyalty[order(-education_loyalty$loyalty),]
sorted_education_loyalty
# Count the total number of spending score values based on loyalty points. 
#count(sorted_spend_loyalty)
nrow(sorted_education_loyalty)

## education  loyalty
## 1        Basic 2265.040
## 3     graduate 1666.058
## 4          PhD 1499.750
## 5 postgraduate 1499.078
## 2      diploma 1336.021
## The highest loyalty points are earned by High School leavers.

library(ggplot2)

ggplot(sorted_education_loyalty, aes(x = reorder(as.factor(education), -loyalty), y = loyalty)) +
  geom_col(fill = "steelblue") +
  labs(title = "Average Loyalty by Education", x = "Education", y = "Average Loyalty") +
  theme_minimal()

# Average loyalty points based on grouping variable (age)
age_loyalty <-aggregate(loyalty ~age, edited_games, mean)
# print out average loyalty points per age in decreasing order
sorted_age_loyalty <- age_loyalty[order(-age_loyalty$loyalty),]
sorted_age_loyalty
nrow(sorted_age_loyalty)
# There seems to be no obvious pattern here. Loyalty points are earned by 
# customers between the ages of 17 and 72.

library(ggplot2)

ggplot(sorted_age_loyalty, aes(x = reorder(as.factor(age), -loyalty), y = loyalty)) +
  geom_col(fill = "steelblue") +
  labs(title = "Average Loyalty by Age", x = "Age", y = "Average Loyalty") +
  theme_minimal()



# Average loyalty points based on grouping variable (salary)
salary_loyalty <-aggregate(loyalty ~salary, edited_games, mean)
# print out average loyalty points per age in decreasing order
sorted_salary_loyalty <- salary_loyalty[order(-salary_loyalty$loyalty),]
print(head(sorted_salary_loyalty, 15))
# The highest value of loyalty point is earned by customers with high salary.
# Count the total number of salaries based on loyalty points. 
nrow(sorted_salary_loyalty)


library(ggplot2)

ggplot(sorted_salary_loyalty, aes(x = reorder(as.factor(salary), -loyalty), y = loyalty)) +
  geom_col(fill = "steelblue") +
  labs(title = "Average Loyalty by Salary", x = "Salary", y = "Average Loyalty") +
  theme_minimal()



# Average loyalty points based on grouping variable (spending,salary and age)
all_var_loyalty <-aggregate(loyalty ~spending+salary+age, edited_games, mean)
# print out average loyalty points per age in decreasing order
sorted_all_var_loyalty <- all_var_loyalty[order(-all_var_loyalty$loyalty),]
# Print the first 15 rows of sorted_all_var_loyalty
print(head(sorted_all_var_loyalty, 15))
# Highest loyalty points belong to customers with £112K salary, and a spending 
# score of 83. They are 45 years of age.


# Scatter Plot of salary and Spending score with Colour for Loyalty 
library(ggplot2)

ggplot(sorted_all_var_loyalty, aes(x = spending, y = salary, color = loyalty)) +
  geom_point(aes(size = loyalty)) +
  scale_color_viridis_c() +
  labs(title = "Loyalty by Spending & Salary",
       x = "Spending Score",
       y = "Salary",
       color = "Avg Loyalty") +
  theme_minimal()


################################################################################

# Creating Bar Plots for Categorical Variables

# Finding Gender distribution by plotting the Bar plot
table(edited_games$gender)
barplot(table(edited_games$gender), main="Gender Distribution", col="lightblue")
# The bar plot shows female to male gender distribution in 1120:880 ratio.

# Finding Education level distribution by plotting the Bar plot
table(edited_games$education)
barplot(table(edited_games$education), main="Education Distribution", col="lightgreen")
# The education distribution shows that the highest number of customers are 
# grasduates and high school leavers are lowest in number.

################################################################################

# HISTOGRAMS (Continuous & Categorical variables)
# Histogram of Spending
#Specify the ggplot function. 
ggplot(edited_games, aes(x = spending)) +
  geom_histogram(fill = 'blue', color = 'red', bins = 10) + # number of bins here
  labs(x = "Customer Spend",
       y = "Number of Customers",
       title = "Distribution of Spending Score")
# The spending score of customers ranges from 0 to 100, with the highest score 
# in the region of 50 and 60

# Histogram of Salary
#Specify the ggplot function. 
ggplot(edited_games, aes(x = salary)) +
  geom_histogram(fill = 'blue', color = 'red', bins = 10) +  #number of bins here
  labs(x = "Customer Salary in K£",
       y = "Number of Customers",
       title = "Customer Salary Distribution")
# Histogram shows that customer salary ranges from 10K to 112. Most salaries are 
# salaries are distributed 40 -70K. The right tail of the histogram indicates 
#positive skewness.

# Histogram of Age
#Specify the ggplot function. 
ggplot(edited_games, aes(x = age)) +
  geom_histogram(fill = 'blue', color = 'red', bins = 10) +  #number of bins here
  labs(x = "Customer Age",
       y = "Number of Customers",
       title = "Customer Age Distribution")
# The distribution shows positive skewness with a few datapoints distributed to
# the right corner of the histogram.


# Histogram of Loyalty Points
#Specify the ggplot function. 
ggplot(edited_games, aes(x = loyalty)) +
  geom_histogram(fill = 'blue', color = 'red', bins = 10) +  #number of bins here
  labs(x = "Customer Loyalty Points",
       y = "Number of Customers",
       title = "Loyalty Points Distribution")
# The histogram is not normally distributed and has a positive skewness.

# SCATTERPLOTS 
# Scatterplot with a line-of-best-fit from linear regression model.
# Scatterplot to indicate relation between salary and loyalty
ggplot(edited_games, aes(x=salary, y=loyalty)) + 
  geom_point(color = 'blue', alpha = 0.5, size = 1.5) +
  geom_smooth(method=lm)+
  # Add labels for title, subtitle, and caption.
  labs(title = "Relationship between Loyalty and Salary")
# There seems to be a correlation between loyaty and salary.

# Scatterplot to indicate relation between spending and loyalty
ggplot(edited_games, aes(x=spending, y=loyalty)) + 
  geom_point(color = 'red', alpha = 0.5, size = 1.5) +
  geom_smooth(method=lm)+
  # Add labels for title, subtitle, and caption.
  labs(title = "Relationship between Loyalty and Spending")
# There seems to be a correlation between loyaty and spending.

# Scatterplot to indicate relation between age and loyalty
ggplot(edited_games, aes(x=age, y=loyalty)) + 
  geom_point(color = 'brown', alpha = 0.5, size = 1.5) +
  geom_smooth(method=lm)+
  # Add labels for title, subtitle, and caption.
  labs(title = "Relationship between Loyalty and Age")
# There seems to be a distinct correlation here. One could say that there is 
# a gradual, almost impeerceptible decrease in customer loyalty as they age. 

# Scatterplot to indicate relation between Product and loyalty
ggplot(edited_games, aes(x=product, y=loyalty)) + 
  geom_point(color = 'salmon', alpha = 0.5, size = 1.5) +
  geom_smooth(method=lm)+
  # Add labels for title, subtitle, and caption.
  labs(title = "Relationship between Loyalty and Product")


# Scatterplot to indicate relation between education, salary and loyalty
ggplot(data = edited_games,
       mapping = aes(x = salary, y = loyalty)) +  
  geom_point(color='seagreen', alpha = 0.5, size = 1.5) +  
  #Add lines of best fit, remove the confidence intervals (se),
  # and set the size.
  geom_smooth(method = 'lm',
              se = FALSE,
              size = 1.5) +
  # Add argument/title (x).
  scale_x_continuous(breaks = seq(0, 200, 20),
                     "Scaled Salary of the Individual") + 
  #Create faceted plots using facet_wrap() which wraps facets into rectangular layout.
  #We'll pass education in the facet_wrap() and prefix by ~ 
  # Add a facet layer.
  facet_wrap(~education) +
  
  # Add labels for title, subtitle, and caption.
  labs(title = "Relationship between Loyalty, education and Salary",
       subtitle = "Comparing the relationship across 5 Education Categories",
       caption = "Source: Turtle Games") +
  # Add a theme layer. 
  theme_bw()  
# The 5 scatterplots show that salary has a positive impact on loyalty 
# and most of the datapoints belong to graduate customers. 



# Scatterplot to indicate relation between education, spending and loyalty 
ggplot(data = edited_games,
       mapping = aes(x = spending, y = loyalty)) +  
  geom_point(color='red', alpha = 0.5, size = 1.5) +  
  #Add lines of best fit, remove the confidence intervals (se),
  # and set the size.
  geom_smooth(method = 'lm',
              se = FALSE,
              size = 1.5) +
  # Add argument/title (x).
  scale_x_continuous(breaks = seq(0, 200, 20),
                     "Scaled Spending of the Individual") + 
  #Create faceted plots using facet_wrap() which wraps facets into rectangular layout.
  #We'll pass education in the facet_wrap() and prefix by ~ 
  # Add a facet layer.
  facet_wrap(~education) +
  # Add labels for title, subtitle, and caption.
  labs(title = "Relationship between Loyalty, education and Spending",
       subtitle = "Comparing the relationship across 5 Education Categories",
       caption = "Source: Turtle Games") +
  # Add a theme layer. 
  theme_bw()  
# The 5 scatterplots show a similar trend that is seen in the previous 
# visualisation. Spending also has a positive impact on loyalty 
# and most of the datapoints belong to graduate customers. 



# Scatterplot to indicate relation between gender, spending and loyalty
ggplot(data = edited_games,
       mapping = aes(x = spending, y = loyalty)) +  
  geom_point(color='red', alpha = 0.5, size = 1.5) +  
  #Add lines of best fit, remove the confidence intervals (se),
  # and set the size.
  geom_smooth(method = 'lm',
              se = FALSE,
              size = 1.5) +
  # Add argument/title (x).
  scale_x_continuous(breaks = seq(0, 200, 20),
                     "Scaled Spending of the Individual") + 
  #Create faceted plots using facet_wrap() which wraps facets into rectangular layout.
  #We'll pass gender in the facet_wrap() and prefix by ~ 
  # Add a facet layer.
  facet_wrap(~gender) +
  # Add labels for title, subtitle, and caption.
  labs(title = "Relationship between Loyalty, gender and Spending",
       subtitle = "Comparing the relationship between the two genders",
       caption = "Source: Turtle Games") +
  # Add a theme layer. 
  theme_bw()  
# Females earn slightly more loyalty points than the male customers based on their 
#spending score.


# Scatterplot to indicate relation between gender, salary and loyalty
ggplot(data = edited_games,
       mapping = aes(x = salary, y = loyalty)) +  
  geom_point(color='green', alpha = 0.5, size = 1.5) +  
  #Add lines of best fit, remove the confidence intervals (se),
  # and set the size.
  geom_smooth(method = 'lm',
              se = FALSE,
              size = 1.5) +
  
  # Add argument/title (x).
  scale_x_continuous(breaks = seq(0, 200, 20),
                     "Scaled Salary of the Individual") + 
  
  #Create faceted plots using facet_wrap() which wraps facets into rectangular layout.
  #We'll pass education in the facet_wrap() and prefix by ~ 
  # Add a facet layer.
  facet_wrap(~gender) +
  
  # Add labels for title, subtitle, and caption.
  labs(title = "Relationship between Loyalty, gender and Salary",
       subtitle = "Comparing the relationship between the two genders",
       caption = "Source: Turtle Games") +
  
  # Add a theme layer. 
  theme_bw()  
# There is no difference between male and female customers when it comes to 
# earning loyalty points based on their salaries.


#BOXPLOT
# Box plot 1. Loyalty Vs Education
ggplot(edited_games, aes(x=education, y=loyalty)) +
  geom_boxplot(fill='green',
               notch=TRUE,
               outlier.color='blue')+
  # Add labels for title, subtitle, and caption.
  labs(title = "Relationship between Loyalty and Education")
# There are significant number of outliers in almost all categories which 
# indicate that datapoints lie outside the range of the main distribution. 

# Box plot 2. Loyalty Vs gender
ggplot(edited_games, aes(x=gender, y=loyalty)) +
  geom_boxplot(fill='red',
               notch=TRUE,
               outlier.color='blue')+
  # Add labels for title, subtitle, and caption.
  labs(title = "Relationship between Loyalty and gender")
# Most of the male and female customer's loyalty points lie below the range of 
# but a significant number of datapoints for both genders are below 
# and over this range.



###############################################################################
###############################################################################


## 4. Create a Multiple Linear Regression model using R to predict loyalty points 
##  using the available features in a multiple linear model and make 
## recommendations to the business regarding suitability of the model type.

## Investigate customer behaviour and the effectiveness of the current loyalty 
## program based on the statistical analysis and MLR.
##  - Can we predict loyalty points given the existing features using a 
## relatively simple MLR model?
##  - Do you have confidence in the model results (Goodness of fit evaluation)
##  - Where should the business focus their marketing efforts?
##  - How could the loyalty program be improved?
##  - How could the analysis be improved?

################################################################################

# 1. View the data 
head(edited_games)
dim(edited_games)
colnames(edited_games)
as_tibble(edited_games)

# 2. Statistical Analysis for all Numeric Columns.

library(moments) 

## Age column
qqnorm(edited_games$age)
qqline(edited_games$age, main = "Age Distribution",col='red')
shapiro.test(edited_games$age)
skewness(edited_games$age)
kurtosis(edited_games$age)
summary(edited_games$age)

## The age distribution is slightly right-skewed (skewness = 0.61), indicating
## a small number of older individuals with higher ages pulling the mean above 
## the median. The kurtosis value of 2.81 suggests a distribution that is 
## approximately normal but slightly flatter. Overall, the age data is fairly
## symmetrical with mild deviation from a perfect normal distribution.

## Salary column
qqnorm(edited_games$salary)
qqline(edited_games$salary, main = "Salary Distribution",col='red')
shapiro.test(edited_games$salary)
skewness(edited_games$salary)
kurtosis(edited_games$salary)
summary(edited_games$salary)

## The salary distribution is slightly right-skewed (skewness = 0.41) with a 
## relatively flat shape (kurtosis = 2.59), and the mean is slightly higher than 
## the median. Although the Q-Q plot suggests near-normality in the center, the 
## Shapiro-Wilk test confirms significant deviation from normality (p < 0.001). 
## This suggests that salary data does not follow a perfect normal distribution,
## particularly due to the presence of higher-income outliers.

## Spending column
qqnorm(edited_games$spending)
qqline(edited_games$spending,main = "Spending Distribution",col='red')
shapiro.test(edited_games$spending)
skewness(edited_games$spending)
kurtosis(edited_games$spending)
summary(edited_games$spending)

##The spending distribution appears to be nearly symmetrical (skewness = -0.04) 
## and relatively flat (kurtosis = 2.11), with a mean and median both at 50. 
## While the Q-Q plot shows only mild deviations from the diagonal, the 
## Shapiro-Wilk test confirms that the data is not perfectly normal 
## (p < 0.001). Overall, spending is approximately symmetric but does not meet 
## the strict assumptions of normality.

## Loytal_points column
qqnorm(edited_games$loyalty)
qqline(edited_games$loyalty, main = "Spending Distribution",col='red')
shapiro.test(edited_games$loyalty)
skewness(edited_games$loyalty)
kurtosis(edited_games$loyalty)
summary(edited_games$loyalty)

## The loyalty variable shows a strong right skew (skewness = 1.46) and is 
## highly leptokurtic (kurtosis = 4.71), indicating a distribution with heavy 
## tails and extreme values. The Q-Q plot confirms this non-normality, with 
##clear deviations from the diagonal, particularly in the upper quantiles. The 
## Shapiro-Wilk test strongly rejects the assumption of normality (p < 0.001),
## confirming that loyalty scores are not normally distributed.



################################################################################

# 3. Check for correlation

# Set your working directory.
#setwd(dir='D:/Course 3- Advanced Analytics for Organisational Impact/assignment_files_new')

# Import the data set.
#games <- read.csv('D:/Course 3- Advanced Analytics for Organisational Impact/assignment_files_new/clean_reviews.csv', header=T)
#games<- read.csv(file.choose(), header=T)

# Return the dimensions of your data frame.
dim(games)
# Print the data frame.
View(games)
View(games[1:50, ])
head(games)
as_tibble(games)

# Drop the non numerical columns and create a new data set.
install.packages("dplyr")
library(dplyr)

game_corr <- select(games, age, salary,loyalty, spending, product)

## Check new data frame
dim(game_corr)
head(game_corr)
col(game_corr)

# View the correlation matrix
cor(game_corr)

## Visualise the correlation
## Import psych packages
library(psych)

## Correlation table
png("correlation_plot.png", width = 1000, height = 800)
corPlot(game_corr, cex = 2)
dev.off()

## Generate a report:
DataExplorer::create_report(game_corr)

## Key insights from the correlation table:
## The analysis shows that product engagement has a moderate positive 
## relationship with salary (0.31), suggesting that wealthier customers tend to 
## interact more with products. Loyalty is strongly tied to both spending (0.67)
## and salary (0.62), while its link to product is weaker (0.18). Spending and 
## age show no meaningful connection to product, implying product preferences 
## are more influenced by income than by age or spending patterns.

## Possible Further Investigations:
  
#  Loyalty and Spending Dynamics:
## Explore the strong associations between loyalty points, spending behavior, 
## and salary to uncover potential customer segments and assess the impact of 
## current loyalty programs.

#  Product Appeal by Income Level:
## The moderate correlation between salary and product interaction may indicate
## that certain products resonate more with higher-income customers, warranting 
## a deeper look into income-based preferences.

################################################################################

# 4.Simple Linear Regression

# Find the best prediction model by conducting simple linear regression using 
# different predictors / variables. 

# Install required packages for outputting regression summary
install.packages("broom")
install.packages("kableExtra")
install.packages("knitr")

#install.packages("kableExtra", dependencies = TRUE, INSTALL_opts = c("--no-multiarch"), quiet = FALSE)
#install.packages("knitr", dependencies = TRUE, INSTALL_opts = c("--no-multiarch"), quiet = FALSE)

# Load the libraries
library(broom)
library(knitr)
library(kableExtra)

# Model 1: Fit the linear regression model (using age, product, salary and spending)
model1 <- lm(loyalty ~ salary + age + spending + product, data = game_corr)

# View model summary
# Create and display a formatted table
tidy(model1) %>%
  kable(digits = 3, caption = "Regression Summary: Predicting Loyalty with Age, Product, Spending and Salary") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = F, position = "center")


## Interpretation of Model 1 summary:
## Spending and salary are the strongest predictors of loyalty. Age also 
## contributes positively, but less so. Product has no significant effect 
## (p = 0.75), meaning it doesn’t help explain loyalty variation in this model.

## Suggested Next Step is to view Model 2 performance by calculating R-Squared 
## and Adjusted R-Squared values.

## Look at R-squared and Adjusted R-squared. Plot predicted vs actual loyalty:
x11()  # Opens a new graphics device window (on Windows)

predicted <- predict(model1)
plot(predicted, game_corr$loyalty,
     xlab = "Predicted Loyalty", ylab = "Actual Loyalty",
     main = "Predicted vs Actual Loyalty")
abline(0, 1, col = "red")


## Calculate Root Mean Squared Error (RMSE)
predicted <- predict(model1)
actual <- game_corr$loyalty

rmse <- sqrt(mean((actual - predicted)^2))
rmse

## Mean Absolute Error (MAE)
mae <- mean(abs(actual - predicted))
mae

## Mean Absolute Error (MAE)    RSME 
## [1] 394.9633                [1] 513.2964

## Interpretation of the findings: 
## Spending, salary, and age all significantly and positively predict loyalty.
## Product is not a significant predictor (p = 0.75), and contributes nothing 
## meaningful to the model. Prediction accuracy is solid with RMSE ≈ 513 and 
## MAE ≈ 395, showing that on average, predictions are within 400–500 
## loyalty points.


# Model 2: Fit the linear regression model (using age, salary and spending)
model2 <- lm(loyalty ~ salary + age + spending, data = game_corr)

# View model summary
# Create and display a formatted table
tidy(model2) %>%
  kable(digits = 3, caption = "Regression Summary: Predicting Loyalty with Age, Salary & Spending") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = F, position = "center")


## Interpretation of Model 2 summary:
## This model shows a strong, positive, and statistically significant 
## relationship between all three predictors and loyalty. All p-values are 
## essentially zero — meaning these predictors are highly significant. Removing 
## product has made the model more simpler, without losing predictive power.

## Suggested Next Step is to view Model 2 performance by calculating R-Squared 
## and Adjusted R-Squared values.

## Look at R-squared and Adjusted R-squared. Plot predicted vs actual loyalty:
x11()  # Opens a new graphics device window (on Windows)

predicted <- predict(model2)
plot(predicted, game_corr$loyalty,
     xlab = "Predicted Loyalty", ylab = "Actual Loyalty",
     main = "Predicted vs Actual Loyalty")
abline(0, 1, col = "red")


## The model captures the general upward trend well, but the error increases at 
## the extremes — especially for very high loyalty scores. Most points touch the 
## line between 0–3000, meaning good predictive accuracy in that range. Beyond 
## ~4000 loyalty, predictions underestimate actual scores — the model may not 
## capture non-linear effects well.

## Calculate Root Mean Squared Error (RMSE)
predicted <- predict(model2)
actual <- game_corr$loyalty

rmse <- sqrt(mean((actual - predicted)^2))
rmse

## Mean Absolute Error (MAE)    RSME 
## [1] 394.9818                [1] 513.3095


> 
  > ## Mean Absolute Error (MAE)
  > mae <- mean(abs(actual - predicted))
> mae

# Model 3: Fit the linear regression model (using salary and spending)
model3 <- lm(loyalty ~ salary + spending, data = game_corr)

# View model summary
# Create and display a formatted table
tidy(model3) %>%
  kable(digits = 3, caption = "Regression Summary: Predicting Loyalty with Salary & Spending") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = F, position = "center")


## Interpretation of Model 3 summary:
## This model is highly statistically significant across both predictors. 
## Spending and salary are both very strong, roughly equal, and positively 
## associated with loyalty. The model is more simpler but still powerful, ideal 
## for interpretation or predictive use.

## Suggested Next Step is to view Model 3 performance by calculating R-Squared 
## and Adjusted R-Squared values.

## Look at R-squared and Adjusted R-squared. Plot predicted vs actual loyalty:
x11()  # Opens a new graphics device window (on Windows)

predicted <- predict(model3)
plot(predicted, game_corr$loyalty,
     xlab = "Predicted Loyalty", ylab = "Actual Loyalty",
     main = "Predicted vs Actual Loyalty")
abline(0, 1, col = "red")

## Summary of the R-Squared and adjusted R-Squared values graph:
## The model demonstrates a strong positive relationship between predicted and 
## actual loyalty scores, especially for mid- to high-range customers. Most 
## predictions are close to the actual values, confirming a good model fit, 
## though there's slight underestimation at very high loyalty levels. Overall, 
## this regression model is highly effective for forecasting loyalty based on 
## salary and spending.


## Calculate Root Mean Squared Error (RMSE)
predicted <- predict(model3)
actual <- game_corr$loyalty

rmse <- sqrt(mean((actual - predicted)^2))
rmse

## Mean Absolute Error (MAE)
mae <- mean(abs(actual - predicted))
mae

## rmse                     Mean Absolute Error (MAE) 
## [1] 533.7413             [1] 414.8323 

## Even though it was observed earlier that the model demonstrates a strong 
## positive relationship between predicted and actual loyalty scores, the RMSE 
## and MAE values are quite high and explain a significant portion of loyalty 
## variation. This could be improved by trying log transformation on loyalty to 
## reduce skew.


## The RSME and MAE of Model 1 and Model 2 differ by less than 0.02, which is 
## negligible. Model 3, while simpler, has slightly higher error, suggesting age
## contributes modestly to loyalty prediction. Product does not improve 
## prediction — since Model 1 and Model 2 are equally accurate, and Model 1 
## includes an unnecessary predictor, it is recommended to use Model 2 because 
## it is just as accurate as Model 1 but simpler and more interpretable — 
## avoiding the inclusion of a non-significant variable (product). To improve 
## RSME and MAE values, test the log-transformed model.


# Model 4: Fit the log-transformed model (Model 2 version)
# 1. Fit the log-transformed model (Model 2 version)
model_log2 <- lm(log(loyalty) ~ salary + spending + age, data = game_corr)

# 2. Predict log(loyalty) and back-transform
log_preds <- predict(model_log2)
predicted_loyalty <- exp(log_preds)  # back-transform to original scale

# 3. Actual loyalty values
actual_loyalty <- edited_games$loyalty

# 4. Calculate RMSE
rmse_log <- sqrt(mean((actual_loyalty - predicted_loyalty)^2))
rmse_log

# 5. Calculate MAE
mae_log <- mean(abs(actual_loyalty - predicted_loyalty))
mae_log

## rmse_log              mae_log
## [1] 905.9958         [1] 485.9206


# 6. Plot predicted vs actual
plot(predicted_loyalty, actual_loyalty,
     xlab = "Predicted Loyalty (log model)",
     ylab = "Actual Loyalty",
     main = "Predicted vs Actual Loyalty (Log Model)",
     pch = 1)
abline(0, 1, col = "blue", lwd = 2)

## Summary of Model 4 Findings:
## Despite applying a log transformation, this model performed worse than the
## previous models. RMSE increased by ~390 points compared to Model 2 (513 → 906)
## MAE increased by ~90 points (395 → 486). The plot also shows predicted 
## values are disproportionately large for some points which means they can 
## exaggerate prediction errors when residuals are large.


# # Model 5: Fit the log-transformed model (Model 3 version)
# 1. Fit the log-transformed model (Model 3 version)
model_log3 <- lm(log(loyalty) ~ salary + spending, data = game_corr)

# 2. Predict log(loyalty) and back-transform
log_preds <- predict(model_log3)
predicted_loyalty <- exp(log_preds)  # back-transform to original scale

# 3. Actual loyalty values
actual_loyalty <- game_corr$loyalty

# 4. Calculate RMSE
rmse_log <- sqrt(mean((actual_loyalty - predicted_loyalty)^2))
rmse_log

# 5. Calculate MAE
mae_log <- mean(abs(actual_loyalty - predicted_loyalty))
mae_log

## rmse_log              mae_log
## [1] 866.1128        [1] 501.4018


# 6. Plot predicted vs actual
plot(predicted_loyalty, actual_loyalty,
     xlab = "Predicted Loyalty (log model)",
     ylab = "Actual Loyalty",
     main = "Predicted vs Actual Loyalty (Log Model)",
     pch = 1)
abline(0, 1, col = "red", lwd = 2)

## Summary of Model 5 Findings:
## This model didn't perform well despite using log transformed loyalty.
## We will be using Model 2 for predictions. 

# Next Step: Calculate VIF and Residuals

## Install and import package
install.packages("car") 
library(car)

## Check for multicollinearity 
vif(model2)
# There is no multicollinearity between the predictors.


# --- Residual Diagnostics for model 2 ---

# View 4 standard diagnostic plots
par(mfrow = c(2, 2))  # Layout for 2x2 grid of plots
plot(model2)

# Extract raw residuals
residuals_model2 <- residuals(model2)

# Residuals vs Fitted Values (custom)
par(mfrow = c(1, 1))  # Reset layout
plot(model2$fitted.values, residuals_model2,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted Values",
     pch = 19, col = "darkgreen")
abline(h = 0, col = "red", lwd = 2)

# Histogram of Residuals
hist(residuals_model2,
     main = "Histogram of Residuals",
     xlab = "Residuals",
     col = "lightblue",
     border = "white",
     breaks = 30)


## The histogram shows that the residuals are approximately normally distributed,
## indicating the normality assumption is reasonably satisfied. However, the 
## U-shaped pattern in the residuals vs fitted values plot suggests a violation
## of linearity, meaning the model may not fully capture the relationship 
## between predictors and loyalty. This implies potential underfitting at the 
## extremes. 



############################################################################### 
  
# Simple Linear Regression with Salary or Spending.

## Simple Linear Regression with Salary and Loyalty.
## Model 6:Fit simple linear regression: loyalty ~ salary
model6 <- lm(loyalty ~ salary, data = game_corr)

# View summary
summary(model6)
# Create and display a formatted table
tidy(model6) %>%
  kable(digits = 3, caption = "Regression Summary: Predicting Loyalty with Salary") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = F, position = "center")


# Predict values
predicted_salary <- predict(model6)
actual_loyalty <- game_corr$loyalty

# Calculate RMSE and MAE
rmse_salary <- sqrt(mean((actual_loyalty - predicted_salary)^2))
mae_salary <- mean(abs(actual_loyalty - predicted_salary))

# Print error metrics
cat("RMSE (Salary Model):", rmse_salary, "\n")
cat("MAE (Salary Model):", mae_salary, "\n")

# Plot predicted vs actual
x11()
plot(predicted_salary, actual_loyalty,
     xlab = "Predicted Loyalty (Salary Model)",
     ylab = "Actual Loyalty",
     main = "Predicted vs Actual Loyalty: Salary Model",
     pch = 1)
abline(0, 1, col = "red", lwd = 2)

## The RSME and MAE values are higher than Multilinear Regression models. The 
## scatterplot of Predicted vs Actual Loyalty shows a weaker fit - the points 
## are more widely scattered around the red line (ideal fit). 
  
  
## Model 7:Fit simple linear regression: loyalty ~ spending
model7 <- lm(loyalty ~ spending, data = game_corr)

# View summary
summary(model7)
# Create and display a formatted table
tidy(model7) %>%
  kable(digits = 3, caption = "Regression Summary: Predicting Loyalty with Spending") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = F, position = "center")


# Predict values
predicted_salary <- predict(model7)
actual_loyalty <- game_corr$loyalty

# Calculate RMSE and MAE
rmse_salary <- sqrt(mean((actual_loyalty - predicted_salary)^2))
mae_salary <- mean(abs(actual_loyalty - predicted_salary))

# Print error metrics
cat("RMSE (Salary Model):", rmse_salary, "\n")
cat("MAE (Salary Model):", mae_salary, "\n")

# Plot predicted vs actual
x11()
plot(predicted_salary, actual_loyalty,
     xlab = "Predicted Loyalty (Salary Model)",
     ylab = "Actual Loyalty",
     main = "Predicted vs Actual Loyalty: Salary Model",
     pch = 1)
abline(0, 1, col = "red", lwd = 2)  
  

## Model 7 performs better than Model 6, confirming that spending is a stronger 
## standalone predictor of loyalty than salary. However, neither performs as 
## well as Model 2 (Multiple Linear Regression model). 



# It is therefore, concluded that non of the models is good for prediction.


###############################################################################





