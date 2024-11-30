# Install Packages
install.packages("tidymodels")
install.packages("rsample")
install.packages("tidyverse")
install.packages("gridExtra")
install.packages("Amelia")
install.packages("psych")
install.packages("vip")
install.packages("extrafont")
install.packages("caret")

# Load Libraries
library(tidyverse) # for data manipulation and visualization
library(tidymodels) # to create linear and logistic regression models
library(gridExtra) # for arranging visualizations
library(Amelia) # to deal with missing values
library(psych) # # for data visualization and descriptive statistics
library(vip) # to visualize variable importance in models
library(extrafont) # for font customization
library(caret) # for addittional model analysis
library(rsample) # for data splits and resamples

# Load dataset

# Duplicate dataset for EDA
duplicated_data = Credit_Rating_Dataset

# Assign the categorical columns to an object
cat_cols <- c("status", "credit_history", "purpose", "savings", "employment_duration",
              "installment_rate", "other_debtors", "present_residence", "property", 
              "other_installment_plans", "housing", "number_credits", "job", 
              "people_liable", "telephone", "foreign_worker", "credit_risk")

# Convert the categorical columns to a factor variable
duplicated_data[,cat_cols] <- lapply(duplicated_data[,cat_cols],factor)

# Change variable values for a better understanding in EDA
duplicated_data <- duplicated_data %>% mutate(credit_risk = ifelse(credit_risk == 0, 'bad', 'good'))

duplicated_data$credit_risk = as.factor(duplicated_data$credit_risk)


duplicated_data$status = ifelse(duplicated_data$status == 1, 'no checking account',
                                ifelse(duplicated_data$status == 2, '<0 USD',
                                       ifelse(duplicated_data$status == 3, '0 USD >= & < 200 USD',
                                              '>= 200 USD')))

duplicated_data$status = factor(duplicated_data$status, levels = c('no checking account', '<0 USD', 
                                                                   '0 USD >= & < 200 USD', '>= 200 USD'))


duplicated_data$savings = ifelse(duplicated_data$savings == 1, 'unknown/no savings account', 
                                 ifelse(duplicated_data$savings == 2, '< 100 USD',
                                        ifelse(duplicated_data$savings == 3, '100 >= & < 500 USD',
                                               ifelse(duplicated_data$savings == 4, '500 >= & < 1000 USD',
                                                      '>= 1000 USD'))))

duplicated_data$savings = factor(duplicated_data$savings, levels = c('unknown/no savings account',
                                                                     '< 100 USD',
                                                                     '100 >= & < 500 USD',
                                                                     '500 >= & < 1000 USD',
                                                                     '>= 1000 USD'))

# Chechout the transformed dataset types
str(duplicated_data)


# View the data summary
summary(duplicated_data)

# Exploratory Data Analysis (Categorical Variables)
# Determine the target variable distribution
credit_risk_dist <- ggplot(duplicated_data, aes(x=credit_risk))+
  geom_bar(width = 0.3, fill = 'darkred')+
  theme_minimal()+
  labs(x = 'Credit Risk',
       y = 'Count',
       title = "Distribution of Target Variable")+
  theme(plot.title = element_text(size = 15, family = "Verdana", hjust = 0.5),
        plot.subtitle = element_text(size = 11, family = "Verdana", hjust = 0.5),
        plot.background = element_rect(fill = 'white'))

credit_risk_dist

# Distribution of Variables by Credit Risk
df = Credit_Rating_Dataset
df <- mutate(df, credit_risk = ifelse(credit_risk == 0, 'bad', 'good'))

# Status
status <- ggplot(data = df, aes(status))+
  geom_histogram(breaks = seq(0, 5, by =1),
                 col = 'darkgreen',
                 aes(fill = after_stat(count)))+
  facet_wrap(~credit_risk)+
  scale_fill_gradient("Count", low = "red", high = "darkred")+
  theme_minimal()

status

duration <- ggplot(data = df, aes(duration))+
  geom_histogram(breaks = seq(0, 80, by =5),
                 col = 'darkgreen',
                 aes(fill = after_stat(count)))+
  facet_wrap(~credit_risk)+
  scale_fill_gradient("Count", low = "red", high = "darkred")+
  theme_minimal()

duration

age <- ggplot(data = df, aes(age))+
  geom_histogram(breaks = seq(0, 80, by =5),
                 col = 'darkgreen',
                 aes(fill = after_stat(count)))+
  facet_wrap(~credit_risk)+
  scale_fill_gradient("Count", low = "red", high = "darkred")+
  theme_minimal()

age

credit_history <- ggplot(data = df, aes(credit_history))+
  geom_histogram(breaks = seq(0, 5, by =1),
                 col = 'darkgreen',
                 aes(fill = after_stat(count)))+
  facet_wrap(~credit_risk)+
  scale_fill_gradient("Count", low = "red", high = "darkred")+
  theme_minimal()

credit_history

job <- ggplot(data = df, aes(job))+
  geom_histogram(breaks = seq(0, 5, by =1),
                 col = 'darkgreen',
                 aes(fill = after_stat(count)))+
  facet_wrap(~credit_risk)+
  scale_fill_gradient("Count", low = "red", high = "darkred")+
  theme_minimal()

job

number_credits <- ggplot(data = df, aes(number_credits))+
  geom_histogram(breaks = seq(0, 5, by =1),
                 col = 'darkgreen',
                 aes(fill = after_stat(count)))+
  facet_wrap(~credit_risk)+
  scale_fill_gradient("Count", low = "red", high = "darkred")+
  theme_minimal()

number_credits

other_installment_plans <- ggplot(data = df, aes(other_installment_plans))+
  geom_histogram(breaks = seq(0, 5, by =1),
                 col = 'darkgreen',
                 aes(fill = after_stat(count)))+
  facet_wrap(~credit_risk)+
  scale_fill_gradient("Count", low = "red", high = "darkred")+
  theme_minimal()

other_installment_plans

savings <- ggplot(data = df, aes(savings))+
  geom_histogram(breaks = seq(0, 5, by =1),
                 col = 'darkgreen',
                 aes(fill = after_stat(count)))+
  facet_wrap(~credit_risk)+
  scale_fill_gradient("Count", low = "red", high = "darkred")+
  theme_minimal()

savings

amount <- ggplot(data = df, aes(amount))+
  geom_histogram(breaks = seq(0, 20000, by =1000),
                 col = 'darkgreen',
                 aes(fill = after_stat(count)))+
  facet_wrap(~credit_risk)+
  scale_fill_gradient("Count", low = "red", high = "darkred")+
  theme_minimal()

amount

# Create a grid for Variables Distribution by Credit Rating
grid.arrange(status, duration, age, credit_history, job, number_credits, other_installment_plans,
             savings, amount, ncol=2)

# Create boxplot for numerical variables
df_num = Credit_Rating_Dataset %>% select(age, amount, duration)
df_num = cbind(df_num, duplicated_data$credit_risk)
colnames(df_num) = c('age', 'amount', 'duration', 'credit_risk')

bp1 <- ggplot(df_num, aes(age))+
  geom_boxplot(fill = "red", color = "black", alpha = 0.3, width = 0.5)+
  facet_wrap(~credit_risk) + coord_flip()+
  theme_minimal()

bp1

bp2 <- ggplot(df_num, aes(amount))+
  geom_boxplot(fill = "red", color = "black", alpha = 0.3, width = 0.5)+
  facet_wrap(~credit_risk) + coord_flip()+
  theme_minimal()

bp2

bp3 <- ggplot(df_num, aes(duration))+
  geom_boxplot(fill = "red", color = "black", alpha = 0.3, width = 0.5)+
  facet_wrap(~credit_risk) + coord_flip()+
  theme_minimal()

bp3

# Create grid for the boxplots
grid.arrange(bp1,bp2,bp3)

# Create Correlation plot to check for correlation within variables
# Transform factor variables to numeric
cor_data = Credit_Rating_Dataset %>% mutate_if(is.factor, as.numeric)

# Correlation Plot
corPlot(cor_data, alpha = 0.8)

dev.off()

corPlot(cor_data, alpha = 0.8)

# Data Preprocessing

# Check for missing values
missmap(Credit_Rating_Dataset)

# Model Development
# Set a seed for random number generation to ensure repeatability
set.seed(1234)

# Split data into training and test sets (70:30)
ind <- sample(2, nrow(Credit_Rating_Dataset), replace = TRUE, prob = c(0.7, 0.3))
train <- Credit_Rating_Dataset[ind == 1,]
test <- Credit_Rating_Dataset[ind == 2,]

# Logistic Regression Model
# Fit the model to predict credit risk
log_reg <- glm(credit_risk~., data = train, family = 'binomial')

summary(log_reg)

# Feature Importance Visualization
log_reg %>%
  vip(num_features = 20,
      geom = "point",
      aesthetics = list(
        size = 2,
        color = "darkred"
      )) +
  theme_minimal(base_size = 18)+
  labs(title = "Logistic Regression: Feature Importance")

# Predition on the training dataset
p1 <- predict(log_reg, train, type = 'response')
pred1 <- ifelse(p1 > 0.5, 1, 0)

pred1

# Predition on the test dataset
p2 <- predict(log_reg, test, type = 'response')
pred2 <- ifelse(p2 > 0.5, 1, 0)

pred2

# Confusion Matrix for test data
cmlg1 <- confusionMatrix(factor(pred1), factor(train$credit_risk), positive = '1')

cmlg1

# Confusion Matrix for train data
cmlg2 <- confusionMatrix(factor(pred2), factor(test$credit_risk), positive = '1')

cmlg2
