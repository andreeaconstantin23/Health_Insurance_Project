## Data and Variables
insurance <- read.csv("insurance.csv")
str(insurance)
#Convert categorical variables to factors
insurance$sex <- as.factor(insurance$sex)
insurance$smoker <- as.factor(insurance$smoker)
insurance$region <- as.factor(insurance$region)

#Summary data
"Charges"
summary(insurance$charges)
"Age"
summary(insurance$age)
"Sex"
summary(insurance$sex)
"BMI"
summary(insurance$bmi)
"Children"
summary(insurance$children)
"Smoker"
summary(insurance$smoker)
"Region"
summary(insurance$region)

hist(insurance$charges)

#Correlation analysis
cor(insurance[, c("age", "bmi", "children", "charges")])

#Visual Exploration
# charges based on if the respondants are smokers or not.
ggplot(insurance, aes(x = smoker, y = charges)) +
  geom_boxplot() +
  labs(title = "Insurance Charges by Smoking Status")

#charges influenced by bmi
ggplot(insurance, aes(x = bmi, y = charges)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm") +
  labs(title = "Charges and BMI")

#Interaction (bmi * smoker)
ggplot(insurance, aes(x = bmi, y = charges, color = smoker)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(title = "Effect of BMI on Charges by Smoking Status", 
       x = "BMI", y = "Charges")

#Preliminary tests. Normality of dependent variable
hist(insurance$charges, breaks = 30, main = "Distribution of Charges")
shapiro.test(insurance$charges[1:500])  # subsample due to size

#Linear Regression Model
model_lm <- lm(charges ~ age + bmi + children + sex + smoker + region, data = insurance)
summary(model_lm)


#Exploring the outliers

Q1 <- quantile(insurance$charges, 0.25)
Q3 <- quantile(insurance$charges, 0.75)
IQR_val <- IQR(insurance$charges)

#only the high-end outliers
outliers <- insurance %>% 
  filter(charges > Q3)
#the first few outliers
head(outliers)

#Extracing the outliers
outlier_values <- boxplot.stats(insurance$charges)$out

#full rows for the outliers
outlier_data <- insurance[insurance$charges %in% outlier_values, ]

nrow(outlier_data)
summary(outlier_data)

#Interaction Regression model
refined_model <- lm(charges ~ age + children + sex + region + bmi * smoker, data = insurance)
summary(refined_model)
