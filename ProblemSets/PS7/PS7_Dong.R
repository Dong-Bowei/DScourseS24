# install.packages(c("mice", "modelsummary"))
install.packages("mice")
library(mice)
library(modelsummary)
library(xtable)

# This data set contains information on ≈ 2, 250 women who were working in the US in 1988. The variables should be self-explanatory, except for tenure, which refers to how long (in years) each woman has been at her current employer, and hgc, which refers to how many years of schooling each woman has completed.
# q4
wages_data <- read.csv("wages.csv")

# q5
wages_data <- wages_data[complete.cases(wages_data$hgc, wages_data$tenure), ]

# q6
summary <- summary(wages_data)
latex_table <- xtable(summary)
print(latex_table, type = "latex")

# MAR

# q7
# q7-1
lm_model.1 <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, 
                 data = na.omit(wages_data))
modelsummary(lm_model.1, out = "latex")
# q7-2
mean_logwage <- mean(wages_data$logwage, na.rm = TRUE)  # Calculate the mean of logwage
wages_data$logwage[is.na(wages_data$logwage)] <- mean_logwage  # Replace NA values with mean
lm_model.2 <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, 
                 data = wages_data)
modelsummary(lm_model.2, out = "latex")
# q7-3
lm_complete <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, 
                  data = na.omit(wages_data))
wages_data$imputed_logwage <- predict(lm_complete, newdata = wages_data)
wages_data$logwage[is.na(wages_data$logwage)] <- wages_data$imputed_logwage[is.na(wages_data$logwage)]
wages_data <- subset(wages_data, select = -c(imputed_logwage))
lm_model.3 <- lm(logwage ~ hgc + tenure + I(tenure^2) + college + age + married, data = wages_data)
modelsummary(lm_model.3, out = "latex")
# q7-4
imp <- mice(wages_data, method = "pmm", m = 5)
lm_model.4 <- with(imp, lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married))
pooled_lm_model <- pool(lm_model.4)
modelsummary(pooled_lm_model, output = "latex")


















