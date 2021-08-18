
#######################################################################################################

library(ggplot2)
library(RColorBrewer)
data <- read.csv("Civil-Service-Survey-2015-CSV.csv",header=T, stringsAsFactors=F)

fill <- "green3"
line <- "midnightblue"


data$marital_status <- factor(data$marital_status,
                              labels = c("Single ", "Married ","Separated ","Widowed","Divorced"))
data$sex <- factor(data$sex,
                   labels = c("Male", "Female"))


data$employment_status <- factor(data$employment_status,
                                 labels = c("Full/part-time","Self-employed","Unemployed","Retired","Home-maker","Student","Other"))


data$Desc_of_residence <- factor(data$Desc_of_residence,
                                 labels = c("Rural","Urban","Other"))
layout(matrix(1:1,ncol=1))

cdplot(data$marital_status ~ age, data=data)      #  valid
cdplot(data$employment_status ~ age, data=data)  #  valid
cdplot(data$Desc_of_residence ~ age, data=data)  #  valid


model1 <- glm(data$employment_status  ~ age, data = data,
              family = binomial())

summary(model1) # invalid

# 95% CI
confint(model1, parm = "age")

# odds-ratio
exp(coef(model1)["age"])

model2 <- glm(data$marital_status  ~ age, data = data,
              family = binomial())

summary(model2)  # valid

# 95% CI
confint(model2, parm = "age")

# odds-ratio
exp(coef(model2)["age"])



model3 <- glm(data$Desc_of_residence  ~ age, data = data,
              family = binomial())

summary(model3) # invalid

# 95% CI
confint(model3, parm = "age")

# odds-ratio
exp(coef(model3)["age"])



model4 <- glm(data$marital_status ~ age + data$employment_status+data$Desc_of_residence,
              data = data, family = binomial())

summary(model4)  # valid

# 95% CI
confint(model4, parm = "age")

# odds-ratio
exp(coef(model4)["age"])



model5 <- glm(data$marital_status ~ age + data$employment_status,
              data = data, family = binomial())

summary(model5)  # valid


# 95% CI
confint(model5, parm = "age")

# odds-ratio
exp(coef(model5)["age"])


anova(model2,model4,model5,test = "Chisq")


# 3D Scatterplot with Coloring and Vertical Lines
# and Regression Plane 
library(scatterplot3d) 
attach(data) 
s3d <-scatterplot3d(age,data$employment_status,data$marital_status, pch=16, highlight.3d=TRUE,
                    type="h", main="3D Scatterplot")
fit <- lm(data$marital_status ~ age+data$employment_status) 
s3d$plane3d(fit)

# Spinning 3d Scatterplot
library(rgl)

plot3d(age, data$employment_status, data$marital_status, col="red", size=5)



##########################################################################################################


