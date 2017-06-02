########### sta 144 hw3 ################
########### Kota Natsume 914530226
########### R code 
#"The codes and results derived by using these codes constitute my own work.
#I have consulted the following resources regarding this assignment:" 
#(https://stats.stackexchange.com/questions/147863/how-to-interpret-the-residual-colors-on-a-mosaic-plot)


setwd("C:/Users/toshiya/Desktop/sta141A")
library(ggplot2) 

# read data
data <- load('NHANES.Rdata')
data <- as.data.frame(NHANES)
# omit na
data <- na.omit(data)
head(data)
pair_data <- cbind(data$Age,data$Weight,data$BMI,data$Diet.Iron,data$Albumin,data$Serum.Iron,data$TIBC,data$Transferin,data$Hemoglobin)
colnames(pair_data) <- c('age','weight','bmi','diet.iron','albumin','serum.iron','tibc','transferin','hemoglobin')
pairs(pair_data)
cor(pair_data)


# 2 mosaic
data <- load('NHANES.Rdata')
data <- as.data.frame(NHANES)
head(data)
# chi-sq test
t1 <- table(data$Cancer.Incidence,data$Smoke)
chisq.test(t1, correct = FALSE) 

mosaic(~ Cancer.Incidence+Smoke, data = data, shade = T, legend = TRUE)
mosaic(~ Cancer.Incidence+Smoke+Race, data = data, shade = TRUE, legend = TRUE)
mosaic(~ Cancer.Incidence+Smoke+Sex, data = data, shade = TRUE, legend = TRUE)

### 3
head(data)

ggplot(data) + geom_point( aes(x = Diet.Iron, y = Transferin) )

# diet.iron vs transferin
plt = ggplot(data, aes(Diet.Iron, Transferin, color = Sex))
plt = plt + geom_point(alpha = 0.3)
plt

plt = ggplot(data, aes(Diet.Iron, Transferin, color = factor(Ed)))
plt = plt + geom_point(alpha=0.5)
plt

plt = ggplot(data, aes(Diet.Iron, Transferin, color = factor(Race)))
plt = plt + geom_point(alpha=0.2)
plt

# diet.iron vs Hemoglobin
plt = ggplot(data, aes(Diet.Iron, Hemoglobin, color = Sex))
plt = plt + geom_point(alpha = 0.5)
plt

plt = ggplot(data, aes(Diet.Iron, Hemoglobin, color = factor(Ed)))
plt = plt + geom_point(alpha=0.5)
plt

plt = ggplot(data, aes(Diet.Iron, Hemoglobin, color = factor(Race)))
plt = plt + geom_point(alpha=0.2)
plt

# Transferin vs Hemoglobin
plt = ggplot(data, aes(Transferin, Hemoglobin, color = Sex))
plt = plt + geom_point(alpha = 0.5)
plt

plt = ggplot(data, aes(Transferin, Hemoglobin, color = factor(Ed)))
plt = plt + geom_point(alpha=0.5)
plt

plt = ggplot(data, aes(Transferin, Hemoglobin, color = factor(Race)))
plt = plt + geom_point(alpha=0.2)
plt

# 4
head(data)
# omit  NA
data <- na.omit(data)
m1 <- lm(BMI~Cancer.Incidence+Cancer.Death+Age+Smoke+Ed+Race+Weight+Diet.Iron+Albumin+Serum.Iron+TIBC+Transferin+Hemoglobin,data=data) 
summary(m1)

m2 <- step(m1,na.action = na.omit)
summary(m2)

