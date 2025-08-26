packages <- c("pheatmap", "MASS", "dplyr", "tidyverse", "reshape2", "ggplot2")
install.packages(setdiff(packages, rownames(installed.packages())))
#1.Data Preprocessing
data3 <- read.table("data/world_happiness_report_2024.csv", sep=",", header=T)
data_tol= subset(data3, select = -c(upperwhisker,lowerwhisker,Social.support,Dystopia...residual))
na_counts_tol <- colSums(is.na(data_tol))
print(na_counts_tol)

data_tol$Log.GDP.per.capita[is.na(data_tol$Log.GDP.per.capita)] <- median(data_tol$Log.GDP.per.capita, na.rm = TRUE)

data_tol$Healthy.life.expectancy[is.na(data_tol$Healthy.life.expectancy)] <- median(data_tol$Healthy.life.expectancy, na.rm = TRUE)
data_tol$Freedom.to.make.life.choices[is.na(data_tol$Freedom.to.make.life.choices)] <- median(data_tol$Freedom.to.make.life.choices, na.rm = TRUE)
data_tol$Generosity[is.na(data_tol$Generosity)] <- median(data_tol$Generosity, na.rm = TRUE)
data_tol$Perceptions.of.corruption[is.na(data_tol$Perceptions.of.corruption)] <- median(data_tol$Perceptions.of.corruption, na.rm = TRUE)

sum(is.na(data_tol))
data = subset(data_tol, select = -c(Country.name,Regional.indicator))
head(data_tol)
str(data_tol)
glimpse(data_tol)


#2.Data Visualization and Analysis
#2.1 Descriptive Statistical Analysis
summary(data)

#2.2 Scatter Plot
pairs(data,panel = panel.smooth,upper.panel = NULL) 

#2.3 Heatmap Visualization
# Compute correlation matrix
correlation_matrix <- cor(data)
window()
# Convert correlation matrix to long format
correlation_data <- as.data.frame(correlation_matrix) %>%
  mutate(x = rownames(correlation_matrix)) %>%
  melt(id.vars = 'x') %>%
  rename(y = variable, Corr = value)
# Set factor levels for proper ordering
levels_list <- rownames(correlation_matrix)
levels_list <- factor(levels_list, levels = levels_list)
windows()
# Plot heatmap
ggplot(correlation_data, aes(factor(x, levels = levels_list),
                             factor(y, levels = levels_list),
                             fill = Corr)) +
  geom_tile() +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red',
                       limits = c(-1, 1), breaks = c(-1, -0.5, 0, 0.5, 1)) +
  labs(x = NULL, y = NULL) +
  theme_bw(base_size = 15)+
  geom_text(aes(label = round(Corr, 2)), color = "black", size = 3)

#2.4 histogram
ggplot(data_tol, aes(x = reorder(Regional.indicator, Ladder.score, FUN = median), y = Ladder.score)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(title = "distribution of happiness across different continents", x = "continents", y = "Ladder score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#2.5 Density Plots of Key Variables
##data$Ladder.score
mean_life_ladder <- mean(data$Ladder.score)
sd_life_ladder <- sd(data$Ladder.score)
ggplot(data, aes(x = Ladder.score)) + 
  geom_density(color = "black", fill = "steelblue") +
  stat_function(fun = dnorm, args = list(mean = mean_life_ladder, sd = sd_life_ladder), 
                color = "red", linetype = "dashed") +
  labs(x = "Ladder.score", y = "Density") +
  theme_minimal()

#data$Log.GDP.per.capita
mean_life_ladder <- mean(data$Log.GDP.per.capita)
sd_life_ladder <- sd(data$Log.GDP.per.capita)
ggplot(data, aes(x = Log.GDP.per.capita)) + 
  geom_density(color = "black", fill = "steelblue") +
  stat_function(fun = dnorm, args = list(mean = mean_life_ladder, sd = sd_life_ladder), 
                color = "red", linetype = "dashed") +
  labs(x = "Log.GDP.per.capita", y = "Density") +
  theme_minimal()

#data$Healthy.life.expectancy
mean_life_ladder <- mean(data$Healthy.life.expectancy)
sd_life_ladder <- sd(data$Healthy.life.expectancy)
ggplot(data, aes(x = Healthy.life.expectancy)) + 
  geom_density(color = "black", fill = "steelblue") +
  stat_function(fun = dnorm, args = list(mean = mean_life_ladder, sd = sd_life_ladder), 
                color = "red", linetype = "dashed") +
  labs(x = "Healthy.life.expectancy", y = "Density") +
  theme_minimal()

#data$Freedom.to.make.life.choices
mean_life_ladder <- mean(data$Freedom.to.make.life.choices)
sd_life_ladder <- sd(data$Freedom.to.make.life.choices)
ggplot(data, aes(x =Freedom.to.make.life.choices)) + 
  geom_density(color = "black", fill = "steelblue") +
  stat_function(fun = dnorm, args = list(mean = mean_life_ladder, sd = sd_life_ladder), 
                color = "red", linetype = "dashed") +
  labs(x = "Freedom.to.make.life.choices", y = "Density") +
  theme_minimal()

#data$Generosity
mean_life_ladder <- mean(data$Generosity)
sd_life_ladder <- sd(data$Generosity)
ggplot(data, aes(x =Generosity)) + 
  geom_density(color = "black", fill = "steelblue") +
  stat_function(fun = dnorm, args = list(mean = mean_life_ladder, sd = sd_life_ladder), 
                color = "red", linetype = "dashed") +
  labs(x = "Generosity", y = "Density") +
  theme_minimal()

#data$Perceptions.of.corruption
mean_life_ladder <- mean(data$Perceptions.of.corruption)
sd_life_ladder <- sd(data$Perceptions.of.corruption)
ggplot(data, aes(x =Perceptions.of.corruption)) + 
  geom_density(color = "black", fill = "steelblue") +
  stat_function(fun = dnorm, args = list(mean = mean_life_ladder, sd = sd_life_ladder), 
                color = "red", linetype = "dashed") +
  labs(x = "Perceptions.of.corruption", y = "Density") +
  theme_minimal()

#3 Regression
#3.1 simple regression analysis 
# Ladder.score~Log.GDP.per.capita 
fit1<-lm(Ladder.score~Log.GDP.per.capita,data=data)
summary(fit1)
plot(fit1)

plot(data$Log.GDP.per.capita, data$Ladder.score,
     xlab = "Log.GDP.per.capita",
     ylab = "Ladder.score",
     main = "relation between Log.GDP.per.capita and Ladder.score")

abline(fit1, col = "steelblue")
# Ladder.score~Healthy.life.expectancy 
fit2<-lm(Ladder.score~Healthy.life.expectancy,data=data)
summary(fit2)
plot(fit2)
plot(data$Healthy.life.expectancy, data$Ladder.score,
     xlab = "Healthy.life.expectancy",
     ylab = "Ladder.score",
     main = "relation between Healthy.life.expectancy and Ladder.score")

abline(fit2, col = "pink")

# Ladder.score~Freedom.to.make.life.choices 
fit3<-lm(Ladder.score~Freedom.to.make.life.choices,data=data)
summary(fit3)
plot(fit3)
plot(data$Freedom.to.make.life.choices, data$Ladder.score,
     xlab = "Freedom.to.make.life.choices",
     ylab = "Ladder.score",
     main = "relation between Freedom.to.make.life.choices and Ladder.score")
abline(fit3,col = "red")


#3.2  Multiple Linear Regression Model 
fit_choose <-glm(Ladder.score ~ Log.GDP.per.capita+ 
                   Healthy.life.expectancy + Freedom.to.make.life.choices + 
                   Generosity + Perceptions.of.corruption, family = gaussian(link = "identity"), data = data)
summary(fit_choose)
#stepAIC(fit_choose)
backwardAIC <- stepAIC(fit_choose, direction = "backward")

fit_last <- glm(Ladder.score ~ Log.GDP.per.capita + Healthy.life.expectancy + 
                  Freedom.to.make.life.choices + Generosity, 
                family = gaussian(link = "identity"), data = data)

summary(fit_last)
anova(fit_choose, fit_last, test="Chisq")
