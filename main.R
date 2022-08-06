install.packages('dplyr')
install.packages('ggplot2')
install.packages("effsize")
library(dplyr)
library(ggplot2)
library(effsize)


##################
# Data cleaning  #
##################
data %>% count(adhd)

data <- read.csv("adhd_repo_data.csv")
data <- data %>%
  select(adhd, starts_with("risk")) %>%
  mutate("adhd" = as.factor(adhd))





data$mean_risk_gain <- rowMeans(select(data, starts_with("risk_g")), na.rm = T)
data$mean_risk_loss <- rowMeans(select(data, starts_with("risk_l")), na.rm = T)
data$diff_risk_gain_loss <- data$mean_risk_gain - data$mean_risk_loss
mean_control <- mean(data[data$adhd == 1,"diff_risk_gain_loss"])
mean_test <- mean(data[data$adhd == 0,"diff_risk_gain_loss"],na.rm = T)
n1 <- 120
n2 <- 50
s1 <- var(data[data$adhd == 1,"diff_risk_gain_loss"])
s2 <- var(data[data$adhd == 0,"diff_risk_gain_loss"])
s_p_squre <- ((n1-1)*s1 + (n2-1) * s2 ) / (n1 + n2 - 2)
s_p <- sqrt(s_p_squre)
t_r <-  (mean_control - mean_test) / (s_p * sqrt( 1 /n1 + 1/ n2))
is.nan(data$mean_risk_loss)
res <- data[is.nan(data$mean_risk_loss),]
p <- ggplot(data, mapping = aes(x = adhd, y = diff_risk_gain_loss))
p +
  geom_violin(draw_quantiles = 0.5) +
  geom_jitter(alpha = .15) +
  labs(y = "difference between gain and lost domain", title = "Box plot of IQ of Combined and Regular class")
t.test(x = data[data$adhd == 1, "diff_risk_gain_loss"],
       y = data[data$adhd == 0, "diff_risk_gain_loss"],
       alternative = "two.sided",
       paired = F,
       var.equal = T,
        )
data <- na.omit(data)

cohen.d(data$diff_risk_gain_loss,data$adhd,within = F,na.rm = T)

sum(data$mean_risk_gain - data$mean_risk_loss)
data %>%
  group_by(adhd) %>%
  summarise(Total = mean(mean_risk_gain))

group_by(data, adhd)
col_to_sum <- data %>% select(starts_with("risk_g"))
data %>% summarise(Freq = sum(starts_with("risk_g")))
# For decisions involving risks, we used a set of incentivized binary choices between a lottery and a safe amount of money.
# The lottery was identical but the safe amount varied systematically across trials71.
# Subjects made risky choices in both the gain domain and in the loss domain.
# In the gain domain, subjects made four choices between a 50/50-gamble (lottery) with the chance of gaining SEK 100 (appr. $10) and a certain gain that varied between SEK 35 and 50.
# The decisions in the loss domain were additive inverses of the decisions in the gain domain, meaning that subjects made four choices between a lottery with a 50/50-chance of losing SEK 100 and a certain loss that varied between SEK 35 and 50.
# The order of trials was randomized within each domain, and the gain domain was always elicited before the loss domain.
# Our main dependent variable (prop. risky choices) for each domain was calculated as the proportion of trials where the participant chose the lottery over the safe amount.
# TODO: effect size, mann witney, boot strap. 

t.test(x = data[data$adhd == 1, "Flexibility"],
       y = data[data$Group == "Swimmer" & data$Side == "Left", "Flexibility"],
       alternative = "two.sided",
       paired = F)
data %>%
  mutate(value = 1) %>%
  spread(age, value, fill = 0)
data %>%
  summary(data)
sum(data$adhd == 0)
sum(data$)
glimpse(data)
tbl_df(data = data)
# a.
partial_data <- head(data, n = 25) %>% select(c(quality, citric.acid))

# b.
for (i in 1:5) {
partial_data <- partial_data %>% mutate("citric.acid.power.{i}" := partial_data$citric.acid^i)
}

# c.
lm_list <- list()

lm_list[[1]] <- lm(quality ~ citric.acid.power.1, partial_data)
lm_list[[2]] <- lm(quality ~ citric.acid.power.1 +
citric.acid.power.2, partial_data)
lm_list[[3]] <- lm(quality ~ citric.acid.power.1 +
citric.acid.power.2 +
citric.acid.power.3, partial_data)
lm_list[[4]] <- lm(quality ~ citric.acid.power.1 +
citric.acid.power.2 +
citric.acid.power.3 +
citric.acid.power.4, partial_data)
lm_list[[5]] <- lm(quality ~ citric.acid.power.1 +
citric.acid.power.2 +
citric.acid.power.3 +
citric.acid.power.4 +
citric.acid.power.5, partial_data)

for (lm_ in lm_list) {
print(summary(lm_))

# d.
print(sum((predict(lm_) - partial_data$quality)^2))
}

# e.
for (i in 1:5) {
print(ggplot(partial_data, aes(citric.acid, quality)) +
geom_point() +
geom_smooth(method = "lm", formula = y ~ poly(x, i), se = F) +
labs(x = "citric acid", y = "quality", title = paste("Quality as function of citric acid up to power", i)))
}

################################
# Question 2 #
################################

white_wines <- data[data$type == "white",] %>% select(quality, alcohol)
red_wines <- data[data$type == "red",] %>% select(quality, alcohol)

# a.
ggplot(white_wines, aes(x = alcohol, y = quality)) +
geom_point() +
labs(x = "alcohol", y = "quality", title = "Quality as function of alcohol in white wines")
ggplot(red_wines, aes(x = alcohol, y = quality)) +
geom_point() +
labs(x = "alcohol", y = "quality", title = "Quality as function of alcohol in red wines")

# b.
cor.test(white_wines$alcohol, white_wines$quality, method = "spearman")
cor.test(red_wines$alcohol, red_wines$quality, method = "spearman")

# c.d.e.f
# White wine
sp_bootstrap_distribution <- c()
for (i in seq_len(10000)){
resample_index <- sample(seq_len(nrow(white_wines)), size = nrow(white_wines), replace = T)
resample <- slice(white_wines, resample_index)
sp_bootstrap_distribution[i] <- cor(resample$alcohol, resample$quality, method = "spearman")
}
CI_lower_white <- quantile(sp_bootstrap_distribution, 0.025)
CI_upper_white <- quantile(sp_bootstrap_distribution, 0.975)

hist(sp_bootstrap_distribution, xlim = c(0.30, 0.45) ,
xlab = "Spearman's rank correlation coefficient",
main = "Bootstrap distribution of Spearman correlation of white wines quality and alcohol pracentage")

abline(v = CI_lower_white, col = "blue", lwd = 2)
abline(v = CI_upper_white, col = "blue", lwd = 2)

# Red whine
sp_bootstrap_distribution <- c()
for (i in seq_len(10000)){
resample_index <- sample(seq_len(nrow(red_wines)), size = nrow(red_wines), replace = T)
resample <- slice(red_wines, resample_index)
sp_bootstrap_distribution[i] <- cor(resample$alcohol, resample$quality, method = "spearman")
}
CI_lower_red <- quantile(sp_bootstrap_distribution, 0.025)
CI_upper_red <- quantile(sp_bootstrap_distribution, 0.975)
hist(sp_bootstrap_distribution, xlim = c(0.30, 0.45),
xlab = "Spearman's rank correlation coefficient",
main = "Bootstrap distribution of Spearman correlation of red wines quality and alcohol pracentage")
abline(v = CI_lower_red, col = "blue", lwd = 2)
abline(v = CI_upper_red, col = "blue", lwd = 2)

# g. (bonus)
white_wines_sp <- cor(white_wines$alcohol, white_wines$quality, method = "spearman")
red_wines_sp <- cor(red_wines$alcohol, red_wines$quality, method = "spearman")
sp_data <- c(white_wines_sp, red_wines_sp)
sp_origin_data <- data.frame(sp_data)
sp_origin_data$type <- as.factor(c("white", "red"))
sp_origin_data$CI_lower <- c(CI_lower_white, CI_lower_red)
sp_origin_data$CI_upper <- c(CI_upper_white, CI_upper_red)

ggplot(sp_origin_data, aes(x = type, y = sp_data)) +
theme_bw()+
geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.3) +
geom_point(size = 4, color = "darkred") +
labs(x = "wine type", y = "Spearman correlation",
caption = "Error bars are 95% CI",
title = "Spearman correlation of white and red whines of quality and alchol pracentage")

################################
# Question 3 #
################################

# a.
ggplot(data = data, aes(x = type, y = quality)) +
geom_violin(draw_quantiles = 0.5)+
geom_jitter(alpha = .15) +
labs(x = "wine type", y = "quality", title = "Quality of white and red wine")

white_wines <- data[data$type == "white", "quality"]
red_wines <- data[data$type =="red", "quality"]

# b.
wilcox.test(white_wines, red_wines, correct = FALSE)

# c.
n1 <- length(white_wines)
n2 <- length(red_wines)
ranks <- rank(c(white_wines, red_wines))
R1 <- sum(ranks[1:n1])
R2 <- sum(ranks[n1 + (1:n2)])
U1 <- n1 * n2 + n1 * (n1 + 1)/ 2 - R1
U2 <- n1 * n2 +n2 *(n2 + 1)/ 2 - R2
print(paste("U1:", U1, "U2:", U2))
# d.
rbc <- abs((U1 - U2) / (n1 * n2))
print(paste("RBC:", rbc))

