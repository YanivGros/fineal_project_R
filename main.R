install.packages('dplyr')
install.packages('ggplot2')
install.packages("effsize")
install.packages("afex")

library(dplyr)
library(ggplot2)
library(effsize)
library(afex)

##################
# Data cleaning  #
##################

data_raw <- read.csv("adhd_repo_data.csv")
data <- data_raw %>%
  select(adhd, starts_with("risk")) %>%
  mutate("adhd" = as.factor(adhd))
data$mean_risk_gain <- rowMeans(select(data, starts_with("risk_g")), na.rm = T)
data$mean_risk_loss <- rowMeans(select(data, starts_with("risk_l")), na.rm = T)
data$diff_risk_gain_loss <- data$mean_risk_gain - data$mean_risk_loss
p <- ggplot(data, mapping = aes(x = adhd, y = diff_risk_gain_loss))
p +
  geom_violin(draw_quantiles = 0.5) +
  geom_jitter(alpha = .15) +
  labs(y = "difference between gain and lost domain", title = "Violin plot difference between decision involving risk in gain and lost domain of adhd and control group")

data <- data %>% arrange(diff_risk_gain_loss, adhd)
data$ind <- seq_along(data$mean_risk_loss)
p <- ggplot(data, mapping = aes(x = ind, y = diff_risk_gain_loss, color = adhd))
p +
  geom_point() +
  labs(y = "difference between gain and lost domain", x = "subject",
       title = "Scatter plot of difference between decision involving risk in gain and lost domain of adhd and control group")

t.test(x = data[data$adhd == 1, "diff_risk_gain_loss"],
       y = data[data$adhd == 0, "diff_risk_gain_loss"],
       alternative = "two.sided",
       paired = F,
       var.equal = T
)
cohen.d(data$diff_risk_gain_loss, data$adhd, paired = F, na.rm = T)

########################
# Further Exploration  #
########################

data <- data_raw %>%
  select(adhd,age,female,meanAltruans,meanmoralAccept,GmeanRiskans,LmeanRiskans,meanDiscSoon,CRT_nCorr) %>%
  na.omit()
data$id <- seq_along(data$adhd)
data$Rdiff <- data$GmeanRiskans - data$LmeanRiskans

wilcox.test(x = data[data$adhd == 1, "Rdiff"],
            y = data[data$adhd == 0, "Rdiff"])

m <- t.test(x = data[data$female == 1, "Rdiff"],
       y = data[data$female== 0, "Rdiff"],
       alternative = "two.sided",
       paired = F,
       var.equal = T)
mean(m$statistic)
aov_ez(data = data,
       id = "id",
       between = "age",
       dv = "Rdiff",
       anova_table = list(es = "pes"))

sp_bootstrap_distribution <- c()
nr <- nrow(data)
num_itr <- 10000
for (i in seq_len(num_itr)) {
  resample_index <- sample(seq_len(nr), size = nr, replace = T)
  resample <- slice(data, resample_index)
  sp_bootstrap_distribution[i] <- t.test(resample[resample$adhd == 1, "meanDiscSoon"],resample[resample$adhd == 0, "meanDiscSoon"],paired = F)$statistic
}
CI_lower_t <- quantile(sp_bootstrap_distribution, 0.025)
CI_upper_t <- quantile(sp_bootstrap_distribution, 0.975)
real_t <- t.test(data[data$adhd == 1, "meanDiscSoon"],data[data$adhd == 0, "meanDiscSoon"],paired = F,var.equal = T)$statistic
hist(sp_bootstrap_distribution,xlim = ,
     xlab = "Spearman's rank correlation coefficient",
     main = "Bootstrap distribution of the statistic t")
abline(v = list(CI_upper_t,CI_lower_t), col = "blue", lwd = 2)
abline(v = real_t , col = "red", lwd = 2)
cor(data$Rdiff,data$meanDiscSoon,method="pearson")
m <- lm(adhd ~ Rdiff + meanDiscSoon, data)
summary(m)
