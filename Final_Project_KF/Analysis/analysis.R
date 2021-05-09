# 0. Load necessary packages

library(tidyverse)
library(lme4)
library(lmerTest)
library(readxl)
library(Rmisc)
library(plotly)

# 1. Run sampling and data exclusion criteria

# Remove data from participants who did the experiment more than once and who did not complete high school
# 221 and 222: same participant, did the experiment twice
# 195 and 237: had not completed high school

'%ni%' <- Negate('%in%') 

data <- read.csv("data.csv")

data <- data%>%
  filter(subj %ni% c("221", "222", "195", "237"))

# first: Filter out data from participants outside age range (18-30 yo) or that don't have Brazilian Portuguese as a first language

data <- data%>%
  filter(idade >= 18 && idade <= 30)

data <- data%>%
  filter(nativlang == "sim")

# second: Remove all data from participants that answered more than 1/4 of the questions incorrectly
# n = number of words in sentence 

data <- data%>%
  group_by(subj)%>%
  mutate(accuracy = sum(is_correct == 1)/n())

# number of subjects left 

length(unique(data$subj)) #339

data <- data%>%
  filter(accuracy >= 0.750)

# number of subjects left 

length(unique(data$subj)) #337

# third: discarding data from observations in which participants answered the question incorrectly

data <- data%>%
  filter(is_correct == 1)

# number of subjects left 

length(data$subj) #337

# erased data: 6%

# 2. Discard filler data

data <- data%>%
  filter(grepl("prev", type))

# 3. Define 'item_id', 'subj', 'type' as factors so R recognizes them as categorical variables
data$item_id <- as.factor(data$item_id)
data$subj <- as.factor(data$subj)
data$type <- as.factor(data$type)

# 4. Get critical regions for each item from a file called 'critical_regions.csv' 

regions <- read.csv("critical_regions.csv")

# 5. Add columns with the critical regions to the main data file

regions$item_id <- as.factor(regions$item_id)

data <- left_join(data, regions, by = "item_id")

# 6. Organize data by the six regions of interest

rt_art <- data%>%
  filter(region == art)

rt_adj1 <- data%>%
  filter(region == adj1)

rt_conj <- data%>%
  filter(region == conj)

rt_adj2 <- data%>%
  filter(region == adj2)

rt_subst <- data%>%
  filter(region == subst)

rt_spill <- data%>%
  filter(region == spill)

# 7. check data distribution

#article  = not normal, positively skewed 

ggplot(rt_art, aes(x = rt))+
  geom_histogram()

ggplot(rt_art, aes(x = rt))+
  geom_histogram()+
  facet_wrap(~type)

ggplot(rt_art, aes(x = type, y = rt))+
  geom_boxplot()

#adjective 1 = not normal, positively skewed 

ggplot(rt_adj1, aes(x = rt))+
  geom_histogram()

ggplot(rt_adj1, aes(x = rt))+
  geom_histogram()+
  facet_wrap(~type)

ggplot(rt_adj1, aes(x = type, y = rt))+
  geom_boxplot()

#conjunction = not normal, positively skewed 

ggplot(rt_conj, aes(x = rt))+
  geom_histogram()

ggplot(rt_conj, aes(x = rt))+
  geom_histogram()+
  facet_wrap(~type)

ggplot(rt_conj, aes(x = type, y = rt))+
  geom_boxplot()

#adjective 2 = not normal, positively skewed 

ggplot(rt_adj2, aes(x = rt))+
  geom_histogram()

ggplot(rt_adj2, aes(x = rt))+
  geom_histogram()+
  facet_wrap(~type)

ggplot(rt_adj2, aes(x = type, y = rt))+
  geom_boxplot()

#noun = not normal, positively skewed 

ggplot(rt_subst, aes(x = rt))+
  geom_histogram()

ggplot(rt_subst, aes(x = rt))+
  geom_histogram()+
  facet_wrap(~type)

ggplot(rt_subst, aes(x = type, y = rt))+
  geom_boxplot()

#spillover = not normal, positively skewed 

ggplot(rt_spill, aes(x = rt))+
  geom_histogram()

ggplot(rt_spill, aes(x = rt))+
  geom_histogram()+
  facet_wrap(~type)

ggplot(rt_spill, aes(x = type, y = rt))+
  geom_boxplot()  

#Descriptive statistics 

rt_art%>%
  group_by(type)%>%
  summarise(mean = mean(rt), median = median(rt))

rt_adj1%>%
  group_by(type)%>%
  summarise(mean = mean(rt), median = median(rt))

rt_conj%>%
  group_by(type)%>%
  summarise(mean = mean(rt), median = median(rt))  

rt_adj2%>%
  group_by(type)%>%
  summarise(mean = mean(rt), median = median(rt)) 

rt_subst%>%
  group_by(type)%>%
  summarise(mean = mean(rt), median = median(rt))

rt_spill%>%
  group_by(type)%>%
  summarise(mean = mean(rt), median = median(rt))

# 8. Eliminate outliers (any observation > 2500 ms or less than 100 ms) 

rt_art <- rt_art%>%
  filter(rt <= 2500 & rt >= 100) #6375

rt_adj1 <- rt_adj1%>%
  filter(rt <= 2500 & rt >= 100) #6363

rt_conj <- rt_conj%>%
  filter(rt <= 2500 & rt >= 100) #6365

rt_adj2 <- rt_adj2%>%
  filter(rt <= 2500 & rt >= 100) #6363

rt_subst <- rt_subst%>%
  filter(rt <= 2500 & rt >= 100) #6370

rt_spill <- rt_spill%>%
  filter(rt <= 2500 & rt >= 100) #6375

# 9. Log-transform data so it falls into a more normal distribution

rt_art <- rt_art%>%
  mutate(rt.log = log(rt))

rt_adj1 <- rt_adj1%>%
  mutate(rt.log = log(rt))

rt_conj <- rt_conj%>%
  mutate(rt.log = log(rt))

rt_adj2 <- rt_adj2%>%
  mutate(rt.log = log(rt))

rt_subst <- rt_subst%>%
  mutate(rt.log = log(rt))

rt_spill <- rt_spill%>%
  mutate(rt.log = log(rt))

# 10. Check log transformed distributions 

# histograms = more normal distribution 

ggplot(rt_art, aes(x = rt.log))+
  geom_histogram()

ggplot(rt_adj1, aes(x = rt.log))+
  geom_histogram()

ggplot(rt_conj, aes(x = rt.log))+
  geom_histogram()

ggplot(rt_adj2, aes(x = rt.log))+
  geom_histogram()

ggplot(rt_subst, aes(x = rt.log))+
  geom_histogram()

ggplot(rt_spill, aes(x = rt.log))+
  geom_histogram()

# boxplots = more normal distribution 

ggplot(rt_art, aes(x = type, y = rt.log))+
  geom_boxplot()  

ggplot(rt_adj1, aes(x = type, y = rt.log))+
  geom_boxplot() 

ggplot(rt_adj2, aes(x = type, y = rt.log))+
  geom_boxplot()

ggplot(rt_conj, aes(x = type, y = rt.log))+
  geom_boxplot()

ggplot(rt_subst, aes(x = type, y = rt.log))+
  geom_boxplot()

ggplot(rt_spill, aes(x = type, y = rt.log))+
  geom_boxplot()

# 11. Descriptive statistics after outlier removal (mean and median are slightly closer to each other)

rt_art%>%
  group_by(type)%>%
  summarise(mean = mean(rt), median = median(rt))

rt_adj1%>%
  group_by(type)%>%
  summarise(mean = mean(rt), median = median(rt))

rt_conj%>%
  group_by(type)%>%
  summarise(mean = mean(rt), median = median(rt))  

rt_adj2%>%
  group_by(type)%>%
  summarise(mean = mean(rt), median = median(rt)) 

rt_subst%>%
  group_by(type)%>%
  summarise(mean = mean(rt), median = median(rt))

rt_spill%>%
  group_by(type)%>%
  summarise(mean = mean(rt), median = median(rt))

# 12. adjusting models (models that have been commented out are the ones that failed to converge or that were not needed)

# article

#model.art <- lmer(rt.log ~ type + (1+type|subj) + (1+type|item_id), data = rt_art, REML = FALSE)
#model.art <- lmer(rt.log ~ type + (1+type|subj) + (1|item_id), data = rt_art, REML = FALSE)
model.art <- lmer(rt.log ~ type + (1|subj) + (1|item_id), data = rt_art, REML = FALSE)

summary(model.art) # no significant p value

# adjective 1

#model.adj1 <- lmer(rt.log ~ type + (1+type|subj) + (1+type|item_id), data = rt_adj1, REML = FALSE)
#model.adj1 <- lmer(rt.log ~ type + (1+type|subj) + (1|item_id), data = rt_adj1, REML = FALSE)
model.adj1 <- lmer(rt.log ~ type + (1|subj) + (1|item_id), data = rt_adj1, REML = FALSE)

summary(model.adj1) # no significant p value

# conjunction

model.conj <- lmer(rt.log ~ type + (1+type|subj) + (1+type|item_id), data = rt_conj, REML = FALSE)
#model.conj <- lmer(rt.log ~ type + (1|subj) + (1+type|item_id), data = rt_conj)
#model.conj <- lmer(rt.log ~ type + (1|subj) + (1|item_id), data = rt_conj)

summary(model.conj) # no significant p value

# adjective 2

model.adj2 <- lmer(rt.log ~ type + (1+type|subj) + (1+type|item_id), data = rt_adj2, REML = FALSE)
#model.adj2 <- lmer(rt.log ~ type + (1|subj) + (1+type|item_id), data = rt_adj2)
#model.adj2 <- lmer(rt.log ~ type + (1|subj) + (1|item_id), data = rt_adj2)

summary(model.adj2) # no significant p value

# noun

model.subst <- lmer(rt.log ~ type + (1+type|subj) + (1+type|item_id), data = rt_subst, REML = FALSE)
#model.subst <- lmer(rt.log ~ type + (1|subj) + (1+type|item_id), data = rt_subst)
#model.subst <- lmer(rt.log ~ type + (1|subj) + (1|item_id), data = rt_subst)

summary(model.subst)

# Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method [
# lmerModLmerTest]
# Formula: rt.log ~ type + (1 + type | subj) + (1 + type | item_id)
#    Data: rt_subst
# 
#      AIC      BIC   logLik deviance df.resid 
#   2991.6   3052.4  -1486.8   2973.6     6361 
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -3.0351 -0.5967 -0.1190  0.4381  6.3202 
# 
# Random effects:
#  Groups   Name         Variance  Std.Dev. Corr 
#  subj     (Intercept)  0.0647169 0.25440       
#           typeprev.sim 0.0028745 0.05361  -0.65
#  item_id  (Intercept)  0.0026544 0.05152       
#           typeprev.sim 0.0006014 0.02452  -0.07
#  Residual              0.0799358 0.28273       
# Number of obs: 6370, groups:  subj, 337; item_id, 20
# 
# Fixed effects:
#               Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)   6.059033   0.018723 84.927688 323.618  < 2e-16 ***
# typeprev.sim -0.040696   0.009506 22.315328  -4.281 0.000296 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr)
# typeprev.sm -0.319

# spillover region

model.spill <- lmer(rt.log ~ type + (1+type|subj) + (1+type|item_id), data = rt_spill, REML = FALSE)
#model.spill <- lmer(rt.log ~ type + (1|subj) + (1+type|item_id), data = rt_spill)
#model.spill <- lmer(rt.log ~ type + (1|subj) + (1|item_id), data = rt_spill)

summary(model.spill)

# Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method [
# lmerModLmerTest]
# Formula: rt.log ~ type + (1 + type | subj) + (1 + type | item_id)
#    Data: rt_spill
# 
#      AIC      BIC   logLik deviance df.resid 
#   1798.6   1859.4   -890.3   1780.6     6366 
# 
# Scaled residuals: 
#     Min      1Q  Median      3Q     Max 
# -4.6765 -0.5854 -0.1192  0.4263  5.7607 
# 
# Random effects:
#  Groups   Name         Variance Std.Dev. Corr 
#  subj     (Intercept)  0.040568 0.20141       
#           typeprev.sim 0.001004 0.03169  -0.24
#  item_id  (Intercept)  0.004803 0.06931       
#           typeprev.sim 0.002433 0.04932  -0.94
#  Residual              0.067006 0.25885       
# Number of obs: 6375, groups:  subj, 337; item_id, 20
# 
# Fixed effects:
#              Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)   6.05962    0.01956 41.84481 309.869  < 2e-16 ***
# typeprev.sim -0.05685    0.01296 20.46425  -4.387 0.000272 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#             (Intr)
# typeprev.sm -0.742


# 13. Check models' residuals

# art

adjusted.art = fitted(model.art)
residuals.art  = residuals(model.art)

aj.residuals.art = data.frame(adjusted.art, residuals.art)

ggplot(aj.residuals.art, aes(adjusted.art, residuals.art))+
  geom_point(size=3)

ggplot(aj.residuals.art, aes(sample = adjusted.art))+
  stat_qq()+
  stat_qq_line() # normal

# adj1

adjusted.adj1 = fitted(model.adj1)
residuals.adj1  = residuals(model.adj1)

aj.residuals.adj1 = data.frame(adjusted.adj1, residuals.adj1)

ggplot(aj.residuals.adj1, aes(adjusted.adj1, residuals.adj1))+
  geom_point(size=3)

ggplot(aj.residuals.adj1, aes(sample = adjusted.adj1))+
  stat_qq()+
  stat_qq_line() # normal

# conj

adjusted.conj = fitted(model.conj)
residuals.conj  = residuals(model.conj)

aj.residuals.conj = data.frame(adjusted.conj, residuals.conj)

ggplot(aj.residuals.conj, aes(adjusted.conj, residuals.conj))+
  geom_point(size=3)

ggplot(aj.residuals.conj, aes(sample = adjusted.conj))+
  stat_qq()+
  stat_qq_line() # normal

# adj2

adjusted.adj2 = fitted(model.adj2)
residuals.adj2  = residuals(model.adj2)

aj.residuals.adj2 = data.frame(adjusted.adj2, residuals.adj2)

ggplot(aj.residuals.adj2, aes(adjusted.adj2, residuals.adj2))+
  geom_point(size=3)

ggplot(aj.residuals.adj2, aes(sample = adjusted.adj2))+
  stat_qq()+
  stat_qq_line() # normal

# subst

adjusted.subst = fitted(model.subst)
residuals.subst  = residuals(model.subst)

aj.residuals.subst = data.frame(adjusted.subst, residuals.subst)

ggplot(aj.residuals.subst, aes(adjusted.subst, residuals.subst))+
  geom_point(size=3)

ggplot(aj.residuals.subst, aes(sample = adjusted.subst))+
  stat_qq()+
  stat_qq_line() # normal

# spill

adjusted.spill = fitted(model.spill)
residuals.spill  = residuals(model.spill)

aj.residuals.spill = data.frame(adjusted.spill, residuals.spill)

ggplot(aj.residuals.spill, aes(adjusted.spill, residuals.spill))+
  geom_point(size=3)

ggplot(aj.residuals.spill, aes(sample = adjusted.spill))+
  stat_qq()+
  stat_qq_line() # normal

# 14. Adjust p-values bonferroni correction for multiple analyses 

p.values <- c(0.605, 0.0868, 0.606, 0.636, 0.000296, 0.000272)
# Adjusted p values [1] 1.000000 0.520800 1.000000 1.000000 0.001776 0.001632

# 15. Analyze original data with glmer(family = Gamma()) - models that have been commented out did not converge or were not necessary

# article

data$type <- as.factor(data$type)

#model.art.glm <- glmer(rt ~ type + (1+type|subj) + (1+type|item_id), data = rt_art, family = Gamma(link = "identity"))
#model.art.glm <- glmer(rt ~ type + (1+type|subj) + (1|item_id), data = rt_art, family = Gamma(link = "identity"))
model.art.glm <- glmer(rt ~ type + (1|subj) + (1|item_id), data = rt_art, family = Gamma(link = "identity")) # still failed to converge

summary(model.art.glm) # no significant p value 

# adjective 2

#model.adj1.glm <- glmer(rt ~ type + (1+type|subj) + (1+type|item_id), data = rt_adj1, family = Gamma(link = "identity"))
#model.adj1.glm <- glmer(rt ~ type + (1+type|subj) + (1|item_id), data = rt_adj1, family = Gamma(link = "identity"))
model.adj1.glm <- glmer(rt ~ type + (1|subj) + (1|item_id), data = rt_adj1, family = Gamma(link = "identity"))

summary(model.adj1.glm) # no significant p value 

# conjunction

model.conj.glm <- glmer(rt ~ type + (1+type|subj) + (1+type|item_id), data = rt_conj, family = Gamma(link = "identity"))
#model.conj.glm <- glmer(rt ~ type + (1+type|subj) + (1|item_id), data = rt_adj1, family = Gamma(link = "identity"))
#model.conj.glm <- glmer(rt ~ type + (1|subj) + (1|item_id), data = rt_adj1, family = Gamma(link = "identity"))

summary(model.conj.glm)

# Generalized linear mixed model fit by maximum likelihood (Laplace
#                                                           Approximation) [glmerMod]
# Family: Gamma  ( identity )
# Formula: rt ~ type + (1 + type | subj) + (1 + type | item_id)
# Data: rt_conj
# 
# AIC      BIC   logLik deviance df.resid 
# 80858.1  80918.9 -40420.0  80840.1     6356 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.7892 -0.4952 -0.1927  0.2175 10.0373 
# 
# Random effects:
#   Groups   Name         Variance  Std.Dev. Corr 
# subj     (Intercept)  5944.6818 77.1018       
# typeprev.sim 4537.8907 67.3639  -0.41
# item_id  (Intercept)   992.5132 31.5042       
# typeprev.sim  684.8586 26.1698  -0.68
# Residual                 0.1564  0.3955       
# Number of obs: 6365, groups:  subj, 337; item_id, 20
# 
# Fixed effects:
#   Estimate Std. Error t value Pr(>|z|)    
# (Intercept)   473.022      4.621 102.367   <2e-16 ***
#   typeprev.sim   -9.830      4.570  -2.151   0.0315 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr)
# typeprev.sm -0.053

# noun

#model.subst.glm <- glmer(rt ~ type + (1+type|subj) + (1+type|item_id), data = rt_subst, family = Gamma(link = "identity"))
#model.subst.glm <- glmer(rt ~ type + (1+type|subj) + (1|item_id), data = rt_subst, family = Gamma(link = "identity"))
model.subst.glm <- glmer(rt ~ type + (1|subj) + (1|item_id), data = rt_subst, family = Gamma(link = "identity"))

summary(model.subst.glm)

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [
#   glmerMod]
# Family: Gamma  ( identity )
# Formula: rt ~ type + (1 | subj) + (1 | item_id)
# Data: rt_subst
# 
# AIC      BIC   logLik deviance df.resid 
# 80405.5  80439.3 -40197.7  80395.5     6365 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -1.7801 -0.5283 -0.1946  0.2516 11.3062 
# 
# Random effects:
#   Groups   Name        Variance Std.Dev.
# subj     (Intercept) 7237.818 85.0754 
# item_id  (Intercept)  290.129 17.0332 
# Residual                0.135  0.3675 
# Number of obs: 6370, groups:  subj, 337; item_id, 20
# 
# Fixed effects:
#   Estimate Std. Error t value Pr(>|z|)    
# (Intercept)   489.687      4.135 118.425  < 2e-16 ***
#   typeprev.sim  -17.442      2.721  -6.411 1.45e-10 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr)
# typeprev.sm -0.023

# spillover region (adjust as necessary)

#model.spill.glm <- glmer(rt ~ type + (1+type|subj) + (1+type|item_id), data = rt_spill, family = Gamma(link = "identity"))
#model.spill.glm <- glmer(rt ~ type + (1+type|subj) + (1|item_id), data = rt_spill, family = Gamma(link = "identity"))
model.spill.glm <- glmer(rt ~ type + (1|subj) + (1|item_id), data = rt_spill, family = Gamma(link = "identity"))

summary(model.spill.glm)

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [
#   glmerMod]
# Family: Gamma  ( identity )
# Formula: rt ~ type + (1 | subj) + (1 | item_id)
# Data: rt_spill
# 
# AIC      BIC   logLik deviance df.resid 
# 79071.2  79105.0 -39530.6  79061.2     6370 
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -2.2289 -0.5430 -0.1951  0.2541  9.3149 
# 
# Random effects:
#   Groups   Name        Variance  Std.Dev.
# subj     (Intercept) 4470.1481 66.8592 
# item_id  (Intercept)  231.0738 15.2011 
# Residual                0.1051  0.3242 
# Number of obs: 6375, groups:  subj, 337; item_id, 20
# 
# Fixed effects:
#   Estimate Std. Error t value Pr(>|z|)    
# (Intercept)   475.440      4.318 110.102   <2e-16 ***
#   typeprev.sim  -24.161      2.647  -9.128   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr)
# typeprev.sm 0.015 

# adjusting p-values for the glmer() models
p.values.glm <- c(0.436, 0.0639, 0.0315, 0.555, 1.45e-10, 2e-16)

p.adjust(p.values.glm, method = "bonferroni")
# Adjusted p values: 1, 0.3834, 0.1890, 1, 0.0000000008700, 0.0000000000000002

# 16. Create graphs 

#### creating plot dataset 

plot.art <- rt_art %>% select(subj, type, item_id, rt) %>% dplyr::rename(a = rt)

plot.adj1 <- rt_adj1 %>% select(subj, type, item_id, rt) %>% dplyr::rename(b = rt)

plot.conj <- rt_conj %>% select(subj, type, item_id, rt) %>% dplyr::rename(c = rt)

plot.adj2 <- rt_adj2 %>% select(subj, type, item_id, rt) %>% dplyr::rename(d = rt)

plot.subst <- rt_subst %>% select(subj, type, item_id, rt) %>% dplyr::rename(e = rt)

plot.spill <- rt_spill %>% select(subj, type, item_id, rt) %>% dplyr::rename(f = rt)


## merge, more columns = wide data
plot.data <- merge(merge(merge(merge(merge(plot.art, plot.adj1, by = 1:3), plot.conj, by = 1:3), plot.adj2, by = 1:3), plot.subst, by = 1:3), plot.spill, by = 1:3, all = TRUE)

##reshape, fewer columns = long data, restructured to make graph 

plot.data <- plot.data %>% gather(region, rt, a, b, c, d, e, f)

### extracting within-subject standard-errors
# circle is the mean 

## 1) aggregate data
# each line represents a unique combination 

dat_agg <- plot.data %>% 
  dplyr::group_by(subj, region, type) %>% 
  dplyr::summarise(mean_agg = mean(rt, na.rm = TRUE))

dat_agg2 <- plot.data %>% 
  dplyr::group_by(subj, region, type) %>% 
  dplyr::summarise(mean_agg = mean(log(rt), na.rm = TRUE))

library(plotly) #to see descriptions on boxplots 

# Figure 3
dat_agg %>%
  ggplot(aes(y = mean_agg, x = region, linetype=type)) +
  geom_boxplot() + 
  labs(x = "Sentence region", y = "Reaction time (ms)") + 
  scale_linetype_discrete(name = "Condition",
                          labels = c("predictable noun", "predictable noun"))+
  scale_x_discrete(labels = c("det", "adj1", "conj", "adj2", "noun", "spill"))+
  theme_bw()+
  theme(legend.position= "bottom")

# Figure 1

dat_agg2 %>%
  ggplot(aes(y = mean_agg, x = region, linetype=type)) + 
  geom_boxplot() + 
  labs(x = "Sentence region", y = "Reaction time (ms)") +
  scale_linetype_discrete(name ="Condition",  labels = c("predictable noun", "predictable noun"))+
  scale_x_discrete(labels = c("det", "adj1", "conj", "adj2", "noun", "spill"))+
  theme_bw()+
  theme(legend.position= "bottom")

# 2) apply summarySEwithin() from Rmisc package

dat_summary <- summarySEwithin(dat_agg,
                               measurevar = "mean_agg",
                               withinvars = c("region", "type"),
                               idvar = c("subj"))
#Get the standard error to create the plot 

dat_summary

### plot
# 61 to 62 creates the error bar
# 63 connects the points

pd <- position_dodge(0.1) # move them .05 to the left and right (to avoid overlap)

# Figure 4
ggplot(dat_summary, aes(x=region, y=mean_agg, linetype=type, group=type)) + 
  geom_errorbar(aes(ymin=mean_agg-se, ymax=mean_agg+se), colour="black", width=.1, position=pd, linetype = 1) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
  labs(x = "Sentence region", y = "Reaction time (ms)") +
  scale_linetype_discrete(name = "Condition",
                          labels = c("predictable noun", "predictable noun"))+
  scale_x_discrete(labels = c("det", "adj1", "conj", "adj2", "noun", "spill"))+
  theme_bw()+
  theme(legend.position= "bottom")

# Figure 2

dat_summary2 <- summarySEwithin(dat_agg2,
                                measurevar = "mean_agg",
                                withinvars = c("region", "type"),
                                idvar = c("subj"))

ggplot(dat_summary2, aes(x=region, y=mean_agg, linetype=type, group=type)) + 
  geom_errorbar(aes(ymin=mean_agg-se, ymax=mean_agg+se), colour="black", width=.1, position=pd, linetype = 1) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
  labs(x = "Sentence region", y = "Reaction time (ms)") +
  scale_linetype_discrete(name = "Condition",
                          labels = c("predictable noun", "predictable noun"))+
  scale_x_discrete(labels = c("det", "adj1", "conj", "adj2", "noun", "spill"))+
  theme_bw()+
  theme(legend.position= "bottom")

