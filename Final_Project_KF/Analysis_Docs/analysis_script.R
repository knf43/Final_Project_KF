##############################################
######## SPR 2020 - run on ibexfarm ##########
############## data analysis #################
##############################################

#----------------------------------------------------------------------------- SUMMARY -----------------------------------------------------------------------------------

# 0. loading necessary packages
# 1. running exclusion criteria
# 2. discarding filler data
# 3. defining factors and dropping discarded levels
# 4. getting critical regions
# 5. adding columns with the critical regions to the main df
# 6. organizing data by regions of interest
# 7. checking data distribution
# 8. eliminating outliers
# 9. rechecking distributions
# 10. performing log transformations on the data
# 11. checking log transformed distributions
# 12. descriptive statistics
# 13. adjusting models
# 14. adjusting p-values
# 15. corrective analysis for the spillover region
# 16. additional/exploratory analyses

#-------------------------------------------------------------------------- END OF SUMMARY -------------------------------------------------------------------------------

# 0. loading necessary packages

library(tidyverse)
library(lme4)
library(lmerTest)
library(readxl)
str(data)

# 1. running sampling and data exclusion criteria

# edit (June 24, 2020): removing data from participants who did the experiment more than once and who did not complete high school

'%ni%' <- Negate('%in%') # declaring function used in the filter below

data <- read.csv("./Project_Docs/data.csv")

data <- data%>%
  filter(subj %ni% c("221", "222", "195", "237"))
# 221 and 222: same participant, did the experiment twice
# 195 and 237: had not completed high school

# first: filtering out data from participants outside age range (18-30 yo) or that don't have Brazilian Portuguese as a first language

data <- data%>%
  filter(idade >= 18 && idade <= 30)

data <- data%>%
  filter(nativlang == "sim")

# second: removing all data from participants that answered more than 1/4 of the questions incorrectly

data <- data%>%
  group_by(subj)%>%
  mutate(accuracy = sum(is_correct == 1)/n())

# subjs left: 

length(unique(data$subj)) #339

data <- data%>%
  filter(accuracy >= 0.750)

# subjs left: 

length(unique(data$subj)) #337

# third: discarding data from observations in which participants answered the question incorrectly

data <- data%>%
  filter(is_correct == 1)

# subjs left: 

length(data$subj)

# erased data: 6%

# 2. discarding filler data

data <- data%>%
  filter(grepl("prev", type))

# 3. defining 'item_id', 'subj', 'type' as factors and dropping previously discarded levels before continuing

data$item_id <- as.factor(data$item_id)
data$subj <- as.factor(data$subj)
data$subj_uid <- as.factor(data$subj_uid)
data$type <- as.factor(data$type)
levels(data$subj_uid)
str(data$subj_uid)

data$subj <- droplevels(data$subj)
data$subj_uid <- droplevels(data$subj_uid)

# 4. getting critical regions for each item from an excel file called 'critical_regions.xlsx' 

regions <- read.csv("./Project_Docs/critical_regions.csv")

# 5. adding columns with the critical regions to the main df

regions$item_id <- as.factor(regions$item_id)

data <- left_join(data, regions, by = "item_id")

# cloze <- read_excel("cloze.xlsx")
# cloze$item_id <- as.factor(cloze$item_id)
# cloze$type <- as.factor(cloze$type)

# data <- left_join(data, cloze, by = c("item_id", "type"))

# 6. organizing data by regions of interest

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

# 7. checking data distribution

#article

ggplot(rt_art, aes(x = rt))+
  geom_histogram()

ggplot(rt_art, aes(x = rt))+
  geom_histogram()+
  facet_wrap(~type)

ggplot(rt_art, aes(x = type, y = rt))+
  geom_boxplot()

#adjective 1

ggplot(rt_adj1, aes(x = rt))+
  geom_histogram()

ggplot(rt_adj1, aes(x = rt))+
  geom_histogram()+
  facet_wrap(~type)

ggplot(rt_adj1, aes(x = type, y = rt))+
  geom_boxplot()

#conjunction

ggplot(rt_conj, aes(x = rt))+
  geom_histogram()

ggplot(rt_conj, aes(x = rt))+
  geom_histogram()+
  facet_wrap(~type)

ggplot(rt_conj, aes(x = type, y = rt))+
  geom_boxplot()

#adjective 2

ggplot(rt_adj2, aes(x = rt))+
  geom_histogram()

ggplot(rt_adj2, aes(x = rt))+
  geom_histogram()+
  facet_wrap(~type)

ggplot(rt_adj2, aes(x = type, y = rt))+
  geom_boxplot()

#noun

ggplot(rt_subst, aes(x = rt))+
  geom_histogram()

ggplot(rt_subst, aes(x = rt))+
  geom_histogram()+
  facet_wrap(~type)

ggplot(rt_subst, aes(x = type, y = rt))+
  geom_boxplot()

#spillover

ggplot(rt_spill, aes(x = rt))+
  geom_histogram()

ggplot(rt_spill, aes(x = rt))+
  geom_histogram()+
  facet_wrap(~type)

ggplot(rt_spill, aes(x = type, y = rt))+
  geom_boxplot()  

# 8. eliminating outliers (any observation > 2500 ms)

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

# erased data: 1% - there were originally 6389 observations in each dataframe

# 9. check distributions again (run section 7 one more time)

# 10. performing log transformations on the data (visual inspection showed that data followed a clear non-normal distribution)

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

# 11. checking log transformed distributions

# histograms

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

# boxplots

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


# 12. descriptive (run this section before and after outlier removal)

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

# 13. adjusting models (models that have been commented out are the ones that failed to converge or that were not needed)

# article

#model.art <- lmer(rt.log ~ type + (1+type|subj) + (1+type|item_id), data = rt_art, REML = FALSE)
#model.art <- lmer(rt.log ~ type + (1+type|subj) + (1|item_id), data = rt_art, REML = FALSE)
model.art <- lmer(rt.log ~ type + (1|subj) + (1|item_id), data = rt_art, REML = FALSE)

summary(model.art) # using only summary because there is only 1 variable

# adjective 1

#model.adj1 <- lmer(rt.log ~ type + (1+type|subj) + (1+type|item_id), data = rt_adj1, REML = FALSE)
#model.adj1 <- lmer(rt.log ~ type + (1+type|subj) + (1|item_id), data = rt_adj1, REML = FALSE)
model.adj1 <- lmer(rt.log ~ type + (1|subj) + (1|item_id), data = rt_adj1, REML = FALSE)

summary(model.adj1)

# conjunction

model.conj <- lmer(rt.log ~ type + (1+type|subj) + (1+type|item_id), data = rt_conj, REML = FALSE)
#model.conj <- lmer(rt.log ~ type + (1|subj) + (1+type|item_id), data = rt_conj)
#model.conj <- lmer(rt.log ~ type + (1|subj) + (1|item_id), data = rt_conj)

summary(model.conj)

# adjective 2

model.adj2 <- lmer(rt.log ~ type + (1+type|subj) + (1+type|item_id), data = rt_adj2, REML = FALSE)
#model.adj2 <- lmer(rt.log ~ type + (1|subj) + (1+type|item_id), data = rt_adj2)
#model.adj2 <- lmer(rt.log ~ type + (1|subj) + (1|item_id), data = rt_adj2)

summary(model.adj2)

# noun

model.subst <- lmer(rt.log ~ type + (1+type|subj) + (1+type|item_id), data = rt_subst, REML = FALSE)
#model.subst <- lmer(rt.log ~ type + (1|subj) + (1+type|item_id), data = rt_subst)
#model.subst <- lmer(rt.log ~ type + (1|subj) + (1|item_id), data = rt_subst)

summary(model.subst)

# spillover region

#model.spill <- lmer(rt.log ~ type + (1+type|subj) + (1+type|item_id), data = rt_spill, REML = FALSE)
#did not converge after corrective analysis for the spillover region

model.spill <- lmer(rt.log ~ type + (1|subj) + (1+type|item_id), data = rt_spill)
#model.spill <- lmer(rt.log ~ type + (1|subj) + (1|item_id), data = rt_spill)

summary(model.spill)

# checking models' residuals

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

# 14. adjusting p-values

p.values <- c(0.605, 0.0868, 0.606, 0.636, 0.000296, 0.000272)

p.values <- c(0.605, 0.0868, 0.606, 0.636, 0.000296, 0.000757) # spillover model p-value obtained after corrective analysis

p.adjust(p.values, method = "fdr")
#[1] 0.636000 0.173600 0.636000 0.636000 0.000888 0.000888
#[1] 0.636000 0.173600 0.636000 0.636000 0.001776 0.002271 (after corrective analysis for the spillover region)

# 15. corrective analysis for the spillover region

# one of the items had a four-syllable adjective in the spillover region, which makes it stand out when compared to the other 1-2-syllable prepositions/adverbs in the    other items. in this section, we remove observations from these items in the spillover region.

rt_spill <- rt_spill%>%
  filter(word != "temperada")%>%
  filter(word != "temperado")

# data loss = 322 observations, ~5% of spillover data
# effects in the spillover region were maintained after this corrective analysis, both in the lmer() and in the glmer().

# 16. additional/exploratory analyses

# analyzing original data with glmer(family = Gamma()) - models that have been commented out did not converge or were not necessary

# article

data$type <- as.factor(data$type)

#model.art.glm <- glmer(rt ~ type + (1+type|subj) + (1+type|item_id), data = rt_art, family = Gamma(link = "identity"))
#model.art.glm <- glmer(rt ~ type + (1+type|subj) + (1|item_id), data = rt_art, family = Gamma(link = "identity"))
model.art.glm <- glmer(rt ~ type + (1|subj) + (1|item_id), data = rt_art, family = Gamma(link = "identity")) # still failed to converge

summary(model.art.glm)

# adjective 1

#model.adj1.glm <- glmer(rt ~ type + (1+type|subj) + (1+type|item_id), data = rt_adj1, family = Gamma(link = "identity"))
#model.adj1.glm <- glmer(rt ~ type + (1+type|subj) + (1|item_id), data = rt_adj1, family = Gamma(link = "identity"))
model.adj1.glm <- glmer(rt ~ type + (1|subj) + (1|item_id), data = rt_adj1, family = Gamma(link = "identity"))

summary(model.adj1.glm)

# conjunction

model.conj.glm <- glmer(rt ~ type + (1+type|subj) + (1+type|item_id), data = rt_conj, family = Gamma(link = "identity"))

summary(model.conj.glm)

# adjective 2

#model.adj2.glm <- glmer(rt ~ type + (1+type|subj) + (1+type|item_id), data = rt_adj2, family = Gamma(link = "identity"))
#model.adj2.glm <- glmer(rt ~ type + (1+type|subj) + (1|item_id), data = rt_adj2, family = Gamma(link = "identity"))
model.adj2.glm <- glmer(rt ~ type + (1|subj) + (1|item_id), data = rt_adj2, family = Gamma(link = "identity"))

summary(model.adj2.glm)

# noun

#model.subst.glm <- glmer(rt ~ type + (1+type|subj) + (1+type|item_id), data = rt_subst, family = Gamma(link = "identity"))
#model.subst.glm <- glmer(rt ~ type + (1+type|subj) + (1|item_id), data = rt_subst, family = Gamma(link = "identity"))
model.subst.glm <- glmer(rt ~ type + (1|subj) + (1|item_id), data = rt_subst, family = Gamma(link = "identity"))

summary(model.subst.glm)

# spillover region (adjust as necessary)

#model.spill.glm <- glmer(rt ~ type + (1+type|subj) + (1+type|item_id), data = rt_spill, family = Gamma(link = "identity"))
#model.spill.glm <- glmer(rt ~ type + (1+type|subj) + (1|item_id), data = rt_spill, family = Gamma(link = "identity"))
model.spill.glm <- glmer(rt ~ type + (1|subj) + (1|item_id), data = rt_spill, family = Gamma(link = "identity"))

summary(model.spill.glm)

# adjusting p-values for the glmer() models

p.values.glm <- c(0.380669, 0.07208218, 0.01512347, 0.5728371, 2.921944e-11, 1.638411e-21)

p.values.glm <- c(0.380669, 0.07208218, 0.01512347, 0.5728371, 2.921944e-11, 7.7035e-17) # spillover model p-value obtained after corrective analysis

p.adjust(p.values.glm, method = "fdr")
#[1] 4.568028e-01 1.081233e-01 3.024694e-02 5.728371e-01 8.765832e-11 9.830466e-21
#[1] 4.568028e-01 1.081233e-01 3.024694e-02 5.728371e-01 8.765832e-11 4.622100e-16 (after corrective analysis for the spillover region)
