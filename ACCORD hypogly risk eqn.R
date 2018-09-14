library(haven)
library(tidyverse)
library(magrittr)
analysis_dir = '~/Data/accord/3-Data_Sets-Analysis/3a-Analysis_Data_Sets/'
analysis_baseline_data_files = c( # only files that contain some baseline data about most patients
  "bloodpressure.sas7bdat",
  "concomitantmeds.sas7bdat",
  "ecg.sas7bdat",
  "lipids.sas7bdat",
  "otherlabs.sas7bdat")
followup_dir = '~/Data/accord/4-Data_Sets-CRFs/4a-CRF_Data_Sets/'
followup_baseline_data_files = c( # only files that contain some baseline data about most patients
  "f07_baselinehistoryphysicalexam.sas7bdat",
  "f08_09_glycemiamanagement.sas7bdat",
  "f19_healthutilitiesindex.sas7bdat")
other_baseline_covariates = 
  list(
    list(followup_dir, analysis_dir),
    list(followup_baseline_data_files, analysis_baseline_data_files)
  ) %>%
  pmap(~str_c(.x, .y)) %>% # concat dir with appropriate file names
  unlist %>%
  map(function(file) {
    read_sas(file) %>%
      filter(Visit == "BLR") # remove all follow-up data from each data.frame
  }) %>%
  reduce(full_join, by="MaskID")
key = read_sas(str_c(analysis_dir,"accord_key.sas7bdat")) %>% 
  mutate(gly_trt = ifelse(arm %in% c(3,4,7,8), T, F),
         htn_trt = ifelse(arm %in% c(1,3), T, F),
         fib_trt = ifelse(arm %in% c(5,7), T, F)) 
outcomes = read_sas(str_c(analysis_dir,"hypoglycemiatime1st.sas7bdat")) 
all_data = list(key, outcomes, other_baseline_covariates) %>%
  reduce(full_join, by="MaskID")
a1c = read_sas("~/Data/accord/3-Data_Sets-Analysis/3a-Analysis_Data_Sets/hba1c.sas7bdat")
a1cdelta = a1c %>%
  group_by(MaskID) %>%
  summarise(a1cbase = first(hba1c),
         a1cdelta = nth(hba1c,17)-first(hba1c))
all_data$censor_med = 1-all_data$censor_med
accdata = left_join(all_data,a1cdelta,by=c("MaskID","MaskID"))
# how predictive is a very basic model with most widely-available data? [age, sex, starting A1c, change in A1c]
require(rms)
mod1b <- lrm(censor_med~baseline_age+female+a1cbase+a1cdelta,data=accdata)
mod1b # not very good, low R2, low brier, mod auc


# how predictive is an extensive model with all accord data?
library(h2o)
h2o.init()

train <- as.h2o(accdata)
y <- "censor_med"
x <- setdiff(names(train), y)
train[,y] <- as.numeric(train[,y])

glm_model <- h2o.glm(family= "binomial", x= x, y=y, training_frame=train)
glm_model
summary(glm_model) # excellent but with endogenous predictors driving performance
print(as.data.frame(h2o.varimp(glm_model)))

# now make a reasonable subset with commonly-avaiable data
subsetdat = accdata[,c("censor_med",
                       "sbp",
                       "alt",
                       "fvibrat",
                       "g2avepba",
                       "baseline_age",
                       "sulfonylurea",
                       "female",
                       "visloss",
                       "screat",
                       "a1cdelta",
                       "a1cbase",
                       "yrsdiab")]
train <- as.h2o(subsetdat)
y <- "censor_med"
x <- setdiff(names(train), y)
train[,y] <- as.numeric(train[,y])

glm_model <- h2o.glm(family= "binomial", x= x, y=y, training_frame=train)
glm_model
summary(glm_model) # excellent but with endogenous predictors driving performance
print(as.data.frame(h2o.varimp(glm_model)))


mod1b <- lrm(censor_med~baseline_age+female+a1cbase+a1cdelta+
               sbp+alt+fvibrat+g2avepba+sulfonylurea+visloss+screat+yrsdiab,data=subsetdat)
mod1b # improved score


mod1b <- glm(censor_med~baseline_age+female+a1cbase+a1cdelta+
               sbp+alt+fvibrat+g2avepba+sulfonylurea+visloss+screat+yrsdiab,data=subsetdat, family = binomial())
mod1b # improved score

save(mod1b,file="hypomodel")


h2o.shutdown(prompt = FALSE)




