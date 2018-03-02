#### Read in data ##########################################################

library(haven)
raw_data <- read_dta("brain_health.dta")

#### Data cleaning #########################################################

### Cognitive functioning variable
# which column is the first of the series?
index_start <- which(names(raw_data) == "Q3_1")
# which is the last?
index_end <- which(names(raw_data) == "Q3_12")
# For all those columns, loop through and recode -1 to NA
raw_data[index_start:index_end] <- 
  lapply(raw_data[index_start:index_end], function(x) {
    ifelse(x == -1, NA, x)
  })
# For all those columns, loop through and reverse code so 
# more function is higher
raw_data[index_start:index_end] <- 
  lapply(raw_data[index_start:index_end], function(x) {
    (x * -1) + 6
  })
# For all those columns, recode all the "increase" responses to "stay the same"
raw_data[index_start:index_end] <- 
  lapply(raw_data[index_start:index_end], function(x) {
    ifelse(x > 3, 3, x)
  })
# The mean of those columns is the cognitive functioning variable
raw_data$cog_fun <- rowMeans(raw_data[index_start:index_end])

### Create social engagement variable
# which column is the first of the series?
index_start <- which(names(raw_data) == "Q18_A")
# which is the last?
index_end <- which(names(raw_data) == "Q18_G")
# For all those columns, loop through and recode -1 to NA
raw_data[index_start:index_end] <- 
  lapply(raw_data[index_start:index_end], function(x) {
    ifelse(x == -1, NA, x)
  })
# The mean of all these rows is the engagement variable
raw_data$engagement <- rowMeans(raw_data[index_start:index_end])

# Media variables
raw_data$watch_tv <- (raw_data$Q37_1 * -1) + 7
raw_data$surf_net <- (raw_data$Q37_3 * -1) + 7
raw_data$use_facebook <- (raw_data$Q37_5 * -1) + 7

# Demographics
raw_data$age <- raw_data$ppagecat
raw_data$female <- raw_data$ppgender - 1
raw_data$hispanic <- as.numeric(raw_data$ppethm == 4)
raw_data$black <- as.numeric(raw_data$ppethm == 2)
raw_data$white <- as.numeric(raw_data$ppethm == 1)
raw_data$educ <- raw_data$ppeducat

#### Set up survey data #####################################################

library(survey)
# Create survey design object
survey_data <- svydesign(data = raw_data, ids = ~1, weights = ~WEIGHT3)

# Get the mean of a variable
svymean(~cog_fun, design = survey_data, na.rm = TRUE)

#### Fiting models ##########################################################

# Fit unweighted model
fit <- lm(cog_fun ~ engagement + watch_tv + surf_net + use_facebook + age +
            female + educ + black + hispanic, data = raw_data)
summary(fit)

# Fit survey-weighted model
fits <- svyglm(cog_fun ~ engagement + watch_tv + surf_net + use_facebook +
                 age + female + educ + black + hispanic, design = survey_data)
summary(fits)

#### Testing ignorability of weights #########################################

library(jtools)
wgttest(fit, weights = WEIGHT3, data = raw_data)
pf_sv_test(fit, weights = WEIGHT3, data = raw_data)
