Predicting Recidivism in Broward County
================

*Aaron Dunmore and [Minseon
Lee](https://github.com/mins-lee/recidivism-prediction)*

Our models predict the risk of recidivism and of violent recidivism
within two years of a given arrest. To construct and evaluate models, we
used the data released by
[ProPublica](https://github.com/propublica/compas-analysis). They
investigated racial bias in the recidivism prediction tool COMPAS used
by the courts in Browdward County, FL.

### Table of Contents

1.  **[Data Processing](#data-processing)**
2.  **[Data Exploration](#data-exploration)**
3.  **[Modeling](#modeling)**
4.  **[Model Selection](#model-selection)**
5.  **[Important Variables](#important-variables)**
6.  **[Racial, Gender, and Age Bias](#bias)**
7.  **[Prediction Comparison with COMPAS](#comparison-with-compas)**

## 1\. Data Processing<a name="data-processing"></a>

### 1.1. Introduction

#### a) compas.db

Our data source is a sqlite database file. It includes 5 relevant
tables:

| Name          | Description                                                                                                     |
| :------------ | --------------------------------------------------------------------------------------------------------------- |
| casearrest    | The main table of interest. Contains one row per criminal charge per arrest event.                              |
| people        | One row per person. Includes basic information such as sex, race, date of birth, and juvenile criminal history. |
| charge        | One row per charge. Includes detailed information about charges, including charge descriptions.                 |
| jailhistory   | Includes each individual’s history of jail custody                                                              |
| prisonhistory | Includes each individual’s history of prison custody.                                                           |

#### b) Descriptives

compas includes 128180 arrest-charges, which represent 55732 arrest
events for 10963 people. Arrest dates range from 1975-07-06 to
2016-03-30.

### 1.2. Transformations

We synthesized a number of variables for each arrest event.

#### a) violent\_offense

We identified whether each arrest event was charged as a violent
offense. We followed the [FBI’s definition of violent
crime](https://ucr.fbi.gov/crime-in-the-u.s/2018/crime-in-the-u.s.-2018/topic-pages/violent-crime):
murder, rape, robbery, and aggravated assault.

We used simple text filters on each charge’s description to identify
each crime as either violent or non-violent.

This method identified just 3187 violent crimes, so it might have failed
to identify some violent crimes in the dataset. With better information
about how the Broward County court system labels violent crimes, we
could have performed this step with higher confidence. This is a
limitation of our analysis of violent recidivism.

This also means that class imbalance will be a concern as we try to
predict violent recidivism. This issue is addressed in section 3.1.b.

#### b) recidivism and violent\_recidivism

We also identifed whether each arrest event was followed up by an event
of recidivism (or violent recidivism). We followed the definition of
recidivism suggested by the authors of ProPublica’s analysis of the
COMPAS Recidivism Algorithm: recidivism occurs when an individual
commmits a new (violent) felony or misdemeanor offense within two years
of a given arrest.

If a given arrest event occurred \<2 years before the end of the data
(2016-03-30), then we could not conclude whether or not the arrest was
followed by recidivism. Such cases were excluded from our training data.

#### c) prior criminal history

We generated variables summarizing each individual’s prior criminal
history leading up to the arrest in question. We counted the number of
prior (violent) misdemeanor & felony offenses the individual had been
charged for overall, within 5 years, and within 2 years.

#### d) arrest age

Using each individual’s date of birth, we calculated their age at the
time of each arrest.

### 1\. 3 Other Processing

#### a) Filtering Municipal Offenses

We excluded Municipal Offenses from this data. We neither counted them
as recidivism events, nor included them in our training or test data.

#### b) Data Cleaning

The original dataset included some arrest events labeled as taking place
in the future (2020 and later). We removed these events from the data.

## 2\. Data Exploration<a name="data-exploration"></a>

We examined how recidivism and violent recidivism differ across our
independent variables. Based on the plots, for both recidivism and
violent recidivism, younger defendants are more likely to recidivate.
Men are more likely to recidivate than women. Native American and
African-American are more likely to recidivate. Defendants with more
overall prior arrests are more likely to recidivate. For other
variables, such as charge degree and the number of prior arrests within
the last two years and five years, we did not find any interesting
pattern.

### 2.1. Recidivism plots

![](Figs/two-year%20recidivism-1.png)<!-- -->

### 2.2 Violent recidivism plots

![](Figs/two-year%20violent%20recidivism-1.png)<!-- -->

## 3\. Modeling<a name="modeling"></a>

### 3.1 Loading & Preprocessing

``` r
arrest_history <- read_csv("../Data/arrest_history.csv") %>% 
  mutate(race = as.factor(race),
    arrest_age_category = as.factor(arrest_age_category),
    sex = as.factor(sex),
    recidivated = as.factor(recidivated)) %>% 
  select(-person_id) %>% 
  arrange(arrest_date)

arrest_history_violent <- read_csv("../Data/arrest_history_violent.csv") %>% 
  mutate(race = as.factor(race),
    arrest_age_category = as.factor(arrest_age_category),
    sex = as.factor(sex),
    recidivated_violent = as.factor(recidivated_violent)) %>% 
  select(-person_id) %>% 
  arrange(arrest_date)
```

#### a) Time-based Train & Test Splits

To address the temporal nature of our data in our validation and
train-test schemes, we split the data on the basis of time.

We create a ~70-30 train-test split, with the test set as the last 3
years of the data set.

``` r
arrest_history_train <- arrest_history %>% 
  filter(arrest_date < ymd(max(.$arrest_date)) - years(3)) %>% 
  select(-arrest_date)

arrest_history_test <- arrest_history %>% 
  filter(arrest_date > ymd(max(.$arrest_date)) - years(3)) %>% 
  select(-arrest_date)

arrest_history_violent_train <- arrest_history_violent %>% 
  filter(arrest_date < ymd(max(.$arrest_date)) - years(3)) 

arrest_history_violent_test <- arrest_history_violent %>% 
  filter(arrest_date > ymd(max(.$arrest_date)) - years(3)) %>% 
  select(-arrest_date)
```

#### b) Upsampling

Class imbalance is a major issue with our violent recidivism dataset.
Out of the 43755 observations, only 3185 in our violent recidivism
dataset represent violent recidivism events, a class imbalance of 7.28%.

We address this problem by upsampling the violent recidivism training
set using caret’s built-in upsampling
function.

``` r
arrest_history_violent_upsample <- upSample(x = arrest_history_violent_train[, -2],
                                          y = arrest_history_violent_train$recidivated_violent) %>% 
  arrange(arrest_date) %>% 
  select(-Class, -arrest_date) 

arrest_history_violent_train <- arrest_history_violent_train %>% select(-arrest_date)
arrest_history_violent_length <- nrow(arrest_history_violent_upsample)
```

#### c) Time-based validation scheme

This code defines variables used by our time-based validation set
schemes.

``` r
arrest_history_length <- nrow(arrest_history_train)

arrest_history_violent_length <- nrow(arrest_history_violent_train)
```

### 3.2 Fitting models

#### a) Logistic

``` r
slice_size <- floor(arrest_history_length / 100)
trControl_logit <- trainControl(method = "timeslice",
                              initialWindow = slice_size,
                              horizon = slice_size,
                              skip = slice_size,
                              summaryFunction = twoClassSummary,
                              classProbs = TRUE,
                              fixedWindow = TRUE,
                              savePredictions = TRUE)

recidivism_logit <- train(recidivated ~ .,
                    data = arrest_history_train,
                    method = "glm",
                    family = binomial,
                    trControl = trControl_logit)
```

``` r
slice_size <- floor(arrest_history_violent_length / 100)
trControl_logit_violent <- trainControl(method = "timeslice",
                              initialWindow = slice_size,
                              horizon = slice_size,
                              skip = slice_size,
                              summaryFunction = twoClassSummary,
                              classProbs = TRUE,
                              fixedWindow = TRUE)




recidivism_violent_logit <- train(recidivated_violent ~ .,
                    data = arrest_history_violent_upsample,
                    method = "glm",
                    family = binomial,
                    trControl = trControl_logit)
```

#### b) Regularized

For regularized regressions (and a few other model types) we used a
caching strategy to save us time when rerunning our notebook on
unchanged data and modeling code.

``` r
slice_size <- floor(arrest_history_length / 100)
trControl_reg <- trainControl(method = "timeslice",
                             initialWindow = slice_size,
                             horizon = slice_size,
                             skip = slice_size,
                             summaryFunction = twoClassSummary,
                             fixedWindow = TRUE,
                             classProbs = TRUE,
                             savePredictions = TRUE)

cache_string <- '../Cache/recidivism_reg.rds'

if(file.exists(cache_string)) {
  recidivism_reg <- readRDS(file = cache_string)
  print(str_c('last run at: ', file.info(cache_string)$ctime))
} else {
recidivism_reg <- train(recidivated ~ .,
                    data = arrest_history_train,
                    method = "glmnet",
                    family = 'binomial',
                    trControl = trControl_reg
                    )
saveRDS(recidivism_reg, cache_string)
}
## [1] "last run at: 2020-01-01 17:30:44"
```

``` r
slice_size <- floor(arrest_history_violent_length / 100)
trControl_reg <- trainControl(method = "timeslice",
                             initialWindow = slice_size,
                             horizon = slice_size,
                             skip = slice_size,
                             fixedWindow = TRUE,
                             summaryFunction = twoClassSummary,
                             classProbs = TRUE)

cache_string <- '../Cache/recidivism_violent_reg.rds'

if(file.exists(cache_string)) {
  recidivism_violent_reg <- readRDS(file = cache_string)
  print(str_c('last run at: ', file.info(cache_string)$ctime))
} else {
recidivism_violent_reg <- train(recidivated_violent ~ .,
                    data = arrest_history_violent_upsample,
                    method = "glmnet",
                    family = 'binomial',
                    trControl = trControl_reg
                    )
saveRDS(recidivism_violent_reg, cache_string)
}
## [1] "last run at: 2020-01-01 17:30:44"
```

#### c) Tree

Note: rpart and tree::tree() are both implementations of the same
recursive partitioning algorithm.

See details section:
<https://www.rdocumentation.org/packages/rpart/versions/4.1-15/topics/rpart>

Because of the heavy class imbalance present in our violent recidivism
dataset, and because tree models work poorly in conditions of class
imbalance, we did not train a tree model for that problem.

``` r
slice_size <- floor(arrest_history_length / 10)
trControl_tree <- trainControl(method = "timeslice",
                              initialWindow = slice_size,
                              horizon = slice_size,
                              skip = slice_size,
                              summaryFunction = twoClassSummary,
                              classProbs = TRUE,
                              fixedWindow = TRUE)

recidivism_tree <- train(recidivated ~ .,
                    data = arrest_history_train,
                    method = "rpart",
                    trControl = trControl_tree)
```

#### d) Random Forest

``` r
slice_size <- floor(arrest_history_length / 10)
trControl_rf <- trainControl(method = "timeslice",
                             initialWindow = slice_size,
                             horizon = slice_size,
                             skip = slice_size,
                             fixedWindow = TRUE,
                             summaryFunction = twoClassSummary,
                             classProbs = TRUE)


cache_string <- '../Cache/recidivism_rf.rds'

if(file.exists(cache_string)) {
  recidivism_rf <- readRDS(file = cache_string)
  print(str_c('last run at: ', file.info(cache_string)$ctime))
} else {
recidivism_rf <- train(recidivated ~ .,
                    data = arrest_history_train,
                    method = "ranger",
                    trControl = trControl_rf
                    )

saveRDS(recidivism_rf, file = cache_string)
}
## [1] "last run at: 2020-01-01 17:30:44"
```

``` r

slice_size <- floor(arrest_history_violent_length / 10)
trControl_rf <- trainControl(method = "timeslice",
                             initialWindow = slice_size,
                             horizon = slice_size,
                             skip = slice_size,
                             fixedWindow = TRUE,
                             summaryFunction = twoClassSummary,
                             classProbs = TRUE)


cache_string <- '../Cache/recidivism_violent_rf.rds'

if(file.exists(cache_string)) {
  recidivism_violent_rf <- readRDS(file = cache_string)
  print(str_c('last run at: ', file.info(cache_string)$ctime))
} else {
recidivism_rf_violent <- train(recidivated_violent ~ .,
                    data = arrest_history_violent_upsample,
                    method = "ranger",
                    trControl = trControl_rf
                    )

saveRDS(recidivism_violent_rf, file = cache_string)
}
## [1] "last run at: 2020-01-01 17:30:44"
```

#### e) Boosting

``` r
cache_string <- '../Cache/recidivism_boost.rds'


slice_size <- floor(arrest_history_length / 10)
trControl_boost <- trainControl(method = "timeslice",
                             initialWindow = slice_size,
                             horizon = slice_size,
                             skip = slice_size,
                             fixedWindow = TRUE,
                             summaryFunction = twoClassSummary,
                             classProbs = TRUE)

if(file.exists(cache_string)) {
  recidivism_boost <- readRDS(file = cache_string)
} else {
  recidivism_boost <- train(recidivated ~ .,
                    data = arrest_history_train,
                    method = "xgbTree",
                    trControl = trControl_boost
                    )
  saveRDS(recidivism_boost, file = cache_string)
}
```

``` r
cache_string <- '../Cache/recidivism_boost_violent.rds'


slice_size <- floor(arrest_history_violent_length / 10)
trControl_boost <- trainControl(method = "timeslice",
                             initialWindow = slice_size,
                             horizon = slice_size,
                             skip = slice_size,
                             fixedWindow = TRUE,
                             summaryFunction = twoClassSummary,
                             classProbs = TRUE)

if(file.exists(cache_string)) {
  recidivism_violent_boost <- readRDS(file = cache_string)
} else {
  recidivism_violent_boost <- train(recidivated_violent ~ .,
                    data = arrest_history_violent_upsample,
                    method = "xgbTree",
                    trControl = trControl_boost
                    )
  saveRDS(recidivism_violent_boost, file = cache_string)
}
```

## 4\. Model Selection<a name="model-selection"></a>

### 4.1 Model Evaluation

#### a) Generating Test Set Predictions

``` r
testdata_violent <- arrest_history_violent_test

# Logit
recidivism_logit.pred <- predict(recidivism_logit, 
                                 newdata = arrest_history_test, 
                                 type = 'prob')
recidivism_violent_logit.pred <- predict(recidivism_violent_logit, 
                                         newdata = arrest_history_violent_test, 
                                         type = 'prob')




# Regularized
recidivism_reg.pred <- predict(recidivism_reg, 
                               newdata = arrest_history_test, 
                               type = 'prob')


recidivism_violent_reg.pred <- predict(recidivism_violent_reg, 
                                       newdata = arrest_history_violent_test,
                                       type = 'prob')



# Tree
recidivism_tree.pred <- predict(recidivism_tree,
                                newdata = arrest_history_test, 
                                type = 'prob')


# RF
recidivism_rf.pred <- predict(recidivism_rf, 
                              newdata = arrest_history_test, 
                              type = 'prob')



recidivism_violent_rf.pred <- predict(recidivism_violent_rf, 
                                      newdata = arrest_history_violent_test, 
                                      type = 'prob')


# Boosted Tree
recidivism_boost.pred <- predict(recidivism_boost, 
                                 newdata = arrest_history_test, type = 'prob')


recidivism_violent_boost.pred <- predict(recidivism_violent_boost, 
                                         newdata = arrest_history_violent_test, 
                                         type = 'prob')
```

#### b) Sensitivity/Specificity

We compared models on the basis of specificity & sensitivity. For this
problem statement, we wanted a model with high specificity, because of
the costs of imprisonment to an individual who is at low risk of
recidivation.

``` r
# recidivism
logit <- recidivism_logit$results[c("Spec", "Sens")] # logistic

recidivism_reg$bestTune # regularization
```

    ##   alpha     lambda
    ## 6  0.55 0.01376665

``` r
reg <- dplyr::slice(recidivism_reg$results, 6)[c("Spec", "Sens")]

recidivism_boost$bestTune
```

    ##    nrounds max_depth eta gamma colsample_bytree min_child_weight subsample
    ## 25      50         2 0.3     0              0.6                1         1

``` r
boost <- dplyr::slice(recidivism_boost$results, 25)[c("Spec", "Sens")] # boost

recidivism_rf$bestTune
```

    ##   mtry splitrule min.node.size
    ## 1    2      gini             1

``` r
rf <- dplyr::slice(recidivism_rf$results, 1)[c("Spec", "Sens")] # rf

recidivism_tree$bestTune
```

    ##           cp
    ## 1 0.00135318

``` r
tr <- dplyr::slice(recidivism_rf$results, 2)[c("Spec", "Sens")] # tree


rbind(logit = logit,
      reg = reg,
      boost = boost,
      rf = rf, 
      tr = tr)
```

    ##            Spec         Sens
    ## logit 0.9209148 0.1396470517
    ## reg   0.9959148 0.0106346061
    ## boost 0.9564132 0.1015662097
    ## rf    0.9996806 0.0018145161
    ## tr    1.0000000 0.0002016129

Our logistic regression and boosted tree achieve comparable performance
in both specificity and sensitivity.

However, the logistic regression achieves higher sensitivity (~37.5%
higher), for only a small loss in specificity.

``` r
# violent recidivism
recidivism_violent_boost$bestTune
```

    ##   alpha     lambda
    ## 9     1 0.01376665

``` r
boost_v <- dplyr::slice(recidivism_violent_boost$results, 4)[c("Spec", "Sens")]

logit_v <- recidivism_violent_logit$results[c("Spec", "Sens")]

recidivism_violent_reg$bestTune
```

    ##   alpha      lambda
    ## 3   0.1 0.004268492

``` r
reg_v <- dplyr::slice(recidivism_violent_reg$results, 3)[c("Spec", "Sens")]

recidivism_violent_rf$bestTune
```

    ##   mtry  splitrule min.node.size
    ## 4   18 extratrees             1

``` r
rf_v <- dplyr::slice(recidivism_violent_rf$results, 4)[c("Spec", "Sens")]

rbind(boost_v = boost_v,
      logit_v = logit_v,
      reg_v = reg_v,
      rf_v = rf_v)
```

    ##               Spec      Sens
    ## boost_v 0.90668710 0.1499058
    ## logit_v 0.32713083 0.7648292
    ## reg_v   0.04890871 0.9885607
    ## rf_v    0.16323506 0.9276657

The boost model has much higher specificity compared to other models,
making it a good candidate for our choice to predict violent recidivism.

#### c) Calculating Calibration & Lift Curves

A note: For some reason, the caret calibration function inverted our
results. For example: in the case of the .875 to .925 bin, for which the
calibration function estimated an observed event percentage of ~5%,
manual calculations show the true observed event percentage was
approximately 95%.

``` r
cal_results <- data.frame(recidivated = arrest_history_test$recidivated,
                           logit = recidivism_logit.pred$yes)

cal_results %>% 
  filter(logit > .875 & logit < .925) %>% 
  group_by(recidivated) %>% 
  summarise(n())
```

    ## # A tibble: 2 x 2
    ##   recidivated `n()`
    ##   <fct>       <int>
    ## 1 no             83
    ## 2 yes          1190

For this reason, we invert the y-axis on our calibration plots. This
gives us plots like what we would expect to see.

``` r
cal_results <- data.frame(recidivated = arrest_history_test$recidivated,
                           logit = recidivism_logit.pred$yes,
                           reg = recidivism_reg.pred$yes,
                           tree = recidivism_tree.pred$yes,
                           rf = recidivism_rf.pred$yes,
                           boost = recidivism_boost.pred$yes)

cal_object <- calibration(recidivated ~ logit + reg + tree + rf + boost, data = cal_results,
            cuts = 20) 

cal_object$data$Percent <- ifelse(cal_object$data$Percent == 0, 0, 100 - cal_object$data$Percent)

cal_object %>% plot(auto.key = list(columns = 3,
                                          lines = TRUE,
                                          points = FALSE))
```

![](Figs/Recidivism%20Calibration%20Plots-1.png)<!-- --> From this
calibration curve, it appears that the logistic regression has slightly
more stable performance than other models: At lower values of predicted
probability, it stays closer to the diagonal than other models, such as
our boosted
tree.

``` r
cal_results_violent <- data.frame(recidivated_violent = arrest_history_violent_test$recidivated_violent,
                           logit = recidivism_violent_logit.pred$yes,
                           reg = recidivism_violent_reg.pred$yes,
                           rf = recidivism_violent_rf.pred$yes,
                           boost = recidivism_violent_boost.pred$yes)

cal_object <- calibration(recidivated_violent ~ logit + reg + rf + boost, data = cal_results_violent,
            cuts = 20) 

cal_object$data$Percent <- ifelse(cal_object$data$Percent == 0, 0, 100 - cal_object$data$Percent)

cal_object %>% plot(auto.key = list(columns = 3,
                                          lines = TRUE,
                                          points = FALSE))
```

![](Figs/Violent%20Recidivism%20Calibration%20Plots-1.png)<!-- -->

This calibration plot demonstrates that, when comparing predicted and
observed probabilities of violent recidivism, all of our models perform
poorly. Observed percentage is below 20% for all of our models.

We could have used recalibration to fix this problem.

### 4.2 Choice of Models

#### a) Recidivism

Comparing specificity and sensitivity across all of our models, our
logistic regression performed the best in terms of the tradeoff between
the two. And our calibration plot showed no meaningful difference in
performance between our logistic regression and our other model.

Furthermore, the logistic regression’s interpretability makes it
desirable in our problem setting, where justice system officials might
want to understand the factors contributing to a particular predicted
risk score.

For these reasons, we selected the logistic regression as our model
predicting recidivism.

#### b) Violent Recidivism

Our boosted tree was our only model predicting violent recidivism that
performed well in terms of specificity. A calibration plot showed no
difference between the performance of our boosted tree and that of our
other models.

We selected our boosted tree as our model predicting violent recidivism.

## 5\. Important Variables<a name="important-variables"></a>

### 5.1. Recidivism

``` r
varImp(recidivism_logit)
```

    ## glm variable importance
    ## 
    ##   only 20 most important variables shown (out of 35)
    ## 
    ##                                      Overall
    ## `arrest_age_categoryLess than 25`     100.00
    ## charge_degree_.M2.TRUE                 69.51
    ## prior_arrest_2yr                       69.50
    ## charge_degree_.M1.TRUE                 51.58
    ## charge_degree_.F3.TRUE                 48.62
    ## prior_arrest_overall                   47.20
    ## charge_degree_.MO3.TRUE                45.37
    ## raceOther                              40.97
    ## raceCaucasian                          40.30
    ## charge_degree_.F1.TRUE                 35.39
    ## juv_misd_count                         34.51
    ## sexMale                                31.71
    ## prior_arrest_5yr                       29.96
    ## raceHispanic                           25.50
    ## `arrest_age_categoryGreater than 45`   22.42
    ## juv_other_count                        17.39
    ## charge_degree_.F7.TRUE                 17.35
    ## prior_prisontime_days                  14.97
    ## prior_arrest_2yr_violent               12.99
    ## charge_degree_.TC4.TRUE                11.47

For redicivism, age, recent criminal history, and misdemeanor status,
and race accounted for the most important variables.

### 5.2. Violent recidivism

``` r
varImp(recidivism_violent_logit)
```

    ## glm variable importance
    ## 
    ##   only 20 most important variables shown (out of 34)
    ## 
    ##                                      Overall
    ## prior_arrest_overall_violent          100.00
    ## prior_arrest_2yr                       94.70
    ## `arrest_age_categoryLess than 25`      80.62
    ## charge_degree_.M2.TRUE                 61.56
    ## raceCaucasian                          47.69
    ## `arrest_age_categoryGreater than 45`   40.46
    ## juv_misd_count                         38.17
    ## charge_degree_.F7.TRUE                 36.00
    ## charge_degree_.MO3.TRUE                33.86
    ## charge_degree_.F1.TRUE                 33.09
    ## raceOther                              28.14
    ## charge_degree_.F3.TRUE                 27.58
    ## prior_arrest_5yr                       22.08
    ## prior_jailtime_days                    21.44
    ## juv_other_count                        20.23
    ## charge_degree_.M1.TRUE                 18.85
    ## prior_arrest_5yr_violent               18.11
    ## raceHispanic                           17.92
    ## charge_degree_.TCX.TRUE                17.60
    ## charge_degree_.X.TRUE                  13.11

For violent recidivism, prior arrest within two years, the number of
prior arrests associated with violent charge, age are important
predictors.

## 6\. Racial, Gender, and Age Bias<a name="bias"></a>

### 6.1 Bias in recidivism

``` r
# summary(recidivism_logit)

intercept <- recidivism_logit$finalModel$coefficients['(Intercept)']
raceWhite <- recidivism_logit$finalModel$coefficients["raceCaucasian"]
ageYoung <- recidivism_logit$finalModel$coefficients["`arrest_age_categoryLess than 25`"]
sexMale <- recidivism_logit$finalModel$coefficients["sexMale"]
misdemeanor_1 <- recidivism_logit$finalModel$coefficients["charge_degree_.M3.TRUE"]


control <- exp(intercept) / (1 + exp(intercept))
exp(raceWhite) / (1 - control + (control * exp(raceWhite)))
```

    ## raceCaucasian 
    ##      0.911014

``` r
exp(ageYoung) / (1 - control + (control * exp(ageYoung)))
```

    ## `arrest_age_categoryLess than 25` 
    ##                          1.215722

``` r
exp(sexMale) / (1 - control + (control * exp(sexMale)))
```

    ##  sexMale 
    ## 1.089477

``` r
exp(misdemeanor_1) / (1 - control + (control * exp(misdemeanor_1)))
```

    ## charge_degree_.M3.TRUE 
    ##               1.944478

According to the model, all else held constant, whites are 11% less
likely to recidivate than blacks. Young people (those less than 25 years
old) are predicted as being 20% more likely to recidivate than
middle-aged people. Men are 9.5% more likely to recidivate than women.
And if an arrest is charged as a 3rd degree Misdemeanor, it is twice as
likely to be followed by recidivism than not.

### 6.2. Bias in violent recidivism

``` r
# summary(recidivism_violent_logit)



intercept <- recidivism_violent_logit$finalModel$coefficients['(Intercept)']
raceWhite <- recidivism_violent_logit$finalModel$coefficients["raceCaucasian"]
ageYoung <- recidivism_violent_logit$finalModel$coefficients["`arrest_age_categoryLess than 25`"]
sexMale <- recidivism_violent_logit$finalModel$coefficients["sexMale"]
misdemeanor_1 <- recidivism_violent_logit$finalModel$coefficients["charge_degree_.M3.TRUE"]


control <- exp(intercept) / (1 + exp(intercept))
exp(raceWhite) / (1 - control + (control * exp(raceWhite)))
```

    ## raceCaucasian 
    ##     0.8902748

``` r
exp(ageYoung) / (1 - control + (control * exp(ageYoung)))
```

    ## `arrest_age_categoryLess than 25` 
    ##                          1.181084

``` r
exp(sexMale) / (1 - control + (control * exp(sexMale)))
```

    ##   sexMale 
    ## 0.9927166

According to the model, all else held constant, whites are 17% less
likely to recidivate than blacks. Young people (those less than 25 years
old) are predicted as being 38% more likely to recidivate than
middle-aged people. Men are 15% less likely to recidivate than
women.

### 6.3 Observed recidivism rates per-score level across demographic groups

In this section, we look for disparities in the observed recidivism
rates of individuals within the same risk classes, in different
demographic groups.

We performed this analysis across race, age, and sex.

  - Defining low, medium, and high risk categories

We mapped our estimates of recidivism risk to low, medium, and high-risk
categories. To allow for direct comparison to the COMPAS scores, we
developed a method of mapping recidivism risk estimates to risk
categories.

First, we calculated deciles of predicted (violent) recidivism risk.
Then, we mapped arrest events in deciles 0-4 to low-risk, events in
deciles 5-8 to medium-risk, and events in deciles 9-10 to high risk.

``` r
deciles <- c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1)

deciles <- quantile(recidivism_logit.pred$yes, deciles) # generates deciles based on predicted probabilities


# adding column for raw model prediction scores
arrest_history_test <- cbind(arrest_history_test, predict(recidivism_logit, arrest_history_test, type = 'prob')$yes) %>%
  rename(recid_risk = `predict(recidivism_logit, arrest_history_test, type = "prob")$yes`) %>% 
  mutate(decile_score = case_when( # adding column for decile score
    recid_risk < deciles[1] ~ 0,
    recid_risk < deciles[1] ~ 1,
    recid_risk < deciles[2] ~ 2,
    recid_risk < deciles[3] ~ 3,
    recid_risk < deciles[4] ~ 4,
    recid_risk < deciles[5] ~ 5,
    recid_risk < deciles[6] ~ 6,
    recid_risk < deciles[7] ~ 7,
    recid_risk < deciles[8] ~ 8,
    recid_risk < deciles[9] ~ 9,
    recid_risk < deciles[10] ~ 10,    
  )) %>% 
  mutate(score_text = case_when(decile_score < 4 ~ 'Low', # creating score levels, like COMPAS's equivalent
                                decile_score < 8 ~ 'Medium',
                                TRUE ~ 'High'),
         score_text = factor(score_text, c('Low', 'Medium', 'High')))
```

#### a) Racial bias

``` r
arrest_history_test %>% 
  group_by(score_text, race) %>% 
  summarise(recid_ppn = sum(recidivated == 'yes') / n()) %>% 
  pivot_wider(names_from = score_text, values_from = recid_ppn)
```

    ## # A tibble: 6 x 4
    ##   race               Low Medium  High
    ##   <fct>            <dbl>  <dbl> <dbl>
    ## 1 African-American 0.561  0.796 0.924
    ## 2 Asian            0.5    0.889 1    
    ## 3 Caucasian        0.590  0.792 0.922
    ## 4 Hispanic         0.485  0.773 0.935
    ## 5 Native American  0.778  0.85  0.941
    ## 6 Other            0.546  0.841 0.953

This analysis shows no strong evidence of racial bias. Individuals
classified as high-risk were approximately equally likely to recidivate
(~93% probability) across all of the high-n racial groups. Medium and
low-risk classifications saw only slightly more variation. However,
there was very little difference in the probabilities of recidivism of
African Americans and Caucasians in these categories.

#### b) Age bias

``` r
arrest_history_test %>% 
  group_by(score_text, arrest_age_category) %>% 
  summarise(recid_ppn = sum(recidivated == 'yes') / n()) %>% 
  pivot_wider(names_from = score_text, values_from = recid_ppn)
```

    ## # A tibble: 3 x 4
    ##   arrest_age_category   Low Medium  High
    ##   <fct>               <dbl>  <dbl> <dbl>
    ## 1 25 - 45             0.587  0.827 0.936
    ## 2 Greater than 45     0.497  0.790 0.900
    ## 3 Less than 25        0.588  0.765 0.919

This analysis shows evidence of a small disparity in observed
probability of recidivism of high-risk individuals, across age. Mainly,
high-risk classified individuals in the \>45 age group were
approximately 3 percentage points less likely to recidivate than
individuals in the 25-45 age group, indicating slight bias against those
in the oldest age group.

Furthermore, younger low-risk classified individuals were much more
likely to reoffend (approximately 9 percentage points) than the oldest
low-risk classified individuals.

#### c) Gender bias

``` r
arrest_history_test %>% 
  group_by(score_text, sex) %>% 
  summarise(recid_ppn = sum(recidivated == 'yes') / n()) %>% 
  pivot_wider(names_from = score_text, values_from = recid_ppn)
```

    ## # A tibble: 2 x 4
    ##   sex      Low Medium  High
    ##   <fct>  <dbl>  <dbl> <dbl>
    ## 1 Female 0.563  0.791 0.944
    ## 2 Male   0.566  0.795 0.923

This analysis shows little evidence of a disparity in observed
probability of recidivism across sex. Men and women were approximately
equally likely to reoffend in each risk
category.

### 6.4 Observed violent recidivism rates per-score level across demographic groups

``` r
deciles <- c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1)


deciles <- quantile(recidivism_violent_logit.pred$yes, deciles) # generates deciles based on predicted probabilities


# adding column for raw model prediction scores
arrest_history_violent_test <- cbind(arrest_history_violent_test, predict(recidivism_violent_logit, arrest_history_violent_test, type = 'prob')$yes) %>%
  rename(recid_risk = `predict(recidivism_violent_logit, arrest_history_violent_test, `) %>% 
  mutate(decile_score = case_when( # adding column for decile score
    recid_risk < deciles[1] ~ 0,
    recid_risk < deciles[1] ~ 1,
    recid_risk < deciles[2] ~ 2,
    recid_risk < deciles[3] ~ 3,
    recid_risk < deciles[4] ~ 4,
    recid_risk < deciles[5] ~ 5,
    recid_risk < deciles[6] ~ 6,
    recid_risk < deciles[7] ~ 7,
    recid_risk < deciles[8] ~ 8,
    recid_risk < deciles[9] ~ 9,
    recid_risk < deciles[10] ~ 10,    
  )) %>% 
  mutate(score_text = case_when(decile_score < 4 ~ 'Low', # creating score levels, like COMPAS's equivalent
                                decile_score < 8 ~ 'Medium',
                                TRUE ~ 'High'),
         score_text = factor(score_text, c('Low', 'Medium', 'High')))
```

#### a) Racial bias

``` r
arrest_history_violent_test %>% 
  group_by(score_text, race) %>% 
  summarise(recid_ppn = sum(recidivated_violent == 'yes') / n()) %>% 
  pivot_wider(names_from = score_text, values_from = recid_ppn)
```

    ## # A tibble: 6 x 4
    ##   race                 Low  Medium  High
    ##   <fct>              <dbl>   <dbl> <dbl>
    ## 1 African-American  0.0370  0.0626 0.185
    ## 2 Asian             0.0488 NA      0    
    ## 3 Caucasian         0.0346  0.0711 0.157
    ## 4 Hispanic          0.0183  0.0547 0.130
    ## 5 Other             0.0185  0.0965 0.283
    ## 6 Native American  NA       0.0952 0.2

We see evidence of a small disparity in probability of violent reoffense
across race within risk groups. Caucasians classified as high-risk are
slightly (3 percentage points) less likely to violently reoffend than
African Americans. And high-risk Hispanics are significantly (6
percentage points) less likely to violently reoffend than African
Americans.

Medium and low-risk classifications see only small variations in
probability of reoffense across race.

#### b) Age bias

``` r
arrest_history_violent_test %>% 
  group_by(score_text, arrest_age_category) %>% 
  summarise(recid_ppn = sum(recidivated_violent == 'yes') / n()) %>% 
  pivot_wider(names_from = score_text, values_from = recid_ppn)
```

    ## # A tibble: 3 x 4
    ##   arrest_age_category    Low Medium  High
    ##   <fct>                <dbl>  <dbl> <dbl>
    ## 1 25 - 45             0.0368 0.0610 0.184
    ## 2 Greater than 45     0.0228 0.0556 0.110
    ## 3 Less than 25        0.0395 0.0703 0.185

There appears to be significant bias in our classifications across age
group. Mainly, high-risk classified individuals in the oldest age
category are approximately 8 percentage points less likely to violently
reoffend than younger individuals classified as high risk.

Medium and low-risk classifications see only small variations in
probability of violent reoffense across race.

#### c) Gender bias

``` r
arrest_history_violent_test %>% 
  group_by(score_text, sex) %>% 
  summarise(recid_ppn = sum(recidivated_violent == 'yes') / n()) %>% 
  pivot_wider(names_from = score_text, values_from = recid_ppn)
```

    ## # A tibble: 2 x 4
    ##   sex       Low Medium  High
    ##   <fct>   <dbl>  <dbl> <dbl>
    ## 1 Female 0.0213 0.0558 0.166
    ## 2 Male   0.0353 0.0676 0.179

Men and women within the same risk classifications are approximately
equally likely to reoffend
violently.

## 7\. Prediction Comparison with COMPAS<a name="comparison-with-compas"></a>

For this section, we will compare the predictions of our model on test
data with COMPAS’s risk assessments.

### 7\. 1 Recidivism

#### a) Creating comparison dataset

We first create a dataset that combines scores from COMPAS’s recidivism
risk assessment with our own scores.

We subset this data so that each row represents an arrest event that is
both: a.) In our test set b.) In the set of arrests assessed by COMPAS
ensuring that we’re comparing test set predictions by both models.

``` r
## must be run after train(recidivism_logit)

deciles <- c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1)

deciles <- quantile(recidivism_logit.pred$yes, deciles) # generates deciles based on predicted probabilities

# rereading arrest_history dataset
arrest_history_comparison <- read_csv("../Data/arrest_history.csv") %>% 
  mutate(race = as.factor(race),
         arrest_age_category = as.factor(arrest_age_category),
         sex = as.factor(sex),
         recidivated = as.factor(recidivated),
         arrest_date = ymd(arrest_date)) %>% 
  filter(arrest_date > ymd(max(.$arrest_date)) - years(3)) # filtering by date, ensures we get rows from the test set

# adding column for raw model prediction scores
arrest_history_comparison <- cbind(arrest_history_comparison, predict(recidivism_logit, arrest_history_comparison, type = 'prob')$yes) %>%
  rename(recid_risk = `predict(recidivism_logit, arrest_history_comparison, type = "prob")$yes`) %>% 
  mutate(decile_score = case_when( # adding column for decile score
    recid_risk < deciles[1] ~ 0,
    recid_risk < deciles[1] ~ 1,
    recid_risk < deciles[2] ~ 2,
    recid_risk < deciles[3] ~ 3,
    recid_risk < deciles[4] ~ 4,
    recid_risk < deciles[5] ~ 5,
    recid_risk < deciles[6] ~ 6,
    recid_risk < deciles[7] ~ 7,
    recid_risk < deciles[8] ~ 8,
    recid_risk < deciles[9] ~ 9,
    recid_risk < deciles[10] ~ 10,    
  )) %>% 
  mutate(score_text = case_when(decile_score < 4 ~ 'Low', # creating score levels, like COMPAS's equivalent
                                decile_score < 8 ~ 'Medium',
                                TRUE ~ 'High'),
         score_text = factor(score_text, c('Low', 'Medium', 'High')))
```

``` r
conn <- dbConnect(drv = dbDriver('SQLite'), # tells R to use SQLite
                            '../Data/compas.db') # tells R the location of our .db file

# Loading COMPAS data
compas <- dbGetQuery(conn, statement = ' 
  SELECT distinct person_id, arrest_date, compas.decile_score, compas.score_text 
    FROM casearrest JOIN people ON people.id = casearrest.person_id
  JOIN compas USING(person_id)
  WHERE arrest_date = c_offense_date AND type_of_assessment = \'Risk of Recidivism\'') %>% 
  mutate(arrest_date = as.Date(arrest_date)) %>% 
  rename(compas_decile_score = decile_score,
         compas_score_text = score_text)
```

``` r
joined_score_data <- arrest_history_comparison %>% # Joining the two datasets on the basis of person ID & arrest_date
  left_join(compas, by=c('arrest_date', 'person_id')) %>% 
  filter(!is.na(compas_decile_score)) # Removing rows not assessed by COMPAS
```

#### b) Score Group Sizes

``` r
inner_join(joined_score_data %>% 
  group_by(score_text) %>% 
  summarise(score_ppn = n() / nrow(joined_score_data)),

joined_score_data %>% 
  group_by(compas_score_text) %>% 
  summarise(compas_score_ppn = n() / nrow(joined_score_data)),

by = c('score_text' = 'compas_score_text'))
```

    ## # A tibble: 3 x 3
    ##   score_text score_ppn compas_score_ppn
    ##   <chr>          <dbl>            <dbl>
    ## 1 Low            0.480            0.539
    ## 2 Medium         0.394            0.259
    ## 3 High           0.126            0.201

Our model classifies a much lower proportion of individuals as high- or
low-risk, than does COMPAS, meaning that it classifies many more
individuals as medium risk.

#### c) Observed Recidivism Probability by Score Level

``` r
recid_proportion <- joined_score_data %>% 
  group_by(score_text) %>% 
  summarise(recid_proportion = sum(recidivated == 'yes') / n())

compas_recid_proportion <- joined_score_data %>% 
  group_by(compas_score_text) %>% 
  summarise(compas_recid_proportion = sum(recidivated == 'yes') / n())


left_join(recid_proportion, compas_recid_proportion, by = c('score_text' = 'compas_score_text')) %>% arrange(desc(recid_proportion))
```

    ## # A tibble: 3 x 3
    ##   score_text recid_proportion compas_recid_proportion
    ##   <chr>                 <dbl>                   <dbl>
    ## 1 High                  0.864                   0.835
    ## 2 Medium                0.658                   0.688
    ## 3 Low                   0.414                   0.409

Both models’ score levels correspond to very similar recidivism
risks.

``` r
# African American, Caucasian, and Hispanic individuals are most highly represented in the data.

joined_score_data %>% 
  group_by(race) %>% 
  summarise(n())
```

    ## # A tibble: 6 x 2
    ##   race             `n()`
    ##   <fct>            <int>
    ## 1 African-American  2807
    ## 2 Asian               27
    ## 3 Caucasian         1934
    ## 4 Hispanic           461
    ## 5 Native American     18
    ## 6 Other              299

``` r
joined_score_data %>% 
  group_by(score_text, race) %>% 
  summarise(recid_ppn = sum(recidivated == 'yes') / n()) %>% 
  pivot_wider(names_from = score_text, values_from = recid_ppn)
```

    ## # A tibble: 6 x 4
    ##   race               Low Medium  High
    ##   <fct>            <dbl>  <dbl> <dbl>
    ## 1 African-American 0.429  0.674 0.867
    ## 2 Asian            0.28   1     1    
    ## 3 Caucasian        0.427  0.627 0.83 
    ## 4 Hispanic         0.351  0.637 0.833
    ## 5 Native American  0.625  0.667 1    
    ## 6 Other            0.388  0.6   1

``` r
joined_score_data %>% 
  group_by(compas_score_text, race) %>% 
  summarise(recid_ppn = sum(recidivated == 'yes') / n()) %>% 
  pivot_wider(names_from = compas_score_text, values_from = recid_ppn)
```

    ## # A tibble: 6 x 5
    ##   race              High   Low Medium `N/A`
    ##   <fct>            <dbl> <dbl>  <dbl> <dbl>
    ## 1 African-American 0.843 0.451  0.694   1  
    ## 2 Asian            1     0.2    0.6    NA  
    ## 3 Caucasian        0.814 0.394  0.702   0.2
    ## 4 Hispanic         0.745 0.361  0.622  NA  
    ## 5 Native American  1     0.714  0.571  NA  
    ## 6 Other            0.889 0.362  0.583   0

In our model’s predictions, recidivism rate per score-group is very
similar across race (in the case of the three well-represented race
groups, African-Americans, Caucasians, and Hispanics).

Recidivism rate per-score group varies among racial groups in the COMPAS
model. Mainly, within the high risk group, African Americans are about 3
percentage points more likely to recidivate than Caucasians, and
Hispanics are about 7 percentage points less likely to recidivate than
Caucasians.

This suggests that our model’s score level classifications may be more
equitable by race, than COMPAS’s.

### 7\. 2 Violent Recidivism

``` r
## must be run after train(recidivism_logit)

deciles <- c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1)

deciles <- quantile(recidivism_violent_boost.pred$yes, deciles) # generates deciles based on predicted probabilities

# rereading arrest_history dataset
arrest_history_violent_comparison <- read_csv("../Data/arrest_history_violent.csv") %>% 
  mutate(race = as.factor(race),
         arrest_age_category = as.factor(arrest_age_category),
         sex = as.factor(sex),
         recidivated_violent = as.factor(recidivated_violent),
         arrest_date = ymd(arrest_date)) %>% 
  filter(arrest_date > ymd(max(.$arrest_date)) - years(3)) # filtering by date, ensures we get rows from the test set

# adding column for raw model prediction scores
arrest_history_violent_comparison <- cbind(arrest_history_violent_comparison, predict(recidivism_violent_boost, arrest_history_violent_comparison, type = 'prob')$yes) %>%
  rename(recid_violent_risk = `predict(recidivism_violent_boost, arrest_history_violent_comparison, `) %>% 
  mutate(decile_score = case_when( # adding column for decile score
    recid_violent_risk < deciles[1] ~ 0,
    recid_violent_risk < deciles[1] ~ 1,
    recid_violent_risk < deciles[2] ~ 2,
    recid_violent_risk < deciles[3] ~ 3,
    recid_violent_risk < deciles[4] ~ 4,
    recid_violent_risk < deciles[5] ~ 5,
    recid_violent_risk < deciles[6] ~ 6,
    recid_violent_risk < deciles[7] ~ 7,
    recid_violent_risk < deciles[8] ~ 8,
    recid_violent_risk < deciles[9] ~ 9,
    recid_violent_risk < deciles[10] ~ 10,    
  )) %>% 
  mutate(score_text = case_when(decile_score < 4 ~ 'Low', # creating score levels, like COMPAS's equivalent
                                decile_score < 8 ~ 'Medium',
                                TRUE ~ 'High'))
```

``` r
conn <- dbConnect(drv = dbDriver('SQLite'), # tells R to use SQLite
                            '../Data/compas.db') # tells R the location of our .db file

# Loading COMPAS data
compas_violent <- dbGetQuery(conn, statement = ' 
  SELECT distinct person_id, arrest_date, compas.decile_score, compas.score_text 
    FROM casearrest JOIN people ON people.id = casearrest.person_id
  JOIN compas USING(person_id)
  WHERE arrest_date = c_offense_date AND type_of_assessment = \'Risk of Recidivism\'') %>% 
  mutate(arrest_date = as.Date(arrest_date)) %>% 
  rename(compas_decile_score = decile_score,
         compas_score_text = score_text)
```

``` r
joined_score_data_violent <- arrest_history_violent_comparison %>% # Joining the two datasets on the basis of person ID & arrest_date
  left_join(compas, by=c('arrest_date', 'person_id')) %>% 
  filter(!is.na(compas_decile_score)) %>%  # Removing rows not assessed by COMPAS
  mutate(score_text = factor(score_text, c('High', 'Medium', 'Low')),
         compas_score_text = factor(compas_score_text, c('High', 'Medium', 'Low')))
```

#### a) Score Group Sizes

``` r
inner_join(joined_score_data_violent %>% 
  group_by(score_text) %>% 
  summarise(score_ppn = n() / nrow(joined_score_data)),

joined_score_data_violent %>% 
  group_by(compas_score_text) %>% 
  summarise(compas_score_ppn = n() / nrow(joined_score_data)),

by = c('score_text' = 'compas_score_text'))
```

    ## # A tibble: 3 x 3
    ##   score_text score_ppn compas_score_ppn
    ##   <fct>          <dbl>            <dbl>
    ## 1 High           0.118            0.160
    ## 2 Medium         0.379            0.228
    ## 3 Low            0.413            0.521

Again, our model classifies a higher proportion of individuals as
medium-risk, and fewer as low- or high-risk, than does COMPAS.

#### b) Observed Recidivism Probability by Score Level

``` r
recid_proportion <- joined_score_data_violent %>% 
  group_by(score_text) %>% 
  summarise(violent_recid_proportion = sum(recidivated_violent == 'yes') / n())

compas_recid_proportion <- joined_score_data_violent %>% 
  group_by(compas_score_text) %>% 
  summarise(compas_violent_recid_proportion = sum(recidivated_violent == 'yes') / n())


left_join(recid_proportion, compas_recid_proportion, by = c('score_text' = 'compas_score_text')) %>% arrange(desc(violent_recid_proportion))
```

    ## # A tibble: 3 x 3
    ##   score_text violent_recid_proportion compas_violent_recid_proportion
    ##   <fct>                         <dbl>                           <dbl>
    ## 1 High                         0.104                           0.100 
    ## 2 Medium                       0.0509                          0.0594
    ## 3 Low                          0.0231                          0.0222

Both models’ score levels are associated with very similar probabilities
of violent
recidivism.

``` r
# African American, Caucasian, and Hispanic individuals are most highly represented in the data.

joined_score_data_violent %>% 
  group_by(race) %>% 
  summarise(n())
```

    ## # A tibble: 6 x 2
    ##   race             `n()`
    ##   <fct>            <int>
    ## 1 African-American  2503
    ## 2 Asian               28
    ## 3 Caucasian         1771
    ## 4 Hispanic           436
    ## 5 Native American     11
    ## 6 Other              298

``` r
joined_score_data_violent %>% 
  group_by(score_text, race) %>% 
  summarise(recid_ppn = sum(recidivated_violent == 'yes') / n()) %>% 
  pivot_wider(names_from = score_text, values_from = recid_ppn) %>% 
  arrange(race)
```

    ## # A tibble: 6 x 4
    ##   race                High Medium    Low
    ##   <fct>              <dbl>  <dbl>  <dbl>
    ## 1 African-American  0.107  0.0502 0.0217
    ## 2 Asian            NA      0.0909 0     
    ## 3 Caucasian         0.0860 0.0591 0.0263
    ## 4 Hispanic          0.0345 0.0368 0.0184
    ## 5 Native American   0      0      0.25  
    ## 6 Other             0.267  0.0286 0.0141

``` r
joined_score_data_violent %>% 
  group_by(compas_score_text, race) %>% 
  summarise(recid_ppn = sum(recidivated_violent == 'yes') / n()) %>% 
  pivot_wider(names_from = compas_score_text, values_from = recid_ppn) %>% 
  arrange(race)
```

    ## # A tibble: 6 x 5
    ##   race               High Medium    Low  `NA`
    ##   <fct>             <dbl>  <dbl>  <dbl> <dbl>
    ## 1 African-American 0.102  0.0548 0.0230    NA
    ## 2 Asian            0      0.167  0         NA
    ## 3 Caucasian        0.102  0.0737 0.0213     0
    ## 4 Hispanic         0.0556 0.0395 0.0216    NA
    ## 5 Native American  0      0      0.25      NA
    ## 6 Other            0.133  0.0417 0.0214     0

In our model, probability of violent recidivism varies between race
groups. African Americans rated as high-risk are slightly more likely
than Caucasians to reoffend violently. And high-risk Hispanics are
significantly (5 percentage points) less likely.

In COMPAS’s predictions, observed probability is basically the same for
high-risk African Americans and Caucasians, but significantly lower (~5
percentage points) for high-risk Hispanics.

### 7.3 Conclusion

Our model’s predictions of recidivism score risk are comparable to those
of COMPAS’, in how they predict probability of recidivism. Furthermore,
our model may be more equitable in how its score levels predict
recidivism across race.

Likewise, our model’s predictions of violent recidivism risk score are
comparable to those of COMPAS’, in how they predict observed probability
of recidivism. However, both our model and COMPAS’ model are somewhat
inequatitable in how violent recidivism risk scores are assigned across
race.
