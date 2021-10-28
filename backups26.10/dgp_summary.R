library(vtable)
library(uplift)
library(gbm)
library(caret)

setwd("Z:/skolarbete/gradu/R")
source('dgp.R')



get_gbm_classification_metrics <- function(
  data,
  seed
){
  ## 75% of the sample size
  smp_size <- floor(0.75 * nrow(data))
  
  ## set the seed to make your partition reproducible
  set.seed(seed)
  train_ind <- sample(seq_len(nrow(data)), size = smp_size)
  data <- data[3:14]
  train <- data[train_ind, ]
  test <- data[-train_ind, ]
  
  model_gbm = gbm(train$y ~ .,
                  data = train[,1:11],
                  distribution = "bernoulli",
                  cv.folds = 10,
                  shrinkage = .01,
                  n.minobsinnode = 10,
                  n.trees = 1500)
  

  
  pred_y_probabilities = predict.gbm(model_gbm,test[,1:11],type = "response")
  pred_y <- ifelse(pred_y_probabilities>0.5,1,0)
  y <- test$y
  
  conf_matrix <- confusionMatrix(data = factor(pred_y),reference = factor(y))
  # Classification metric calculations
  
  accuracy <- conf_matrix$overall[[1]]
  precision <- conf_matrix$byClass[[5]] 
  sensitivity <- conf_matrix$byClass[[1]]
  specificity <- conf_matrix$byClass[[2]]
  
  
  auc <- gbm.roc.area(y,pred_y_probabilities)
  
  list(
    accuracy = accuracy,
    precision = precision,
    sensitivity = sensitivity,
    specificity = specificity,
    auc = auc
  )
}


uplift_esitmate_summaries <- function(data,seed){
  
  delta <- 1 # cost of conditional intervention
  
  colnumbers.covariates <- 4:13
  
  smp_size <- floor(0.75 * nrow(data))
  
  ## set the seed to make your partition reproducible
  set.seed(seed)
  train_ind <- sample(seq_len(nrow(data)), size = smp_size)
  data <- data
  train <- data[train_ind, ]
  test <- data[-train_ind, ]
  
  
  myform      <- stats::as.formula(paste(
    "y ~ trt(w) + ",
    paste0(colnames(data[, colnumbers.covariates]),
           collapse   = "+"
    )
  ))
  
  myliftmodel <- upliftRF(
    myform,
    data         = train,
    ntree        = 100,
    split_method = "KL",
    verbose      = FALSE
  )
  
  test_preds <- stats::predict(myliftmodel, test)
  
  r1_test <- test_preds[,1]
  r0_test <- test_preds[,2]
  
  
  
  # Estimate effect on cash flows
  
  mymodelvaluelift <- upliftKNN(
    train = train[, colnumbers.covariates],
    test = test[, colnumbers.covariates],
    y = train$revenues,
    ct = train$w,
    k = 10,
    dist.method = "euclidean",
    p = 2,
    ties.meth = "min",
    agg.method = "mean"
  )
  
  
  
  # expected margin if targeted on the test sample
  m1_test <- mymodelvaluelift[, 2]
  
  # expected margin if not targeted on the test sample
  m0_test <- mymodelvaluelift[, 1]
  
  
  rmlift_test <- ((1 - r1_test) * (m1_test - delta)) - ((1 - r0_test) * m0_test)
  
  list(
    r0_mean = mean(r0_test),
    r0_sd = sd(r0_test),
    r1_mean = mean(r1_test),
    r1_sd = sd(r1_test),
    r_lift_sd = sd(r0_test-r1_test),
    m0_mean = mean(m0_test),
    m0_sd = sd(m0_test),
    m1_mean = mean(m1_test),
    m1_sd = sd(m1_test),
    profit_lift_mean = mean(rmlift_test),
    profit_lift_sd = sd(rmlift_test)
  )
}


# Summary statistics for generated with different values for errors

fractions_to_loop <- seq(0,1,0.11)
seed <- 1
i <- 1
summary_df <- data.frame()
for (r_error_fraction in fractions_to_loop) {
  for (m_error_fraction in fractions_to_loop) {
    for (tau_error_fraction in fractions_to_loop) {
      
      print(paste(i , 'th iteration out of ' , length(fractions_to_loop)^3  ))
      data_buffer_unedited <- gen_data(r_error_fraction,m_error_fraction,tau_error_fraction,2000,seed)
      gbm_buffer <- get_gbm_classification_metrics(data.frame(data_buffer_unedited),seed)
      uplift_buffer <- uplift_esitmate_summaries(data.frame(data_buffer_unedited),seed)
      data_buffer <- list(
        r_error_fraction = r_error_fraction,
        m_error_fraction = m_error_fraction,
        tau_error_fraction = tau_error_fraction,
        y_mean = mean(data_buffer_unedited$y),
        y_sd = sd(data_buffer_unedited$y),
        m_mean = mean(data_buffer_unedited$revenues),
        m_sd = sd(data_buffer_unedited$revenues),
        r_mean = mean(data_buffer_unedited$r),
        r_sd = sd(data_buffer_unedited$r),
        tau_uplift_prob_mean = mean(data_buffer_unedited$tau_prob),
        tau_uplift_prob_sd = sd(data_buffer_unedited$tau_prob),
        accuracy = gbm_buffer$accuracy,
        precision = gbm_buffer$precision,
        sensitivity = gbm_buffer$sensitivity,
        specificity = gbm_buffer$specificity,
        auc = gbm_buffer$auc,
        r0_mean = uplift_buffer$r0_mean,
        r0_sd = uplift_buffer$r0_sd,
        r1_mean = uplift_buffer$r1_mean,
        r1_sd = uplift_buffer$r1_sd,
        r_lift_sd = uplift_buffer$r_lift_sd,
        m0_mean = uplift_buffer$m0_mean,
        m0_sd = uplift_buffer$m0_sd,
        m1_mean = uplift_buffer$m1_mean,
        m1_sd = uplift_buffer$m1_sd,
        profit_lift_mean = uplift_buffer$profit_lift_mean,
        profit_lift_sd = uplift_buffer$profit_lift_sd
      )
      summary_df <- rbind(summary_df,data.frame(data_buffer) )
      i <- i + 1
    }
  }
}

write.csv(summary_df,"test.csv",row.names = FALSE)





