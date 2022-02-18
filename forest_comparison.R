r_error_fractions     <- c(0,0.2,0.4,0.6,0.8) # What error fractions should datasets be generated with
m_error_fractions     <- c(0,1) #  be generated with
tau_error_fractions   <- c(0.1,0.5,0.9)
n_to_loop <- c(2000,5000,8000,20000)

B <- 1:30
summary_df <- data.frame()

for (b in B) {
  
  for (r_error_fraction in r_error_fractions) {
    for (m_error_fraction in m_error_fractions) {
      for (tau_error_fraction in tau_error_fractions) {
        for (n in n_to_loop) {
          start_time_fraction <- Sys.time()
          print(paste(b , 'th iteration out of ' , length(r_error_fractions)*length(m_error_fractions)*length(tau_error_fractions)  ))
          seed <- b
          data <- data.frame(gen_data(r_error_fraction,m_error_fraction,tau_error_fraction,n,1))
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
            mtry         = 10,
            ntree        = 1000,
            split_method = "KL",
            verbose      = FALSE 
          )
          
          test_preds <- stats::predict(myliftmodel, test)
          
          r1_test <- test_preds[,1]
          r0_test <- test_preds[,2]
          
          
          
          
          positive_reaction <- ifelse(test$tau_prob>0,0,1)
          
          
          auc_positive_reactions_upliftrf <- gbm.roc.area(positive_reaction,r1_test-r0_test)
          
          
          Y <- train$y
          X <- train[,colnumbers.covariates]
          W <- train$w
          
          cforest <- causal_forest(
            X, Y, W,
            mtry = 23,
            honesty = TRUE,
            num.trees = 2000
          )
          
          cforest_preds <- predict(cforest,test[,colnumbers.covariates])
          
          
          
          auc_positive_reactions_cforest <- gbm.roc.area(positive_reaction,cforest_preds)
          
          
          data_buffer <- list(
            r_error_fraction = r_error_fraction,
            m_error_fraction = m_error_fraction,
            tau_error_fraction = tau_error_fraction,
            n_obs = n,
            auc_positive_reactions = auc_positive_reactions
          )
          summary_df <- rbind(summary_df,data.frame(data_buffer) )
          save.image('dgp_summary_results_upliftrf_mtry10_ntree1k.RData')
          end_time <- Sys.time()
          print(paste("Single fraction examination time elapsed:", round(end_time - start_time_fraction,2),"minutes/hours"))
          end_time - start_time_fraction
        }
      }
    }
  }
  
}





plot_by_parameters <- function(data,column,title){
  
  data <- aggregate(list(column = data[,column]), 
                    list(
                      r_error_parameter = data$r_error_fraction,
                      m_error_parameter = data$m_error_fraction,
                      tau_error_parameter = data$tau_error_fraction,
                      n_obs = data$n_obs), 
                    mean)
  data$m_error_parameter <- as.factor(data$m_error_parameter)
  
  
  plot <- ggplot(data,aes(x = r_error_parameter,y = column,group = m_error_parameter,color=m_error_parameter))+
    geom_line(size = 1)+
    facet_grid(
      n_obs~tau_error_parameter,
      labeller = label_both)+
    ggtitle(title)+
    ylab(column)+
    theme_test() +
    scale_colour_calc(name = "M error parameter")
  
  return(plot)
}
