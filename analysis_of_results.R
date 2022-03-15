
rm(list = ls())
setwd("Z:/skolarbete/gradu/R")
load("results/TESTRESULTS1.RData")
nobs <- 5000

# use this code to copy paste all the variables below

for (var in ls(pattern = 'results_with')) {
  cat(substr(var,30,50),' = ',var, ',\n')
}


results <- list(
  r_0.2_m_0_tau_0.1  =  results_with_error_fractions_r_0.2_m_0_tau_0.1 ,
  r_0.2_m_0_tau_0.5  =  results_with_error_fractions_r_0.2_m_0_tau_0.5 ,
  r_0.2_m_0_tau_0.9  =  results_with_error_fractions_r_0.2_m_0_tau_0.9 ,
  r_0.2_m_1_tau_0.1  =  results_with_error_fractions_r_0.2_m_1_tau_0.1 ,
  r_0.2_m_1_tau_0.5  =  results_with_error_fractions_r_0.2_m_1_tau_0.5 ,
  r_0.2_m_1_tau_0.9  =  results_with_error_fractions_r_0.2_m_1_tau_0.9 ,
  r_0.4_m_0_tau_0.1  =  results_with_error_fractions_r_0.4_m_0_tau_0.1 ,
  r_0.4_m_0_tau_0.5  =  results_with_error_fractions_r_0.4_m_0_tau_0.5 ,
  r_0.4_m_0_tau_0.9  =  results_with_error_fractions_r_0.4_m_0_tau_0.9 ,
  r_0.4_m_1_tau_0.1  =  results_with_error_fractions_r_0.4_m_1_tau_0.1 ,
  r_0.4_m_1_tau_0.5  =  results_with_error_fractions_r_0.4_m_1_tau_0.5 ,
  r_0.4_m_1_tau_0.9  =  results_with_error_fractions_r_0.4_m_1_tau_0.9 ,
  r_0.6_m_0_tau_0.1  =  results_with_error_fractions_r_0.6_m_0_tau_0.1 ,
  r_0.6_m_0_tau_0.5  =  results_with_error_fractions_r_0.6_m_0_tau_0.5 ,
  r_0.6_m_0_tau_0.9  =  results_with_error_fractions_r_0.6_m_0_tau_0.9 ,
  r_0.6_m_1_tau_0.1  =  results_with_error_fractions_r_0.6_m_1_tau_0.1 ,
  r_0.6_m_1_tau_0.5  =  results_with_error_fractions_r_0.6_m_1_tau_0.5 ,
  r_0.6_m_1_tau_0.9  =  results_with_error_fractions_r_0.6_m_1_tau_0.9 ,
  r_0.8_m_0_tau_0.1  =  results_with_error_fractions_r_0.8_m_0_tau_0.1 ,
  r_0.8_m_0_tau_0.5  =  results_with_error_fractions_r_0.8_m_0_tau_0.5 ,
  r_0.8_m_0_tau_0.9  =  results_with_error_fractions_r_0.8_m_0_tau_0.9 ,
  r_0.8_m_1_tau_0.1  =  results_with_error_fractions_r_0.8_m_1_tau_0.1 ,
  r_0.8_m_1_tau_0.5  =  results_with_error_fractions_r_0.8_m_1_tau_0.5 ,
  r_0.8_m_1_tau_0.9  =  results_with_error_fractions_r_0.8_m_1_tau_0.9 ,
  r_0_m_0_tau_0.1  =  results_with_error_fractions_r_0_m_0_tau_0.1 ,
  r_0_m_0_tau_0.5  =  results_with_error_fractions_r_0_m_0_tau_0.5 ,
  r_0_m_0_tau_0.9  =  results_with_error_fractions_r_0_m_0_tau_0.9 ,
  r_0_m_1_tau_0.1  =  results_with_error_fractions_r_0_m_1_tau_0.1 ,
  r_0_m_1_tau_0.5  =  results_with_error_fractions_r_0_m_1_tau_0.5 ,
  r_0_m_1_tau_0.9  =  results_with_error_fractions_r_0_m_1_tau_0.9  # delete comma here
)

# holdoutprofit columns:
# 1: lift-model
# 2: Classic SGB model
# 3: Classic reordered SGB model
# 4: Profit wSGB model, left weighting
# 5: Profit wSGB model, right weighting
# 6: Profit wSGB model, symmetric weighting


profit_df <- rank_df  <- data.frame(
  r_error_fraction = numeric(),
  m_error_fraction = numeric(),
  tau_error_fraction = numeric(),
  lift = numeric(),
  classic_sgb = numeric(),
  classic_reordered_sgb = numeric(),
  profitloss_leftw = numeric(),
  profitloss_rightw = numeric(),
  profitloss_symmw = numeric()
)

# getting all the fractions in the right order
fraction_vector <- c()

for (var in ls(pattern = 'results_with')) {
  
  buffer_vec <- c(
  as.double(strsplit(var,'_')[[1]][6]),
  as.double(strsplit(var,'_')[[1]][8]),
  as.double(strsplit(var,'_')[[1]][10])
  )
  fraction_vector <- c(fraction_vector,buffer_vec)
}



i <- 1
for (item in results) {
  start_idx <- i*3-2
  end_idx <- i*3
  profit_df[i,1:3] <- rank_df[i,1:3]  <- fraction_vector[start_idx:end_idx]
  profit_df[i,4:9] <- colMeans(item$holdoutprofit)
  rank_df[i,4:9] <- rank(profit_df[i,4:9])
  i <- i + 1 
}





#write.csv(profit_df,"results/results_5x4x2_cv30.csv")
#write.csv(rank_df,"results/ranks_5x4x2_cv30_wrongseed.csv")

##



# OPTIMAL PROFITS & CURVES

library(ggplot2)

data <- rank_df
for (colname in colnames(data)) {
  data[,colname] <- as.factor(data[,colname])
  
  plot <- ggplot(data=data,aes(x = r_error_fraction,y = data[,colname],group = m_error_fraction,color=m_error_fraction)) +
    geom_line(size = 1) +
    facet_grid(~tau_error_fraction) +
    ggtitle(colname)
  print(plot)
}


library(tidyr)
library(ggthemes)
data <- profit_df
data <- gather(data,c('m_error_fraction','r_error_fractio'))

data$m_error_fraction <- as.factor(data$m_error_fraction)
profit_plot <- ggplot(data,aes(x = r_error_fraction))+
  geom_line(aes(y = lift,color = 'Uplift RF'), size = 1)+
  geom_line(aes(y = classic_sgb, color = 'SGB'), size = 1)+
  geom_line(aes(y = classic_reordered_sgb, color = 'Indirect CATE profit'), size = 1)+
  geom_line(aes(y = profitloss_leftw, color = 'Lemmens Gupta leftw'), size = 1)+
  geom_line(aes(y = profitloss_rightw, color = 'Lemmens Gupta rightw'), size = 1)+
  geom_line(aes(y = profitloss_symmw, color = 'Lemmens Gupta symmw'), size = 1)+
  facet_grid(m_error_fraction~tau_error_fraction,labeller = label_both)+
  ggtitle('Profits for all sets of error parameters') +
  ylab("Profit") +
  theme_test() +
  scale_colour_calc(name = "Scoring method")

profit_plot 



# PROFIT LIFT CURVES


extract_lift_data <- function(result_list,variable){
  curve <- colMeans(result_list[[variable]])
  n <- length(curve)
  # change model name
  modelname <- str_replace_all(
    variable,
    c(
      "holdoutcampaign.profit.curve.mysgbmodel" = "SGB",
      "holdoutcampaign.profit.curve.myrsgbmodel" = "Indirect CATE profit", 
      "holdoutcampaign.profit.curve.myliftmodel" = "Uplift RF",
      'holdoutcampaign.profit.curve.mywsgbmodel' = "Lemmens Gupta leftw",
      'holdoutcampaign.profit.curve.mywsgb2model' = "Lemmens Gupta rightw",
      'holdoutcampaign.profit.curve.mywsgb3model' = "Lemmens Gupta symmw"))
  return(data.frame(
    list(
      lift = curve,
      customer_fraction = seq(n),
      model = rep(modelname,times = n)
    )
  ))
}

model_profitcurve_names <- c(
  'holdoutcampaign.profit.curve.mysgbmodel',
  'holdoutcampaign.profit.curve.myrsgbmodel',
  'holdoutcampaign.profit.curve.myliftmodel',
  'holdoutcampaign.profit.curve.mywsgbmodel',
  'holdoutcampaign.profit.curve.mywsgb2model',
  'holdoutcampaign.profit.curve.mywsgb3model'
)




lift_curve_df <- data.frame()

for (i in seq_along(results)) {
  buffer_df_singleresult <- data.frame()
  for (curvename in model_profitcurve_names) {
    buffer_df_singlecurve <- extract_lift_data(results[[i]],curvename)
    buffer_df_singleresult <- rbind(buffer_df_singleresult,buffer_df_singlecurve)
  }
  r <- as.double(strsplit(names(results)[i],'_')[[1]][2])
  m <- as.double(strsplit(names(results)[i],'_')[[1]][4])
  tau <- as.double(strsplit(names(results)[i],'_')[[1]][6])
  buffer_df_parameter <- data.frame(
    list(
      r_error_fraction = rep(r,times = 300),
      m_error_fraction = rep(m,times = 300),
      tau_error_fraction = rep(tau,times = 300)
    )
  )
  buffer_df_singleresult <- cbind(buffer_df_singleresult,buffer_df_parameter)
  lift_curve_df <- rbind(lift_curve_df,buffer_df_singleresult)
}


# now the cumulative profits are so that 1 increment = 2%
# -> x-axis goes from 1 to 50
# -> change that it goes from 1 to 100
lift_curve_df$customer_fraction <- lift_curve_df$customer_fraction*2




lift_curve_df <- aggregate(list(lift = lift_curve_df[,'lift']), 
                           list(
                             r_error_fraction = lift_curve_df$r_error_fraction, #  
                             m_error_fraction = lift_curve_df$m_error_fraction,
                             tau_error_fraction = lift_curve_df$tau_error_fraction,
                             model  = lift_curve_df$model,
                             customer_fraction = lift_curve_df$customer_fraction), 
                           mean)



# Adding values for optimal lift curve and lift curve if treatments sent at random

theoretical_lift_if_contacted_values <- function(nobs,sorted){
  
  len_values <- 50
  cross_validations <- 30
  cumsum_matrix <- matrix(rep(NA,len_values*cross_validations),len_values,cross_validations)
  
  for (cv in 1:cross_validations) {
    data <- gen_data(0,0,0,round(nobs/3),seed = cv)
    data$profit_if_contacted <- 0
    # convincibles
    data$profit_if_contacted <- ifelse(data$r - data$w*data$tau_prob <= 0.5 & data$r + (1-data$w)*data$tau_prob > 0.5,data$revenues - 1,data$profit_if_contacted)
    # sleeping dogs
    data$profit_if_contacted <- ifelse(data$r - data$w*data$tau_prob > 0.5 & data$r + (1-data$w)*data$tau_prob <= 0.5,-data$revenues,data$profit_if_contacted)
    # customers that would stay no matter what
    data$profit_if_contacted <- ifelse(data$r - data$w*data$tau_prob > 0.5 & data$r + (1-data$w)*data$tau_prob > 0.5, - 1,data$profit_if_contacted)
    
    if (sorted) {
      data$profit_if_contacted <- sort(data$profit_if_contacted,decreasing = TRUE)
    }
    
    lendata <- length(data$profit_if_contacted)
    for (i in 1:len_values) {
      cumsum_matrix[i,cv] <- sum(data$profit_if_contacted[1:round(i*0.02*lendata)])
    }
  }
  
  return(rowMeans(cumsum_matrix))
}


optimal_curve_values <- theoretical_lift_if_contacted_values(nobs,TRUE)
optimal_curve_df <- data.frame(list(
  customer_fraction = 1:50,
  lift = optimal_curve_values
))
random_contacting_curve_values <- theoretical_lift_if_contacted_values(nobs,FALSE)
random_curve_df <- data.frame(list(
  customer_fraction = 1:50,
  lift = random_contacting_curve_values
))



drops <- c('lift','model')
lift_curve_df_copy <- unique(lift_curve_df[ , !(names(lift_curve_df) %in% drops)])
lift_curve_df_optimals <- merge(lift_curve_df_copy,optimal_curve_df,by = 'customer_fraction')
lift_curve_df_random <- merge(lift_curve_df_copy,random_curve_df,by = 'customer_fraction')

lift_curve_df_optimals$model <- 'optimal'
lift_curve_df_random$model <- "random"

#lift_curve_df <- rbind(lift_curve_df,lift_curve_df_optimals)
# lift_curve_df <- rbind(lift_curve_df,lift_curve_df_random)




plot_m1 <- ggplot(data = lift_curve_df[which(lift_curve_df$m_error_fraction==1),],aes(x = customer_fraction,y = lift,group = model,color=model))+
  geom_line(size = 1)+
  facet_grid(tau_error_fraction~r_error_fraction,labeller = label_both)+
  ggtitle('Lift curves when m_error_fraction = 1')+
  ylab('Lift $') +
  theme_test() +
  scale_colour_calc(name = "Scoring method")+ 
  theme(legend.text=element_text(size=8))

plot_m0 <- ggplot(data = lift_curve_df[which(lift_curve_df$m_error_fraction==0),],aes(x = customer_fraction,y = lift,group = model,color=model))+
  geom_line(size = 1)+
  facet_grid(tau_error_fraction~r_error_fraction,labeller = label_both)+
  ggtitle('Lift curves when m_error_fraction = 0')+
  ylab('Lift $') +
  theme_test() +
  scale_colour_calc(name = "Scoring method")+ 
  theme(legend.text=element_text(size=8))



#  JAKTA! MUUTA DESCRIPTIONIT
plot_m1
plot_m0



### Curve comparing the best outcome to the optimal profits


lift_curve_df_with_optimals <- rbind(lift_curve_df,lift_curve_df_optimals)


data <- lift_curve_df_with_optimals[which(
  lift_curve_df_with_optimals$r_error_fraction == 0 &
  lift_curve_df_with_optimals$m_error_fraction == 0 &
  lift_curve_df_with_optimals$tau_error_fraction == 0.1
),]

plot_optimal_comparison <- ggplot(data = data,aes(x = customer_fraction,y = lift,group = model,color=model))+
  geom_line(size = 1)+
  ggtitle('Lift curves when r & m = 0 and tau = 0.1')+
  ylab('Lift $')

plot_optimal_comparison



## T-TEST TABLE

# Two independent sample t-test


do_ttest_for_single_result <- function(singleresult){
  
  
  singleresult_profits <- as.data.frame(singleresult$holdoutprofit)
  colnames(singleresult_profits) <- c('lift','sgb','rsgb','wsgb','wsgb2','wsgb3')
  
  t_test_function <- function(baseline_var,compared_var)  {
    ttest <- t.test(baseline_var,compared_var,paired = TRUE)
    list(
      pval = format(round(ttest$p.value,3),nsmall=3),
      stderr = format(round(ttest$stderr,2),nsmall=2)
    )
  }
  
  largest_profit_sofar <- mean(singleresult_profits$sgb)
  profit_differences_ttest_vector<-c(paste0(format(round(mean(largest_profit_sofar),1),nsmall=1),'           '))
  
  for (i in c('rsgb','lift','wsgb','wsgb2','wsgb3')) {
    compared_col <- singleresult_profits[,i]
    meanprofit <- mean(compared_col)
    buffer_ttest <- t_test_function(singleresult_profits$sgb,compared_col)
    
    if (buffer_ttest$pval>0.1) {
      stars <- "   "
    } else if (buffer_ttest$pval>0.05) {
      stars <- "*  "
    } else if (buffer_ttest$pval>0.01){
      stars <- "** "
    } else {
      stars <- "***"
    }
    
    buffer_vector <- c(
      paste0(
        format(round(mean(compared_col),1),nsmall=1),
        " (",
        buffer_ttest$stderr,
        ")",
        stars
      )
    )
    profit_differences_ttest_vector <- append(profit_differences_ttest_vector,buffer_vector)
    
    largest_profit_sofar <- ifelse(meanprofit>largest_profit_sofar,meanprofit,largest_profit_sofar)
  }
  
  largest_profit_aschar <- as.character(format(round(mean(largest_profit_sofar),1),nsmall=1))
  
  # making the largest profit bolded in latex
  profit_differences_ttest_vector <- gsub(
    largest_profit_aschar,
    paste0("textbf{",largest_profit_aschar,"}"),
    profit_differences_ttest_vector)
  
  return(profit_differences_ttest_vector)
  
}

# Actually make the DF
ttest_df  <- data.frame(
  R_error = numeric(),
  M_error = numeric(),
  Tau_error = numeric(),
  SGB = character(),
  Indirect_CATE_profit = character(),
  Uplift_RF = character(),
  Lemmens_Gupta_leftw = character(),
  Lemmens_Gupta_rightw = character(),
  Lemmens_Gupta_symmw = character()
)

colnames(ttest_df) <- gsub("_"," ",colnames(ttest_df))

i <- 1
for (item in results) {
  start_idx <- i*3-2
  end_idx <- i*3
  ttest_df[i,1:3]  <- fraction_vector[start_idx:end_idx]
  ttest_df[i,4:9] <- do_ttest_for_single_result(item)

  i <- i + 1 
}

#sorting df

ttest_df <- ttest_df[order(ttest_df$'R error'),]


# i hate this solution but i want to get the bold texts working. If i had $backslash$textbf in the rows, it got interpreted as tab break.
# solution: write it first to txt and then replace

library(xtable)

# adding note to table
comment <- list()
comment$pos <- list()
comment$pos[[1]] <- c(nrow(ttest_df))
comment$command <- c(paste("\\hline\n",
                           "& & & & & Format: profit (stderr) & * p $<$ 0.1,** p $<$ 0.05, & *** p $<$ 0.01 \n", sep = ""))

print(
  xtable(ttest_df),
  file="ttest_table_pretabular_inserted.txt",
  include.rownames=FALSE,
  table.placement = 'b',
  scalebox = 1.1,
  add.to.row = comment, 
  hline.after = c(-1,0))

shell.exec("format_ttest_latex_txt.bat")
# TAKE RESULTS FROM THE FILE THIS .bat MODIFIED AND INSERT TO TEXWORKS
