
rm(list = ls())
#setwd("Z:/skolarbete/gradu/R")
load("results/cv30_fractions_5x4x2.RData")



# use this code to copy paste all the variables below

for (var in ls(pattern = 'results_with')) {
  cat(substr(var,30,50),' = ',var, ',\n')
}


results <- list(
  r_0.2_m_0.3_tau_0.1  =  results_with_error_fractions_r_0.2_m_0.3_tau_0.1 ,
  r_0.2_m_0.3_tau_0.9  =  results_with_error_fractions_r_0.2_m_0.3_tau_0.9 ,
  r_0.2_m_0.6_tau_0.1  =  results_with_error_fractions_r_0.2_m_0.6_tau_0.1 ,
  r_0.2_m_0.6_tau_0.9  =  results_with_error_fractions_r_0.2_m_0.6_tau_0.9 ,
  r_0.2_m_0.9_tau_0.1  =  results_with_error_fractions_r_0.2_m_0.9_tau_0.1 ,
  r_0.2_m_0.9_tau_0.9  =  results_with_error_fractions_r_0.2_m_0.9_tau_0.9 ,
  r_0.2_m_0_tau_0.1  =  results_with_error_fractions_r_0.2_m_0_tau_0.1 ,
  r_0.2_m_0_tau_0.9  =  results_with_error_fractions_r_0.2_m_0_tau_0.9 ,
  r_0.4_m_0.3_tau_0.1  =  results_with_error_fractions_r_0.4_m_0.3_tau_0.1 ,
  r_0.4_m_0.3_tau_0.9  =  results_with_error_fractions_r_0.4_m_0.3_tau_0.9 ,
  r_0.4_m_0.6_tau_0.1  =  results_with_error_fractions_r_0.4_m_0.6_tau_0.1 ,
  r_0.4_m_0.6_tau_0.9  =  results_with_error_fractions_r_0.4_m_0.6_tau_0.9 ,
  r_0.4_m_0.9_tau_0.1  =  results_with_error_fractions_r_0.4_m_0.9_tau_0.1 ,
  r_0.4_m_0.9_tau_0.9  =  results_with_error_fractions_r_0.4_m_0.9_tau_0.9 ,
  r_0.4_m_0_tau_0.1  =  results_with_error_fractions_r_0.4_m_0_tau_0.1 ,
  r_0.4_m_0_tau_0.9  =  results_with_error_fractions_r_0.4_m_0_tau_0.9 ,
  r_0.6_m_0.3_tau_0.1  =  results_with_error_fractions_r_0.6_m_0.3_tau_0.1 ,
  r_0.6_m_0.3_tau_0.9  =  results_with_error_fractions_r_0.6_m_0.3_tau_0.9 ,
  r_0.6_m_0.6_tau_0.1  =  results_with_error_fractions_r_0.6_m_0.6_tau_0.1 ,
  r_0.6_m_0.6_tau_0.9  =  results_with_error_fractions_r_0.6_m_0.6_tau_0.9 ,
  r_0.6_m_0.9_tau_0.1  =  results_with_error_fractions_r_0.6_m_0.9_tau_0.1 ,
  r_0.6_m_0.9_tau_0.9  =  results_with_error_fractions_r_0.6_m_0.9_tau_0.9 ,
  r_0.6_m_0_tau_0.1  =  results_with_error_fractions_r_0.6_m_0_tau_0.1 ,
  r_0.6_m_0_tau_0.9  =  results_with_error_fractions_r_0.6_m_0_tau_0.9 ,
  r_0.8_m_0.3_tau_0.1  =  results_with_error_fractions_r_0.8_m_0.3_tau_0.1 ,
  r_0.8_m_0.3_tau_0.9  =  results_with_error_fractions_r_0.8_m_0.3_tau_0.9 ,
  r_0.8_m_0.6_tau_0.1  =  results_with_error_fractions_r_0.8_m_0.6_tau_0.1 ,
  r_0.8_m_0.6_tau_0.9  =  results_with_error_fractions_r_0.8_m_0.6_tau_0.9 ,
  r_0.8_m_0.9_tau_0.1  =  results_with_error_fractions_r_0.8_m_0.9_tau_0.1 ,
  r_0.8_m_0.9_tau_0.9  =  results_with_error_fractions_r_0.8_m_0.9_tau_0.9 ,
  r_0.8_m_0_tau_0.1  =  results_with_error_fractions_r_0.8_m_0_tau_0.1 ,
  r_0.8_m_0_tau_0.9  =  results_with_error_fractions_r_0.8_m_0_tau_0.9 ,
  r_0_m_0.3_tau_0.1  =  results_with_error_fractions_r_0_m_0.3_tau_0.1 ,
  r_0_m_0.3_tau_0.9  =  results_with_error_fractions_r_0_m_0.3_tau_0.9 ,
  r_0_m_0.6_tau_0.1  =  results_with_error_fractions_r_0_m_0.6_tau_0.1 ,
  r_0_m_0.6_tau_0.9  =  results_with_error_fractions_r_0_m_0.6_tau_0.9 ,
  r_0_m_0.9_tau_0.1  =  results_with_error_fractions_r_0_m_0.9_tau_0.1 ,
  r_0_m_0.9_tau_0.9  =  results_with_error_fractions_r_0_m_0.9_tau_0.9 ,
  r_0_m_0_tau_0.1  =  results_with_error_fractions_r_0_m_0_tau_0.1 ,
  r_0_m_0_tau_0.9  =  results_with_error_fractions_r_0_m_0_tau_0.9 # delete comma here
)

# holdoutprofit columns:
# 1: lift-model
# 2: Classic SGB model
# 3: Classic reordered SGB model
# 4: Profit wSGB model, left weighting
# 5: Profit wSGB model, right weighting
# 6: Profit wSGB model, symmetric weighting


profit_df <- data.frame(
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

# T-test abut the profit distributiones
# HERE!!!

i <- 1
for (item in results) {
  start_idx <- i*3-2
  end_idx <- i*3
  profit_df[i,1:3] <- fraction_vector[start_idx:end_idx]
  profit_df[i,4:9] <- colMeans(item$holdoutprofit)
  i <- i + 1 
}


write.csv(profit_df,"results/results_5x4x2_cv30.csv")

##

# testing
plot(colMeans( r_0_m_0_tau_0.1$holdoutcampaign.profit.curve.mysgbmodel),type = 'l')


temp <- data.frame(gen_data(0.2,0.9,0.1,2000))
temp1 <- data.frame(gen_data(0,0.9,0.1,2000))

mean(temp$revenues)
mean(temp1$revenues)

for (item in names(results)) {
  print(item)
}
