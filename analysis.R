
rm(list = ls())
#setwd("Z:/skolarbete/gradu/R")
load("results/cv30_fractions_3x3x2.RData")



# use this code to copy paste all the variables below

for (var in ls(pattern = 'results_with')) {
  cat(substr(var,30,50),' <- ',var, ',\n')
}


results <- list(
  r_0.4_m_0  <-  results_with_error_fractions_r_0.4_m_0 ,
  r_0.4_m_0.4  <-  results_with_error_fractions_r_0.4_m_0.4 ,
  r_0.4_m_0.8  <-  results_with_error_fractions_r_0.4_m_0.8 ,
  r_0.8_m_0  <-  results_with_error_fractions_r_0.8_m_0 ,
  r_0.8_m_0.4  <-  results_with_error_fractions_r_0.8_m_0.4 ,
  r_0.8_m_0.8  <-  results_with_error_fractions_r_0.8_m_0.8 ,
  r_0_m_0  <-  results_with_error_fractions_r_0_m_0 ,
  r_0_m_0.4  <-  results_with_error_fractions_r_0_m_0.4 ,
  r_0_m_0.8  <-  results_with_error_fractions_r_0_m_0.8   # delete comma here
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
  as.double(strsplit(var,'_')[[1]][8])
  )
  fraction_vector <- c(fraction_vector,buffer_vec)
}

# T-test abut the profit distributiones
# HERE!!!

i <- 1
for (item in results) {
  start_idx <- i*2-1
  end_idx <- i*2
  profit_df[i,1:2] <- fraction_vector[start_idx:end_idx]
  profit_df[i,3:8] <- colMeans(item$holdoutprofit)
  i <- i + 1 
}


write.csv(profit_df,"results_test.csv")

