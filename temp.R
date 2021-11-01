
data <- data.frame(gen_data(0,0,0,2000))


calculate_max_and_min_profit <- function(data) {
  # calculating true max profit of a contacting campaign:
  # 1. identify convinceable customers
  # 2. multiply with revenue
  
  # Min profit:
  # 1. identify customers that will leave if contacted "sleeping dogs"
  # 2. multiply with revenue
  
  # in the ifelse, the first term corresponds to the true r_0 and the second one to r_1
  data['convinceable'] <- ifelse(data$r - data$w*data$tau_prob < 0.5 & data$r + (1-data$w)*data$tau_prob > 0.5,1,0)
  data['sleeping_dog'] <- ifelse(data$r - data$w*data$tau_prob > 0.5 & data$r + (1-data$w)*data$tau_prob < 0.5,1,0)
  
  maxprofit <- sum(data$convinceable * data$revenues)
  minprofit <- sum(data$sleeping_dog * -data$revenues)
  
  list(
    maxprofit = maxprofit,
    minprofit = minprofit,
    convinceables = sum(data$convinceable),
    sleeping_dogs = sum(data$sleeping_dogs)
  )

}


maxprofits <- c()
minprofits <- c()
sum_convinceables <- c()
sum_sleeping_dogs <- c()
for (i in 1:40) {
  data <- data.frame(gen_data(0,0,0,2000,i))
  calculation <- calculate_max_and_min_profit(data)
  maxprofits[i] <- calculation$maxprofit
  minprofits[i] <- calculation$minprofit
  sum_convinceables[i] <- calculation$convinceables
  sum_sleeping_dogs[i] <- calculation$sleeping_dogs
}

max(maxprofits)
min(maxprofits)

mean(maxprofits)
sd(maxprofits)

mean(minprofits)
sd(minprofits)


calculate_max_and_min_profit(data.frame(gen_data(0,0,0,2000,1)))$maxprofit
