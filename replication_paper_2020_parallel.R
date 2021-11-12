# =============================#
# Main running script (MAIN)  #
# =============================#

setwd('C:/Users/lassi/Documents/gradu')
#setwd("Z:/skolarbete/gradu/R")

library(foreach)
library(doParallel)
registerDoParallel(cores=8)

library(devtools)
load_all('ProfitBoost')
source("dgp.R")
start_time <- Sys.time()
#-----------#
# load data #
#-----------#

# variable names:
# y = post-intervention churn
# w = treated or not
# revenues = post-intervention cash flow



# load data
#load("data/mysynthdata.rda")


# data specification
n                     <- 2000 # rows in data to generate
p                     <- 10   # Columns in generated data
r_error_fractions     <- c(0,0.2,0.4,0.6,0.8) # What error fractions should datasets be generated with
m_error_fractions     <- c(0,1) #  be generated with
tau_error_fractions   <- c(0.1,0.5,0.9)
colnumbers.covariates <- 4:13 # indicates the position of all predictors
                              # excluding w (excludes w, y and revenues)
pos.covariates        <- 3:13 # indicates the position (column number)
                              # of all predictors including w
                              # (excludes w, y and revenues)
pos.y                 <- 14   # indicates the position (column number)
                              # of y in the data set
delta                 <- 1   # intervention cost
conditional           <- TRUE # type of offer
increment             <- .02  # granularity level to determine optimal
                              # campaign size
budget                <- 100 # budget to determine target size using budget
                              # constraint
buffer                <- .10  # add buffer of 10% to optimized target size

# hyperparameters for the SGB algorithm

maxiter               <- 3000
miniter               <- 1500
rho                   <- .001
stoch                 <- TRUE
ratio                 <- .3

B                     <- 30 # change this number to get multiple iterations


# Holdout profit for all methods, including symmetric,
# left and right weighting (for Table 1)
holdoutprofit         <- array(NA, c(B, 6))

# Holdout profit for alternative target size selections (for Table 2)
fixedsize.churn <-
  fixedsize.budget <-
  optsize.verbeke <-
  optsize.buffer <-
  array(NA, c(B, 1))

# Holdout gini for all methods (for Table 3)
holdoutgini           <- array(NA, c(B, 4))

# Holdout tdl for all methods
holdouttdl            <- array(NA, c(B, 4))

TSlength <-
  length(seq(0,n / 3, n / 3 * increment)[-1])

# campaign profit curves
holdoutcampaign.profit.curve.myliftmodel <-
  holdoutcampaign.profit.curve.mysgbmodel <-
  holdoutcampaign.profit.curve.myrsgbmodel <-
  holdoutcampaign.profit.curve.mywsgbmodel <-
  holdoutcampaign.profit.curve.mywsgb2model <-
  holdoutcampaign.profit.curve.mywsgb3model <-
  array(NA, c(B, TSlength))

# customer overlap curves for 10 deciles
splitn <- 10
overlap.SGB.WSGB <- overlap.RSGB.WSGB <- overlap.LIFTrm.WSGB <-
  array(NA, c(B, splitn + 1))

# for reproducible results w.r.t. older versions of R
suppressWarnings(RNGkind(sample.kind = "Rounding"))

# start bootstrapping procedure:

for (r_error_fraction in r_error_fractions) {
  for (m_error_fraction in m_error_fractions) {
    for (tau_error_fraction in tau_error_fractions) {
      mysynthdata <- data.frame(gen_data(r_error_fraction,m_error_fraction,tau_error_fraction,n))
      start_time_fraction <- Sys.time()
      print(paste0('error fraction r:',r_error_fraction," error fraction m: ",m_error_fraction," error fraction tau: ",tau_error_fraction," cv iterations: ",B))
      
      foreach_results_buffer <- foreach (b=1:B,
                                      .packages='devtools') %dopar% {
        load_all('ProfitBoost')
        print(paste0('error fraction r:',r_error_fraction," error fraction m: ",m_error_fraction," error fraction tau: ",tau_error_fraction," cv iteration: ",b," out of ",B))
        
        # Variables that need to be defined within foreach
        holdoutgini <- c()
        holdoutprofit <- c()
        holdouttdl <- c()
        
        #-----------------------------------------#
        # Uplift Models for Retention and Margins #
        #-----------------------------------------#
      
        print("Estimating Uplift Model")
      
        # 1. estimate model on calibration sample, and make predictions
        # for all observations
        myliftproc <- lift.procedure(
          myseed = b,
          mydata = mysynthdata,
          colnumbers.covariates = colnumbers.covariates,
          delta = delta,
          conditional = conditional
        )
      
        # 2. optimize target size in the validation sample
        # result: optimal target size, in percentage
        targetsize.mylift.valid <- targetsizeoptimization(
          y = myliftproc$valid.data$y,
          treated = myliftproc$valid.data$w,
          scores = myliftproc$rmlift.valid,
          m = myliftproc$valid.data$revenues,
          delta = delta,
          conditional = conditional,
          increment = increment,
          plot = FALSE
        )$targetsize.maxprofit.trimmed / length(myliftproc$valid.data$y)
      
        # 3. evaluate campaign profit on the holdout test sample
        myeval.myliftmodel <- campaignevaluation(
          y = myliftproc$test.data$y,
          treated = myliftproc$test.data$w,
          scores = myliftproc$rmlift.test,
          m = myliftproc$test.data$revenues,
          delta = delta,
          conditional = conditional,
          opttargetperc = targetsize.mylift.valid,
          increment = increment,
          plot = TRUE
        )
      
        # holdout campaign profit at optimized target size # TABLE 1
        holdoutprofit[1] <- myeval.myliftmodel$campaign.evaluation
      
        # campaign profit curve on the test sample # FIGURE 2
        holdoutcampaign.profit.curve.myliftmodel <-
          myeval.myliftmodel$campaign.profit.curve
      
        # 5. Calculate Holdout Gini Coefficient and Top Decile Lift
        holdoutgini[1] <-
          gini(y = myliftproc$test.data$y, p = myliftproc$rmlift.test)
      
        holdouttdl[1] <- top(
          y = myliftproc$test.data$y,
          p = myliftproc$rmlift.test,
          share = .1
        )
      
        #-------------------#
        # Classic SGB model #
        #-------------------#
      
        print("Estimating SGB with classic loss")
      
        # 1. estimate w-sgb on calibration sample and make predictions on
        # all three samples
        mysgbproc <- wsgb.procedure(
          myseed = b,
          mydata = mysynthdata,
          pos.covariates = pos.covariates,
          pos.y = pos.y,
          delta = delta,
          conditional = conditional,
          loss = "classic",
          # choice between symmetric weighting, left weighting,
          # right weighting, equal weigthing
          reorder = FALSE,
          pi = (myliftproc$rmlift.train) / 10,
          # divided by 10 to keep scale manageable (cf log computation)
          pi.valid = (myliftproc$rmlift.valid) / 10,
          increment = increment,
          maxiter = maxiter,
          miniter = miniter,
          rho = rho,
          stoch = stoch,
          ratio = ratio,
          m0.train = myliftproc$m0.train,
          # for re-ordered classic loss
          m1.train = myliftproc$m0.train,
          m0.valid = myliftproc$m0.valid,
          m1.valid = myliftproc$m0.valid,
          m0.test = myliftproc$m0.test,
          m1.test = myliftproc$m0.test,
          verbose = FALSE
        )
      
        # 2. optimize target size in the validation sample
        # result: optimal target size, in percentage
        targetsize.sgb.valid <- targetsizeoptimization(
          y = mysgbproc$valid.data$y,
          treated = mysgbproc$valid.data$w,
          scores = mysgbproc$scores.valid,
          m = mysgbproc$valid.data$revenues,
          delta = delta,
          conditional = conditional,
          increment = increment,
          plot = FALSE
        )$targetsize.maxprofit.trimmed / length(mysgbproc$valid.data$y)
      
        # 3. evaluate campaign profit on the holdout test sample
        myeval.mysgbmodel <- campaignevaluation(
          y = mysgbproc$test.data$y,
          treated = mysgbproc$test.data$w,
          scores = mysgbproc$scores.test,
          m = mysgbproc$test.data$revenues,
          delta = delta,
          conditional = conditional,
          opttargetperc = targetsize.sgb.valid,
          increment = increment,
          plot = TRUE
        )
        # holdout campaign profit at optimized target size # TABLE 1
        holdoutprofit[2] <- myeval.mysgbmodel$campaign.evaluation
      
        # campaign profit curve on the test sample # FIGURE 2
        holdoutcampaign.profit.curve.mysgbmodel <-
          myeval.mysgbmodel$campaign.profit.curve
      
        # 5. Calculate Holdout Gini Coefficient and Top Decile Lift
        holdoutgini[2] <-
          gini(y = mysgbproc$test.data$y, p = mysgbproc$scores.test)
      
        holdouttdl[2] <- top(
          y = mysgbproc$test.data$y,
          p = mysgbproc$scores.test,
          share = .1
        )
      
        #-----------------------------#
        # Classic reordered SGB model #
        #-----------------------------#
        print("Estimating SGB with reordered classic loss")
        # 1. estimate r-sgb on calibration sample and make predictions on
        # all three samples
        myrsgbproc <- wsgb.procedure(
          myseed = b,
          mydata = mysynthdata,
          pos.covariates = pos.covariates,
          pos.y = pos.y,
          delta = delta,
          conditional = conditional,
          loss = "classic",
          # choice between symmetric weighting, left weighting,
          # right weighting, equal weigthing
          reorder = TRUE,
          pi = (myliftproc$rmlift.train) / 10,
          # divided by 10 to keep scale manageable (cf log computation)
          pi.valid = (myliftproc$rmlift.valid) / 10,
          increment = increment,
          maxiter = maxiter,
          miniter = miniter,
          rho = rho,
          stoch = stoch,
          ratio = ratio,
          m0.train = myliftproc$m0.train,
          # for re-ordered classic loss
          m1.train = myliftproc$m0.train,
          m0.valid = myliftproc$m0.valid,
          m1.valid = myliftproc$m0.valid,
          m0.test = myliftproc$m0.test,
          m1.test = myliftproc$m0.test,
          verbose = FALSE
        )
        
        # POTENTIAL FIX FOR WRONG RSGB RESULTS!
        myrsgbproc$scores.valid <- myrsgbproc$scores.valid*-1
        myrsgbproc$scores.test <- myrsgbproc$scores.test*-1
      
        # 2. optimize target size in the validation sample
        # result: optimal target size, in percentage
        targetsize.rsgb.valid <- targetsizeoptimization(
          y = myrsgbproc$valid.data$y,
          treated = myrsgbproc$valid.data$w,
          scores = myrsgbproc$scores.valid,
          m = myrsgbproc$valid.data$revenues,
          delta = delta,
          conditional = conditional,
          increment = increment,
          plot = FALSE
        )$targetsize.maxprofit.trimmed / length(myrsgbproc$valid.data$y)
      
        # 3. evaluate campaign profit on the holdout test sample
        myeval.myrsgbmodel <- campaignevaluation(
          y = myrsgbproc$test.data$y,
          treated = myrsgbproc$test.data$w,
          scores = myrsgbproc$scores.test,
          m = myrsgbproc$test.data$revenues,
          delta = delta,
          conditional = conditional,
          opttargetperc = targetsize.rsgb.valid,
          increment = increment,
          plot = TRUE
        )
      
        # holdout campaign profit at optimized target size # TABLE 1
        holdoutprofit[3] <- myeval.myrsgbmodel$campaign.evaluation
      
        # campaign profit curve on the test sample # FIGURE 2
        holdoutcampaign.profit.curve.myrsgbmodel <-
          myeval.myrsgbmodel$campaign.profit.curve
      
        # 5. Calculate Holdout Gini Coefficient and Top Decile Lift
        holdoutgini[3] <- gini(y = myrsgbproc$test.data$y,
                                  p = myrsgbproc$scores.test)
        holdouttdl[3] <- top(
          y = myrsgbproc$test.data$y,
          p = myrsgbproc$scores.test,
          share = .1
        )
      
        #-----------------------------------#
        # Profit wSGB model, left weighting #
        #-----------------------------------#
      
        print("Estimating wSGB with left weighting")
      
        # 1. estimate w-sgb on calibration sample and make predictions on
        # all three samples
        mywsgbproc <- wsgb.procedure(
          myseed = b,
          mydata = mysynthdata,
          pos.covariates = pos.covariates,
          pos.y = pos.y,
          delta = delta,
          conditional = conditional,
          loss = "left weighting",
          # choice between symmetric weighting, left weighting,
          # right weighting, equal weigthing
          reorder = FALSE,
          pi = (myliftproc$rmlift.train) / 10,
          # divided by 10 to keep scale manageable (cf log computation)
          pi.valid = (myliftproc$rmlift.valid) / 10,
          increment = increment,
          maxiter = maxiter,
          miniter = miniter,
          rho = rho,
          stoch = stoch,
          ratio = ratio,
          m0.train = myliftproc$m0.train,
          # for re-ordered classic loss
          m1.train = myliftproc$m0.train,
          m0.valid = myliftproc$m0.valid,
          m1.valid = myliftproc$m0.valid,
          m0.test = myliftproc$m0.test,
          m1.test = myliftproc$m0.test,
          verbose = FALSE
        )
      
        # 2. optimize target size in the validation sample
        # result: optimal target size, in percentage
        targetsize.wsgb.valid <- targetsizeoptimization(
          y = mywsgbproc$valid.data$y,
          treated = mywsgbproc$valid.data$w,
          scores = mywsgbproc$scores.valid,
          m = mywsgbproc$valid.data$revenues,
          delta = delta,
          conditional = conditional,
          increment = increment,
          plot = FALSE
        )$targetsize.maxprofit.trimmed / length(mywsgbproc$valid.data$y)
      
        # 3. evaluate campaign profit on the holdout test sample
        myeval.mywsgbmodel <- campaignevaluation(
          y = mywsgbproc$test.data$y,
          treated = mywsgbproc$test.data$w,
          scores = mywsgbproc$scores.test,
          m = mywsgbproc$test.data$revenues,
          delta = delta,
          conditional = conditional,
          opttargetperc = targetsize.wsgb.valid,
          increment = increment,
          plot = TRUE
        )
      
        # holdout campaign profit at optimized target size # TABLE 1
        holdoutprofit[4] <- myeval.mywsgbmodel$campaign.evaluation
      
        # campaign profit curve on the test sample # FIGURE 2
        holdoutcampaign.profit.curve.mywsgbmodel <-
          myeval.mywsgbmodel$campaign.profit.curve
      
        # 4. alternative target size selections
      
        # 4.1. Fixed target size based on churnrate
        fixedsize.churn <- fixedsizecampaignevaluation(
          y = mywsgbproc$test.data$y,
          treated = mywsgbproc$test.data$w,
          scores = mywsgbproc$scores.test,
          m = mywsgbproc$test.data$revenues,
          delta = delta,
          conditional = conditional,
          fixedsizeperc = mean(mywsgbproc$valid.data$y),
          increment = increment
        )$profit.fixedsize # holdout campaign profit at fixed size # TABLE 2
      
        # 4.2. Fixed target size based on budget constraint
        fixedsize.budget <- fixedsizecampaignevaluation(
          y = mywsgbproc$test.data$y,
          treated = mywsgbproc$test.data$w,
          scores = mywsgbproc$scores.test,
          m = mywsgbproc$test.data$revenues,
          delta = delta,
          conditional = conditional,
          fixedsizeperc = (budget /
            delta) / length(mywsgbproc$test.data$y),
          increment = increment
        )$profit.fixedsize # holdout campaign profit at fixed size # TABLE 2
      
        # 4.3. Optimized target size based on Verbeke
        targetsize.verbeke <- verbeke(
          y = mywsgbproc$valid.data$y,
          scores = mywsgbproc$scores.valid,
          m = mywsgbproc$valid.data$revenues,
          delta = delta,
          increment = increment,
          gamma = mean(myliftproc$r0.valid - myliftproc$r1.valid),
          # i.e., average (estimated) churn lift
          c = 0,
          A = 0,
          plot = FALSE
        )$targetsize.maxprofit / length(mywsgbproc$valid.data$y)
      
        optsize.verbeke <- campaignevaluation(
          y = mywsgbproc$test.data$y,
          treated = mywsgbproc$test.data$w,
          scores = mywsgbproc$scores.test,
          m = mywsgbproc$test.data$revenues,
          delta = delta,
          conditional = conditional,
          opttargetperc = targetsize.verbeke,
          increment = increment,
          plot = TRUE
        )$campaign.evaluation
      
        # 4.4. 10% Buffer
        optsize.buffer <- campaignevaluation(
          y = mywsgbproc$test.data$y,
          treated = mywsgbproc$test.data$w,
          scores = mywsgbproc$scores.test,
          m = mywsgbproc$test.data$revenues,
          delta = delta,
          conditional = conditional,
          opttargetperc = targetsize.sgb.valid + buffer,
          increment = increment,
          plot = TRUE
        )$campaign.evaluation
      
        # 5. Calculate Holdout Gini Coefficient and Top Decile Lift
      
        holdoutgini[4] <-
          gini(y = mywsgbproc$test.data$y, p = mywsgbproc$scores.test)
      
        holdouttdl[4] <- top(
          y = mywsgbproc$test.data$y,
          p = mywsgbproc$scores.test,
          share = .1
        )
      
        #------------------------#
        # Overlap between models #
        #------------------------#
      
        overlap.SGB.WSGB <-
          overlap.per.decile(mysgbproc$scores.test, mywsgbproc$scores.test, plot = F)
      
        overlap.RSGB.WSGB <-
          overlap.per.decile(myrsgbproc$scores.test, mywsgbproc$scores.test, plot = F)
      
        overlap.LIFTrm.WSGB <-
          overlap.per.decile(myliftproc$rmlift.test, mywsgbproc$scores.test, plot = F)
      
      
        #------------------------------------#
        # Profit wSGB model, right weighting #
        #------------------------------------#
      
        print("Estimating wSGB with right weighting")
      
        # 1. estimate w-sgb on calibration sample and
        #    make predictions on all three samples
      
        mywsgbproc2 <- wsgb.procedure(
          myseed = b,
          mydata = mysynthdata,
          pos.covariates = pos.covariates,
          pos.y = pos.y,
          delta = delta,
          conditional = conditional,
          loss = "right weighting",
          # choice between symmetric weighting, left weighting,
          # right weighting, equal weigthing
          reorder = FALSE,
          pi = (myliftproc$rmlift.train) / 10,
          # divided by 10 to keep scale manageable (cf log computation)
          pi.valid = (myliftproc$rmlift.valid) / 10,
          increment = increment,
          maxiter = maxiter,
          miniter = miniter,
          rho = rho,
          stoch = stoch,
          ratio = ratio,
          m0.train = myliftproc$m0.train,
          # for re-ordered classic loss
          m1.train = myliftproc$m0.train,
          m0.valid = myliftproc$m0.valid,
          m1.valid = myliftproc$m0.valid,
          m0.test = myliftproc$m0.test,
          m1.test = myliftproc$m0.test,
          verbose = FALSE
        )
      
        # 2. optimize target size in the validation sample
        #    result: optimal target size, in percentage
      
        targetsize.wsgb2.valid <- targetsizeoptimization(
          y = mywsgbproc2$valid.data$y,
          treated = mywsgbproc2$valid.data$w,
          scores = mywsgbproc2$scores.valid,
          m = mywsgbproc2$valid.data$revenues,
          delta = delta,
          conditional = conditional,
          increment = increment,
          plot = FALSE
        )$targetsize.maxprofit.trimmed / length(mywsgbproc2$valid.data$y)
      
        # 3. evaluate campaign profit on the holdout test sample
      
        myeval.mywsgb2model <- campaignevaluation(
          y = mywsgbproc2$test.data$y,
          treated = mywsgbproc2$test.data$w,
          scores = mywsgbproc2$scores.test,
          m = mywsgbproc2$test.data$revenues,
          delta = delta,
          conditional = conditional,
          opttargetperc = targetsize.wsgb2.valid,
          increment = increment,
          plot = TRUE
        )
      
        # holdout campaign profit at optimized target size # TABLE 1
        holdoutprofit[5] <- myeval.mywsgb2model$campaign.evaluation
      
        # campaign profit curve on the test sample # FIGURE 2
        holdoutcampaign.profit.curve.mywsgb2model <-
          myeval.mywsgb2model$campaign.profit.curve
      
      
        #----------------------------------------#
        # Profit wSGB model, symmetric weighting #
        #----------------------------------------#
      
        print("Estimating wSGB with symmetric weighting")
      
        # 1. estimate w-sgb on calibration sample and make predictions
        # on all three samples
        mywsgbproc3 <- wsgb.procedure(
          myseed = b,
          mydata = mysynthdata,
          pos.covariates = pos.covariates,
          pos.y = pos.y,
          delta = delta,
          conditional = conditional,
          loss = "symmetric weighting",
          # choice between symmetric weighting, left weighting,
          # right weighting, equal weigthing
          reorder = FALSE,
          pi = (myliftproc$rmlift.train) / 10,
          # divided by 10 to keep scale manageable (cf log computation)
          pi.valid = (myliftproc$rmlift.valid) / 10,
          increment = increment,
          maxiter = maxiter,
          miniter = miniter,
          rho = rho,
          stoch = stoch,
          ratio = ratio,
          m0.train = myliftproc$m0.train,
          # for re-ordered classic loss
          m1.train = myliftproc$m0.train,
          m0.valid = myliftproc$m0.valid,
          m1.valid = myliftproc$m0.valid,
          m0.test = myliftproc$m0.test,
          m1.test = myliftproc$m0.test,
          verbose = FALSE
        )
      
        # 2. optimize target size in the validation sample
        # result: optimal target size, in percentage
        targetsize.wsgb3.valid <- targetsizeoptimization(
          y = mywsgbproc3$valid.data$y,
          treated = mywsgbproc3$valid.data$w,
          scores = mywsgbproc3$scores.valid,
          m = mywsgbproc3$valid.data$revenues,
          delta = delta,
          conditional = conditional,
          increment = increment,
          plot = FALSE
        )$targetsize.maxprofit.trimmed / length(mywsgbproc3$valid.data$y)
      
        # 3. evaluate campaign profit on the holdout test sample
        myeval.mywsgb3model <- campaignevaluation(
          y = mywsgbproc3$test.data$y,
          treated = mywsgbproc3$test.data$w,
          scores = mywsgbproc3$scores.test,
          m = mywsgbproc3$test.data$revenues,
          delta = delta,
          conditional = conditional,
          opttargetperc = targetsize.wsgb3.valid,
          increment = increment,
          plot = TRUE
        )
        # holdout campaign profit at optimized target size # TABLE 1
        holdoutprofit[6] <- myeval.mywsgb3model$campaign.evaluation
      
        # campaign profit curve on the test sample # FIGURE 2
        holdoutcampaign.profit.curve.mywsgb3model <-
          myeval.mywsgb3model$campaign.profit.curve
        
        list(
          fixedsize.budget = fixedsize.budget,
          fixedsize.churn = fixedsize.churn,
          holdoutcampaign.profit.curve.myliftmodel = holdoutcampaign.profit.curve.myliftmodel,
          holdoutcampaign.profit.curve.myrsgbmodel = holdoutcampaign.profit.curve.myrsgbmodel,
          holdoutcampaign.profit.curve.mysgbmodel = holdoutcampaign.profit.curve.mysgbmodel,
          holdoutcampaign.profit.curve.mywsgb2model = holdoutcampaign.profit.curve.mywsgb2model,
          holdoutcampaign.profit.curve.mywsgb3model = holdoutcampaign.profit.curve.mywsgb3model,
          holdoutcampaign.profit.curve.mywsgbmodel = holdoutcampaign.profit.curve.mywsgbmodel,
          holdoutgini = holdoutgini,
          holdoutprofit = holdoutprofit,
          holdouttdl = holdouttdl,
          optsize.buffer = optsize.buffer,
          optsize.verbeke = optsize.verbeke,
          overlap.LIFTrm.WSGB = overlap.LIFTrm.WSGB,
          overlap.RSGB.WSGB = overlap.RSGB.WSGB,
          overlap.SGB.WSGB = overlap.SGB.WSGB
        )
        
      } # end of the bootstrapping
      
      # Extracting the items from the foreach list
      
      for (i in 1:length(foreach_results_buffer)) {
        fixedsize.budget[i] <- foreach_results_buffer[[i]]$fixedsize.budget
        fixedsize.churn[i] <- foreach_results_buffer[[i]]$fixedsize.churn
        holdoutcampaign.profit.curve.myliftmodel[i, ] <- foreach_results_buffer[[i]]$holdoutcampaign.profit.curve.myliftmodel
        holdoutcampaign.profit.curve.myrsgbmodel[i, ] <- foreach_results_buffer[[i]]$holdoutcampaign.profit.curve.myrsgbmodel
        holdoutcampaign.profit.curve.mysgbmodel[i, ] <- foreach_results_buffer[[i]]$holdoutcampaign.profit.curve.mysgbmodel
        holdoutcampaign.profit.curve.mywsgb2model[i, ] <- foreach_results_buffer[[i]]$holdoutcampaign.profit.curve.mywsgb2model
        holdoutcampaign.profit.curve.mywsgb3model[i, ] <- foreach_results_buffer[[i]]$holdoutcampaign.profit.curve.mywsgb3model
        holdoutcampaign.profit.curve.mywsgbmodel[i, ] <- foreach_results_buffer[[i]]$holdoutcampaign.profit.curve.mywsgbmodel
        holdoutgini[i, ] <- foreach_results_buffer[[i]]$holdoutgini
        holdoutprofit[i, ] <- foreach_results_buffer[[i]]$holdoutprofit
        holdouttdl[i, ] <- foreach_results_buffer[[i]]$holdouttdl
        optsize.buffer[i] <- foreach_results_buffer[[i]]$optsize.buffer
        optsize.verbeke[i] <- foreach_results_buffer[[i]]$optsize.verbeke
        overlap.LIFTrm.WSGB[i, ] <- foreach_results_buffer[[i]]$overlap.LIFTrm.WSGB
        overlap.RSGB.WSGB[i, ] <- foreach_results_buffer[[i]]$overlap.RSGB.WSGB
        overlap.SGB.WSGB[i, ] <- foreach_results_buffer[[i]]$overlap.SGB.WSGB
      }
      
      # storing results
      
      result_buffer_list = list(
        fixedsize.budget = fixedsize.budget,
        fixedsize.churn = fixedsize.churn,
        holdoutcampaign.profit.curve.myliftmodel = holdoutcampaign.profit.curve.myliftmodel,
        holdoutcampaign.profit.curve.myrsgbmodel = holdoutcampaign.profit.curve.myrsgbmodel,
        holdoutcampaign.profit.curve.mysgbmodel = holdoutcampaign.profit.curve.mysgbmodel,
        holdoutcampaign.profit.curve.mywsgb2model = holdoutcampaign.profit.curve.mywsgb2model,
        holdoutcampaign.profit.curve.mywsgb3model = holdoutcampaign.profit.curve.mywsgb3model,
        holdoutcampaign.profit.curve.mywsgbmodel = holdoutcampaign.profit.curve.mywsgbmodel,
        holdoutgini = holdoutgini,
        holdoutprofit = holdoutprofit,
        holdouttdl = holdouttdl,
        optsize.buffer = optsize.buffer,
        optsize.verbeke = optsize.verbeke,
        overlap.LIFTrm.WSGB = overlap.LIFTrm.WSGB,
        overlap.RSGB.WSGB = overlap.RSGB.WSGB,
        overlap.SGB.WSGB = overlap.SGB.WSGB
      )
      
      assign(
        paste0(
          "results_with_error_fractions_r_",r_error_fraction,"_m_",m_error_fraction,'_tau_',tau_error_fraction),
        result_buffer_list)
      save.image("results/cv30_fractions_5x2x3_nobs_2000.RData", compress = TRUE)
      
      end_time <- Sys.time()
      print(paste("Single fraction examination time elapsed:", round(end_time - start_time_fraction,2),"min"))
      end_time - start_time_fraction
    }

  }
}
end_time <- Sys.time()
print('Total Time Elapsed:')
end_time - start_time


save.image("results/cv30_fractions_5x2x3_nobs_2000.RData", compress = TRUE)





stopImplicitCluster()




