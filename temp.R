
data <- data.frame(gen_data(0,0,0,2000))

delta <- 1 # cost of conditional intervention
seed <- 1
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



# uutta


# 1. ifelse: convinceables 2. sleeping doges
convinceables_classified <- ifelse( test$r + test$tau_prob > 0.5 & test$r < 0.5,1,0)
sleeping_dogs_classified <- ifelse(test$r + test$tau_prob < 0.5 & test$r >  0.5,1,0)

# tee luokittelut paperilla olevan tekstin mukaan
convinceables_test <- ifelse((1-r1_test) > 0.5 & (1-r0_test) < 0.5,1,0)
sleeping_dogs_test <- ifelse((1-r1_test) < 0.5 & (1-r0_test) > 0.5,1,0)

# convinceables
convinceables_factor <- factor(convinceables_test)
levels(convinceables_factor) <- c(0,1) # Coercing predictions to have 2 levels
cm = confusionMatrix(data = factor(convinceables_classified),reference =convinceables_factor) 


auc_convinceables <- gbm.roc.area(convinceables_classified,r0_test-r1_test)
accuracy_convinceables <- cm$overall[[1]]
sensitivity_convinceables <- cm$byClass[[1]]
specificity_convinceables <- cm$byClass[[2]]
precision_convinceables <- cm$byClass[[5]]

# count of how many sleeping dogs identified correctly
sleeping_dogs_correctly_identified <- sum(sleeping_dogs_classified[which(sleeping_dogs_test==1)])/length(sleeping_dogs_classified)


