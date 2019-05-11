# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download packages that H2O depends on.
pkgs <- c("RCurl","jsonlite")
for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}

# Now we download, install and initialize the H2O package for R.
install.packages("h2o", type="source", repos="http://h2o-release.s3.amazonaws.com/h2o/rel-yates/3/R")

library(h2o)
h2o.init()
setwd("D:/ALL/Study/KUL/Stats/Amalesh/3rd sem/Advanced analyics for business")
source("./assigment2_functions_macode.r")
train.dat <- read_rds('./train_ma.rds')
test.dat <- read_rds('./test_ma.rds')
churn<-read_rds('Y_train.rds')
train.dat1<- cbind(train.dat,churn)
churn<-read_rds('Y_test.rds')
test.dat1<- cbind(test.dat,churn)
training<- as.h2o(train.dat1)
testing<- as.h2o(test.dat1)
rf_h2o <- h2o.randomForest(
  y = 'churn',
  training_frame = training,
  validation_frame = testing,
  ntrees = 1000,
  stopping_metric = "AUC",    
  stopping_rounds = 10,         
  stopping_tolerance = 0.005,
  seed = 123
)
#modelperformance
h2o.auc(rf_h2o, valid = TRUE)
#automl
h2o_ai<- h2o.automl(y = 'churn',
                training_frame = training,
                validation_frame = testing,
                max_models = 50,stopping_metric = "AUC",
                max_runtime_secs = 1200,seed = 120)
#print all models
lb <- h2o_ai@leaderboard
print(lb, n = nrow(lb))
model_ids <- as.data.frame(h2o_ai@leaderboard$model_id)[,1]
#selecting best ensamble model from leadderboard
se <- h2o.getModel(grep("StackedEnsemble_AllModels", model_ids, value = TRUE)[1])
#summary of best ensamble
se
metalearner <- h2o.getModel(se@model$metalearner$name)
h2o.varimp(metalearner)
h2o.varimp_plot(metalearner)
#gbm
gbm <- h2o.getModel(grep("GBM", model_ids, value = TRUE)[1])
#summary of best ensamble
gbm
#knowing parameters used
gbm@allparameters
h2o.varimp(gbm)
h2o.varimp_plot(gbm)

# #explanation
# explainer_rf <- lime::lime(
#   as.data.frame(training[,-36]),
#   model = rf_h2o,
#   bin_continuous = F
# )
# explanation_rf <- lime::explain(
#   as.data.frame(testing[, -36]), 
#   explainer = explainer_rf, 
#   n_labels     = 1, 
#   n_features   = 4)
# klm<-plot_features(explanation_rf,ncol = 5,cases = 63)
