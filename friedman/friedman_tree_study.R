# Generating list of simulated data;
# rm(list=ls())
library(spBART5)
library(mpsBART4)
library(mlbench)
library(dbarts)
library(purrr)
# Setting a seed
set.seed(42)
rm(list=ls())
# Generating the list of simulated data.
n_rep_ <- 10
n_ <- 100
sd_ <- 1
all_sim <- vector("list",length = n_rep_)
all_mod <- vector("list", length = n_rep_)

# Creating the list of data elements
for(i in 1:10){

     # Generating the training and test sample
     train_sim <- mlbench.friedman1(n = n_,sd = sd_) %>% as.data.frame()
     test_sim <- mlbench.friedman1(n = n_,sd = sd_) %>% as.data.frame()

     all_sim[[i]]$x_train <- train_sim[,1:5,drop = FALSE]
     all_sim[[i]]$y_train <- train_sim$y
     all_sim[[i]]$x_test <- test_sim[,1:5,drop = FALSE]
     all_sim[[i]]$y_test <- test_sim$y

}

# Changing the number of nodes;
# grid_ntree <- c(10,20,50,100,200)
grid_ntree <- c(10,20,50,100,200)

#Storing values
all_metrics <- data.frame(n_rep = numeric(),
                          model = character(),
                          metric_type = character(),
                          ntree = character(),
                          value = numeric())

# Generating the models
for(i in 1:10){


     for(j in 1:length(grid_ntree)){

          # Generating the models,
          spBART <- spBART5::rbart(x_train = all_sim[[i]]$x_train,
                                   y = all_sim[[i]]$y_train,
                                   x_test = all_sim[[i]]$x_test,
                                   n_tree = grid_ntree[j],dif_order = 0,
                                   nIknots = 5)


          # Getting the metrics and comparing;
          rmse <- function(x,y){sqrt(mean((x-y)^2))}
          spBART_test <- rmse(x = spBART$y_hat_test %>% rowMeans(),y = all_sim[[i]]$y_test)
          spBART_train <- rmse(x = spBART$y_hat %>% rowMeans(),
                               y = all_sim[[i]]$y_train)


          # Covering all values
          all_metrics <- rbind(all_metrics,
                               data.frame(n_rep = i,model = "spBART",
                                          metric_type = "rmse_train",
                                          ntree = grid_ntree[j],
                                          value = spBART_train))
          all_metrics <- rbind(all_metrics,
                               data.frame(n_rep = i,model = "spBART",
                                          metric_type = "rmse_test",
                                          ntree = grid_ntree[j],
                                          value = spBART_test))


     }


     # Running the BART baseline
     dbarts <- dbarts::bart(x.train = all_sim[[i]]$x_train,
                            y.train = all_sim[[i]]$y_train,
                            x.test = all_sim[[i]]$x_test)

     bart_test <- rmse(x = dbarts$yhat.test.mean,y = all_sim[[i]]$y_test)
     bart_train <- rmse(x = dbarts$yhat.train.mean,y = all_sim[[i]]$y_train)


     all_metrics <- rbind(all_metrics,
                          data.frame(n_rep = i,model = "BART",
                                     metric_type = "rmse_train",
                                     ntree = grid_ntree[j],
                                     value = bart_train))
     all_metrics <- rbind(all_metrics,
                          data.frame(n_rep = i,model = "BART",
                                     metric_type = "rmse_test",
                                     ntree = grid_ntree[j],
                                     value = bart_test))

     cat("Running repetition number: ", i , "\n")

}


# Importing tidyverse to plots
library(tidyverse)
all_metrics <- all_metrics %>% mutate(ntree = as.factor(ntree))
ggplot()+
geom_boxplot(all_metrics %>% filter(metric_type == "rmse_test"),
             mapping = aes(x = ntree, y = value, col = model))


# Storing the results;
saveRDS(object = all_metrics,file = paste0("friedman_nonoise_n_",n_,"_sd_",sd_,".Rds"))
