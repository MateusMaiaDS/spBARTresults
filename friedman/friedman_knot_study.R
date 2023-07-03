# Generating list of simulated data;
library(spBART5)
library(mpsBART4)
library(mlbench)
library(dbarts)
library(purrr)
# Setting a seed
set.seed(42)
# rm(list=ls())
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
grid_nIknots <- c(3,5,10,15,20)
#Storing values
all_metrics <- data.frame(n_rep = numeric(),
           model = character(),
           metric_type = character(),
           nInkots = character(),
           value = numeric())

# Generating the models
for(i in 1:10){


        for(j in 1:length(grid_nIknots)){

                # Generating the models,
                spBART <- spBART5::rbart(x_train = all_sim[[i]]$x_train,
                                      y = all_sim[[i]]$y_train,
                                      x_test = all_sim[[i]]$x_test,
                                      n_tree = 100,dif_order = 0,
                                      nIknots = grid_nIknots[j])


                # Getting the metrics and comparing;
                rmse <- function(x,y){sqrt(mean((x-y)^2))}
                spBART_test <- rmse(x = spBART$y_hat_test %>% rowMeans(),y = all_sim[[i]]$y_test)
                spBART_train <- rmse(x = spBART$y_hat %>% rowMeans(),
                                  y = all_sim[[i]]$y_train)


                # Covering all values
                all_metrics <- rbind(all_metrics,
                                  data.frame(n_rep = i,model = "spBART",
                                             metric_type = "rmse_train",
                                             nIknots = grid_nIknots[j],
                                             value = spBART_train))
                all_metrics <- rbind(all_metrics,
                                  data.frame(n_rep = i,model = "spBART",
                                             metric_type = "rmse_test",
                                             nIknots = grid_nIknots[j],
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
                                        nIknots = grid_nIknots[j],
                                        value = bart_train))
        all_metrics <- rbind(all_metrics,
                             data.frame(n_rep = i,model = "BART",
                                        metric_type = "rmse_test",
                                        nIknots = grid_nIknots[j],
                                        value = bart_test))

        cat("Running repetition number: ", i , "\n")

}


# Importing tidyverse to plots
library(tidyverse)
all_metrics <- all_metrics %>% mutate(nIknots = as.factor(nIknots))
ggplot()+
        geom_boxplot(all_metrics ,
                     mapping = aes(x = nIknots, y = value, col = metric_type))

# saveRDS(object = all_metrics,file = paste0("friedman/nIknot_eval_friedman_nonoise_n_",n_,"_sd_",sd_,".Rds"))

