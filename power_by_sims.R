rm(list=ls())

# Load useful packages
  require(pbapply) # Adds progress bars to simulation runs
  require(ggplot2) # Helps generate plots

# Function running a single simulated sample, and testing to see if it "succeeded"
single_test <- function(n,beta_treat,sd,CI_width){
  # Generate sample from our "universe" set by formulas
    treat <- sample(0:1,n,replace=TRUE)
    y <- treat*beta_treat + rnorm(n,0,sd)
    df <- data.frame(treat,y)
  # Runs our statistical test of choice. In this case it's OLS...
    test <- lm(df$y~df$treat) # ... where we run our analyis test of choice ...
    CI <- confint(test)[2,] # ... where we pull the 95% CI of the variable of interest ...
    CI_width_test <- CI[2]-CI[1] # ... and (in the example case) checks how wide that CI is
  # Return whether the test "succeeded" as a 1 if yes, 0 if no
    return(as.integer(CI_width_test<=CI_width))
    
  # Note: Slightly more common case where your "success" is p<=0.05
  # In that case you might have the following:
    # test <- summary(lm(df$y~df$treat))
    # p_value <- test$coefficients[2,4]
    # p_value_test <- p_value<=0.05
    # return(as.integer(p_value_test))
}

# Obtain power if sample size is 150, true beta = 2, sd = 3, and CI width threshold of interest = 2
  iters <- 10000
  mean(pbreplicate(iters,single_test(n=150,beta_treat=2,sd=3,CI_width=2)))

# Same as above, but wrapped in a function
power_test <- function(iters,n,beta_treat,sd,CI_width){
  # Repeat/replicate single_test function (consider using pbreplicate)
    simulated_tests <- replicate(iters,single_test(n,beta_treat,sd,CI_width))
  # The power is the proportion of "successes" (i.e. the mean of 1's and 0's)
    power <- mean(simulated_tests)
  # Output a one-line data frame with the parameters and results of this test
    data.frame(n,beta_treat,sd,CI_width,iters,power)
}

# Check power at different sample sizes
  # Generates a vector of every 10th n from 100:200
    ns <- seq(100,200,10) 
  # Run our power test at each of those ns
    tests <- pblapply(ns,function(x) power_test(iters=1000,n=x,beta_treat=2,sd=3,CI_width=2))
  # Puts them all together int a dataset
    df <- do.call("rbind",tests)
  # Chart it!
    ggplot(data=df,aes(x=n,y=power)) +
      geom_point() +
      geom_line(color="blue") +
      theme_light()

# Check power for many combinations of parameters!
  # Generate a data frame with every combination of parameters of interest
    params <- expand.grid(iters=100,
                          n=seq(100,200,5),
                          beta_treat=2,
                          sd=seq(1,5,.1),
                          CI_width=2)
  # Run them all using mapply (pbmapply to get progress bar and time estimates)
    tests <- pbmapply(power_test,
                      params$iters,
                      n=params$n,
                      beta_treat=params$beta_treat,
                      sd=params$sd,
                      CI_width=params$CI_width,
                      SIMPLIFY = FALSE) # Ensures that we get a list output, just like lapply
  # Combine them
    df <- do.call("rbind",tests)
  # Plot them
    ggplot(data=df,aes(x=n,y=sd,z=power)) +
      geom_raster(aes(fill=power),interpolate = TRUE) +
      scale_fill_gradientn(colours=c("#FF0000FF","#FFFFFFFF","#0000FFFF"))+
      theme_light() +
      scale_x_continuous(expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0))
