#### Source code ####
library(sandwich)
library(lmtest)
library(xtable)
library(ggplot2)
library(dplyr)
library(shiny)
library(shinythemes)
library(BH)
library(blockTools)
library(RItools)
library(sjPlot)
library(reshape2)
library(pander)
library(scales)
library(effects)
library(lmtest)
library(rgeos) 
# library(UScensus2000tract)
# library(UScensus2010)
library(RColorBrewer)
library(choroplethr)


#function that returns the span MDE
mde.span <- function(n, cr, colrate){
  export.df <- data.frame(ITT=numeric(), TOT=numeric(), per.treat=numeric())
  percent.treat <- .05
  
  for(i in 1:10){
    return.mde <- mde(.50,n,power=0,percent.treat, cr, 1)
    return.mde$per.treat2 <- percent.treat
    export.df <- rbind(export.df, return.mde)
    percent.treat <- percent.treat + .05
  }
  return(export.df)
}


#--------------function that finds the statistical power
#pie is the percent expectation of the control group
#n- size of experimental universe
#power=keep at 0
#treatment = %in treatment
#contactRate
#colrate = collection rate (between 0 and 1)

mde <- function(pie,n,power,treatment,cr,colrate){
  #MDE:
  n <- n*colrate
  y <- ((pie*(1-pie))*(1 - power^2)) / 
    ((treatment *(1-treatment)) * n)
  mu <- 2.49 * sqrt(y)
  se <- sqrt(y) #standard error of the impact estimator
  
  ITT <- mu
  TOT <- mu/cr
  export.mde <- as.data.frame(cbind(ITT,TOT))
  export.mde <- melt(export.mde)
  return(export.mde)
}


#function that creates the random unit assignment: SRA
ra <- function(N,m,seed){
  set.seed(seed)
  assign <- cbind(1:N, 1:N %in% sample(1:N,m))
  return(assign)
}

# Robust Standard Errors
commarobust <- function(fit){
  coeftest(fit,vcovHC(fit, type="HC2"))
}

# Summary
table_maker_2 <- function(Y,D,Z){
  tab <- rbind(c(mean(Y[Z==1]), mean(Y[Z==0])),
               c(mean(D[Z==1]), mean(D[Z==0])))
  colnames(tab) <- c("Treatment", "Control")
  rownames(tab) <- c("Percent Voted", "Percent Contacted")
  return(round(tab,5))
  
}

# Stat Results for Analysis
statistical_results_2 <-  function(Y,D,Z){
  
  itt_fit_r <- commarobust(lm(Y~Z))
  itt_d_fit <- lm(D~Z)
  
  #Sums
  total.in.treatment <- nrow(cbind(Y[Z==1]))
  total.in.control <- nrow(cbind(Y[Z==0]))
  total.convos.treatment <- sum(D[Z==1])
  total.convos.control <- sum(D[Z==0])
  
  contact_rate <-  coef(itt_d_fit)[2]
  itt_hat <- itt_fit_r[2,1]
  se_itt <- itt_fit_r[2,2]
  cace <- itt_hat/contact_rate
  se_cace <- se_itt/contact_rate
  ui <- cace + 1.96*se_cace
  li <- cace - 1.96*se_cace
  
  p <- 2*pnorm(abs(itt_hat), sd = se_itt, lower.tail = FALSE)
  
  mu_t <- mean(Y[Z==1]) ; mu_c <- mean(Y[Z==0])
  power <- power_calculator(mu_t = mu_t, 
                            mu_c = mu_c, 
                            sigma = sd(Y),
                            N =length(Y))
  
  pr_at <- mean(D[Z==0])
  E_Y_1_at <- mean(Y[D==1 & Z ==0])
  if(is.nan(E_Y_1_at)){E_Y_1_at <- 0}
  
  pr_nt <- mean(D[Z==1]==0)
  E_Y_0_nt <- mean(Y[D==0 & Z ==1])
  if(is.nan(E_Y_0_nt)){E_Y_0_nt <- 0}
  
  
  results <- matrix(c(sprintf("%.4f", contact_rate), 
                      sprintf("%.4f", itt_hat), 
                      sprintf("%.4f", se_itt), 
                      sprintf("%.4f", cace), 
                      sprintf("%.4f", se_cace), 
                      paste0(sprintf("%.3f", li), ", ",sprintf("%.3f", ui)), 
                      sprintf("%.4f", p), 
                      sprintf("%.4f", power),
                      sprintf("%.1f", total.in.treatment),
                      sprintf("%.1f", total.in.control),
                      sprintf("%.1f", total.convos.treatment),
                      sprintf("%.1f", total.convos.control)))
  
  rownames(results) <-  c("Contact Rate:", 
                          "Estimated Intent-to-treat Effect (ITT):",
                          "Standard Error of ITT:",
                          "Average Treatment Effect on the Treated (TOT)",
                          "Standard Error of  TOT",
                          "95% Confidence Interval for TOT:",
                          "Two-tailed p-value:",
                          "Power of the Experiment:",
                          "Total Cases in Treatment:",
                          "Total Cases in Control:",
                          "Total Treated in Treatment:",
                          "Total Accidentally Treated in Control:")
  return(xtable(results))
}



# Post Experimental Analysis: Power
power_calculator <- function(mu_t, mu_c, sigma, alpha=0.05, N){
  lowertail <- (abs(mu_t - mu_c)*sqrt(N))/(2*sigma)
  uppertail <- -1*lowertail
  beta <- pnorm(lowertail- qnorm(1-alpha/2), lower.tail=TRUE) + 
    1-  pnorm(uppertail- qnorm(1-alpha/2), lower.tail=FALSE)
  return(beta)
}

# Helper for checking the datatype
is_binary <- function(x){
  return(all(x %in% c(0,1)))
}



#this is the section to add modeling section