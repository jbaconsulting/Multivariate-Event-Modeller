# This code was written by Jeremy Benn Associates Limited as part of the Multivariate Event Modeller tool.
# Copyright (C) 2023 Jeremy Benn Associates Limited

# This program is free software; you can redistribute it and/or modify it under the 
# terms of the GNU General Public License (version 2) as published by the Free Software 
# Foundation.

# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
# without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
# See the GNU General Public License for more details.
 
# You should have received a copy of the GNU General Public License along with this program; 
# if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
# Boston, MA 02110-1301 USA.


# Set up functions that are used in the app -------------------------------

# Set function for fitting gpd to catch warnings and errors
gpdWarning <- function (data, thr, cmax, r) {return(tryCatch(fpot(data, thr, cmax=cmax, npp=365.25, r=r), warning=function(w) w))}

warningMessage <- function(data, threshold, declustered){
  warnMess <- rep(NA, ncol(data))
  if(declustered == FALSE){
    cmaxVal = TRUE; rVal = 7;
    for(d in 1:ncol(data)){
      data.timeseries <- data[, d]
      thresh.data <- as.numeric(quantile(data.timeseries, threshold))
      fpot.mod <- gpdWarning(data.timeseries, thresh.data, cmax=cmaxVal, r=rVal)
      if (!is.null(fpot.mod$message)){   # if no error but a warning message when using std.err=T
        warnMess[d] <- fpot.mod$message
      }
    }
    if(sum(is.na(warnMess)) < ncol(data)){
      return("Warning: The input data is not a sufficiently long record to permit robust analysis of the extremes, or might be lacking
             sufficient extreme events (e.g. this is possible with a groundwater flow series).")
    } else{
      return(NULL)
    }
  }
  if(declustered == TRUE){
    cmaxVal = FALSE; rVal = 1;
    for(d in 1:ncol(data)){
      data.timeseries <- data[, d]
      thresh.data <- as.numeric(quantile(data.timeseries, threshold))
      fpot.mod <- gpdWarning(data.timeseries, thresh.data, cmax=cmaxVal, r=rVal)
      if (!is.null(fpot.mod$message)){   # if no error but a warning message when using std.err=T
        print(paste("Warning in fpot:", fpot.mod$message)) 
      } else{
        print("")
      }
    }
  }
}

# Number of independent events after declustering
No.Ind.Events <- function(data, threshold){
  No.Events <- rep(NA, ncol(data)) 
  for(d in 1:ncol(data)){
    data.timeseries <- data[, d]
    thresh.data <- as.numeric(quantile(data.timeseries, threshold))
    fpot.mod <- fpot(data.timeseries, thresh.data, cmax=TRUE, npp=365.25, r=7)  
    No.Events[d] <- length(fpot.mod$exceedances)
  }
  return(No.Events)
}

# Empirical CDF
emp.cdf <- function(data){
  ans <- rank(data)/(length(data)+1)
  return(ans)
} 

# Laplace quantile function
lap.quant.func <- function(p){
  ans <- rep(NA, length(p))
  for(i in 1:length(ans)){
    if(p[i] >= 0.5){
      ans[i] <- -log(2*(1-p[i]))
    }
    else{
      ans[i] <- log(2*p[i])
    }
  }
  return(ans)
} 

# Convert probabilities from the event sets
fromUnifToEmp <- function(unif, gpd.par, x){
  y = quantile(x, unif, na.rm = T)
  u = gpd.par[1]
  sig.u = gpd.par[2]
  xi = gpd.par[3]
  # determine the probability of being above the threshold
  p.thresh = mean(x < u, na.rm = T)
  p.gpd = 1 - (1 - unif[unif >= p.thresh])/(1 - p.thresh)
  # convert those above the threshold to their original scale
  y[unif > p.thresh] = texmex::qgpd(p.gpd, sigma = sig.u, xi = xi, u = u)
  return(y)
}

# Convert GPD parameters to GEV parameters
GPDtoGEV <- function(gpd.par, n, n.exc){
  # GPD parameters
  u <- gpd.par[1]
  sigma.u <- gpd.par[2]
  xi <- gpd.par[3]
  # number of observations
  n.year <- n/365.25
  lambda <- n.exc/n.year
  # determine GEV parameters
  mu <- u+sigma.u*(lambda^xi-1)/xi
  sigma <- sigma.u*lambda^xi
  par.gev <- c(mu, sigma, xi)
  return(par.gev)
}

# Fit the Heffernan-Tawn multivariate extreme value model (including declustered data)
HT.model.fit <- function(data, threshold, declustered){
  # Inputs
  ## data set of observations with the time column removed
  ## threshold used for the marginal and dependence threshold 
  ## declustered argument to specify whether the data have already been declustered 
  # count number of missing values
  na.count <- sum(is.na(data))
  if(na.count > 0){
    stop("Missing values exist: analysis cannot be completed.", call. = FALSE)
  }
  else{   
    # limit the size of the problem 
    ncol.data <- ncol(data)
    if(ncol.data > 10){
      stop("Too many variables are being considered.", call. = FALSE)
    }
    else{
      # lists storing the output of the model 
      residual.list <- list()
      dep.par.list <- list()
      dependence.threshold <- list()
      name.data <- names(data)
      marg.par.list <- list()
      # marginal modelling
      if(declustered == FALSE){
        for(d in 1:ncol(data)){
          data.timeseries <- data[, d]
          thresh.data <- as.numeric(quantile(data.timeseries, threshold))
          fpot.mod <- fpot(data.timeseries, thresh.data, cmax=TRUE, npp=365.25, r=7)              
          marg.par.list[[d]] <- fpot.mod
        }
      }
      if(declustered == TRUE){
        for(d in 1:ncol(data)){
          data.timeseries <- data[, d]
          thresh.data <- as.numeric(quantile(data.timeseries, threshold))
          fpot.mod <- fpot(data.timeseries, thresh.data, cmax=FALSE, npp=365.25, r=1)            
          marg.par.list[[d]] <- fpot.mod
        }
      }
      # transformed data on the same scale
      migpd.object <- migpd(data, mqu=threshold)
      transformed.model <- mexDependence(migpd.object, dqu=threshold, which=name.data[1], marTransform="empirical")
      transformed.data <- transformed.model$margins$transformed
      # Heffernan-Tawn modelling conditional on each variable 
      withProgress(message = 'Modelling data', value = 0, detail = "0% complete", {
        for(d in 1:ncol(data)){
          data.model.ht <- cbind(data)
          # Fit the marginal and dependence model concurrently 
          dep.model <- try(mex(data.model.ht, mqu=threshold, dqu=threshold, which=name.data[d]))
          if(class(dep.model) == "try-error"){
            stop("Model not fitting", call. = FALSE)
          }
          else{
            # store the dependence threshold used for the model fitting 
            dependence.threshold[d] <- dep.model$dependence$dth
            # store the parameter estimates of the Heffernan-Tawn model      
            # dependence parameters
            dep.par.list[[d]] <- dep.model$dependence$coeff[c(1:2,5:6),]
            # store the residuals of the Heffernan-Tawn model
            residual.list[[d]] <- dep.model$dependence$Z
          }
          # Increment the progress bar and update the detail text
          incProgress(1/ncol(data), detail = paste(paste0(round(100*(d/ncol(data)),0), "%"), "complete"))
          
          # Pause for 0.1 seconds to simulate a long computation.
          Sys.sleep(0.1)
        }
      }
      )
  }
      # Return the relevant output
      ## Transformed - the observed data are on a 'standardised' scale - in this case they have Laplace margins
      ## Dependence threshold - the threshold used to fit Heffernan and Tawn on Laplace margins 
      ## MarginalParameter - the marginal GP parameters for each of the gauges 
      ## DependenceParameter - the a and b parameters of the Heffernan and Tawn model
      ## Residuals - the Zs (residuals) from fitting Heffernan and Tawn, which are used to simulate events 
      list.results <- list(Transformed=transformed.data, DepThreshold=dependence.threshold,
                           MarginalParameter=marg.par.list, DependenceParameter=dep.par.list,
                           Residuals=residual.list)
      return(list.results)
  }
}
 
# Simulate from the Heffernan-Tawn multivariate extreme value model
HT.simulation <- function(results, no.years.sim, name.variables, observed.data, threshold,
                          declustered, no.years.data){
  # Inputs
  ## results - output from the HT.model.fit function - must be a list
  ## no.years.sim - number of years of events to simulate
  ## name.variables - name of the variables fitted in the Heffernan and Tawn analysis 
  ## observed data - complete set of observations which the user has inputted into the app 
  ## threshold - percentile used for the dependence modelling 
  ## declustered - determining whether the user has already declustered their data 
  ## no.years.data - a value of this is only specified if 'declustered data' has been selected (the user provides this as an input)
  if(class(results) != "list"){
    stop("The dependence structure must be in the form of a list.", call. = FALSE)
  }
  else{
    # rename output from fitting the Heffernan-Tawn model 
    dependence.threshold <- results$DepThreshold
    par.list <- results$DependenceParameter
    residual.list <- results$Residuals
    # simulate the total number of events to be generated
    # no.events.per.year is the number of extreme events divided by the number of years of data
    # if declustered then number of years is from user input else it's calculated from the data
    if(declustered == TRUE){
      no.events.per.year <- nrow(residual.list[[1]])/no.years.data 
    } else if(declustered == FALSE){
      no.events.per.year <- nrow(residual.list[[1]])/(dim(observed.data)[1]/365.25)
    }
    # set seed to be random so that simulation is random rather than
    # picking up any seed saved in the global environment
    set.seed(NULL)
    no.events.sim <- rpois(no.years.sim, no.events.per.year)  # counts per year
    n.total.event.sim <- sum(no.events.sim)  # total number of simulated events needed
    # index the events year and event number
    year.setup <- formatC(c(1:no.years.sim), width = nchar(no.years.sim), flag = '0')
    no.year.setup <- rep(year.setup, no.events.sim)
    year.id.sim <- paste(no.year.setup, "-01-01", sep = "")
    # determine the probability of a component being the largest
    data.common.margin <- results$Transformed
    data.extreme.mat.test <- matrix(0, nrow(data.common.margin), ncol(data.common.margin))
    for(c in 1:ncol(data.common.margin)){
      data.extreme.mat.test[, c] <- as.numeric(data.common.margin[, c] >= dependence.threshold[c])
    }
    prob.extreme <- length(data.extreme.mat.test[, 1])/length(data.common.margin[, 1])
    prob.each.vec <- rep(NA, ncol(data.common.margin))
    if(declustered == TRUE){
      # select which columns are skew surge
      skew.surge.cols <- grep("SkewSurge", names(as.data.frame(data.common.margin)))
      if(ncol(data.common.margin) > 2){
        # select how many rows/events have the selected column/variable greater than all the other variables and the proportion of these out of all events
        for(r in 1:ncol(data.common.margin)){   
          prob.each.vec[r] <- 
            length(data.common.margin[data.common.margin[, r] > apply(data.common.margin[, -r], 1, max), 1])/length(data.common.margin[, 1])
        }
      } else{
        prob.each.vec[1] <- 
          length(data.common.margin[data.common.margin[, 1] >= data.common.margin[, 2], 1])/length(data.common.margin[, 1])
        prob.each.vec[2] <- 
          length(data.common.margin[data.common.margin[, 2] >= data.common.margin[, 1], 1])/length(data.common.margin[, 1])
      }
      # probability of each component being most extreme
      prob.each <- c(prob.each.vec)/sum(prob.each.vec)
      prob.each[-skew.surge.cols] <- 0
    }
    if(declustered == FALSE){
      if(ncol(data.common.margin) > 2){
        for(r in 1:ncol(data.common.margin)){
          prob.each.vec[r] <- 
            length(data.common.margin[data.common.margin[, r] > apply(data.common.margin[, -r], 1, max), 1])/length(data.common.margin[, 1])
        }
      } else{
        prob.each.vec[1] <- 
          length(data.common.margin[data.common.margin[, 1] >= data.common.margin[, 2], 1])/length(data.common.margin[, 1])
        prob.each.vec[2] <- 
          length(data.common.margin[data.common.margin[, 2] >= data.common.margin[, 1], 1])/length(data.common.margin[, 1])
      }
      # probability of each component
      prob.each <- c(prob.each.vec)/sum(prob.each.vec)
    }
    # set seed to be random so that simulation is random rather than
    # picking up any seed saved in the global environment
    set.seed(NULL)
    # conditional simulation to work out which component is the largest 
    cond.trigger <- rmultinom(n.total.event.sim, 1, prob.each)
    # store the timing of events
    cond.trigger.loc <- list()
    for(j in 1:nrow(cond.trigger)){
      cond.trigger.loc[[j]] <- which(cond.trigger[j,] == 1)
    }
    id.condtrigger <- rep(NA, n.total.event.sim)
    for(j in 1:length(cond.trigger.loc)){
      id.condtrigger[cond.trigger.loc[[j]]] <- j
    }
    # number of times each component is the largest in the simulation
    n.exc.each <- apply(cond.trigger, 1, sum)
    ncol.cond <- ncol(data.common.margin)-1
    id <- 0
    # simulate a set of events 
    data.list.sim <- list()
    if(ncol(data.common.margin) > 2){
      withProgress(message = 'Simulating events', value = 0, detail = "0% complete", {
        for(s in 1:ncol(data.common.margin)){
          if(n.exc.each[s] > 0){
            data.sim.mat.temp <- matrix(NA, nrow=n.exc.each[s], ncol(data.common.margin)-1)
            x.sim <- dependence.threshold[[s]] + rexp(n.exc.each[s], 1)
            for(j in 1:n.exc.each[s]){
              x.sim.temp <- x.sim[j]
              a <- as.numeric(matrix(par.list[[s]][1,], ncol=ncol.cond))
              b <- as.numeric(matrix(par.list[[s]][2,], ncol=ncol.cond))
              orig.resid <- residual.list[[s]]
              level.consider <- x.sim.temp*(1-a)/x.sim.temp^b
              resid.one.dummy <- orig.resid
              for(i in 1:ncol(resid.one.dummy)){
                resid.one.dummy[, i] <- orig.resid[, i] < level.consider[i]
              }
              resid.one.use <- which(apply(resid.one.dummy, 1, sum) == ncol(resid.one.dummy))
              resid.use <- matrix(matrix(orig.resid, ncol=ncol.cond)[resid.one.use, ], ncol=ncol.cond)
              resid.use <- orig.resid[resid.one.use, ]
              Z <- resid.use[sample(1:length(resid.use[, 1]), 1, replace = TRUE), ]
              data.sim.mat.temp[j, ] <- as.numeric(a)*x.sim.temp+x.sim.temp^as.numeric(b)*Z
            }
            max.y <- apply(data.sim.mat.temp, 1, max)
            x.sim.keep <- x.sim[x.sim > max.y]
            ygivenx.keep <- data.sim.mat.temp[x.sim > max.y, ]
            sim.data.ht <- cbind(x.sim.keep, ygivenx.keep)
            
            if(dim(sim.data.ht)[1] >= n.exc.each[s]){
              if(s == 1){
                data.list.sim[[s]] <- sim.data.ht[c(1:n.exc.each[s]), ]
              } else{
                id.col.selection <- 2:s
                data.list.sim[[s]] <- cbind(sim.data.ht[, c(id.col.selection)], sim.data.ht[, 1], sim.data.ht[, -c(1:s)])[c(1:n.exc.each[s]), ]
              }
            } else{
              id <- 1
            }
          } else{
            data.list.sim[[s]] <- NULL
          }
          # Increment the progress bar and update the detail text
          incProgress(1/ncol(data.common.margin), detail = paste(paste0(round(100*(s/ncol(data.common.margin)),0), "%"), "complete"))
          
          # Pause for 0.1 seconds to simulate a long computation
          Sys.sleep(0.1)
        }
      }) 
    }
    if(ncol(data.common.margin) == 2){
      withProgress(message = 'Simulating events', value = 0, detail = "0% complete", {
        for(s in 1:ncol(data.common.margin)){
          if(n.exc.each[s]>0){
            data.sim.mat.temp <- matrix(NA, nrow=n.exc.each[s], ncol(data.common.margin)-1)
            x.sim <- dependence.threshold[[s]] + rexp(n.exc.each[s], 1)
            for(j in 1:n.exc.each[s]){
              x.sim.temp <- x.sim[j]
              a <- as.numeric(par.list[[s]][1])
              b <- as.numeric(par.list[[s]][2])
              orig.resid <- residual.list[[s]]
              level.consider <- x.sim.temp*(1-a)/x.sim.temp^b
              resid.one.use <- which(orig.resid[, 1] < level.consider[1])
              resid.use <- orig.resid[resid.one.use, ]
              Z <- resid.use[sample(1:length(resid.use), 1, replace=TRUE)]
              data.sim.mat.temp[j,] <- as.numeric(a)*x.sim.temp+x.sim.temp^as.numeric(b)*Z
            }
            max.y <- apply(data.sim.mat.temp, 1, max)
            x.sim.keep <- x.sim[x.sim > max.y]
            ygivenx.keep <- data.sim.mat.temp[x.sim > max.y, ]
            sim.data.ht <- cbind(x.sim.keep, ygivenx.keep)
            if(dim(sim.data.ht)[1] >= n.exc.each[s]){
              if(s == 1){
                data.list.sim[[s]] <- sim.data.ht[c(1:n.exc.each[s]), ]
              }
              else{
                id.col.selection <- 2:s
                data.list.sim[[s]] <- cbind(sim.data.ht[, c(id.col.selection)], sim.data.ht[, 1], sim.data.ht[, -c(1:s)])[c(1:n.exc.each[s]), ]
              }
            }
            else{
              id <- 1
            }
          }
          else{
            data.list.sim[[s]] <- NULL
          }
          # Increment the progress bar and update the detail text
          incProgress(1/ncol(data.common.margin), detail = paste(paste0(round(100*(s/ncol(data.common.margin)),0), "%"), "complete"))
          
          # Pause for 0.1 seconds to simulate a long computation
          Sys.sleep(0.1)
        }
      }) 
    }
    
    withProgress(message = 'Processing events', value = 0, detail = "0% complete", {
    # convert the data from an array into a matrix
    # combine the simulations conditional on each variable (created above) - for declustered this is just the skew-surge columns
    # so they are ordered in terms of the conditioning variable, e.g. the first n are based on var 1
    data.sim <- do.call(rbind, data.list.sim)  
    # timing of events
    start.id.orig <- c(1, cumsum(n.exc.each)+1)
    start.id <- start.id.orig[-length(start.id.orig)]
    end.id <- c(cumsum(n.exc.each))
    data.sim.ordered <- data.sim
    for(j in 1:ncol(data.sim)){  # places the simulated events conditioned on different variables in the right locations
      if(length(cond.trigger.loc[[j]]) > 0){
        data.sim.ordered[cond.trigger.loc[[j]], ] <- data.sim[c(start.id[j]:end.id[j]), ]
      }
    }
    # convert the simulated data on Laplace margins to probabilities
    data.sim <- data.sim.ordered
    prob.data.sim <- data.sim
    # determine whether the predictions are below or above zero
    id.data.sim.neg <- which(data.sim<0, arr.ind=TRUE)
    id.data.sim.pos <- which(data.sim>=0, arr.ind=TRUE)
    if(length(id.data.sim.neg) > 0){
      for(i in 1:length(id.data.sim.neg[, 1])){
        prob.data.sim[id.data.sim.neg[i, 1], id.data.sim.neg[i, 2]] <- 
          1/2*exp(data.sim[id.data.sim.neg[i, 1], id.data.sim.neg[i, 2]])
      }
    }
    if(length(id.data.sim.pos) > 0){
      for(i in 1:length(id.data.sim.pos[, 1])){
        prob.data.sim[id.data.sim.pos[i, 1], id.data.sim.pos[i, 2]] <- 
          1-1/2*exp(-1*data.sim[id.data.sim.pos[i, 1], id.data.sim.pos[i, 2]])
      }
    }
    # convert to values according to the Generalised Pareto distribution
    # process GP parameters from the results to calculate actual values 
    marg.model.comp <- results$MarginalParameter
    RP.mat <- matrix(NA, nrow=nrow(data.sim), ncol=ncol(data.sim))
    value.mat <- matrix(NA, nrow=nrow(data.sim), ncol=ncol(data.sim))
    GPpar.mat <- as.data.frame(matrix(NA, nrow=ncol(data.sim), ncol=4))
    names(GPpar.mat) <- c("Threshold", "Sigma", "Xi", "NoClusterPerYear")
    
    # Increment the progress bar and update the detail text
    incProgress(1/3, detail = "33% complete")
    
    # Pause for 0.1 seconds to simulate a long computation
    Sys.sleep(0.1)
    
    if(declustered == FALSE){
      for(i in 1:ncol(data.sim)){
        # retrieve GEV parameters
        GP.par <- c(as.numeric(marg.model.comp[[i]]$threshold), marg.model.comp[[i]]$estimate[1], marg.model.comp[[i]]$estimate[2])
        # value of the simulated events
        value.mat[, i] <- fromUnifToEmp(prob.data.sim[, i], GP.par, observed.data[, i]) 
        # number of clusters per year
        no.cluster.pyear <- length(marg.model.comp[[i]]$exceedances)/(length(observed.data[, i])/365.25)
        # store GP parameters
        GPpar.mat[i, ] <- c(GP.par, no.cluster.pyear)
        # return period calculation
        RP.mat[, i] <- rt.gpd.cmax(m=no.cluster.pyear, threshold=GP.par[1], sigma_u=GP.par[2],
                                   xi=GP.par[3], level=value.mat[, i])
      }
    }
    if(declustered == TRUE){
      for(i in 1:ncol(data.sim)){
        # retrieve GEV parameters
        GP.par <- c(as.numeric(marg.model.comp[[i]]$threshold), marg.model.comp[[i]]$estimate[1], marg.model.comp[[i]]$estimate[2])
        # value of the simulated events
        value.mat[, i] <- fromUnifToEmp(prob.data.sim[, i], GP.par, observed.data[, i])
        # number of clusters per year
        no.cluster.pyear <- length(marg.model.comp[[i]]$exceedances)/(no.years.data)
        # store GP parameters
        GPpar.mat[i, ] <- c(GP.par, no.cluster.pyear)
        # return period calculation
        RP.mat[, i] <- rt.gpd.cmax(m=no.cluster.pyear, threshold=GP.par[1], sigma_u=GP.par[2],
                                   xi=GP.par[3], level=value.mat[, i])
      }
    }
    
    # Increment the progress bar and update the detail text
    incProgress(1/3, detail = "67% complete")
    
    # Pause for 0.1 seconds to simulate a long computation
    Sys.sleep(0.1)
    
    # Outputs
    ## standardised
    data.sim <- as.data.frame(data.sim)
    names(data.sim) <- c(name.variables)
    ## return period values 
    simulated.eventset.rp <- as.data.frame(cbind(year.id.sim, RP.mat))
    names(simulated.eventset.rp) <- c("Date", name.variables)
    ## original scale variables
    simulated.eventset.value <- as.data.frame(cbind(year.id.sim, value.mat))
    names(simulated.eventset.value) <- c("Date", name.variables)
    # return the different outputs from the model simulation
    ## Standardised - the simulated data on common (Laplace) margins
    ## Probability - the simulated data on a (0, 1) scale
    ## ReturnPeriod - the simulated event set data on the return period scale 
    ## Value - the simulated data on the measurement scale 
    ## IDTrigger - determining which gauge is the trigger location 
    ## GPDParameters - the GPD parameters and the number of clusters per year
    ## simYears - number of simulated years 
    ## simEventsPerYear - number of simulated events per year
    numYears <- no.years.sim
    numEventsPerYear <- no.events.per.year
    return(list(Standardised=data.sim, Probability=prob.data.sim, ReturnPeriod=simulated.eventset.rp,
                Value=simulated.eventset.value, IDTrigger=id.condtrigger, GPDParameters=GPpar.mat,
                simYears=numYears, simEventsPerYear=numEventsPerYear))
    
    # Increment the progress bar and update the detail text
    incProgress(1/3, detail = "100% complete")
    
    # Pause for 0.1 seconds to simulate a long computation
    Sys.sleep(0.1)
    })
  }
}

# Thinning function - thins the data for plotting 
thin.data.function <- function(data, thinning.percentage){
  set.seed(42)
  n.data.total <- length(data)
  if(n.data.total == 8){
    n.data <- n.data.total-3
  } else if(n.data.total == 9){
    n.data <- n.data.total-4 
  }
  n.data.id <- n.data
  n.data.id.plus1 <- n.data.id + 1
  n.data.not.alter <- n.data.id.plus1:n.data.total
  # Inputs
  ## data are the output from the HT.simulation function
  ## thinning.percentage is the percentage of the data that are visualised
  if(thinning.percentage < 0 | thinning.percentage > 100){
    stop("Please define a thinning percentage that lies between 0 and 100", call. = FALSE)
  }
  else{
    # number of simulations in the overall data
    nrow.sim <- nrow(data[[1]])
    # number of simulations in the thinned data set
    n.sample <- round(nrow.sim*(thinning.percentage/100))
    samp.location <- sort(sample(1:nrow.sim, n.sample, replace=FALSE))
    data.thin <- list()
    n.data.restrict <- n.data-1
    for(i in 1:n.data.restrict){
      data.thin[[i]] <- data[[i]][samp.location, ]
    }
    data.thin[[n.data.id]] <- data[[n.data.id]][samp.location]
    for(i in 1:length(n.data.not.alter)){
      data.thin[[n.data.not.alter[i]]] <- data[[n.data.not.alter[i]]]
    }
    names(data.thin) <- names(data)
  }
  # return the thinned data set
  return(data.thin)
} 

# Quantiles of the cluster maxima GP
quant.gpd.cmax <- function(prob, m, threshold, sigma_u, xi){
  # Inputs
  ## prob - non-exceedance probability 
  ## m - number of clusters per year 
  ## threshold - threshold of the GPD
  ## sigma_u - scale parameter of the GPD
  ## xi - shape parameter of the GPD
  level <- threshold+(sigma_u/xi)*(((-log(prob)/m)^-xi)-1)
  return(level)
} 

# Return period of the cluster maxima GP
rt.gpd.cmax <- function(m, threshold, sigma_u, xi, level){
  # Inputs
  ## m - number of clusters per year 
  ## threshold 
  ## sigma_u, xi are parameters of the GPD
  main.term <- exp(-m*pmax((1+(xi/sigma_u)*(level-threshold)),0)^(-1/xi))
  ans <- (1-main.term)^-1
  return(ans)
}

# Convert AEP to level using the GL distribution parameters 
glog.quant.ind <- function(aep, location, scale, shape){
  # Inputs
  ## aep - annual exceedance probabilities to evaluate
  ## parameters are three parameters of the generalised logistic distribution
  MargPar.glo <- vec2par(c(location, scale, shape), type="glo")
  prob <- 1-aep
  aep.level <- quaglo(prob, MargPar.glo)
  return(aep.level)
} 

# Converting AEP to Level (GPD)
AEPtoLevel.GP <- function(GPparameters, AEP, name.variables, min.data){
  # Inputs
  ## GPparameters - GPD parameters from the model:
  ## 4 rows containing:
  ### m - number of clusters per year - this needs to be specified for each of the variables
  ### threshold
  ### sigma_u - scale parameter of the GPD
  ### xi - shape parameter of the GPD
  ## AEP - vector containing the annual exceedance probabilities 
  ## name.variables - name of the variables (e.g. gauges) 
  ## min.data - minimum of the observed data 
  level.vec <- rep(NA, length(AEP))
  # convert to non-exceedance probability 
  prob <- 1-AEP
  # calculation when there are no missing values
  if(sum(is.na(AEP)) == 0){
    for(i in 1:nrow(GPparameters)){  # for each gauge 
      if(prob[i] < 0 || prob[i] > 1){
        level.vec[i] <- NaN
      }
      if(prob[i] == 0){level.vec[i] <- min.data[i]}
      if(prob[i] == 1){level.vec[i] <- Inf}
      if(prob[i] > 0 & prob[i] < 1){
        level.vec[i] <- quant.gpd.cmax(prob[i], m=GPparameters[i,4], threshold=GPparameters[i,1],
                                       sigma_u=GPparameters[i,2], xi=GPparameters[i,3])
      }
    }
  }
  # deal with missing values 
  else{
    id.NA.AEP <- is.na(AEP) == "TRUE"
    for(i in 1:nrow(GPparameters)){
      if(id.NA.AEP[i] == "TRUE"){
        level.vec[i] <- NA
      } else if(prob[i] < 0 || prob[i] > 1){
        level.vec[i] <- NaN
      } else if(prob[i] == 0){
        level.vec[i] <- min.data[i]
        } else if(prob[i] == 1){
          level.vec[i] <- Inf
        } else if(prob[i] > 0 & prob[i] < 1){
        level.vec[i] <- quant.gpd.cmax(prob[i], m=GPparameters[i,4], threshold=GPparameters[i,1], 
                                       sigma_u=GPparameters[i,2], xi=GPparameters[i,3])
      }
    }
  }
  # return the data with the names of variables
  level.mat <- as.data.frame(matrix(level.vec, ncol=length(AEP)))
  names(level.mat) <- c(name.variables)
  return(level.mat)	
}

# Convert Level to AEP (GPD) 
LeveltoAEP.GP <- function(GPparameters, Level, name.variables, min.data, declustered){
  # Inputs
  ## GPparameters - GPD parameters from the model fits in H+T function
  ## 4 rows containing:
  ### m - number of clusters per year - this needs to be specified for each of the variables
  ### threshold
  ### sigma_u - scale parameter of the GPD
  ### xi - shape parameter of the GPD
  ## Level - vector containing the levels 
  ## name.variables - name of the variables (e.g. gauges)
  ## min.data - minimum of the observed data 
  if(declustered == TRUE){
    AEP.vec <- as.numeric(rep(NA, length(Level)))
  } else{
    prob.vec <- rep(NA, length(Level))  # set up empty vector for non-exceedance probabilities
    # calculation when there are no missing values
    if(sum(is.na(Level)) == 0){
      for(i in 1:nrow(GPparameters)){
        rt.calc <- rt.gpd.cmax(level=Level[i], m=GPparameters[i,4], threshold=GPparameters[i,1],
                               sigma_u=GPparameters[i,2], xi=GPparameters[i,3])
        prob.vec[i] <- max(1-1/rt.calc, 0.00051)  # restrict the AEP value to 99.9% rather than letting it round up to 100%
        prob.vec[i] <- min(prob.vec[i], 0.99949)  # restrict the AEP value to 0.1% rather than 0%
      }
    }
    # deal with missing values 
    else{
      id.NA.Level <- is.na(Level) == "TRUE"
      for(i in 1:nrow(GPparameters)){
        if(id.NA.Level[i] == "TRUE"){
          prob.vec[i] <- NA
        }
        else{
          rt.calc <- rt.gpd.cmax(level=Level[i], m=GPparameters[i,4], threshold=GPparameters[i,1],
                                 sigma_u=GPparameters[i,2], xi=GPparameters[i,3])
          prob.vec[i] <- max(1-1/rt.calc, 0.00051)  # restrict the AEP value to 99.9% rather than letting it round up to 100%
          prob.vec[i] <- min(prob.vec[i], 0.99949)  # restrict the AEP value to 0.1% rather than 0%
        }
      }
    }
    
    # convert to AEP 
    AEP.vec <- 1-prob.vec
  }
  
  # return the data with the names of gauges
  AEP.mat <- as.data.frame(matrix(AEP.vec, ncol=length(Level)))
  names(AEP.mat) <- c(name.variables)
  return(AEP.mat)	
}

# Convert AEP to Level (GL)
AEPtoLevel.GLO <- function(GLOparameters, AEP, name.variables, min.data){
  # Inputs
  ## GLOparameters - GLO parameters from the user-supplied marginals 
  ## AEP - vector containing the annual exceedance probabilities 
  ## name.variables - name of the variables (e.g. gauges)
  ## min.data - minimum of the observed data at each variable
  level.vec <- rep(NA, length(AEP))
  # convert to non-exceedance probability 
  prob <- 1-AEP
  # calculation when there are no missing values
  if(sum(is.na(AEP)) == 0){
    for(i in 1:nrow(GLOparameters)){
      if(prob[i] < 0 || prob[i] > 1){
        level.vec[i] <- NaN
      } 
      if(prob[i] == 0){level.vec[i] <- min.data[i]}
      if(prob[i] == 1){level.vec[i] <- Inf}
      else if(prob[i] > 0 & prob[i] < 1){
        level.vec[i] <- glog.quant.ind(AEP[i], location = GLOparameters[i,1], 
                                       scale = GLOparameters[i,2], shape = GLOparameters[i,3])
      }
    }
  }
  # deal with missing values 
  else{
    id.NA.AEP <- is.na(AEP) == "TRUE"
    for(i in 1:nrow(GLOparameters)){
      if(id.NA.AEP[i] == "TRUE"){
        level.vec[i] <- NA
      } else if(prob[i] < 0 || prob[i] > 1){
        level.vec[i] <- NaN  
      } else if(prob[i] == 0){
        level.vec[i] <- min.data[i]
        } else if(prob[i] == 1){
          level.vec[i] <- Inf
        } else if(prob[i] > 0 & prob[i] < 1){
        level.vec[i] <- glog.quant.ind(AEP[i], location = GLOparameters[i,1], 
                                       scale = GLOparameters[i,2], shape = GLOparameters[i,3])
      }
    }
  }
  # return the data with the names of variables
  level.mat <- as.data.frame(matrix(level.vec, ncol=length(AEP)))
  names(level.mat) <- c(name.variables)
  return(level.mat)	
}

# Convert Level to AEP (GL) 
LeveltoAEP.GLO <- function(GLOparameters, Level, name.variables, min.data){
  # Inputs
  ## GLOparameters - GLO parameters from the user supplied marginal analysis
  ## Level - vector containing the levels 
  ## name.variables - name of the variables (e.g. gauges)
  ## min.data - minimum of the observed data at each variable
  AEP.vec <- rep(NA,length(Level))  # set up empty vector for annual exceedance probabilities
  # calculation when there are no missing values
  if(sum(is.na(Level)) == 0){
    for(i in 1:nrow(GLOparameters)){
      GLOpara <- vec2par(c(GLOparameters[i,1], GLOparameters[i,2], GLOparameters[i,3]), type="glo")
      AEP.vec[i] <- min(1-cdfglo(Level[i], GLOpara), 0.99949)  # restrict the AEP value to 99.9% rather than letting it round up to 100%
      AEP.vec[i] <- max(AEP.vec[i], 0.00051)  # restrict the AEP value to 0.1% rather than 0%  
    }
  }
  # deal with missing values 
  else{
    id.NA.Level <- is.na(Level) == "TRUE"
    for(i in 1:nrow(GLOparameters)){
      if(id.NA.Level[i] == "TRUE"){
        AEP.vec[i] <- NA
      }
      else{
        GLOpara <- vec2par(c(GLOparameters[i,1], GLOparameters[i,2], GLOparameters[i,3]), type="glo")
        AEP.vec[i] <- min(1-cdfglo(Level[i], GLOpara), 0.99949)  # restrict the AEP value to 99.9% rather than letting it round up to 100%
        AEP.vec[i] <- max(AEP.vec[i], 0.00051)  # restrict the AEP value to 0.1% rather than 0%
      }
    }
  }
  # return the data with the names of gauges
  AEP.mat <- as.data.frame(matrix(AEP.vec, ncol=length(Level)))
  names(AEP.mat) <- c(name.variables)
  return(AEP.mat)	
}

# Extremal dependence function (Chi)
ext.dep.function.chi <- function(data, name.data.select){
  # Inputs
  ## data - observed data with no date column and each column header containing the name of the variable
  ## name.data.select - names of the two variables for which the extremal dependence measures are calculated
  data.select <- data[, c(name.data.select)]
  extdep.est <- chi(data.select)
  # test for any independence or dependence 
  chi.est <- extdep.est$chi[99,2]  # 99th percentile for consistency with the FD2308 method, column 2 is Chi (the other two columns are the upper and lower limits) 
  chi.est2 <- max(chi.est, 0)
  return(chi.est2)
}


# Marginal analysis tab functions -----------------------------------------

# AEP to level (GP) 
AEP.Level.Calc.GP <- function(aep, parameters, name.variables){
  # Inputs
  ## aep - annual exceedance probabilities 
  ## parameters - GPD parameters 
  ## name.variables - variable names
  aep.level.data <- as.data.frame(matrix(NA, length(aep), ncol=nrow(parameters)))
  names(aep.level.data) <- name.variables
  for(i in 1:nrow(parameters)){
    aep.level.data[,i] <- quant.gpd.cmax(1-aep, m=parameters[i,4], threshold=parameters[i,1],
                                         sigma_u=parameters[i,2], xi=parameters[i,3])
  }
  return(aep.level.data)
}   

# AEP to level (GL)
glog.quant <- function(aep, parameters){
  # Inputs
  ## aep - annual exceedance probabilities to evaluate
  ## parameters - three parameters of the generalised logistic distribution
  prob <- 1-aep
  name.par <- rownames(parameters)  
  aep.level.data.glog <- as.data.frame(matrix(NA, length(aep), ncol=length(name.par)))
  names(aep.level.data.glog) <- name.par
  # calculate the quantiles 
  for(i in 1:length(name.par)){
    location <- parameters$Location[i]
    scale <- parameters$Scale[i]
    shape <- parameters$Shape[i]
    MargPar.glo <- vec2par(c(location, scale, shape), type="glo")
    aep.level.data.glog[,i] <- quaglo(prob, MargPar.glo)
  }
  return(aep.level.data.glog)
}  

# Transform the simulation outputs to be consistent with the user marginal parameters
HT.userMarginals <- function(ModelFit, SimOutput, ObservedData, MarginalParameters,
                             no.years.data, threshold, declustered){
  # Inputs
  ## ModelFit - output from the HT.model.fit function
  ## SimOutput - output from the HT.simulation function
  ## ObservedData - observed data to which the H+T function was fitted 
  ## MarginalParameters - marginal parameters (each row corresponds to a different variable) 
  ## no.years.data - number of years of data (only needed when the user specifies that their data are declustered) 
  ## threshold is the marginal and dependence threshold
  # Set up empty data frames
  LevelMat <- matrix(NA, nrow=nrow(SimOutput$Probability), ncol=ncol(SimOutput$Probability))
  RPMat <- matrix(NA, nrow=nrow(SimOutput$Probability), ncol=ncol(SimOutput$Probability))
  # Set up marginal parameters
  phi <- 1-threshold  # probability of exceedance
  # marginal thresholds 
  thresh.value <- rep(NA, length(ModelFit$MarginalParameter))
  for(i in 1:length(thresh.value)){
    thresh.value[i] <- ModelFit$MarginalParameter[[i]]$threshold
  }
  # determine the number of clusters per year
  no.clusters <- rep(NA, length(ModelFit$MarginalParameter))
  if(declustered == TRUE){
    for(i in 1:length(no.clusters)){
      no.clusters[i] <- length(ModelFit$MarginalParameter[[i]]$exceedances)/(no.years.data)
    }
  }
  if(declustered == FALSE){
    for(i in 1:length(no.clusters)){
      no.clusters[i] <- length(ModelFit$MarginalParameter[[i]]$exceedances)/(length(ModelFit$MarginalParameter[[i]]$data)/365.25)
    }
  }
  npy <- rep(365.25, length(no.clusters))*(1/no.clusters)
  # Using the user-supplied marginal parameters
  # Determine level
  withProgress(message = 'Updating simulated data', value = 0, detail = "0% complete", {
    for(i in 1:nrow(MarginalParameters)){
      MargPar <- MarginalParameters[i,]
      MargPar.glo <- vec2par(c(MargPar$Location, MargPar$Scale, MargPar$Shape), type="glo")
      ObsData <- ObservedData[,i]
      ecdf.fun <- ecdf(ObsData)
      Prob.Unif <- SimOutput$Probability[,i]
      # Threshold on the modelling threshold 
      u.thresh <- quantile(ObsData, threshold)
      threshold.break <- cdfglo(u.thresh, MargPar.glo)^(1/npy[i])
      # Points below the threshold 
      Prob.Lower.Loc <- which(Prob.Unif < threshold.break)
      Prob.Lower <- Prob.Unif[which(Prob.Unif < threshold.break)]
      p.glo <- cdfglo(u.thresh, MargPar.glo)^(1/npy[i])
      p.below.emp <- Prob.Lower*(ecdf.fun(u.thresh)/(p.glo))
      # generate quantiles below the threshold - ECDF
      quant.below <- quantile(ObsData, p.below.emp, na.rm=TRUE)
      # Above the modelling threshold 
      Prob.Above.Loc <- which(Prob.Unif >= threshold.break)
      Prob.Above <- Prob.Unif[which(Prob.Unif >= threshold.break)]	
      ProbExc.Above.Calc <- Prob.Above^(npy[i])
      # generate quantiles above the threshold - generalised logistic distribution
      Exc.Prob.Above.Calc <- 1-ProbExc.Above.Calc
      quant.above <- quaglo(ProbExc.Above.Calc, MargPar.glo)
      # place the levels back into the matrix
      LevelMat[Prob.Lower.Loc, i] <- quant.below
      LevelMat[Prob.Above.Loc, i] <- quant.above
      
      # Increment the progress bar and update the detail text
      incProgress(1/nrow(MarginalParameters), detail = paste(paste0(round(100*(i/nrow(MarginalParameters)),0), "%"), "complete"))
      
      # Pause for 0.1 seconds to simulate a long computation
      Sys.sleep(0.1)
    }
  })
  
  # Determine return period
  withProgress(message = 'Processing data', value = 0, detail = "0% complete", {
    for(i in 1:nrow(MarginalParameters)){
      MargPar <- MarginalParameters[i,]
      MargPar.glo <- vec2par(c(MargPar$Location, MargPar$Scale, MargPar$Shape), type="glo")
      RPMat[,i] <- 1/(1-cdfglo(LevelMat[,i], MargPar.glo))
    }
    # Increment the progress bar and update the detail text
    incProgress(1/3, detail = "33% complete")
    
    # Pause for 0.1 seconds to simulate a long computation
    Sys.sleep(0.1)

  # Format data
  Date.Format <- as.character(SimOutput$ReturnPeriod$Date)
  # Level 
  Level.Data <- as.data.frame(cbind(Date.Format, LevelMat))
  names(Level.Data) <- names(SimOutput$ReturnPeriod)
  
  # Increment the progress bar and update the detail text
  incProgress(1/3, detail = "67% complete")
  
  # Pause for 0.1 seconds to simulate a long computation
  Sys.sleep(0.1)
  
  # Return period
  RP.Data <- as.data.frame(cbind(Date.Format, RPMat))
  names(RP.Data) <- names(SimOutput$ReturnPeriod)
  # Outputs
  ## Standardised - data simulations on the simulation scale (Laplace marginals)
  ## Probability - simulations on the probability scale
  ## ReturnPeriod - return period of the simulated data
  ## Value - original measurement value of the simulated data 
  ## IDTrigger - determining which gauge is the trigger location 
  ## GPDParameters - the GPD parameters and the number of clusters per year
  ## GLOParameters - the GLO parameters
  ## simYears - number of years of simulated data
  ## simEventsPerYear - number of simulated events per year
  UserMargInfo <- list(Standardised=SimOutput$Standardised, Probability=SimOutput$Probability,
                       ReturnPeriod=RP.Data, Value=Level.Data, IDTrigger=SimOutput$IDTrigger,
                       GPDParameters=SimOutput$GPDParameters, GLOParameters=MarginalParameters,
                       simYears=SimOutput$simYears, simEventsPerYear=SimOutput$simEventsPerYear)
  
  # Increment the progress bar and update the detail text
  incProgress(1/3, detail = "100% complete")
  
  # Pause for 0.1 seconds to simulate a long computation
  Sys.sleep(0.1)
  })
  return(UserMargInfo)
}


# Joint probability analysis tab functions -----------------------------------------

# Joint probability calculation
joint.rp.func.year <- function(data, vec.interest, scale="AEP", years.data){
  # Inputs
  ## data - containing the event set in terms of return period as well as value (a list)
  ## vec.interest - vector that the user inputs to threshold the data over (a vector)
  ## scale - whether the joint probability calculation is made based on either return period or level (text argument)
  ## years.data - number of years of simulated data 
  # constraints to deal with missing values (no threshold values or just one) 
  n.1 <- length(vec.interest)-1
  if(sum(is.na(vec.interest)) == length(vec.interest)	|	sum(is.na(vec.interest)) == n.1){
    stop("Please define values for which the joint probability is calculated", call. = FALSE)
  }
  else{
    if(sum(is.na(vec.interest)) == 0){  # determining whether there are any missing values 
      if((min(vec.interest) <= 0 | max(vec.interest) > 1) & scale == "AEP"){
        stop("Annual exceedance probability must be between 0 and 1", call. = FALSE)
        return(NA)
      }
      else{
        if(scale == "AEP"){
          date <- as.character(data$ReturnPeriod[,1])
          data.calc <- data$ReturnPeriod[,-1]  # remove date column
          data.calc <- apply(data.calc, 2, function(x){as.numeric(as.character(x))})
          rp.vec <- 1/(vec.interest)
          ind.exceedance <- apply(apply(data.calc, 1, function(x){x > rp.vec}), 2, sum)
          id.loc.exceedance <- which(ind.exceedance == length(vec.interest))  # find where every variable is bigger than the specified event 
          date.id.loc.exc <- date[id.loc.exceedance]
          n.level.year <- nlevels(factor(date.id.loc.exc))
          if(length(id.loc.exceedance) == 0){
            stop("No events in the simulated event set are greater than your specified event. Please simulate a larger event set.", call. = FALSE)		
          }
          else{
            jp.calc <- n.level.year/years.data
            jp.calc.round <- jp.calc
            No.Events <- length(id.loc.exceedance)
            JP.Info <- list(NoEvents=No.Events, JPCalc=jp.calc.round)
          }
        }
        if(scale == "Level"){
          date <- as.character(data$Level[,1])
          data.calc <- apply(data$Level[,-1], 2, function(x){as.numeric(as.character(x))})
          ind.exceedance <- apply(apply(data.calc, 1, function(x){x > vec.interest}), 2, sum)
          id.loc.exceedance <- which(ind.exceedance == length(vec.interest))
          date.id.loc.exc <- date[id.loc.exceedance]
          n.level.year <- nlevels(factor(date.id.loc.exc))
          if(length(id.loc.exceedance) == 0){
            stop("No events in the simulated event set are greater than your specified event. Please simulate a larger event set.", call. = FALSE)
          }
          else{
            jp.calc <- n.level.year/years.data
            jp.calc.round <- jp.calc
            No.Events <- length(id.loc.exceedance)
            JP.Info <- list(NoEvents=No.Events, JPCalc=jp.calc.round)
          }
        }
        # Outputs
        ## NoEvents - number of events in the event set that exceed the user-supplied event at all variables
        ## JPCalc - annual joint probability calculation 
        return(JP.Info)
      }
    }
    else{  # deal with missing values 
      if((min(na.omit(vec.interest)) <= 0 | max(na.omit(vec.interest)) > 1) & scale == "AEP"){
        stop("Annual exceedance probability must be between 0 and 1", call. = FALSE)
      }
      else{
        if(scale == "AEP"){
          id.NA.vec <- which(is.na(vec.interest) == "TRUE")
          vec.int.narm <- na.omit(vec.interest)
          date <- as.character(data$ReturnPeriod[,1])
          data.calc.na <- data$ReturnPeriod[,-1]  # remove date column
          data.calc <- data.calc.na[, -c(id.NA.vec)]
          data.calc <- apply(data.calc, 2, function(x){as.numeric(as.character(x))})
          rp.vec <- 1/(vec.int.narm)
          ind.exceedance <- apply(apply(data.calc, 1, function(x){x > rp.vec}), 2, sum)
          id.loc.exceedance <- which(ind.exceedance == length(vec.int.narm))
          date.id.loc.exc <- date[id.loc.exceedance]
          n.level.year <- nlevels(factor(date.id.loc.exc))
          if(length(id.loc.exceedance) == 0){
            stop("No events in the simulated event set are greater than your specified event. Please simulate a larger event set.", call. = FALSE)
            return(NA)
          }
          else{
            jp.calc <- n.level.year/years.data
            jp.calc.round <- jp.calc
            No.Events <- length(id.loc.exceedance)
            JP.Info <- list(NoEvents=No.Events, JPCalc=jp.calc.round)
          }
        }
        if(scale=="Level"){
          id.NA.vec <- which(is.na(vec.interest) == "TRUE")
          vec.int.narm <- na.omit(vec.interest)
          date <- as.character(data$Level[,1])
          data.calc.na <- data$Level[,-1]  # remove date column
          data.calc <- data.calc.na[, -c(id.NA.vec)]
          data.calc <- apply(data.calc, 2, function(x){as.numeric(as.character(x))})
          ind.exceedance <- apply(apply(data.calc, 1, function(x){x > vec.int.narm}), 2, sum)
          id.loc.exceedance <- which(ind.exceedance == length(vec.int.narm))
          date.id.loc.exc <- date[id.loc.exceedance]
          n.level.year <- nlevels(factor(date.id.loc.exc))
          if(length(id.loc.exceedance) == 0){
            stop("No events in the simulated event set are greater than your specified event. Please simulate a larger event set.", call. = FALSE)
            return(NA)
          }
          else{
            jp.calc <- n.level.year/years.data
            jp.calc.round <- jp.calc
            No.Events <- length(id.loc.exceedance)
            JP.Info <- list(NoEvents=No.Events, JPCalc=jp.calc.round)
          }
        }
        # Outputs
        ## NoEvents - number of events in the event set that exceed the user-supplied event at all variables
        ## JPCalc - annual joint probability calculation 
        return(JP.Info)
      }
    }
  }
}

# Encounter probabilities
encounter.prob.func <- function(prob.jp, no.sim.years, encounter.years){
  # Inputs
  ## prob.jp - probability of the joint probability event
  ## no.sim.years - number of years' worth of simulated events
  ## encounter.years - years of the encounter probabilities
  prob.year <- prob.jp*no.sim.years
  no.years <- no.sim.years-prob.year
  prob <- no.years/no.sim.years
  encounter.prob <- 1-(prob^encounter.years)
  # convert the probabilities into percentages
  encounter.percentage <- encounter.prob*100
  encounter.percentage.round <- round(encounter.percentage, 2)
  encount.year.char <- as.character(encounter.years)
  encounter.table <- as.data.frame(cbind(encount.year.char, encounter.percentage.round))
  names(encounter.table) <- c("Years", "Percentage chance")
  return(encounter.table)
}
