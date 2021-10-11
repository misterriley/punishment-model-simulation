library(loo)
library(beepr)

source("Constants.R")

bootstrap <- function(data, 
                      R, 
                      model.indices,
                      chains,
                      iter,
                      warmup,
                      thin,
                      adapt.delta)
{
  ret <- data.frame()
  
  for(i in 1:R)
  {
    writeLines(paste0(i))
    boot.data <- data #prepareSample(data)
    looICs <- compare.models(boot.data, 
                   model.indices,
                   chains,
                   iter,
                   warmup,
                   thin,
                   adapt.delta)
    ret <- rbind(ret, looICs)
  }
  
  colnames(ret) <- models[model.indices]
  return (ret)
}

prepareSample <- function(data)
{
  ret <- data.frame()
  
  unique.IDs <- unique(data$Subject.ID)
  for(i in 1:length(unique.IDs))
  {
    id <- unique.IDs[i]
    rows <- subset(data, Subject.ID == id)
    boot.indexes <- sample.int(nrow(rows), replace = TRUE)
    boot.rows <- rows[boot.indexes,]
    
    ret <- rbind(ret, boot.rows)
  }
  
  return (ret)
}

# Compares the LOO IC values of two models fitted on a data set.  
# If the first is a better fit, this function returns a negative number.
# If the second is a better fit, it returns a positive number.  The 
# value returned equals the LOO IC of the first minus the LOO IC of the
# second model.  
compare.models <- function(data,
                           model.indices, 
                           chains,
                           iter,
                           warmup,
                           thin,
                           adapt.delta)
{
  ret <- numeric()
  
  for(model.index in model.indices)
  {
    model <- models[model.index]
    fit <- fit.to.model(model.name = model, 
                          data = data, 
                          chains = chains, 
                          iter = iter, 
                          warmup = warmup,
                          thin = thin,
                          adapt.delta = adapt.delta)
    loglik.values <- extract_log_lik(fit)
    loo <- loo(loglik.values)
    
    ret <- append(ret, loo$looic)
  }
  
  names(ret) <- models[model.indices]
  print(ret)
  return (ret)
}

model.one.index <- 1
model.two.index <- 2
chains <- 3
iter <- 6000
warmup <- 1000
thin <- 1
adapt.delta <- .999
replications <- 100

boot.sample <- bootstrap(data = file.data, 
                    R = replications, 
                    model.indices = c(1,2,3,4,5,6,7,8),
                    chains = chains,
                    iter = iter,
                    warmup = warmup,
                    thin = thin,
                    adapt.delta = adapt.delta)

print(boot.sample)
boot.output.file <- "Outputs/BootstrapComparison.xlsx"
write.xlsx(boot.sample, boot.output.file)

beep(0)