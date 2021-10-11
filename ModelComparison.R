source("Constants.R")

library(xlsx)
library(loo)
library(beepr)

output.header <- c("Model",	
                   "a (mean)",	
                   "a (sd)",	
                   "ar (mean)",	
                   "ar (sd)",	
                   "ap (mean)",	
                   "ap (sd)",	
                   "b (mean)",	
                   "b (sd)",	
                   "c (mean)",	
                   "c (sd)",	
                   "log likelihood",	
                   "Params (effective)",
                   "DIC",	
                   "LOO IC",	
                   "LOO IC SE",	
                   "WAIC",	
                   "WAIC SE")

putInVector <- function(vector, input, header, targetHeaderItem)
{
  index <- match(c(targetHeaderItem), header)
  vector[index] <- input
  
  return(vector)
}

fitStanModel <- function(modelIndex, dataList)
{
  ret <- character(length(output.header))
  model.name <- models[modelIndex]
  ret <- putInVector(ret, model.name, output.header, "Model")
  
  fit <- fit.to.model(model.name, 
                      dataList, 
                      chains = 3, 
                      iter = 6000, 
                      warmup = 1000, 
                      thin = 1, 
                      adapt.delta = .999)
  
  fit_summary <- summary(fit)$summary #gives information about all converged chains
  fit_params <- rownames(fit_summary)
  
  if("a_mean" %in% fit_params)
  {
    ret <- putInVector(ret, fit_summary["a_mean", "mean"], output.header, "a (mean)")
    ret <- putInVector(ret, fit_summary["a_sd", "mean"], output.header, "a (sd)")
  }
  if("ap_mean" %in% fit_params)
  {
    ret <- putInVector(ret, fit_summary["ap_mean", "mean"], output.header, "ap (mean)")
    ret <- putInVector(ret, fit_summary["ap_sd", "mean"], output.header, "ap (sd)")
  }
  if("ar_mean" %in% fit_params)
  {
    ret <- putInVector(ret, fit_summary["ar_mean", "mean"], output.header, "ar (mean)")
    ret <- putInVector(ret, fit_summary["ar_sd", "mean"], output.header, "ar (sd)")
  }
  if("b_mean" %in% fit_params)
  {
    ret <- putInVector(ret, fit_summary["b_mean", "mean"], output.header, "b (mean)")
    ret <- putInVector(ret, fit_summary["b_sd", "mean"], output.header, "b (sd)")
  }
  if("c_mean" %in% fit_params)
  {
    ret <- putInVector(ret, fit_summary["c_mean", "mean"], output.header, "c (mean)")
    ret <- putInVector(ret, fit_summary["c_sd", "mean"], output.header, "c (sd)")
  }
  
  total_log_lik <- fit_summary["total_log_lik", "mean"]
  
  writeLines("Calculating IC values")
  loglik.values <- extract_log_lik(fit)
  deviance <- extract(fit)$total_log_lik * -2
  
  loo <- loo(loglik.values)
  waic <- waic(loglik.values)
  params <- var(deviance)/2 #based on Gelman et al., 2004, p. 182
  
  ret <- putInVector(ret, total_log_lik, output.header, "log likelihood")
  ret <- putInVector(ret, params, output.header, "Params (effective)")
  ret <- putInVector(ret, 2*params - 2*total_log_lik, output.header, "DIC")
  ret <- putInVector(ret, loo$looic, output.header, "LOO IC")
  ret <- putInVector(ret, loo$se_looic, output.header, "LOO IC SE")
  ret <- putInVector(ret, waic$waic, output.header, "WAIC")
  ret <- putInVector(ret, waic$se_waic, output.header, "WAIC SE")
  
  writeLines(paste("Finished with", model.name, "\n"))
  
  return (ret)
}

output.data <- do.call("rbind", lapply(1:length(models), fitStanModel, dataList = file.data))

write.xlsx(rbind(output.header, output.data), output.file, row.names=FALSE, col.names = FALSE)
writeLines(paste("Done! Data written to ", output.file))
beep(0)