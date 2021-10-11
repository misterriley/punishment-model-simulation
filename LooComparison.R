source("Constants.R")
library(loo)
library(beepr)
library(xlsx)

chains <- 24
reps <- 6000
warmup <- 1000
thin <- 1
adapt.delta <- .999

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
                   "LOO IC",	
                   "LOO IC SE",	
                   "LOO IC vs. Classic",	
                   "LOO IC vs. Classic (SE)",
                   "LOO IC vs. GML",
                   "LOO IC vs. GML (SE)")

fit.classic <- fit.to.model("ClassicUnaltered", file.data, chains, reps, warmup, thin, adapt.delta)
fit.generalized <- fit.to.model("GeneralizedMatchingLaw", file.data, chains, reps, warmup, thin, adapt.delta)

loglik.values.classic <- extract_log_lik(fit.classic)
loglik.values.generalized <- extract_log_lik(fit.generalized)

loo.classic <- loo(loglik.values.classic)
loo.generalized <- loo(loglik.values.generalized)

writeComparison <- function(fit, model.name)
{
  ret <- character(length(output.header))
  ret <- putInVector(ret, model.name, output.header, "Model")
  
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
  
  writeLines("Calculating IC values")
  loglik.values <- extract_log_lik(fit)
  loo <- loo(loglik.values)
  
  ret <- putInVector(ret, loo$looic, output.header, "LOO IC")
  ret <- putInVector(ret, loo$se_looic, output.header, "LOO IC SE")
  
  compare.classic <- compare(loo, loo.classic)
  compare.generalized <- compare(loo, loo.generalized)
  
  writeLines(paste("Finished with", model.name, "\n"))
  
  ret <- putInVector(ret, compare.classic["elpd_diff"]*-2, output.header, "LOO IC vs. Classic")
  ret <- putInVector(ret, compare.classic["se"]*2, output.header, "LOO IC vs. Classic (SE)")
  
  ret <- putInVector(ret, compare.generalized["elpd_diff"]*-2, output.header, "LOO IC vs. GML")
  ret <- putInVector(ret, compare.generalized["se"]*2, output.header, "LOO IC vs. GML (SE)")
  
  return (ret)
}

out.data <- writeComparison(fit.classic, "ClassicUnaltered")
out.data <- rbind(out.data, writeComparison(fit.generalized, "GeneralizedMatchingLaw"))

other.function.indices <- list(2,3,4,5,7,8)

fitAndCompare <- function(modelIndex)
{
  model.name <- models[modelIndex]
  fit <- fit.to.model(model.name, file.data, chains, reps, warmup, thin, adapt.delta)
  return(writeComparison(fit, model.name))
}

putInVector <- function(vector, input, header, targetHeaderItem)
{
  index <- match(c(targetHeaderItem), header)
  vector[index] <- input
  
  return(vector)
}

call.data <- do.call("rbind", lapply(other.function.indices, fitAndCompare))
data.to.write <- rbind(out.data, call.data)

write.xlsx(rbind(output.header, data.to.write), "Outputs/OutputsCritchfieldWithLOOICs.xlsx", row.names=FALSE, col.names = FALSE)
writeLines(paste("Done! Data written to ", output.file))
beep(0)