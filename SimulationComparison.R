library(xlsx)
library(stats)
library(stats4)

out.of.bounds <- -10000

minuslogl <- function(params, x.function, data)
{  
  ret <- 0
  if("c" %in% names(params) &
     params["c"] < 0)
  {
    return (out.of.bounds)
  }
  
  for(i in seq_len(nrow(data)))
  {
    y <- data[i,"log.B.Ratio"]
    x <- do.call(x.function, args=list(params, data, row.index = i))
    
    log_lik <- dnorm(y, x, params["se"], log = TRUE)
    ret <- ret + log_lik
  }
  
  return (ret)
}

minuslogl.se.only <- function(se, x.function, data)
{
  ret <- 0
  
  for(i in seq_len(nrow(data)))
  {
    y <- data[i,"log.B.Ratio"]
    x <- do.call(x.function, args=list(data = data, row.index = i))
    
    log_lik <- dnorm(y, x, se, log = TRUE)
    ret <- ret + log_lik
  }
  
  return (ret)
}

generate <- function(params, x.function, data)
{
  ret <- data
  
  for(i in seq_len(nrow(data)))
  {
    x <- do.call(x.function, args = list(params, data, row.index = i))
    y <- rnorm(1, mean = x, sd = params["se"])
    
    ret[i, "log.B.Ratio.Generated"] <- y
  }
  
  return (ret)
}

roe.classic.cs.x <- function(params, data, row.index)
{
  num <- data[row.index, "r1"] + params["c"] * data[row.index, "p2"]
  denom <- data[row.index, "r2"] + params["c"] * data[row.index, "p1"]
  
  return (log(num/denom))
}

roe.classic.ds.x <- function(params, data, row.index)
{
  num <- data[row.index, "r1"] - params["c"] * data[row.index, "p1"]
  denom <- data[row.index, "r2"] - params["c"] * data[row.index, "p2"]
  
  return (log(num/denom))
}

classic.unaltered.x <- function(params = NULL, data, row.index)
{
  return (data[row.index,"log.r.Ratio"])
}

classic.cs.x <- function(params = NULL, data, row.index)
{
  return (data[row.index,"log.cs.Ratio"])
}

classic.ds.x <- function(params = NULL, data, row.index)
{
  return (data[row.index,"log.ds.Ratio"])
}

gml.x <- function(params, data, row.index)
{
  return (params["a"] * data[row.index, "log.r.Ratio"] + log(params["b"]))
}

gen.cs.x <- function(params, data, row.index)
{
   return (params["a"] * data[row.index,"log.cs.Ratio"] + log(params["b"]))
}

gen.ds.x <- function(params, data, row.index)
{
  return (params["a"] * data[row.index,"log.ds.Ratio"] + log(params["b"]))
}

get.initial.params <- function(x.function)
{
  if(x.function == "gml.x" | 
     x.function == "gen.ds.x" | 
     x.function == "gen.cs.x")
  {
    return (c(a = 1, b = 1, se = 1))
  }
  
  if(x.function == "roe.classic.ds.x" |
     x.function == "roe.classic.cs.x")
  {
    return (c(c = 1, se = 1))
  }
  
  return (c(se = 1))
}

data.file <- "PunishmentDataCritchfield.xlsx"

writeLines(paste("Loading from", data.file, "\n"))
file.data <- read.xlsx(paste0("Data/", data.file), 1)
file.data[,"log.B.Ratio"] <- log(file.data[,"B1"]/file.data[,"B2"])
file.data[,"log.r.Ratio"] <- log(file.data[,"r1"]/file.data[,"r2"])
file.data[,"log.cs.Ratio"] <- log((file.data[,"r1"]+file.data[,"p2"])/(file.data[,"r2"]+file.data[,"p1"]))
file.data[,"log.ds.Ratio"] <- log((file.data[,"r1"]-file.data[,"p1"])/(file.data[,"r2"]-file.data[,"p2"]))
file.data <- subset(file.data, B1*B2*r1*r2 > 0) #anything with b or r values of 0 will kill fits

IDs <- unique(file.data[,"Subject.ID"])

x.functions <- c(
                "classic.unaltered.x",
                 "classic.cs.x",
                 "classic.ds.x",
                 "roe.classic.cs.x",
                 "roe.classic.ds.x",
                "gml.x",
                "gen.cs.x",
                "gen.ds.x"
  )

for(x.function in x.functions)
{
  writeLines(paste0("\n--- ", x.function, " ---\n"))
  for(id in IDs)
  {
    subj.data <- file.data[which(file.data[,"Subject.ID"]==id),]
    par <- get.initial.params(x.function)
    if(length(par) == 1)
    {
      fit <- optimize(minuslogl.se.only,
                      interval = c(-1000, 1000),
                      maximum = TRUE,
                      data = subj.data,
                      x.function = x.function)
      
      par.out <- c(se = fit$maximum)
    }
    else
    {
      fit <- optim(par,
                  minuslogl, 
                  data = subj.data,
                  x.function = x.function,
                  control = c(fnscale = -1)
      )
      par.out <- fit$par
    }
    simulation <- generate(par.out, x.function, subj.data)
    print(subj.data)
    print(par.out)
    print(simulation)
    writeLines("\n")
  }
}