library(rstan)
library(xlsx)

#The following options are recommended when running a multicore CPU with excess RAM.  Comment out if this is not the case.
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
options(boot.parallel = "multicore")
#end comment

models <- c("ClassicUnaltered", 
            "ClassicCompetitiveSuppression",
            "ClassicDirectSuppression",
            "ROEClassicCompetitiveSuppression",
            "ROEClassicDirectSuppression",
            "GeneralizedMatchingLaw",
            "GeneralizedCompetitiveSuppression",
            "GeneralizedDirectSuppression",
            "ProspectModel",
            "SplitExponentCompetitiveSuppression",
            "SplitExponentDirectSuppression",
            "ROESplitExponentCompetitiveSuppression",
            "ROESplitExponentDirectSuppression"
)

data.files <- c("PunishmentDataCritchfield.xlsx",
                "PunishmentDataFarley.xlsx", 
                "PunishmentDataKlapes.xlsx",
                "PunishmentDataKlapesCritchfield.xlsx",
                "PunishmentDataCritchfieldExpanded.xlsx")

output.files <- c("OutputCritchfield.xlsx",
                  "OutputFarley.xlsx", 
                  "OutputKlapes.xlsx",
                  "OutputKlapesCritchfield.xlsx",
                  "OutputsCritchfieldExpanded.xlsx")

current.analysis.index <- 1
output.file <- paste0("Outputs/", output.files[current.analysis.index])
writeLines(paste("Loading from", data.files[current.analysis.index], "\n"))
file.data <- read.xlsx(paste0("Data/", data.files[current.analysis.index]), 1) 
file.data <- subset(file.data, B1*B2*r1*r2 > 0) #anything with b or r values of 0 will kill fits
#file.data <- subset(file.data, r1 > p1 & r2 > p2) #this is what Critchfield did on analysis, so we repeat it

fit.to.model <- function(model.name, data, chains, iter, warmup, thin, adapt.delta)
{
  model.file <- paste0("Models/", model.name, ".stan")
  writeLines(paste("Fitting", model.name))
  
  ids <- data[,"Subject.ID"]
  ids.unique <- unique(ids)
  
  stan.data <- list(N = length(ids.unique),
                    unique_subject_IDs = ids.unique,
                    num_points = nrow(data),
                    subject_IDs = ids,
                    b1 = data[,"B1"],
                    b2 = data[,"B2"],
                    r1 = data[,"r1"],
                    r2 = data[,"r2"],
                    p1 = data[,"p1"],
                    p2 = data[,"p2"]) 
  
  fit <- stan(file = model.file,
              data = stan.data,
              chains = chains,
              iter = iter,
              warmup = warmup,
              thin = thin,
              control = list(adapt_delta = adapt.delta))
  
  return (fit)
}