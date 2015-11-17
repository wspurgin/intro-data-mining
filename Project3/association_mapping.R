# Usage Rscript path/to/association_mapping.R Census_Data_2010.csv Graduation_Rates.csv Max_Tract_Overlap.csv F33-finance.csv

# Will Spurgin
# Copyright November 7th, 2015
# Introduction to Data Mining, GradNation Project 3


# Initialize --------------------------------------------------------------

# Get path to initializing script
if (!is.null(sys.frames())) {
  script.dir <- dirname(sys.frame(1)$ofile)
} else {
  # If not called using RScript, assume we're in interactive RStudio session
  # at the root repo level.
  script.dir <- file.path(getwd(), "Project3")
}

init.file <- file.path(script.dir, "..", "Shared", "init.R")

source(init.file)

rm(script.dir, init.file)

# Create Transaction Data Sets --------------------------------------------

discretized.districts <- districts %>% select(-one_of("gidtr", "district.id", "percentage"))
colsToDescretize <- colnames(discretized.districts)[!colnames(discretized.districts) == "state.name"]

library("arules")
library("arulesViz")

# Being lazy and only using frequency...
for(i in 1:length(colsToDescretize))
  discretized.districts[,colsToDescretize[i]] <- discretize(discretized.districts[,colsToDescretize[i]],
                                                           method = "frequency", categories = 3,
                                                           labels = c("low", "medium", "high"))


# Helpful visuals
smoothed <- density(districts$all.rate, bw = "nrd")
hist(districts$all.rate, breaks = 40, freq = FALSE, xlab = "District Grad Rate", main = "District Grad Rate Densities w/ Smoothing")
lines(smoothed, col="red", lwd = 2)

barplot(table(discretized.districts$all.rate) / nrow(discretized.districts), main = "Grad Rate Discretized Group Density", ylab="Density", xlab="Discretized Group", ylim = c(0, .5))

# Association Rule Mapping ------------------------------------------------

national.trans <- as(discretized.districts, "transactions")
national.trans

# Only contains with all.rate in the right hand side (should be around 473K)
r.national <- apriori(trans, parameter = list(support=0.02),
             appearance = list(rhs = c("all.rate=low",
                                       "all.rate=medium",
                                       "all.rate=high"),
                               default = "lhs"))

m <- interestMeasure(r, measure = c("chiSquared", "fishersExactTest"),
                     transactions = trans)
quality(r) <- cbind(quality(r), m)

texas.discretized.districts <- districts[districts$state.name == "Texas", ] %>%
  select(-one_of("gidtr", "district.id", "percentage", "state.name"))
for ( i in 1:ncol(discretized.districts))
  texas.discretized.districts[, i] <- discretize(texas.discretized.districts[, i], method = "frequency", categories = 3,labels = c("low", "medium", "high"))

texas.trans <- discretized.districts %>% as("transactions")
tecas.trans

r.low <- apriori(trans, parameter = list(support=0.05),
                  appearance = list(rhs = c("all.rate=medium"),
                                    default = "lhs"))

m <- interestMeasure(r.low, measure = c("chiSquared", "fishersExactTest"),
                     transactions = texas.trans)
quality(r.low) <- cbind(quality(r.low), m)

r.medium <- apriori(trans, parameter = list(support=0.05),
             appearance = list(rhs = c("all.rate=medium"),
                               default = "lhs"))

m <- interestMeasure(r.medium, measure = c("chiSquared", "fishersExactTest"),
                     transactions = texas.trans)
quality(r.medium) <- cbind(quality(r.medium), m)

r.high <- apriori(trans, parameter = list(support=0.05),
                    appearance = list(rhs = c("all.rate=medium"),
                                      default = "lhs"))

m <- interestMeasure(r.high, measure = c("chiSquared", "fishersExactTest"),
                     transactions = texas.trans)
quality(r.high) <- cbind(quality(r.high), m)

rm(m, i)
# Plot your associations using the following command. Substitute "r" with your Formal class rules' object.
# plot(head(sort(r, by = "fisher", decreasing = FALSE), n = 50), method = "graph", interactive = TRUE)
