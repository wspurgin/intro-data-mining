# Usage Rscript path/to/cleaning.R Census_Data_2010.csv Graduation_Rates.csv Max_Tract_Overlap.csv F33-finance.csv

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

# Being lazy and only using frequency...
for(i in 1:length(colsToDescretize))
  discretized.districts[,colsToDescretize[i]] <- discretize(discretized.districts[,colsToDescretize[i]],
                                                           method = "frequency", categories = 3,
                                                           labels = c("low", "medium", "high"))


# Association Rule Mapping ------------------------------------------------

trans <- as(discretized.districts, "transactions")
trans

r <- apriori(trans, parameter = list(support=0.02))

# Only run inspect if you want to. It takes "awhile".
# inspect(head(sort(r, by = "lift")))

# Only contains with all.rate in the right hand side (should be around 350K).
r2 <- subset(r, rhs %pin% "all.rate")
rm(r)

inspect(head(sort(r2, by = "lift")))

plot(r2)
