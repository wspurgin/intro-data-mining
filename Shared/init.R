# Usage Rscript path/to/cleaning.R Census_Data_2010.csv Graduation_Rates.csv Max_Tract_Overlap.csv F33-finance.csv

# Will Spurgin
# Copyright October 12th, 2015
# Introduction to Data Mining, GradNation Project 2

library("dplyr")      # currently using => 0.4.3
library("rpart")      # currently using => 4.1-10
library("rpart.plot") # currently using => 1.5.3
library("caret")      # currently using => 6.0-57
library("corrplot")   # currently using => 0.73


###### Load Data Sets ######
args<-commandArgs(TRUE)

if (length(args) == 4) {
  census.data.csv <- arg[1]
  grad.rates.csv <- arg[2]
  max.tract.overlap.csv <- arg[3]
  f33.data.csv <- arg[4]
} else {
  warning("Unkown number of arguments, going with defaults")
  census.data.csv <- path.expand(file.path("~", "Projects", "dataDumps", "GradNation", "Census_Data_2010.csv"))
  grad.rates.csv <- path.expand(file.path("~", "Projects", "dataDumps", "GradNation", "Graduation_Rates.csv"))
  max.tract.overlap.csv <- path.expand(file.path("~", "Projects", "dataDumps", "GradNation", "Max_Tract_Overlap.csv"))
  f33.data.csv <- path.expand(file.path("~", "Projects", "dataDumps", "GradNation", "sdf121a.csv"))
}

c(census.data.csv, grad.rates.csv, max.tract.overlap.csv) %>%  paste("Reading data from:", .) %>% cat(sep = '\n')
census.data.orig <- read.csv(census.data.csv)
grad.rates <- read.csv(grad.rates.csv)
max.tract.overlap <- read.csv(max.tract.overlap.csv)
f33.data <- read.csv(f33.data.csv, sep = "\t", na.strings = c("NA", "NULL", "-2", "-1"))

rm(max.tract.overlap.csv, grad.rates.csv, census.data.csv, f33.data.csv, args)

###### Function definitions ######
income.as.numeric <- function(c) {
  lapply(c,
         function(x) {
           as.numeric(gsub("\\$|,", "", x))
         }
  )
}

rate.to.integer <- function(rate.str) {
  as.integer(sub("(-\\d+)|(GE)|(LT)|(GT)|(LE)|(PS)", "", rate.str))
}

as.percentage <- function(part, total, ...) UseMethod("as.percentage")

as.percentage.list <- function(part, total, ...) {
  lapply(part, FUN = function(p) { as.percentage(p, total) })
}

as.percentage.data.frame <- function(part, total, ...) {
  as.percentage(as.list(part), total)
}

as.percentage.numeric <- function(part, total) {
  part / total * 100
}

# Credit to Bill Dunlap https://stat.ethz.ch/pipermail/r-help/2012-January/300274.html
# Note since this first converts to a matrix, mixed valued lists get wrecked
# with this function.
vllist.to.data.frame <- function(data) {
  nCol <- max(vapply(data, length, 0))
  data <- lapply(data, function(row) c(row, rep(NA, nCol-length(row))))
  data <- matrix(unlist(data), nrow=length(data), ncol=nCol, byrow=TRUE)
  data.frame(data)
}

###### Clean Census Data ######
# once data sets are loaded, proceed with data cleaning
# starting with census.data
census.data <- census.data.orig

# GIDTR is the Global ID for a Tract (GIDTR), but it's loaded as a numeric.
# To preseve the idea that it's Nominal value, we'll transform it to a character
census.data$GIDTR <- as.character(census.data$GIDTR)

# Flag in the census data indicates that the given tract is uninhabitable land.
# However several of the population variables are non-zero (and upon inspection)
# illogical. Therefore those tracts must be stripped out (i.e. we only want the tracts
# where Flag == NA).
census.data <- census.data[is.na(census.data$Flag),]

# There's at least one row that has a valid GIDTR, but all other data is NA
misformatted <- (is.na(census.data$Tract) | is.na(census.data$County) | is.na(census.data$State))
census.data <- census.data[!misformatted,]

# There's at least one row that's missing its GIDTR, but has a state, county,
# and tract code (which makes up the GIDTR). So we'll make the GIDTR for it.
missing.gidtr <- census.data[is.na(census.data$GIDTR),]
missing.gidtr$GIDTR <- paste(missing.gidtr$State, missing.gidtr$County, missing.gidtr$Tract, sep = "")
census.data[row.names(missing.gidtr),] <- missing.gidtr

# There are a few tracts that have no census data, but they do have ACS data.
# So we need to make sure we keep track of those tracts.
no.census.data.tracts <- row.names(census.data[is.na(census.data$Tot_Population_CEN_2010),])

# 14 - 200 columns of census data are all population data
population.variable.numbers <- 14:200

# Having the population data broken out for the time being will make it easier
# to clean.
population.tract.data <- census.data[, population.variable.numbers]

# Set all NA's to 0
population.tract.data[is.na(population.tract.data)] <- 0

# TODO more cleaning as necessary with population.tract.data

# slap the clean data back in
census.data[, c(population.variable.numbers)] <- population.tract.data

# 201 - 245 columns of census data are housing variables (e.g. income)
housing.variable.numbers <- 201:245
housing.tract.data <- census.data[, housing.variable.numbers]

# The first 39 variables' units are all counts, and thus all ratio variables.
# Also, as a bonus, the data is all quite clean.

# The next variables are all income realated. The first two (240 & 241) are
# both counts, but the last 4 are accounting formatted strings ($123,456,789).
# We'll remove the "$" and ","s to make covert them to numbers.
housing.tract.data[,42:45] <- as.data.frame(income.as.numeric(housing.tract.data[,42:45]))

census.data[, housing.variable.numbers] <- housing.tract.data

# Playing nice with other processes on the CPU...
rm(population.tract.data, housing.tract.data, misformatted, missing.gidtr)

# Next comes the housing unit data (as they call it).
housing.unit.variables <- 246:280

# Only the last 4 columns need to be cleaned. They're valuations of housing in
# Accounting formatted strings.
housing.valuation.variables <- tail(housing.unit.variables, n=4)
census.data[, housing.valuation.variables] <- as.data.frame(
  income.as.numeric(census.data[,housing.valuation.variables])
)

# The next set of variables are all operational variables related to taking the
# Census. We don't care about those (at this moment), so they'll be the start
# of the ignored.attirbutes
ignored.attributes <- 281:294

# This last section is pretty valuable, as it contains some nice calculated
# percentages for us! Sill we need to verify them. Also, they're all
# represented in whole percentage (i.e. ratio * 100).
calculated.percentage.variables <- 295:550

# The only exception is variables 503:505 those are averages.
# (which is columns 209:211)
percentage.data <- census.data[, calculated.percentage.variables] / 100
percentage.data[, 209:211] <- census.data[, 503:505]

census.data[, calculated.percentage.variables] <- percentage.data

# Clean up
rm(percentage.data)

# Clean up variable names
names <- colnames(census.data)
names <- gsub("(ACS)(MOE)", "\\2_\\1", names)   # Switch the MOE and ACS and add an _
names <- gsub("(_\\d+)+$", "", names) # Remove _08_12 and _2010
# If we want to remove the ACS and CEN plus the _08_12 and _2010
# names <- gsub("(.*)(_((CEN)|(ACS))_\\d+(_\\d+)*)$", "\\1", names) # Remove the ACS_08_10 or CEN_2010
names <- tolower(names)                   # Make the names lowercase
names <- gsub("_", ".", names)            # _ to . to match R conventions
colnames(census.data) <- names

rm(names)

###### Clean Graduation Rates Data ######
# These first few steps are taken in part from Michael Hahsler at his site:
# http://michael.hahsler.net/SMU/EMIS7332/data/graduationrate/project1.html

# Columns 5:24 are all the cohort sizes and corresponding graduation rates
clean.cohort.rates <- grad.rates[,5:24]
clean.cohort.rates <- as.data.frame(lapply(clean.cohort.rates, FUN = as.character))
# Handle PS entries differently? Otherwise they'll be turned to NAs
clean.cohort.rates <- as.data.frame(lapply(clean.cohort.rates, FUN = rate.to.integer))

clean.grad.data <- as.data.frame(list(state = grad.rates$STNAM,
                                      fed.state.id = as.factor(grad.rates$FIPST),
                                      district.id = as.character(grad.rates$leaid11),
                                      district.name = grad.rates$leanm11), stringsAsFactors = FALSE)
clean.grad.data <- cbind.data.frame(clean.grad.data, clean.cohort.rates, stringsAsFactors = FALSE)
names <- colnames(clean.grad.data)
names <- gsub("(.*)_\\d+$", "\\1", names) # Remove the _1112
names <- tolower(names)                   # Make the names lowercase
names <- gsub("_", ".", names)            # _ to . to match R conventions
colnames(clean.grad.data) <- names

# Clean up memory
rm(clean.cohort.rates, names)

# This should be run only AFTER all cleaning is verified as done.
rm(census.data.orig, grad.rates)

###### Clean Financial F33 Data ######

# This process subjects the LEAID (or school district id) to the same process
# that the LEAIDs from the max.tract.overlap and grad.rates went though when
# they were read in.
#
# The reason we want to do this is that leading 0s in the district id from the
# other files were stripped out. In addition this also removes the "dummy"
# LEAID introduced by the Census Buearu.
f33.data$LEAID <- f33.data$LEAID %>% as.character() %>% as.integer() %>% as.character()

##### Merge Data Sets ######

max.tract.overlap[, "GIDTR"] <- as.character(max.tract.overlap$GIDTR)
max.tract.overlap[, "Leaid"] <- as.character(max.tract.overlap$Leaid)

merged.data <- census.data %>%
  inner_join(select(max.tract.overlap, one_of(c("GIDTR", "Leaid", "Percentage"))), by = c("gidtr" = "GIDTR")) %>%
  inner_join(clean.grad.data, by = c("Leaid" = "district.id")) %>%
  inner_join(f33.data, by = c("Leaid" = "LEAID")) %>%
  rename(district.id = Leaid, percentage = Percentage)

census.key.vars <- c("gidtr", "land.area", "tot.population.acs", "pct.pop.5.17.acs",
                     "pct.prs.blw.pov.lev.acs", "pct.civ.unemp.16p.acs",
                     "pct.civ.unemp.16.24.acs", "pct.born.us.acs", "pct.born.foreign.acs",
                     "pct.hhd.ppl.und.18.acs", "med.hhd.inc.acs",
                     "avg.tot.prns.in.hhd.acs", "pct.not.hs.grad.acs", "pct.college.acs",
                     "med.house.value.acs", "state.name", "percentage")
grad.rates.key.vars <- c("district.id", "all.cohort", "all.rate", "ecd.cohort",
                         "ecd.rate", "mbl.cohort", "mbl.rate", "mhi.cohort",
                         "mhi.rate")

financial.key.vars <- c("TOTALREV", "TFEDREV", "TSTREV", "TLOCREV",
                        "TOTALEXP", "TCURELSC", "TCURINST", "TCURSSVC",
                        "TCUROTH", "TNONELSE", "TCAPOUT", "L12", "C14")

districts <- merged.data %>%
  select(one_of(c(census.key.vars, grad.rates.key.vars,
                  financial.key.vars))) %>%
  rename(total.revenue = TOTALREV, revenue.fed = TFEDREV, revenue.state = TSTREV,
         revenue.local = TLOCREV, total.expenditures = TOTALEXP,
         expend.elem.sec = TCURELSC, expend.instruction = TCURINST,
         expend.support = TCURSSVC, expend.other = TCUROTH,
         expend.non.elem.sec = TNONELSE, expend.capital = TCAPOUT,
         transfer.equity = L12, t1.rev.fed = C14)

districts$pct.college.acs <- districts$pct.college.acs * 100
districts$revenue.per.student <- districts$total.revenue - districts$transfer.equity %>%
  "/"(districts$all.cohort)

districts$pct.hs.grad.acs <- 1 - districts$pct.not.hs.grad.acs

other.cohorts <- c("ecd.cohort", "mbl.cohort", "mhi.cohort")
districts[, other.cohorts] <- districts[, other.cohorts] %>%
  as.percentage(districts$all.cohort)

other.revenue <- c("revenue.fed", "revenue.state", "revenue.local")
districts[, other.revenue] <- districts[, other.revenue] %>%
  as.percentage(districts$total.revenue)

other.expenditures <- c("expend.elem.sec", "expend.instruction", "expend.support", "expend.other",
                        "expend.non.elem.sec", "expend.capital", "transfer.equity")
districts[, other.expenditures] <- districts[, other.expenditures] %>%
  as.percentage(districts$total.expenditures)

small.districts <- districts[districts$all.cohort < 61, ]
districts <- districts[districts$all.cohort > 60, ]

rm(census.data, max.tract.overlap, clean.grad.data, f33.data)


# End of Initialization ---------------------------------------------------
