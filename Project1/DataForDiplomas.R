# Usage Rscript path/to/DataForDiplomas.R Census_Data_2010.csv Graduation_Rates.csv Max_Tract_Overlap.csv

# Will Spurgin
# Copyright September 12th, 2015
# Introduction to Data Mining, GradNation Project 1

require("dplyr")
require("ggplot2")
require("corrplot")
require("ggmap")
require("maptools")
require("gpclib")
require("sp")

###### Load Data Sets ######
args<-commandArgs(TRUE)

if (length(args) == 3) {
  census.data.csv = arg[1]
  grad.rates.csv = arg[2]
  max.tract.overlap.csv = arg[3]
} else {
  warning("Unkown number of arguments, going with defaults")
  census.data.csv = "Census_Data_2010.csv"
  grad.rates.csv = "Graduation_Rates.csv"
  max.tract.overlap.csv = "Max_Tract_Overlap.csv"
}

c(census.data.csv, grad.rates.csv, max.tract.overlap.csv) %>%  paste("Reading data from:", .) %>% cat(sep = '\n')
census.data.orig <- read.csv(census.data.csv)
grad.rates <- read.csv(grad.rates.csv)
max.tract.overlap <- read.csv(max.tract.overlap.csv)

rm(max.tract.overlap.csv, grad.rates.csv, census.data.csv)

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
rm(population.tract.data, housing.tract.data, misformatted, args, missing.gidtr)

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


###### Basic Data Analysis #######
# Get summary statistics
summary.names <- c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.", "NA's")
summary.vars <- c("land.area", "tot.population.acs", "pop.5.17.acs",
                  "college.acs", "prs.blw.pov.lev.acs", "civ.unemp.16plus.acs",
                  "civ.unemp.16.24.acs", "born.us.acs", "born.foreign.acs",
                  "hhd.ppl.und.18.acs", "med.hhd.inc.acs", 
                  "avg.tot.prns.in.hhd.acs")

summaries <- lapply(summary.vars, FUN = function(var) { as.numeric(summary(census.data[,var])) })
summaries <- vllist.to.data.frame(summaries)
colnames(summaries) <- summary.names
rownames(summaries) <- summary.vars

write.csv(summaries, "census_var_summaries.csv")

rm(summaries, summary.names, summary.vars)

smoothed <- density(census.data[!is.na(census.data$tot.population.acs), "tot.population.acs"], bw = "nrd")
hist(census.data$tot.population.acs, breaks = 40, freq = FALSE, xlab = "Total Population", main = "Total Population with Smoothing")
rug(census.data$tot.population.acs)
lines(smoothed, col="red", lwd = 2)

rm(smoothed)

children.living.situation <- data.frame(tract = census.data$gidtr,
                                  state = census.data$state.name,
                                  county = census.data$county.name,
                                  children.per.hhd.18.und =
                                    (census.data$pop.under.5.acs + census.data$pop.5.17.acs) / census.data$hhd.ppl.und.18.acs)

hist(children.living.situation$children.per.hhd.18.und, freq = FALSE, xlab = "Number Students", main = "Students per Household")
rug(children.living.situation$children.per.hhd.18.und)

# TODO more analysis on children living situation
rm(children.living.situation)

# Graduation Rate variable analysis

# Get graduation rate summaries
summary.names <- c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.", "NA's")
summary.vars <- c("all.cohort", "all.rate", "ecd.cohort", "ecd.rate")

summaries <- lapply(summary.vars, FUN = function(var) { as.numeric(summary(clean.grad.data[,var])) })
summaries <- vllist.to.data.frame(summaries)
colnames(summaries) <- summary.names
rownames(summaries) <- summary.vars

write.csv(summaries, "grad_var_summaries.csv")

rm(summaries, summary.names, summary.vars)

total.student.population <- sum(clean.grad.data$all.cohort)

smoothed <- density(clean.grad.data[!is.na(clean.grad.data$all.rate), "all.rate"], from = 0, to = 100, bw = "nrd")
hist(clean.grad.data$all.rate, breaks = 40, freq = FALSE, xlab = "Overall Gradutaion Rate", main = "Graduation Rate with Smoothing")
rug(clean.grad.data$all.rate)
lines(smoothed, col="red", lwd = 2)

# Examining by state, using 10% trimmed mean to preserve data integrity but protect from outliers
rates.by.state <- aggregate(all.rate ~ state, data = clean.grad.data, FUN = mean, trim = 0.1)
rates.by.state <- rates.by.state[order(rates.by.state$all.rate),]

op <- par(mar = c(5.1, 9.1, 4.1, 2.1))
barplot(rates.by.state$all.rate, names.arg = rates.by.state$state,
        horiz=T, las = 2, cex.names = .7,
        xlab = "Graduation rate [%]", xlim = c(0, 100),
        main = "Reported Graduation Rates by State")

abline(v = median(rates.by.state$all.rate), col = "red", lty=3)
mtext(paste("Number of States at 80% or more:", sum(rates.by.state$all.rate >= 80)), side = 1, line=3, at=3)

# reset back to previous settings
par(op)

# Be nice and clean up memory
rm(smoothed, rates.by.state, op)

# Students by State
students.by.state <- aggregate(all.cohort ~ state, data = clean.grad.data, FUN = median)

# remove the larger median cohort size states so the graphic is more meaninful
students.by.state <- students.by.state[students.by.state$all.cohort <= 400,]
students.by.state <- students.by.state[order(students.by.state$all.cohort),]

op <- par(mar = c(5.1, 9.1, 4.1, 2.1))
barplot(students.by.state$all.cohort, names.arg = students.by.state$state,
        horiz=T, las = 2, cex.names = .7,
        xlab = "All Student Cohort Size", xlim = c(0, 400),
        main = "Meidan All Student Cohort Size by State*")

abline(v = 60, col = "red", lty=3)
mtext(paste("Number of States at 60 or more students:", sum(students.by.state$all.cohort > 60)), side = 1, line=3, at=3)

# reset back to previous settings
par(op)

# All cohort size density
target <- target[students.by.state$all.cohort <= 400, "all.cohort"]
smoothed <- density(target, bw = "nrd")
hist(target, freq = FALSE, xlab = "Number of Students", main = "State Median All Student Cohort Size with Smoothing")
rug(target)
lines(smoothed, col="red", lwd = 2)
mtext(paste("Number of States excluded (>= 400):", sum(students.by.state$all.cohort > 400)), side = 1, line=3, at=12)

rm(op, students.by.state, smoothed)

# Max tract overlap
hist(max.tract.overlap$Percentage, freq = FALSE, xlab = "Percentage", main = "Overlap Percentages Density")
rug(max.tract.overlap$Percentage)

###### Visualizations and Relationships ######

## Cohort Size adversum Rate
rate.by.size <- aggregate(all.rate ~ all.cohort, data = clean.grad.data, FUN = median)
rate.by.size <- rate.by.size[order(rate.by.size$all.cohort),]

ggplot(rate.by.size, aes(x=all.cohort, y=all.rate, color = all.cohort>6000)) +
  geom_point(shape=1) +
  geom_smooth(method=lm)

# Examing cohort size 0-100
ggplot(rate.by.size[rate.by.size$all.cohort<100,], aes(x=all.cohort, y=all.rate)) +
  geom_point(shape=1) +
  geom_smooth() +
  ggtitle("All Student Cohort Rates by Size the \"Blurring\" Effect ") +
  xlab("All Student Cohort Size") +
  ylab("All Student Cohort Rate")

# Examing cohort size 0-1000
ggplot(rate.by.size[rate.by.size$all.cohort<1000,], aes(x=all.cohort, y=all.rate, color = all.cohort > 500)) +
  geom_point(shape=1) +
  geom_smooth() +
  ggtitle("All Student Cohort Rates by Size") +
  xlab("All Student Cohort Size") +
  ylab("All Student Cohort Rate") +
  scale_color_discrete(name="Size",
                      breaks=c("FALSE", "TRUE"),
                      labels=c("Less Than 500", "Greater Than 500"))

rm(rate.by.size)

## Merge tracts and school districts
census.key.vars <- c("gidtr", "land.area", "tot.population.acs", "pop.5.17.acs",
                  "college.acs", "prs.blw.pov.lev.acs", "civ.unemp.16plus.acs",
                  "civ.unemp.16.24.acs", "born.us.acs", "born.foreign.acs",
                  "hhd.ppl.und.18.acs", "med.hhd.inc.acs", 
                  "avg.tot.prns.in.hhd.acs")
grad.rates.key.vars <- c("district.id", "all.cohort", "all.rate", "ecd.cohort", "ecd.rate", "mbl.cohort", "mbl.rate", "mhi.cohort", "mhi.rate")
max.tract.overlap[, "GIDTR"] <- as.character(max.tract.overlap$GIDTR)
max.tract.overlap[, "Leaid"] <- as.character(max.tract.overlap$Leaid)

districts <- select(census.data, one_of(census.key.vars)) %>%
  inner_join(select(max.tract.overlap, one_of(c("GIDTR", "Leaid", "Percentage"))), by = c("gidtr" = "GIDTR")) %>%
  inner_join(select(clean.grad.data, one_of(grad.rates.key.vars)), by = c("Leaid" = "district.id")) %>%
  rename(district.id = Leaid, percentage = Percentage)

# Apply overlap percentage across the lot
affected.vars <- c(2:13, 16:25)
districts[, affected.vars] <- districts[, affected.vars] %>% "*"(districts$percentage) / 100

# 13 is purposefully left out!
districts[, affected.vars[-12]] <- lapply(districts[, affected.vars[-12]], FUN = as.integer)
districts$avg.tot.prns.in.hhd.acs <- districts$avg.tot.prns.in.hhd.acs %>% round(6)

districts$pct.college <- districts$college.acs / districts$tot.population.acs * 100
districts$pct.poverty <- districts$prs.blw.pov.lev.acs / districts$tot.population.acs * 100

# Observe trend between people with college degrees in the community and graduation rate
rates.by.educated.populace <- data.frame(rate = districts$all.rate, pct.college = districts$college.acs / districts$tot.population.acs * 100)

# Straight trend of number of college level educated people vs grad rate
ggplot(districts, aes(y=all.rate, x=college.acs)) +
  geom_point(shape=1, alpha = 0.5, color = "#6BDFD0") +
  geom_smooth(color = "#FF7259") +
  ggtitle("All Student Cohort Rates by Number of People in the Community with College Educations") +
  xlab("Persons 25+ in age with College Degrees") +
  ylab("All Student Cohort Rate")

# Relationship of pecentage of population with college-level education vs grad rate
ggplot(rates.by.educated.populace, aes(x=pct.college, y=rate, color = pct.college > 15)) +
  geom_point(shape=1) +
  geom_smooth() +
  ggtitle("All Student Cohort Rate by Percentage of Community with College Educations") +
  xlab("Percent [%] of Community with College Educations")+
  scale_color_manual(values=c("#FF7259", "#68DFD0"), 
                  name="Percent",
                  breaks=c("FALSE", "TRUE"),
                  labels=c("Less Than or Equal to 15%", "Greater Than 15%"))

# Relationship between percentage of community below poverty level and All Student Cohort Rate
rates.by.poor.populace <- data.frame(rate = districts$all.rate, pct.poverty = districts$prs.blw.pov.lev.acs / districts$tot.population.acs * 100)
ggplot(rates.by.poor.populace, aes(x=pct.poverty, y=rate)) +
  geom_point(shape = 1, alpha = 0.5, color = "#FF9999") +
  geom_smooth(color = "#3498DB") +
  ggtitle("All Student Cohort Rates by Percent of Community Below Poverty Level") +
  xlab("Percent [%] Below Poverty Level") +
  ylab("All Student Cohort Rate")


rm(rates.by.educated.populace, rates.by.poor.populace)

##### Correlation Matrices #######
df <- districts[, -c(1, 14:15, 24:25)]
df[, c(3:10)] <- df[, c(3:10)] / districts$tot.population.acs * 100 # convert everything to percentages
df[, c(15, 17, 19)] <- lapply(df[, c(15, 17, 19)], FUN = function(c) { c / districts$all.cohort * 100 }) # convert other cohort sizes to percentages

ggplot(df, aes(x = avg.tot.prns.in.hhd.acs, y=all.rate)) +
  geom_point(shape = 1, alpha = 0.5, color = "#FF9999") +
  geom_smooth(color = "#3498DB") +
  ggtitle("All Student Cohort Rates by Avg. Persons per Household") +
  xlab("Avg. Persons per Household") +
  ylab("All Student Cohort Rate")

colnames(df) <- c("Land Area", "Population", "Pop. btw 5-17",
                  "College Educated", "Below Poverty Level", "Unemployment",
                  "Young-Age Unemployment", "Born in US", "Born Foregin",
                  "Minors", "Income", "Persons per Household", "All Student Cohort",
                  "All Stud. Grad. Rate", "Econ. Disadv.", "Ecd Grad. Rate",
                  "African Amer. Cohort", "African Amer. Grad. Rate",
                  "Hispanic Cohort", "Hispanic Grad. Rate")
# Capture device op from par
op <- par()
M <- cor(df, use="pairwise.complete.obs")
corrplot(M, method = "circle", tl.cex = .7, tl.srt = 45)

# Removes the minority variables
M <- df[, -c(17:20)] %>% cor(use="pairwise.complete.obs")
corrplot(M, method = "circle", tl.cex = 1, tl.srt = 45)

# reset to defaults since the correlation matrix messes them up
par(op)

###### Spatial Data ######
census.key.vars <- c("gidtr", "tract", "state.name", "tot.population.acs")
grad.rates.key.vars <- c("district.id", "all.cohort", "all.rate")
max.tract.overlap[, "GIDTR"] <- as.character(max.tract.overlap$GIDTR)
max.tract.overlap[, "Leaid"] <- as.character(max.tract.overlap$Leaid)

districts <- select(census.data, one_of(census.key.vars)) %>%
  inner_join(select(max.tract.overlap, one_of(c("GIDTR", "Leaid"))), by = c("gidtr" = "GIDTR")) %>%
  inner_join(select(clean.grad.data, one_of(grad.rates.key.vars)), by = c("Leaid" = "district.id")) %>%
  rename(district.id = Leaid)

gpclibPermit()

# read data into R
shapefile <- readShapeSpatial("tl_2010_48_tract10", proj4string = CRS("+proj=longlat +datum=WGS84"))
texas.tracts <- shapefile
texas.tracts@data$id <- row.names(texas.tracts@data)
ttracts.df <- fortify(texas.tracts)
ttracts.df <- ttracts.df %>% inner_join(texas.tracts@data, by="id")
texas.districts <- ttracts.df %>%
  inner_join(districts, by = c("GEOID10" = "gidtr"))

# convert to a data.frame for use with ggplot2/ggmap and plot
qmap("texas", zoom = 6, maptype = "roadmap") +
  geom_polygon(aes(x = long, y = lat, group = group), data = ttracts.df,
              alpha = .4, size = .3) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = all.rate >= 90), data = texas.districts,
                                    alpha = .4, size = .3) +
  scale_fill_discrete(name="Graduation Rate",
                       breaks=c("FALSE", "TRUE"),
                       labels=c("Under 90%", "Greater Than or Equal To 90%"))

rm(shapefile, texas.tracts, ttracts.df, texas.districts, districts)
###### THE END ######
rm(calculated.percentage.variables, census.key.vars, grad.rates.key.vars,
   housing.valuation.variables, housing.unit.variables, housing.variable.numbers,
   ignored.attributes, no.census.data.tracts, population.variable.numbers,
   max.tract.overlap, clean.grad.data, census.data, grad.rates, census.data.orig,
   income.as.numeric, rate.to.integer, vllist.to.data.frame)

