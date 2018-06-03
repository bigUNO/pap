# Placement Analysis Project
#

# Dependencies
# Uncomment the below lines to install deps.
#install.packages("urltools", repos="http://cran.us.r-project.org")
#install.packages('dplyr', repos="http://cran.us.r-project.org")
library(urltools)
library(dplyr)

# Library to calculate performance metrics
source("../calculate_performance_metrics.R")

input.file <- "data/placement_keywords.csv"

# Load data ---------------------------
# Read file into variable
input.data <- read.csv(input.file)

# Creating data sets ---------------------------
# Add input.data to R search path
attach(input.data)

# Assign the second column (URL) to url.vector
url.vector <- input.data[,2]

# Parse the subdomain and second-level domain out of the FQDN
parsed.domain <- url_parse(url.vector)

# Compactly display the resulting `parsed.domain` variable containing the
# second-level domain. This line can be commented out to save processing time
# and condense output.
#str(parsed.domain)

# Bind `domain` column from `parsed.domain` to create new data set.
merged.data <- (bind_cols(input.data, parsed.domain["domain"]))

# Remove `input.data` from R search path.
detach(input.data)

# Add merged.data to R search path
attach(merged.data)

# Create an aggregate data set with the sum of `domain`s from merged.data by
# domain
sum.aggdata <- aggregate(. ~ domain, merged.data, sum)

# Remove `merged.data` from R search path.
detach(merged.data)

# Create a data table from `sum.aggdata`.
aggdata.dataframe <- data.frame(
   sum.aggdata$domain,
   sum.aggdata$Clicks,
   sum.aggdata$Impr.,
   sum.aggdata$Cost)

# Running calculations ---------------------------
# Add aggdata.dataframe to R search path
attach(aggdata.dataframe)

# Calculate CTR and add to `aggdata.dataframe` data frame
aggdata.dataframe$ctr <- CalculatePerformanceMetrics(
   aggdata.dataframe$sum.aggdata.Clicks,
   aggdata.dataframe$sum.aggdata.Impr.,
   "ctr")

# Calculate CPC and add to `aggdata.dataframe` data frame
aggdata.dataframe$cpc <- CalculatePerformanceMetrics(
   aggdata.dataframe$sum.aggdata.Cost,
   aggdata.dataframe$sum.aggdata.Clicks,
   "cpc")

# PFM to replace NaNs in `cpc` with zero
aggdata.dataframe$cpc[ is.nan(aggdata.dataframe$cpc) ] = 0
aggdata.dataframe$ctr[ is.na(aggdata.dataframe$ctr) ] = 0
str(aggdata.dataframe)
# Trying to run a t test for every row of my data frame.
# Want to identify the p value of each domain to understand which CTRs are
# statistically significant based on the number of impressions.

# Creating and naming my linear model to my data set. Trying to predict clicks
# by doing a linear regression based on impressions
summary(aggdata.dataframe$ctr)
summary(aggdata.dataframe$cpc)

t.test(aggdata.dataframe$ctr)
t.test(sum.aggdata.Impr. ~ ctr, data = aggdata.dataframe)
#t.test(sum.aggdata.Impr., ctr)
