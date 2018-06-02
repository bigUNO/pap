# Placement Analysis Project

# Dependencies
# Uncomment the below lines to install deps.
#install.packages("urltools", repos="http://cran.us.r-project.org")
#install.packages('dplyr', repos="http://cran.us.r-project.org")
library(urltools)
library(dplyr)

input.file <- "placement_keywords.csv"

# Load data ---------------------------
# Read file into variable
input.data <- read.csv(input.file)

# attaching the primary data set for more analysis
# TODO: replace attach
attach(input.data)

# Vectoring ---------------------------
# Assign the second column (URL) to url.vector
url.vector <- input.data[,2]

# URL cleansing ---------------------------
# Parse the subdomain and second-level domain out of the FQDN
parsed.domain <- url_parse(url.vector)

# Compactly display the resulting `parsed.domain` variable containing the
# second-level domain.
str(parsed.domain)

# Creating a new data set that contains the original data set, and added in
# the domain column from the parsed.domain
# data set. Using the dplyr package
newdata <- (bind_cols(input.data, parsed.domain['domain']))
attach(newdata)

# Creating an aggregated data frame. Suming all columns in the "newdata" data
# set
Aggregated <- (aggregate(. ~ domain, newdata, sum))



# Creating a clean data table with only the columns needed. In this case,
# clicks, impressions and cost
clean_frame <- data.frame(Aggregated$domain, Aggregated$Clicks, Aggregated$Impr., Aggregated$Cost)
attach(clean_frame)

# Trying to calculate CTR and CPC and add it to my "clean_frame" data set
clean_frame$ctr = (clean_frame$Aggregated.Clicks/clean_frame$Aggregated.Impr.)*100
clean_frame$cpc = (clean_frame$Aggregated.Cost/clean_frame$Aggregated.Clicks)


  ## This is where i'm getting stuck. Trying to run a t test for every row of
  # my data frame.

 # Want to identify the p value of each domain to understand which CTRs are statistically significant

  ## based on the number of impressions.

# Trying to replace all NaN values with a 0 to help complete the below linear
# regression model
mutate_all(.funs = funs(ifelse(is.na(.),0,.)))

# Creating and naming my linear model to my data set. Trying to predict clicks
# by doing a linear regression based on impressions
t.test(Aggregated.Impr. ~ ctr, data = clean_frame)
