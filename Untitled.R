# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 366 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 256 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 54 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 56 observations

# The following variable selections have been noted.

crs$input <- c("MinTemp", "MaxTemp", "Rainfall", "Evaporation",
               "Sunshine", "WindGustDir", "WindGustSpeed", "WindDir9am",
               "WindDir3pm", "WindSpeed9am", "WindSpeed3pm", "Humidity9am",
               "Humidity3pm", "Pressure9am", "Pressure3pm", "Cloud9am",
               "Cloud3pm", "Temp9am", "Temp3pm", "RainToday")

crs$numeric <- c("MinTemp", "MaxTemp", "Rainfall", "Evaporation",
                 "Sunshine", "WindGustSpeed", "WindSpeed9am", "WindSpeed3pm",
                 "Humidity9am", "Humidity3pm", "Pressure9am", "Pressure3pm",
                 "Cloud9am", "Cloud3pm", "Temp9am", "Temp3pm")

crs$categoric <- c("WindGustDir", "WindDir9am", "WindDir3pm", "RainToday")

crs$target  <- "RainTomorrow"
crs$risk    <- "RISK_MM"
crs$ident   <- "Date"
crs$ignore  <- "Location"
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2015-08-10 17:24:47 x86_64-apple-darwin10.8.0 

# Decision Tree 

# The 'rpart' package provides the 'rpart' function.

require(rpart, quietly=TRUE)

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# Build the Decision Tree model.

crs$rpart <- rpart(RainTomorrow ~ .,
                   data=crs$dataset[crs$train, c(crs$input, crs$target)],
                   method="class",
                   parms=list(split="information"),
                   control=rpart.control(usesurrogate=0, 
                                         maxsurrogate=0))

# Generate a textual view of the Decision Tree model.

print(crs$rpart)
printcp(crs$rpart)
cat("\n")

# Time taken: 0.02 secs

#============================================================
# Rattle timestamp: 2015-08-10 17:25:02 x86_64-apple-darwin10.8.0 




pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  tbl <- cbind(round(x/length(actual), 2),
               Error=round(c(x[1,2]/sum(x[1,]),
                             x[2,1]/sum(x[2,])), 2))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
};

overall <- function(x)
{
  if (nrow(x) == 2) 
    cat((x[1,2] + x[2,1]) / sum(x)) 
  else
    cat(1 - (x[1,rownames(x)]) / sum(x))
}



pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  tbl <- cbind(round(x/length(actual), 2),
               Error=round(c(x[1,2]/sum(x[1,]),
                             x[2,1]/sum(x[2,])), 2))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
};

