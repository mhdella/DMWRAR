# Data Mining with Rattle and R: The Art of Excavating Data for Knowledge Discovery
# by Graham Williams

# Chapter 2: Getting Started

library(rattle)
rattle()

# The logical variable 'building' is used to toggle between generating transformations, 
# as when building a model, and simply using the transformations, as when scoring a dataset.
building <- TRUE
scoring  <- !building

# The colorspace package is used to generate the colours used in plots, if available.
library(colorspace)

# A pre-defined value is used to reset the random seed so that results are repeatable.
crv$seed <- 42 

# Load the data.
crs$dataset <- read.csv(system.file("csv", "weather.csv", package="rattle"), encoding="UTF-8")

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

# Decision Tree 
# The 'rpart' package provides the 'rpart' function.
require(rpart, quietly = TRUE)

# Reset the random number seed to obtain the same results each time.
set.seed(crv$seed)

# Build the Decision Tree model.
crs$rpart <- rpart(RainTomorrow ~ .,
                   data = crs$dataset[crs$train, c(crs$input, crs$target)],
                   method = "class",
                   parms = list(split = "information"),
                   control = rpart.control(usesurrogate = 0, maxsurrogate = 0))

# Generate a textual view of the Decision Tree model.
print(crs$rpart)
printcp(crs$rpart)

# Plot the resulting Decision Tree. 
# We use the rpart.plot package.
fancyRpartPlot(crs$rpart, main = "Decision Tree weather.csv $ RainTomorrow")

# List the rules from the tree using a Rattle support function.
asRules(crs$rpart)

# The 'gplots' package provides the 'barplot2' function.
require(gplots, quietly = TRUE)

# Bar Plot 
# Generate the summary data for plotting.
ds <- rbind(summary(na.omit(crs$dataset[crs$sample, ]$RainTomorrow)))
# Sort the entries.
ord <- order(ds[1, ], decreasing = TRUE)
# Plot the data.
bp <-  barplot2(ds[ , ord], beside = TRUE, ylab = "Frequency", xlab = "RainTomorrow", 
                ylim = c(0, 258), col = colorspace::rainbow_hcl(1))
# Add the actual frequencies.
text(bp, ds[ , ord] + 9, ds[ , ord])
# Add a title to the plot.
title(main = "Distribution of RainTomorrow (sample)",
      sub = paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

# Box Plot 
# The 'ggplot2' package provides the 'ggplot' function.
library(ggplot2)

# Box Plot for MinTemp
ggplot(with(crs, dataset[sample,]), aes(y = MinTemp, fill = RainTomorrow)) +
  geom_boxplot(aes(x = RainTomorrow), notch = TRUE) +
  stat_summary(aes(x = RainTomorrow), fun.y = mean, geom = "point", shape = 8) +
  ggtitle("Distribution of MinTemp (sample)") +
  theme(legend.position = "none")

# Box Plot for Sunshine
ggplot(with(crs, dataset[sample,]), aes(y = Sunshine, fill = RainTomorrow)) +
  geom_boxplot(aes(x = RainTomorrow), notch = TRUE) +
  stat_summary(aes(x = RainTomorrow), fun.y = mean, geom = "point", shape = 8) +
  ggtitle("Distribution of Sunshine (sample)") +
  theme(legend.position = "none")

# Histogram Plots 

# The 'dplyr' package provides the 'select' function.
library(dplyr)

# Histogram Plot for MinTemp
ar <- range(with(crs, select(dataset[sample,], MinTemp)))
bw <- (ar[2]-ar[1])/nclass.FD(with(crs, dataset[sample,]$MinTemp))
p  <- ggplot(with(crs, select(dataset[sample,], MinTemp, NULL)), aes(x=MinTemp)) +
  geom_histogram(aes(y=..density..), binwidth=bw, fill="grey", colour="black") +
  geom_density() +
  xlab("MinTemp\n\nRattle 2015-Aug-10 17:20:24 rebeccastevens") +
  ggtitle("Distribution of MinTemp (sample)") +
  labs(colour="", y="Density")
print(p)

# Histogram Plot for MinTemp
ar <- range(with(crs, select(dataset[sample, ], MinTemp)))
bw <- (ar[2] - ar[1])/nclass.FD(with(crs, dataset[sample,]$MinTemp))
ggplot(with(crs, dataset[sample, ]), aes(x = MinTemp, color = RainTomorrow)) +
  geom_histogram(aes(y=..density..), binwidth = bw, fill = "grey", colour = "black") +
  geom_density() +
  ggtitle("Distribution of MinTemp (sample)\nby RainTomorrow")

ar <- range(with(crs, select(dataset[sample, ], Sunshine)), na.rm = TRUE)
nclass.all <- function(x, fun = median) {
  fun(c(
    nclass.Sturges(x), 
    nclass.scott(x),
    nclass.FD(x)
  ))
}
bw <- (ar[2] - ar[1])/nclass.all(with(crs, !is.na(dataset[sample, ]$Sunshine)))
ggplot(with(crs, dataset[sample,]), aes(x = Sunshine, color = RainTomorrow)) +
  geom_histogram(aes(y=..density..), binwidth = bw, fill = "grey", colour = "black") +
  geom_density() +
  ggtitle("Distribution of Sunshine (sample)\nby RainTomorrow")

# Bar Plot 
# Generate the summary data for plotting.
ds <- table(crs$dataset[crs$sample, ]$RainTomorrow, crs$dataset[crs$sample, ]$WindDir9am)
# Sort the entries.
ord <- order(ds[1, ], decreasing = TRUE)
# Plot the data.
bp <-  barplot2(ds[, ord], 
                beside = TRUE, 
                ylab = "Frequency", 
                xlab = "WindDir9am", 
                ylim = c(0, 40), 
                col = c("lightblue", "mistyrose"))
                #col = colorspace::rainbow_hcl(1))
# Add the actual frequencies.
text(bp, ds[,ord] + 1, ds[ ,ord])
# Add a title to the plot.
title(main = "Distribution of WindDir9am (sample)",
      sub = paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
legend("topright", bty="n", c("No", "Yes"), col = c("lightblue", "mistyrose"), pch = 15)

# Dot Plot 
# Generate the summary data for the plot.
ds <- table(crs$dataset[crs$sample, ]$RainTomorrow, crs$dataset[crs$sample, ]$WindDir9am)
# Sort the entries.
ord <- order(ds[1, ], decreasing = TRUE)
# Plot the data.
dotchart(ds[nrow(ds):1,ord], 
         main = "Distribution of WindDir9am (sample)\nby RainTomorrow", 
         sub = "Rattle 2015-Aug-10 17:22:54 rebeccastevens", 
         #col = rev(colorspace::rainbow_hcl(1)), 
         col = c("lightblue", "mistyrose"),
         labels = "", 
         xlab = "Frequency", 
         ylab = "WindDir9am", 
         pch = 15)
         #pch=c(1:0, 19))
# Add a legend.
legend("bottomright", bty = "n", c("Yes", "No"), col = c("lightblue", "mistyrose"), pch = 15)
 
# Mosaic Plot 
# Generate the table data for plotting.
ds <- table(crs$dataset[crs$sample,]$WindDir9am, crs$dataset[crs$sample,]$RainTomorrow)
# Sort the entries.
ord <- order(apply(ds, 1, sum), decreasing = TRUE)
# Plot the data.
mosaicplot(ds[ord,], 
           main = "Mosaic of WindDir9am (sample)\nby RainTomorrow", 
           sub = "Rattle 2015-Aug-10 17:55:36 rebeccastevens", 
           color = colorspace::rainbow_hcl(3)[-1], 
           cex = 0.7, 
           xlab = "WindDir9am", 
           ylab = "RainTomorrow")

# Evaluate model performance. 
# Generate an Error Matrix for the Decision Tree model.

# Helper functions

# Generate the confusion matrix showing proportions.
pcme <- function(actual, cl) {
  x <- table(actual, cl)
  tbl <- cbind(round(x/length(actual), 2),
               Error = round(c(x[1, 2]/sum(x[1, ]), x[2, 1]/sum(x[2, ])), 2))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}

# Calculate the overall error percentage.
overall <- function(x) {
  if (nrow(x) == 2) 
    cat((x[1, 2] + x[2, 1])/sum(x)) 
  else
    cat(1 - (x[1, rownames(x)])/sum(x))
} 

# Calculate the averaged class error percentage.
avgerr <- function(x) 
  cat(mean(c(x[1, 2], x[2, 1])/apply(x, 1, sum))) 

# Evaluation Using the Training Dataset:
# Obtain the response from the Decision Tree model.
crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$sample, c(crs$input, crs$target)], type="class")
# Generate the confusion matrix showing counts.
table(crs$dataset[crs$sample, c(crs$input, crs$target)]$RainTomorrow, crs$pr,
      dnn=c("Actual", "Predicted"))
# Generate the confusion matrix showing proportions.
pcme(crs$dataset[crs$sample, c(crs$input, crs$target)]$RainTomorrow, crs$pr)
# Calculate the overall error percentage.
overall(table(crs$pr, crs$dataset[crs$sample, c(crs$input, crs$target)]$RainTomorrow,  
              dnn=c("Predicted", "Actual")))
# Calculate the averaged class error percentage.
avgerr(table(crs$pr, crs$dataset[crs$sample, c(crs$input, crs$target)]$RainTomorrow,  
             dnn=c("Predicted", "Actual")))

# Evaluation Using the Validation Dataset:
# Obtain the response from the Decision Tree model.
crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$validate, c(crs$input, crs$target)], type="class")
# Generate the confusion matrix showing counts.
table(crs$dataset[crs$validate, c(crs$input, crs$target)]$RainTomorrow, crs$pr,
      dnn=c("Actual", "Predicted"))
# Generate the confusion matrix showing proportions.
pcme(crs$dataset[crs$validate, c(crs$input, crs$target)]$RainTomorrow, crs$pr)
# Calculate the overall error percentage. 
overall(table(crs$pr, crs$dataset[crs$validate, c(crs$input, crs$target)]$RainTomorrow,  
              dnn=c("Predicted", "Actual")))
# Calculate the averaged class error percentage.
avgerr(table(crs$pr, crs$dataset[crs$validate, c(crs$input, crs$target)]$RainTomorrow,  
             dnn=c("Predicted", "Actual")))

# Evaluation Using the Testing Dataset:
# Obtain the response from the Decision Tree model.
crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$test, c(crs$input, crs$target)], type="class")
# Generate the confusion matrix showing counts.
table(crs$dataset[crs$test, c(crs$input, crs$target)]$RainTomorrow, crs$pr,
      dnn=c("Actual", "Predicted"))
# Generate the confusion matrix showing proportions.
pcme(crs$dataset[crs$test, c(crs$input, crs$target)]$RainTomorrow, crs$pr)
# Calculate the overall error percentage.
overall(table(crs$pr, crs$dataset[crs$test, c(crs$input, crs$target)]$RainTomorrow,  
              dnn=c("Predicted", "Actual")))
# Calculate the averaged class error percentage.
avgerr(table(crs$pr, crs$dataset[crs$test, c(crs$input, crs$target)]$RainTomorrow,  
             dnn=c("Predicted", "Actual")))

# Evaluation Using the Full Dataset:
# Obtain the response from the Decision Tree model.
crs$pr <- predict(crs$rpart, newdata=crs$dataset, type="class")
# Generate the confusion matrix showing counts.
table(crs$dataset$RainTomorrow, crs$pr, dnn=c("Actual", "Predicted"))
# Generate the confusion matrix showing proportions.
pcme(crs$dataset$RainTomorrow, crs$pr)
# Calculate the overall error percentage.
overall(table(crs$pr, crs$dataset$RainTomorrow, dnn=c("Predicted", "Actual")))
# Calculate the averaged class error percentage.
avgerr(table(crs$pr, crs$dataset$RainTomorrow, dnn=c("Predicted", "Actual")))

