# Data Mining with Rattle and R
# by Graham Williams

# Appendix B:  Sample Datasets

#install.packages("rattle")

library(rattle)

dim(weather)
dim(weatherAUS)
dim(audit)

# B.1 Weather

# B.1.1 Obtaining Data

Sys.Date()

today <- format(Sys.Date(), format="%Y%m")

today

bom <- paste("http://www.bom.gov.au/climate/dwo/", today,
             "/text/IDCJDW2801.", today, ".csv", sep="")

bom

dsw <- read.csv(bom, skip = 6, check.names = FALSE)

dim(dsw)
head(names(dsw))

# B.1.2 Data Preprocessing

ndsw <- dsw[-c(1, 10)]
names(ndsw) <- c("Date", "MinTemp", "MaxTemp", "Rainfall", "Evaporation", "Sunshine", "WindGustDir", "WindGustSpeed", 
                 "Temp9am", "Humidity9am", "Cloud9am", "WindDir9am", "WindSpeed9am", "Pressure9am", "Temp3pm", 
                 "Humidity3pm", "Cloud3pm", "WindDir3pm", "WindSpeed3pm", "Pressure3pm")

dim(ndsw)
names(ndsw)

# B.1.3 Data Cleaning

vars <- c("WindGustSpeed","WindSpeed9am","WindSpeed3pm")
head(ndsw[vars])

class(ndsw)

apply(ndsw[vars], MARGIN = 2, FUN = class)

ndsw$WindSpeed9am  <- as.character(ndsw$WindSpeed9am)
ndsw$WindSpeed3pm  <- as.character(ndsw$WindSpeed3pm)
ndsw$WindGustSpeed <- as.character(ndsw$WindGustSpeed)
head(ndsw[vars])

# B.1.4 Missing Values

ndsw <- within(ndsw,
{
  WindSpeed9am[WindSpeed9am   == ""] <- NA
  WindSpeed3pm[WindSpeed3pm   == ""] <- NA
  WindGustSpeed[WindGustSpeed == ""] <- NA
})

ndsw <- within(ndsw,
{
  WindSpeed9am[WindSpeed9am   == "Calm"] <- "0"
  WindSpeed3pm[WindSpeed3pm   == "Calm"] <- "0"
  WindGustSpeed[WindGustSpeed == "Calm"] <- "0"
})

ndsw <- within(ndsw,
{
  WindSpeed9am  <- as.numeric(WindSpeed9am)
  WindSpeed3pm  <- as.numeric(WindSpeed3pm)
  WindGustSpeed <- as.numeric(WindGustSpeed)
})
apply(ndsw[vars], 2, class)

vars <- c("WindSpeed9am","WindSpeed3pm","WindGustSpeed")
head(ndsw[vars])
apply(ndsw[vars], 2, class)
levels(ndsw$WindDir9am)

ndsw <- within(ndsw,
{
  WindDir9am[WindDir9am == " "] <- NA
  WindDir9am[is.na(WindSpeed9am) | (WindSpeed9am == 0)] <- NA
  WindDir3pm[WindDir3pm == " "] <- NA
  WindDir3pm[is.na(WindSpeed3pm) | (WindSpeed3pm == 0)] <- NA
  WindGustDir[WindGustDir == " "] <- NA
  WindGustDir[is.na(WindGustSpeed) | (WindGustSpeed == 0)] <- NA
})

# B.1.5 Data Transforms

ndsw$RainToday <- ifelse(ndsw$Rainfall > 1, "Yes", "No")
vars <- c("Rainfall", "RainToday")
head(ndsw[vars])

ndsw$RainTomorrow <- c(ndsw$RainToday[2:nrow(ndsw)], NA)
vars <- c("Rainfall", "RainToday", "RainTomorrow")
head(ndsw[vars])

# B.1.6 Using the Data

# B.2 Audit

# B.2.1 The Adult Survey Dataset

# B.2.2 From Survey to Audit

# B.2.3 Generating Targets

# B.2.4 Finalising the Data

# B.2.5 Using the Data

# B.3 Command Summary


