##                      Data Processing

## Initialize the environment
if (!file.exists("data")) {
        dir.create("data")
}

## Download the Data (Note: This assumes R for Windows)
URL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(URL, "./data/stormdata.bz2")
downloadDate <- date()

## Read the file into R
df <- read.csv(bzfile("./data/stormdata.bz2"))

##                      Initial Analysis

## Summarize the Data
summary(df)
str(df)
head(df)
numVariables <- dim(df)[2]
numObservations <- dim(df)[1]
numEvents <- length(unique(df$EVTYPE))

##              Data Transformation for Population Health

## Create a new data frame with only the relevant data for this analysis
## Fatalities
#df <- df[!grepl("Summary", df$EVTYPE), ]
#df <- subset(df, FATALITIES > 0 | INJURIES > 0 | PROPDMG > 0 | CROPDMG > 0)
df$EVTYPE <- tolower(df$EVTYPE)
sumFatalities <- as.data.frame(tapply(df$FATALITIES, df$EVTYPE, sum))
sumFatalities$EVTYPE <- rownames(sumFatalities)
colnames(sumFatalities) <- c("Injuries", "Type")
sumFatalities <- sumFatalities[, c(2, 1)]
rownames(sumFatalities) <- NULL

## Injuries
sumInjuries <- as.data.frame(tapply(df$INJURIES, df$EVTYPE, sum))
sumInjuries$EVTYPE <- rownames(sumInjuries)
colnames(sumInjuries) <- c("Fatalities", "Type")
sumInjuries <- sumInjuries[, c(2, 1)]
rownames(sumInjuries) <- NULL

## Merge the data to form the new data frame
PopHealthData <- merge(sumInjuries, sumFatalities)

## Add the Injuries and Fatalies totals per event type (IF NEEDED)
PopHealthData$Total <- PopHealthData$Injuries + PopHealthData$Fatalities


##              Data Transformation for Economic Impact

## Create a new data frame with only the relevant data for this analysis
## Property
## View EXP classifcations for Property Damage
PropEXP <- levels(df$PROPDMGEXP)

## Example of a meaningless EXP factor
df$PROPDMG[df$PROPDMGEXP == "8"]

## Create a vector of the relevent EXP factors
##EXP <- c(B = 1000000000, b = 1000000000, M = 1000000, m = 1000000,
##             K = 1000, k = 1000)

EXP <- c(B = as.integer(1000000000), b = as.integer(1000000000),
         M = as.integer(1000000), m = as.integer(1000000),
         K = as.integer(1000), k = as.integer(1000))

## Add a new column to the data frame applying EXP to Property Damage
## To create a data frame for analysis
df$PROPCOST <- df$PROPDMG * EXP[as.character(df$PROPDMGEXP)]
sumPropDMG <- as.data.frame(tapply(df$PROPCOST, df$EVTYPE, sum))
sumPropDMG$EVTYPE <- rownames(sumPropDMG)
sumPropDMG$EVTYPE <- tolower(sumPropDMG$EVTYPE)
colnames(sumPropDMG) <- c("Property", "Type")
sumPropDMG <- sumPropDMG[, c(2, 1)]
rownames(sumPropDMG) <- NULL
sumPropDMG$Property[is.na(sumPropDMG$Property)] <- 0

########################################################################
## This take TOO LONG tp create a whole new Data Frame, even thought the
## result is the same as sumInjuries, therefore add a new colum
## to the existing Data Frame
########################################################################
## Create a new data frame for the Poperty Damage Data
##sumPropDMG <- as.data.frame(df$EVTYPE)
## Apply the EXP factors to the Property Damage Cost
##sumPropDMG$Property <- df$PROPDMG * EXP[as.character(df$PROPDMGEXP)]
##colnames(sumPropDMG) <- c("EVTYPE", "Property")
##sumPropDMG <- as.data.frame(tapply(sumPropDMG$Property, sumPropDMG$EVTYP,
##                                   sum))

## Crops
## Apply the same methologoy for the Crop Damage Data
df$CROPCOST <- df$CROPDMG * EXP[as.character(df$CROPDMGEXP)]
sumCropDMG <- as.data.frame(tapply(df$CROPCOST, df$EVTYPE, sum))
sumCropDMG$EVTYPE <- rownames(sumCropDMG)
sumCropDMG$EVTYPE <- tolower(sumCropDMG$EVTYPE)
colnames(sumCropDMG) <- c("Crops", "Type")
sumCropDMG <- sumCropDMG[, c(2, 1)]
rownames(sumCropDMG) <- NULL
sumCropDMG$Crops[is.na(sumCropDMG$Crops)] <- 0

## Merge the data to form the new data frame
EconomicData <- merge(sumPropDMG, sumCropDMG)
EconomicData$Total <- EconomicData$Property + EconomicData$Crops
