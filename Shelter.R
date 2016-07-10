### Libraries
library(dplyr)
library(ggplot2)
library(caret)
library(tidyr)
library(stringr)
library(randomForest)
library(ggthemes)

### Load and merge

test <- tbl_df(read.csv("/Users/antbae/OneDrive/R/Side_projects/Shelter/test.csv", stringsAsFactors = FALSE))
train <- tbl_df(read.csv("/Users/antbae/OneDrive/R/Side_projects/Shelter/train.csv", stringsAsFactors = FALSE))

combined <- bind_rows(test, train)

### Feature engineering

# Seperate dates and times
combined <-
combined %>%
  separate(DateTime, c("Date", "Time"), sep = " ")

# Create weekdays M-F
combined$Date <- as.Date(combined$Date)
combined$DayName <- weekdays(combined$Date)

# Separate year, month, and date
combined <- 
  combined %>%
  mutate(
    Year = substr(Date, 1, 4),
    Month = substr(Date, 6, 7),
    Day = substr(Date, 9, 10)
  ) %>%
  select(-Date)

# Create seasons Winter ~ Spring
combined <-
combined %>%
  mutate(
    Season = ifelse(Month %in% c(12, 1, 2), "Winter",
             ifelse(Month %in% c(3, 4, 5), "Spring",
             ifelse(Month %in% c(6, 7, 8), "Summer", "Fall")))
  )

# Create AM or PM time
combined <-
  combined %>%
  mutate(
    APM = ifelse(Time >= "00:00:00" & Time <= "11:59:00", "AM", "PM")
  )

str(combined)

# Seperate hours and minutes
combined <- 
combined %>%
  mutate(
    hour = substr(Time, 1, 2),
    minutes = substr(Time, 4, 5)
  )

# Create time of day 
combined <-
combined %>%
  mutate(
    TOD = ifelse(hour >= 5 & hour  <= 11, "Morning", 
          ifelse(hour >= 12 & hour <= 17, "Midday", 
          ifelse(hour >= 18 & hour <= 23, "Evening", "Early Morning")))
  )

# Separate SexuponOutcome Sex and Status (intact, sprayed, etc)
combined <-
combined %>%
  mutate(
    Sex = ifelse(grepl("Female", SexuponOutcome), "F", 
          ifelse(grepl("Male", SexuponOutcome), "M", "U"))
  ) 

combined <-
  combined %>%
  mutate(
    Status = ifelse(grepl("Intact", SexuponOutcome), "Intact", 
             ifelse(grepl("Spayed", SexuponOutcome), "Sprayed", 
             ifelse(grepl("Neutered", SexuponOutcome), "Neutered", "Unknown")))
  )

# New variable mixed breed and mixed color
combined <- combined %>%
  mutate(
    Mix = ifelse(grepl("Mix", Breed), 1,
          ifelse(grepl("/", Breed), 1, 0))
  )

combined <- combined %>%
  mutate(
    Colorful = ifelse(grepl("/", Color), 1, 0)
  )

# Separate age upon agevalue
combined$AgeValue <- word(combined$AgeuponOutcome, 1)

# Agevariable days/weeks/months/years
combined$AgeVariable <- word(combined$AgeuponOutcome, -1)

# Making all age variables the same into days
combined$AgeVariable <- str_replace(combined$AgeVariable, "s", "")
combined$AgeValue <- as.numeric(combined$AgeValue)

combined <-
combined %>%
  mutate(
    Ageindays = ifelse(AgeVariable == "day", AgeValue * 1, 
                ifelse(AgeVariable == "week", AgeValue * 7, 
                ifelse(AgeVariable == "month", AgeValue * 30, 
                ifelse(AgeVariable == "year", AgeValue * 365, NA))))
)

# Animals no longer baby after 365 days
combined <-
combined %>%
  mutate(
    Adult = ifelse(Ageindays > 365, 1, 0)
  )

# Animals no longer baby after 365 days
combined <-
  combined %>%
  mutate(
    LongStay = ifelse(Ageindays > 730, 1, 0)
  )


# Has name
combined <-
combined %>%
  mutate(
    HasName = ifelse(!is.na(Name), 1, 0)
  )
  

# Clean up data 
combined <-
combined %>%
  select(-Time, -AgeuponOutcome, -AgeValue, -AgeVariable, -minutes)

combined <-
combined %>%
  mutate_if(is.character, as.factor)

combined$Mix <- as.factor(combined$Mix)
combined$Colorful <- as.factor(combined$Colorful)
combined$Adult <- as.factor(combined$Adult)
combined$LongStay <- as.factor(combined$LongStay)

# Create testing data 
training <-
combined %>%
  filter(!is.na(OutcomeType))

# Remove NAs from Adult column (or fill in with prediction model)
training <- training %>%
  filter(!is.na(Adult))

### Visualizations 

# Animals and their outcomes
ggplot(training, aes(x = OutcomeType, fill = AnimalType)) +
  geom_bar(position = "fill") +
  theme_few() +
  scale_fill_few() +
  ggtitle("Animals type and outcomes") + 
  xlab("") +
  ylab("") +
  guides(fill = guide_legend(reverse=TRUE))

# Formula to do graphs
plotby <- function(feat){
  ggplot(training, aes(x = OutcomeType, fill = AnimalType)) +
    geom_bar(position = "fill") +
    facet_wrap(feat) +
    theme_few() +
    scale_fill_few() +
    ggtitle("Animals type and outcomes age") + 
    xlab("") +
    ylab("") +
    guides(fill = guide_legend(reverse=TRUE)) +
    theme(axis.text.x  = element_text(angle=90))
}

# Animals and outcomes based on if they are puppy or adult

plotby(~Adult)


# By OutcomeSubTypes
plotby(~OutcomeSubTypes)

# By year
plotby(~Year)

# By month
plotby(~Month)

# By others
plotby(~Mix)
plotby(~Sex)
plotby(~LongStay)

### Random forest

slice <- createDataPartition(training$OutcomeType, p = .9, list = FALSE)
training <- training[slice,]
testing <- training[-slice,]

#m1 12.64 error
set.seed(17)
m1 <- randomForest(OutcomeType ~ AnimalType + DayName + Year + Month +
                     TOD + Sex + Adult + OutcomeSubtype + Status + LongStay, importance=TRUE, data = training)
m1
varImpPlot(m1)

p1 <- predict(m1, newdata = testing)
table(p1, testing$OutcomeType)
mean(p1 == testing$OutcomeType) #88

#m2 17.03 error
set.seed(15)
m2 <- randomForest(OutcomeType ~ Adult + OutcomeSubtype + Status, importance=TRUE, data = training)
m2
varImpPlot(m2)

