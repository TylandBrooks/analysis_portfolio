install.packages('readr','ggplot2', 'tidyr', 'dplyr')
library(readr)
library(gglot2)
library(tidyr)
library(dplyr)

# READ CSV DATA
activity_data <- read.csv("C://Users//tylandb//Documents//coursara//Fitabase//dailyActivity_merged.csv")

# SUMMARY: CSV FILE OF FITBASE DATA SUCH AS LOGGED DISTANCE, MINUTES, DATE, AND USER ID.
# USING THIS DATA, I WILL ANALYZE THE TOTAL AND AVERAGE LOGGED STEPS BY USER ID AND 
# CATEGORIZE THEM INTO AVTIVITY LEVELS



# CHANGE THE DATA TYPE OF THE ID AND ACTIVITY DATE COLUMNS FROM NUMBERS TO 
# STRING AND DATE DATA TYPES
activity_data$Id <- as.character(activity_data$Id)
activity_data$ActivityDate <- as.Date(activity_data$ActivityDate, format = "%m/%d/%Y")
class(activity_data$ActivityDate)


# CREATE NEW COLUMNS CALLED WEEKDAY_NAME AND DAYS_OF_WEEK THAT SHOWS THE NAME OF THE DAY 
# AND THE NUMBER OF THE DAY THEN MUTATE THE ORIGNAL TABLE
weekday_column <- weekdays(activity_data$ActivityDate)
day_column <- format(activity_data$ActivityDate,"%d")

new_data <- activity_data %>%
  mutate(Day_Name = weekday_column, Day_Of_Week = day_column);


# REMOVE NULL VALUES. SHOULD COME BACK FALSE.
has_null_values <- any(is.na(new_data))
print(has_null_values)



# CHECK DUPLICATES BY ROW
duplicated_rows <- new_data[duplicated(new_data), ]
print(duplicated_rows)


# CHECKS FOR DUPLICATES BY SAYING TRUE OR FALSE
has_duplicates <- any(duplicated(new_data))
print(has_duplicates)

# TRIM DATA FRAME COLUMNS OF ANY WHITESPACE 
trimmed_data <- trimws(new_data)

# SUBSET THE DATA, REMOVE DISTANCE COLUMNS BECASUE THEY DON'T GIVE A MEASURMENT OF DISTANCE.
subset_data <- new_data[, c("Id", "ActivityDate", "TotalSteps", "TotalDistance", "VeryActiveMinutes",
                        "FairlyActiveMinutes", "LightlyActiveMinutes", 
                        "SedentaryMinutes", "Calories", "Day_Name", "Day_Of_Week")]

mean(subset_data$VeryActiveMinutes)




# ANALYSIS PHASE
# CATEGORIZE USER ID INTO BASED ON LOGGED STEPS: 
# VeryActive: More than 12868 on average
# Active: Between 4103.32 and 12868 on average 
# Sedentary: Less than 4103.32 on average

# GROUP DATA BY ID
id_group <- subset_data %>%
  group_by(Id)

# FIND THE MEAN OF TOTAL STEPS FOR EACH ID
avg_step_data <- id_group %>%
  summarise(AvgSteps = mean(TotalSteps)
            )
# SORT AVERAGE STEPS BY LARGEST TO SMALLEST
sorted_indices <- order(avg_step_data$AvgSteps, decreasing = TRUE)
sorted_data <- avg_step_data[sorted_indices,]

# MUTATE THE DATAFRAME TO ADD A NEW COLUMN "ACTIVITY" BASED ON CONDITIONS
activity_level <- sorted_data %>%
  mutate(ActivityLevel = case_when(
    AvgSteps > 12868 ~ "Very Active",
    AvgSteps >= 4103.32 & AvgSteps <= 12868 ~ "Medium Activity",
    TRUE ~ "Sedentary"
  ))

# MUTATES ORIGINAL SUBSET DATA TO ADD AVG STEPS AND ACTIVITY LEVEL
df_with_levels <- left_join(activity_level, subset_data, by = "Id") %>%
  select(-ActivityLevel & -AvgSteps, everything())


write.csv(df_with_levels, "fitbit_data.csv", row.names = FALSE)
