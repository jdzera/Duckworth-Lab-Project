# loading in packages
library(dplyr)
library(tidyverse)

# reading in the data here
raw.data <- read_csv("duckworth-lab-application-data-1.csv")

# cleaning the data
cleaned.data <- filter(raw.data, Finished == 1)
cleaned.data <- as.data.frame(cleaned.data)
missing_perc <- (rowMeans(is.na(cleaned.data)) * 100)
data_frame(missing_perc)
cleaned.data <- cbind.data.frame(cleaned.data, missing_perc)
cleaned.data <- filter(cleaned.data, missing_perc <= 20)

## pivot SES
cleaned.data <- cleaned.data %>%
  pivot_longer(
    SES_1:SES_8, 
    names_to = "SES", 
    values_to = "On.Off"
  )

## filter SES
cleaned.data <- cleaned.data %>%
  filter(
    On.Off == "On"
  )

## drop SES
cleaned.data <- cleaned.data %>%
  select(
    !On.Off
  )

## clean SES
cleaned.data <- cleaned.data %>%
  mutate(
    SES = as.integer(gsub("SES_", "", SES))
  )

## reduce race_ethnicity, condense x + multirace to multirace
cleaned.data <- cleaned.data %>% mutate(
  race_sum = white + black + asian
  + hispanic + amindian + pacisl + multirace
  )

## doing this converst the pacisl group over to the multirace group since every pacisl individual
## selected that category and one other
cleaned.data <- cleaned.data %>% mutate(
  multirace = ifelse(race_sum > 1, 1,
            ifelse(race_sum == 1 & multirace == 1, 1, 0)),
  pacisl = ifelse(pacisl == 1 & multirace == 1, 0,
            ifelse(pacisl == 1 & multirace == 0, 1, 0)),
  asian = ifelse(asian == 1 & multirace == 1, 0,
            ifelse(asian == 1 & multirace == 0, 1, 0)),
  hispanic = ifelse(hispanic == 1 & multirace == 1, 0,
                  ifelse(hispanic == 1 & multirace == 0, 1, 0))
  )

## changing the numbers in each variable to maintain distinct values,
## values are ordered according to the structure of the raw data
cleaned.data <- cleaned.data %>%
  mutate(black = black * 2,
         asian = asian * 3,
         hispanic = hispanic * 4,
         amindian = amindian * 5,
         pacisl = pacisl * 6,
         multirace = multirace * 7
  )

## create new race_ethnicity variable
cleaned.data <- cleaned.data %>%
  mutate(race_ethnicity = white + black + asian
         + hispanic + amindian + pacisl + multirace
  )

## drop old race variables
cleaned.data <- cleaned.data %>%
  select(!(white:multirace)
  )

cleaned.data <- cleaned.data %>%
  select(!race_sum)

# reverse code grit_pass1-4 and mindset1-2
cleaned.data <- mutate(cleaned.data, grit_pass1 = grit_pass1 * -1 + 6,
                       grit_pass2 = grit_pass2 * -1 + 6,
                       grit_pass3 = grit_pass3 * -1 + 6,
                       grit_pass4 = grit_pass4 * -1 + 6,
                       mindset1 = mindset1 * -1 + 7,
                       mindset2 = mindset2 * -1 + 7)

# create composite variables

## grit
cleaned.data <- cleaned.data %>%
  rowwise() %>%
  mutate(grit_comp = 
           sum(grit_pass1, grit_pass2, grit_pass3, grit_pass4,
               grit_pers1, grit_pers2, grit_pers3, grit_pers4, na.rm = TRUE),
         .after = grit_pers4
  )

## well being
cleaned.data <- cleaned.data %>%
  mutate(well_being_comp =
           sum(Happy, Sad, Anxious, Joyful, Angry, Relaxed, LS, na.rm = TRUE),
         .after = LS
  )

## curiosity
cleaned.data <- cleaned.data %>%
  mutate(curiosity_comp =
           sum(curiosity1, curiosity2, curiosity3, curiosity4, na.rm = TRUE),
         .after = curiosity4
  )

## self control
cleaned.data <- cleaned.data %>%
  mutate(selfcont_comp =
           sum(selfcont1, selfcont2, selfcont3, selfcont4, na.rm = TRUE),
         .after = selfcont4
  )

## mindset
cleaned.data <- cleaned.data %>%
  mutate(mindset_comp =
           sum(mindset1, mindset2, mindset3, mindset4, na.rm = TRUE),
         .after = mindset4
  )

## gpa    WANT TO TRY TO AMMEND THIS USING SUM(IS.NA())
cleaned.data <- cleaned.data %>%
  mutate(gpa_avg =
           sum(gpa_mp1, gpa_mp2, gpa_mp3, na.rm = TRUE) / 3,
         .after = gpa_mp3
  )

# playing with the data
play.data <- cleaned.data
