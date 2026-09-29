
library(readxl)
library(psych)
library(gt)
library(tidyverse)
library(janitor)



#Read excel file
read_excel("./Attachment_Anxiety_Data.xlsx")

#Create a r Data frame
anxiety_df <- read_excel("./Attachment_Anxiety_Data.xlsx")

#Assign new data frame, then mutate and assign integer objects
anxiety.clean.df <- anxiety_df |>
  mutate( 
          Gender_Marker = case_when(
                                  Gender == 1 ~ "Male",
                                  Gender == 2 ~ "Female"),
         
          Age_Group = case_when(
                                `Age group` == 1 ~ "18-24",
                                `Age group` == 2 ~ "24-34",
                                `Age group` == 3 ~ "35-44",
                                `Age group` == 4 ~ "45-54",
                                `Age group` == 5 ~ "55-64"),
          Relationship_Status = case_when(
                              `Relationship` == 1 ~ "Married",
                              `Relationship`   ==  2 ~ "In a relationship",
                              `Relationship` ==   4 ~ "Divorced",
                              `Relationship`  ==  5 ~ "In a relationship",
                              `Relationship` ==  6 ~ "Never been in a relationship",
                              `Relationship`  ==   7 ~ "Prefer not to say"),
          Ethnicity_Identification = case_when(
                               `Ethnicity` == 1 ~ "White/Caucasian", 
                               `Ethnicity` == 2 ~ "Asian/Asian British", 
                               `Ethnicity` == 5 ~ "Other ethnic group", 
                               `Ethnicity` ==  6~ "Prefer not to say"))

anxiety_df <- anxiety_df |>
  mutate(
    across(c(AA_1:AA_9, SA_1:SA_20, SEst_1:SEst_10), as.integer)
  )

#Using the list() function to create an “Answer Key" for each measure seperately
# format: keys <- list(
#Agreeableness = c("-A1", "A2", "A3", "A4", "A5"),
#Extraversion = c("-E1", "E2", "E3", "E4", "-E5"))
#reverse key items 5, 9, and 11 in the Social Interaction Anxiety Scale responses
#reverse key items 2, 5, 6, 8, and 9 in the Rosenberg Self-Esteem Scale

ecr_keys <- list(
  Experiences_in_Close_Relationships_Scale = c("AA_1", "AA_2", "AA_3", "AA_4", "AA_5", "AA_6", "AA_7", "AA_8", "AA_9")
)

sa_keys <- list(
  Social_Interaction_Anxiety_Scale = c("SA_1","SA_2","SA_3","SA_4","-SA_5","SA_6","SA_7","SA_8","-SA_9","SA_10","-SA_11","SA_12","SA_13","SA_14","SA_15","SA_16","SA_17","SA_18","SA_19","SA_20")
)

sest_keys <- list(
Rosenberg_Self_Esteem_Scale = c("SEst_1", "-SEst_2","SEst_3","SEst_4","-SEst_5","-SEst_6","SEst_7","-SEst_8","-SEst_9","SEst_10")
)

#verify keys were created using ecr_keys, sest_keys, sa_keys

####Troublshooting ####
  #names(anxiety_df)

  #ecr_keys

  #names(anxiety_df)[grepl("^AA_", names(anxiety_df))]

  #str(anxiety_df[, c("AA_1","AA_2","AA_3","AA_4","AA_5","AA_6","AA_7","AA_8","AA_9")])
  #str(anxiety_df$AA_1)



#ScoreItems() compute average and scores Cronbach’s alpha for all three seperately

ecr_scores <-scoreItems (ecr_keys, anxiety_df,
                       totals = F, min = 1, max = 7)

sa_scores <-scoreItems (sa_keys, anxiety_df,
                         totals = F, min = 0, max = 4)

sest_scores <-scoreItems (sest_keys, anxiety_df,
                         totals = F, min = 1, max = 4)

#viewing chroncach alpha scores for each measure

ecr_scores$alpha
sa_scores$alpha
sest_scores$alpha

#Convert to a new dataframe ie thing in the environment 

anxiety_df$ecr_avg <- ecr_scores$scores[,1]

anxiety_df$sa_avg <- sa_scores$scores[,1]

anxiety_df$sest_avg <- sest_scores$scores[,1]

ecr_df <- as.data.frame(ecr_scores$scores)

sa_df <- as.data.frame(sa_scores$scores)

sest_df <- as.data.frame(sest_scores$scores)


#Combine average score into one dataframe

allscores.df <- cbind(ecr_df, sa_df, sest_df)


#Combine average score AND anxiety.df origianl into one dataframe

finalscores.df <- cbind(allscores.df, anxiety.clean.df)

#Reduce dataframe using select() to choose the columns 

analysis_df <- finalscores.df |>
  select(
    URN,
    Gender_Marker,
    Age_Group,
    Relationship_Status,
    Ethnicity_Identification,
    Experiences_in_Close_Relationships_Scale,
    Social_Interaction_Anxiety_Scale,
    Rosenberg_Self_Esteem_Scale
  )

sample_size <- nrow(analysis_df)

#tabyl() function create a demographic summary table containing at least two 
  #demographic variables available in this dataset (these should be the ones which you used mutate on earlier).

demographic_table <- analysis_df |>
  tabyl(Gender_Marker, Relationship_Status)

#gt table creation with title 

Gender_Relationship_Status_Demo<- gt(demographic_table) |>
  tab_header(
    title = "Participant Relationship Status by Gender"
  )

#view the table with the new title
Gender_Relationship_Status_Demo

#Using describe() capture summary statistics for the cleaned and scored dataset 
#across all 3 scales. Assign this the name of “summary”.

describe(analysis_df)

summary <- describe(allscores.df)

summary

#Extract and label the sample size using describe() or nrow() .

summary$n

summary <- summary |>
  rename(`sample size` = n)
  
#Create a column in your summary labelled “Scales”.

summary <- summary |>
  mutate(
    Scales = c(
      "Experiences_in_Close_Relationships_Scale",
      "Social_Interaction_Anxiety_Scale",
      "Rosenberg_Self_Esteem_Scale"
    )
  )

#recall alphas for measures

ecr_scores$alpha
sa_scores$alpha
sest_scores$alpha

#Combine the three Cronbach's alpha values (one per scale), round them to 2 decimal places.

alphas <- round(c(
  ecr_scores$alpha,
  sa_scores$alpha,
  sest_scores$alpha), 2)

#Overwrite and Select only particular values from your summery table. This must include:Scale Titles, Mean, Median, SD, Range
#R does not like spaces in unquoted column names

names(summary)

summary <- summary |>
  
  select(
    Scales,
    mean,
    median,
    sd,
    range)

#Using cbind(), combine your summary table with your Cronbach alphas.

summary <- cbind(summary, alphas)

#Using gt() turn this into a reproducible gt table with an intuitive title. Feel
#free to add subheadings as well as seen in bfi_index.qmd.

Summary_Table <- gt(summary) |>
  tab_header( 
    title= "Combined Anxiety and Self Esteem Scores"
    )

#Filter your cleaned data according to one demographic variable. For example, 
#you might choose only participants of a specific relationship status, gender, or age group.

filtered.df <- analysis_df |>
  filter(Gender_Marker == "Female")

#new filtered dataframe for calculating new alphas

filtered_raw.df <- anxiety.clean.df |>
  filter(Gender_Marker == "Female")

#make sure the filtered is all integers

filtered_raw.df <- filtered_raw.df |>
  mutate(
    across(c(AA_1:AA_9, SA_1:SA_20, SEst_1:SEst_10), as.integer)
  )

#create new Cronbach alphas for filtered.df

filtered_ecr_scores <-scoreItems (ecr_keys, filtered_raw.df,
                                  totals = F, min = 1, max = 7)

filtered_sa_scores <-scoreItems (sa_keys, filtered_raw.df,
                                 totals = F, min = 0, max = 4)

filtered_sest_scores <-scoreItems (sest_keys, filtered_raw.df,
                                   totals = F, min = 1, max = 4)


#Using describe() capture summary statistics for the cleaned and scored 
#dataset across all 3 scales. Assign this the name of “filtered.summary”.

filtered.summary <- describe(
  filtered.df |>
    select(
      Experiences_in_Close_Relationships_Scale,
      Social_Interaction_Anxiety_Scale,
      Rosenberg_Self_Esteem_Scale)
)

#Create a column in your summary labelled “filtered.scales”.

filtered.summary <- filtered.summary |>
  mutate(
    Scales = c(
      "Experiences_in_Close_Relationships_Scale",
      "Social_Interaction_Anxiety_Scale",
      "Rosenberg_Self_Esteem_Scale"
    )
  )

#Combine the three Cronbach's alpha values (one per scale), round them to 2 decimal places.

filtered_alphas <- round(c(
  filtered_ecr_scores$alpha,
  filtered_sa_scores$alpha,
  filtered_sest_scores$alpha), 2)

filtered_alphas

#Overwrite and Select only particular values from your summery table. This must include:Scale Titles, Mean, Median, SD, Range
#R does not like spaces in unquoted column names

filtered.summary <- filtered.summary |>
  
  select(
    Scales,
    mean,
    median,
    sd,
    range)

#Using cbind(), combine your summary table with your Cronbach alphas.

filtered.summary <- cbind(
  filtered.summary, 
  filtered_alphas)

#Using gt() turn this into a reproducible gt table with an intuitive title.

Filtered_Summary_Table <- gt(filtered.summary) |>
  tab_header( 
    title= "Female Anxiety and Self Esteem Scale Scores"
  )

#check make sure everything renders
analysis_df
allscores.df
summary
filtered.df
filtered.summary


Summary_Table

Filtered_Summary_Table
