# raw overview of HOMBRES Data 
# Ana Bravo 
# last update: 2023-04-29


# raw cleaning exploring data
# Ana Bravo 
# last update: 2023-04-29


# set up ------------------------------------------------------------------------------
library(haven)
library(tidyverse)


# bring .sav file into R using Haven ------------------------------------------------------------------------------
HOMBRES_Y_subset <- haven::read_sav("raw_data/Hombres_Youth_Subset.sav", 
                                    encoding = "latin1")


# Hombres .sav file of US born youth. Only includes baseline information ---------------------------------------------
HOMBRES_Youth_USborn_Baseline <- haven::read_sav("raw_data/Hombres Youth USborn Baseline.sav", 
                                                 encoding = "latin1")

# writing R file into .csv file ------------------------------------------------------------------------------
write_csv(x = HOMBRES_Y_subset, 
          file = "/Users/anbravo/OneDrive/FIU Related/Graduate/MPH Biostatistics/CRUSADA/raw_data/HOMBRES_Y_Raw.csv")



# bringing in HOMBRES raw CSV ------------------------------------------------------------------------------
HOMBRES_Y_Raw <- read_csv("raw_data/HOMBRES_Y_Raw.csv")


## cleaning data - names ------------------------------------------------------------------------------

copy_HOMBRES_Y_subset <- HOMBRES_Y_Raw        # making copy of data

names(copy_HOMBRES_Y_subset) <- NULL          # removing previous names

newnames <- c("Redcap_event_name",            # creating new names 
                 "Participant_ID",
                 "Father_or_son",
                 "Language_pref",
                 "Total_household_n",
                 "Age_at_baseline",
                 "Currently_in_school",
                 "Current_grade",
                 "Time_point",
                 "MACV_RSP",
                 "MACV_REL",
                 "Machismo_SS",
                 "Caballerismo_SS",
                 "HSI_Y",
                 "FES_COFLT",
                 "FES_RELIG",
                 "FES_CONTROL",
                 "BAS_Americanism",
                 "BAS_Hispanicism",
                 "SemiRural_or_urban",
                 "Group",
                 "Parent_marital_status",
                 "Pastmonth_total_income")




names(copy_HOMBRES_Y_subset) <-newnames        # pasting new names on copy HOMBRES data





### removing Y variable ### 
clean_names_HOMBRES_Y <- clean_names_HOMBRES_Y %>% 
  mutate(
    Participant_ID = str_remove_all(Participant_ID, "[A-Z]")
    )

## converting participant ID to numebric ####

clean_names_HOMBRES_Y$Participant_ID = as.numeric(clean_names_HOMBRES_Y$Participant_ID)


clean_HOMBRES_Y_subset <- clean_names_HOMBRES_Y


## dropig redcap event - already have my baseline and 6mfu in time_point variable ######


clean_HOMBRES_Y_subset <- clean_HOMBRES_Y_subset %>% 
  select(Redcap_event_name, Participant_ID, Time_point, Group, everything())



## creating intervention/control numerical variable ####

HOMBRES_Y_clean <- clean_HOMBRES_Y_subset %>% 
  mutate(Group_numerical = ifelse(Group == "C", 0, 1)) %>% 
  select(Redcap_event_name ,Participant_ID, Time_point, Group, Group_numerical, everything())


### pivot to wide data set ------------------------------------------------------------------------------
HOMBRES_clean_wide <- HOMBRES_Y_clean %>% 
  select(-Time_point) %>% 
  pivot_wider(
    names_from = Redcap_event_name,
    values_from = c(Group, Group_numerical, Father_or_son, Language_pref, Total_household_n,
                    Age_at_baseline, Currently_in_school, Current_grade, MACV_RSP, MACV_REL,
                    Machismo_SS, Caballerismo_SS, HSI_Y, FES_COFLT, FES_RELIG, FES_CONTROL,
                    BAS_Americanism, BAS_Hispanicism, SemiRural_or_urban, Parent_marital_status,
                    Pastmonth_total_income)
  )
  

# writing clean data set - wide format ------------------------------------------------------------------------------

write_csv(
  x = HOMBRES_clean_wide,
  file = "/Users/anbravo/OneDrive/FIU Related/Graduate/MPH Biostatistics/CRUSADA/clean_data/HOMBRES_Y_clean_wide.csv"
)


# writing clean data set - long format 

write_csv(x = HOMBRES_Y_clean, 
          file = "/Users/anbravo/OneDrive/FIU Related/Graduate/MPH Biostatistics/CRUSADA/clean_data/HOMBRES_Y_clean.csv")



# writing copy HOMBRES ------------------------------------------------------------------------------


write_csv(x = copy_HOMBRES_Y_subset, 
          file = "/Users/anbravo/OneDrive/FIU Related/Graduate/MPH Biostatistics/CRUSADA/clean_data/HOMBRES_Y_clean_copy.csv")


some table prep ------------------------------------------------------------------------------

# table prep 
table_1 <- 
  list(
    "tbl_summary-str:default_con_type" = "continuous2",
    "tbl_summary-str:continuous_stat" = c(
      "{median} ({p25} - {p75})",
      "{mean} ({sd})",
      "{min} - {max}"
    ),
    
    "tbl_summary-str:categorical_stat" = " ({p}%)",
    "style_number-arg:big.mark" = "",
    "tbl_summary-fn:percent_fun" = function(x) style_percent(x, digits = 2)
  )


# subset for table -------------------------------------------------------------------------

table_subset <- subset_YRBS_2019 %>% 
  select(Sex, 
         SexOrientation, 
         Q4, 
         Q30, 
         Q34, 
         Q41, 
         Q45
  )

#actual table ------------------------------------------------------------------------------

gtsummary::set_gtsummary_theme(table_1)

table_subset %>% 
  gtsummary::tbl_summary(
    by = SexOrientation,
    missing = "always",
    missing_text = "Missing",
    list(
      Sex ~ "Sex", Q4 ~ "Hispanic", SexOrientation ~"Sexual Orientation", 
      Q30 ~ "Cig smoke", Q34 ~"Vape", Q41 ~ "Alcohol", Q45 ~ "Marijuana"
    )
  ) %>% 
  bold_labels() %>% 
  modify_header(label ~ "**Variables**")



write_csv(x = copy_clean_HOMBRES_Y, 
          file = "/Users/anbravo/OneDrive/FIU Related/Graduate/MPH Biostatistics/CRUSADA/clean_data/copy_CLEAN_HOMBRES_Y.csv")



###############################################################################
##################### Jan 26 2023 #############################################
###############################################################################

# prep to export SAS csv file to merge with US born data set ------------------



new_copy_hombres_youth <- read_csv("raw_data/new_copy_hombres_youth_toR.csv")

# unique(HOMBRES_Youth_USborn_Baseline[,"acaseid"]) 
# unique(HOMBRES_Y_subset[,"acaseid"])


# writing US born into csv file ------------------------------------------------------------------------------

write_csv(x = HOMBRES_Youth_USborn_Baseline, 
         file = "/Users/anbravo/OneDrive/FIU Related/Graduate/MPH Biostatistics/CRUSADA/raw_data/HOMBRES_Youth_USborn_Baseline.csv")


### bring in US born ------------------------------------------------------------------------------

HOMBRES_Youth_USborn_Baseline <- read_csv("raw_data/HOMBRES_Youth_USborn_Baseline.csv")



### fixing variable name to match SAS data ----------------------------------------------------

names(HOMBRES_Youth_USborn_Baseline) <- NULL 

new_USborn_names <- c(
  "Participant_ID",
  "US_born"
)





# attempt to join data sets (first attempt) --------------------------------------------------------

HOMBRES_joined_USborn <- left_join(
  new_copy_hombres_youth,
  HOMBRES_Youth_USborn_Baseline,
  by = "Participant_ID"
) 


###### making the follow up NA, cause this is baseline only data ##############
## not sure why case_when() function is misbehaving here/not working###########

HOMBRES_joined_USborn_forSAS <- 
  HOMBRES_joined_USborn %>% 
  mutate(US_born= ifelse(
    Redcap_event_name == "second_post_interv_arm_1",
    NA,
    US_born
  ))



## organizing variables for final transfer to SAS -----------------------------------------

HOMBRES_joined_USborn_forSAS <- HOMBRES_joined_USborn_forSAS %>% 
  select(Redcap_event_name, Participant_ID, Father_or_son, Language_pref, 
         Currently_in_school, US_born, Time_point,everything()) 


# write SAS/ or CSV ----------------------------------------------------------------------------
write_sas(HOMBRES_joined_USborn_forSAS, 
          path = "/Users/anbravo/OneDrive/FIU Related/Graduate/MPH Biostatistics/CRUSADA/clean_data/HOMBRES_joined_USborn_forSAS.sas7bdat")


write_csv(
  HOMBRES_joined_USborn_forSAS,
  file = "/Users/anbravo/OneDrive/FIU Related/Graduate/MPH Biostatistics/CRUSADA/clean_data/HOMBRES_joined_USborn.csv"
)

# changing martial status to numerical values if needed ------------------------------------------



HOMBRES_joined_USborn <- read_csv("clean_data/HOMBRES_joined_USborn.csv")


# checking values of variable parent martital status  --------------------------------------------------
unique(HOMBRES_joined_USborn$Parent_marital_status)


HOMBRES_joined_USborn %>% 
  mutate(Parent_marital_status = case_when(
    Parent_marital_status == "Married"                    ~ 1,
    Parent_marital_status == "In a domestic relationship" ~ 2,
    Parent_marital_status == "Single or separated"        ~ 3
  ))

###############################################################################
######## Updated and cleaning on new data based on feedback given on:##########
################## ############2/2/2023 #######################################


HOMBRES_joined_USborn <- read_csv("clean_data/HOMBRES_joined_USborn.csv")

HOMBRES_Father_prepost <- haven::read_sav("raw_data/Hombres Father selected vars pre and post.sav", 
                                          encoding = "latin1")


# transferring fixed variables to post time point for correct analysis ------------------------------------

HOMBRES_Youth_fixed <- HOMBRES_joined_USborn %>% 
  group_by(Participant_ID) %>% 
  fill(Language_pref, .direction = "updown") %>% 
  fill(Currently_in_school, .direction = "updown") %>% 
  fill(US_born, .direction = "updown") %>% 
  fill(numeric_age, .direction = "updown") %>% 
  fill(numeric_current_grade, .direction = "updown") %>% 
  fill(numeric_total_household, .direction = "updown") 



# changing variable Time_point to Time in youth data ------------------------------------------

HOMBRES_Youth_fixed <- HOMBRES_Youth_fixed %>% 
  rename(Time = Time_point) %>% 
  select(Redcap_event_name, Participant_ID, Time, everything()) 
 



#export data ----------------------------------------------------------------------------------



write_csv(
  x = HOMBRES_Youth_fixed,
  file = "/Users/anbravo/OneDrive/FIU Related/Graduate/MPH Biostatistics/CRUSADA/clean_data/HOMBRES_Youth_fixed.csv"
)



# Fixing variables in Father data set ---------------------------------------------------------------------


names(HOMBRES_Father_prepost) <- NULL

fathers_newnames <- c(
  "Participant_ID",
  "Years_in_US_notborn_F",
  "Current_Age_F",
  "Born_in_US_F",
  "Time",
  "Drink_Freq_F",
  "Drink_Quant_F",
  "Binge_Drinker_F"
)

names(HOMBRES_Father_prepost) <- fathers_newnames

write_csv(x = HOMBRES_Father_prepost,
          file = "/Users/anbravo/OneDrive/FIU Related/Graduate/MPH Biostatistics/CRUSADA/raw_data/HOMBRES_Father_Raw.csv")


HOMBRES_Father_cleanNames <- read_csv("raw_data/HOMBRES_Father_Raw.csv")

################################################################################
# preparing to merge with Youth data set by participant ID #####################
########### cleaning Father Participant ID variables ###########################


# switching From F variable to Y variable ------------------------------------------------------------

HOMBRES_Father_cleanNames <- HOMBRES_Father_cleanNames %>% 
  mutate(Participant_ID = str_replace_all(
    Participant_ID,
    "[A-Z]", 
    "Y" 
    ))


# preparing to join Father dad with youth data set --------------------------------------------------


### maybe subsetting the data first for youth###

HOMBRES_Youth_subset <- HOMBRES_Youth_fixed %>% 
  select(Participant_ID, Time, HSI_Y)


### trying to merge youth subset to father data##### 

Youth_Father_subset <- left_join(
  HOMBRES_Father_cleanNames,
  HOMBRES_Youth_subset,
  by = c("Participant_ID", "Time")
) %>% 
  select(Participant_ID, Time, HSI_Y, everything()) 


###trying to merge some father significant variables with Youth subset #####

Youth_Father_second_subset <- left_join(
  HOMBRES_Father_cleanNames,
  HOMBRES_Youth_fixed,
  by = c("Participant_ID", "Time")
) %>% 
  select(Redcap_event_name, Participant_ID, Time, HSI_Y, everything()) 




# exporting new data set with father years in the US -----------------------------------------------

write_csv(x = Youth_Father_second_subset,
          file = "/Users/anbravo/OneDrive/FIU Related/Graduate/MPH Biostatistics/CRUSADA/clean_data/Father_Youth_second_subset.csv")




# exporting the new subset data of youth and father info ---------------------------------------------



write_csv(x = Youth_Father_subset,
          file = "/Users/anbravo/OneDrive/FIU Related/Graduate/MPH Biostatistics/CRUSADA/clean_data/Youth_Father_stress_subset.csv")





# pivot wide youth to merge? ------------------------------------------------------------------------------

#HOMBRES_Youth_wide <- HOMBRES_Youth_fixed %>% 
 # select(-Time_point) %>% 
  #pivot_wider(
    #names_from = Redcap_event_name,
   # values_from = c(Father_or_son, Language_pref, Currently_in_school,
    #US_born, MACV_RSP, MACV_REL, Caballerismo_SS, HSI_Y, FES_COFLT,
    #FES_RELIG, FES_CONTROL, BAS_Americanism, BAS_Hispanicism, SemiRural_or_urban, Group,
    #Parent_marital_status, Pastmonth_total_income, numeric_age, numeric_total_household, 
    #numeric_current_grade, Machismo_SS)
    
 # )
