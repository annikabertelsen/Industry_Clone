# Capacity factor data for all SPP generators
library(tidyverse)
library(readxl)
DZR_UnitID_EIAID <- read_excel("DZR_UnitID_EIAID.xlsx", sheet = "SPP")
names(DZR_UnitID_EIAID) <- janitor::make_clean_names(names(DZR_UnitID_EIAID))


# Creating columns for plant name and generator ID so that we can later aggregate at plant vs generator level
DZR_UnitID_EIAID <- DZR_UnitID_EIAID %>%
  # fix any where II should be 2
  mutate(npcc_full_name = str_replace(npcc_full_name, "II", "2"))%>%
  # split full name into Plant Name and Generator Id
  mutate(generator_id = str_extract(npcc_full_name, "[[:digit:]]+"),
         plant_name = str_trim(str_extract(npcc_full_name, "[A-z\\s]+"))) %>%
  # if plant has only one sector, change the generator id from NA to 1
  mutate(generator_id = ifelse(is.na(generator_id), 1, generator_id)) %>%
  # rename EIA ID to plant ID
  rename('plant_id' = 'eia_id')%>%
  filter(installation_date < retirement_date)

# net generation data
EIA_PG4 <- read_excel("EIA923_Schedules_2_3_4_5_M_10_2022_16DEC2022.xlsx", sheet = "Page 4 Generator Data")
EIA_PG4 <- tail(EIA_PG4, -3)
names(EIA_PG4) <- EIA_PG4[1,]
EIA_PG4 <- tail(EIA_PG4, -1)

# clean column names of EIA page 4 data
names(EIA_PG4) <- gsub(x = names(EIA_PG4), pattern = "[\r\n]", "")
names(EIA_PG4) <- janitor::make_clean_names(names(EIA_PG4))

# create subsetted data frame with plant id, plant name, generator ID, and generation by month, only for SPP
EIA_4<- EIA_PG4 %>% 
  select(plant_name, generator_id, net_generation_january:net_generation_december, 
         ba_code, plant_state, plant_id, reported_prime_mover) %>%
  # SWPP = Southwest Power Pool = SPP
  filter(ba_code == 'SWPP') %>%
# remove prefixes from some generator id's
  # if dash is detected, take the number after the dash
  mutate(generator_id = ifelse(str_detect(generator_id, '-'),
                               sub('.*-(\\d).*', '\\1', generator_id),
                               generator_id)) %>%
  # extract numeric part from all generator IDs
  mutate(generator_id = str_extract(generator_id, "[[:digit:]]+")) %>%
  # remove any leading zeros
  mutate(generator_id = str_remove(generator_id, "^0+"))

# transform EIA data to long format
EIA_4 <- EIA_4 %>%
  pivot_longer(cols = net_generation_january:net_generation_december, 
               names_to = "month", values_to = "ActualGenerationMW")

EIA_capacity <- EIA_4 %>%
  merge(DZR_UnitID_EIAID, by = c('plant_id', 'generator_id'))%>%
  group_by(plant_id, generator_id) %>%
  # column with first two characters of type of fuel from DZR_UnitID_EIAID data.
  #   This will be compared to EIA data for deciding which rows to remove among some duplicate PlantID, GeneratorID pairs
  mutate(type_short = str_extract(type, "^.{2}")) %>%
  mutate(num_rows = n())

# after merging, each plantID, generatorID pair should have 12 rows, each corresponding
#  to one month's output. However, some pairs have more than that because of the way the data
#  was entered (in EIA, some plants have multiple generators with the same generatorID)
#  To ensure the plantID and generatorID together can uniquely identify a generator,
#  we will compare additional columns in the two datasets and try to keep as many correct
#  matches as possible

EIA_capacity_dups_fix <- EIA_capacity %>%
  # subset to plantID, generatorID pairs that had duplicates
  filter(num_rows >12) %>%
  # comparing the two datasets, keep pairs if type is the same or plant names 
  #    are the same (sometimes same plant id would have slightly different names)
  filter(reported_prime_mover == type_short | plant_name.x == plant_name.y) %>%
  # recalculate the number of rows for each pair
  mutate(num_rows = n()) %>%
  # we are left with only 2 plants now where we had duplicate plantid, generatorid values
    #  we will have to filter these out since we cannot determine which of them corresponds to 
    # the summer and winter capacity from the DZR Unit IDs file
  filter(num_rows == 12)


# calculating capacity factor gap:
# winter months for Engie are Dec, Jan, and Feb
winter_months <- c('December', 'January', 'February')

# create capacity factor dataset
EIA_capacity_clean <- EIA_capacity %>% filter(num_rows == 12) %>%
  rbind(EIA_capacity_dups_fix) %>%
  # remove unnecessary cols used when detecting and fixing duplicates
  select(-c(type_short, plant_name.y, num_rows)) %>%
  rename('plant_name' = 'plant_name.x') %>%
  # clean month names
  mutate(month = str_to_title(gsub(x = month, pattern = 'net_generation_', '')))%>%
  # removes Nov and Dec since we did not have generation data for these
  filter(!month %in% c('December', 'November'))%>%
  # convert summer and winter capacity from hourly to monthly
  mutate(SummerMonthlyCapacity = summer_capacity_mw * 31 * 24,
         WinterMonthlyCapacity = winter_capacity_mw * 31 * 24) %>%
  # convert actual capacity from character to numeric
  mutate(ActualGenerationMW = as.numeric(ActualGenerationMW)) %>%
  # calculate capacity factor gap using correct capacity corresponding to current month
  mutate(capacity_factor_gap = ifelse(month %in% winter_months,
                                      ActualGenerationMW/WinterMonthlyCapacity,
                                      ActualGenerationMW/SummerMonthlyCapacity)) %>%
  # variable with the max capacity according to the month (use for graphing)
  mutate(max_capacity = ifelse(month %in% winter_months,
                               WinterMonthlyCapacity,
                               SummerMonthlyCapacity)) %>%
  # arrange months in chronological order
  mutate(month = factor(month, levels = month.name)) %>%
  arrange(month) %>%
  # arrange by plantID, generatorID
  arrange(plant_id, generator_id)
  
# note: will receive one warning message due to NA value for one month for plant 50192

write.csv(EIA_capacity_clean, 'capacityfactorclean.csv')
