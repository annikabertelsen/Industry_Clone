

library(readxl)
library(dplyr)
library(tidyverse)



hourly_dayzer <- read_excel("SPP hourly coal production from Dayzer model output for 2022 backcast (KS state).xlsx")

#Conver Dayzer output from hourly to monthly 
monthly_dayzer <- hourly_dayzer |> 
  mutate(Date = lubridate::ymd(Date)) |> 
  mutate(Date = lubridate::month(Date, label = TRUE)) |> # convert to date object and get month
  group_by(Date, UnitId, ScenarioId,Fuelname,GenericFuelName,UnitName,UnitType,Zone) |> # group by categories
  summarise(FuelPrice = mean(FuelPrice),
            LMP = sum(LMP),
            Energy = sum(Energy),
            Congestion = sum(Congestion),
            Losses = sum(Losses),
            Uplift = sum(Uplift),
            # potentially change
            SprinPrice = mean(SpinPrice),
            GenerationMW = sum(GenerationMW),
            SpinMW = sum(SpinMW), #Potentially change
            SpinMW = mean(SpinMW), #might be sum
            QSMW = mean(QSMW),
            AGCMW = mean(AGCMW),
            FuelPrice = mean(FuelPrice),
            Bid = mean(Bid),
            Cost_Fuel = mean(Cost_Fuel),
            Cost_Emissions = mean(Cost_Emissions),
            Cost_VOM = mean(Cost_VOM),
            SummerCapacity = mean(SummerCapacity),#This and Winter capacity below should also be converted to monthly terms, right now they match so leave this way
            SummerCapacity = mean(SummerCapacity)#Winter capacity below is also in hourly terms, must also convert to monthly 
)

#####################################################################################################################################

DZR_UnitID_EIAID <- read_excel("DZR_UnitID_EIAID.xlsx", sheet = "SPP")

#Rename to UnitId as matches monthly_dayzer
DZR_UnitID_EIAID$UnitId <- DZR_UnitID_EIAID$`CES UnitId` 

#Match monthly_dayzer with DZR_UnitID_EIAID 
monthly_dayzer_EIAID <- merge(x = monthly_dayzer, y = DZR_UnitID_EIAID, by = "UnitId")


# Creating columns for plant name and generator ID so that we can later aggregate at plant vs generator level
monthly_dayzer_EIAID <- monthly_dayzer_EIAID %>%
  # split UnitName into Plant Name and Generator Id
  mutate(`Generator Id` = str_extract(UnitName, "\\d"),
         `Plant Name` = str_trim(str_extract(UnitName, "[A-z\\s]+"))) %>%
  # change EC to Energy Center
  mutate(`Plant Name` = str_replace(`Plant Name`, "EC", "Energy Center")) %>%
  # if plant has only one sector, change the generator id from NA to 1
  mutate(`Generator Id` = ifelse(is.na(`Generator Id`), 1, `Generator Id`))

#####################################################################################################################################
EIA_PG1 <- read_excel("EIA923_Schedules_2_3_4_5_M_10_2022_16DEC2022.xlsx", sheet = "Page 1 Generation and Fuel Data")
EIA_PG1 <- tail(EIA_PG1, -4)
names(EIA_PG1) <- EIA_PG1[1,]
EIA_PG1 <- tail(EIA_PG1, -1)

EIA_PG3 <- read_excel("EIA923_Schedules_2_3_4_5_M_10_2022_16DEC2022.xlsx", sheet = "Page 3 Boiler Fuel Data")
EIA_PG3 <- tail(EIA_PG3, -3)
names(EIA_PG3) <- EIA_PG3[1,]
EIA_PG3 <- tail(EIA_PG3, -1)

EIA_PG4 <- read_excel("EIA923_Schedules_2_3_4_5_M_10_2022_16DEC2022.xlsx", sheet = "Page 4 Generator Data")
EIA_PG4 <- tail(EIA_PG4, -3)
names(EIA_PG4) <- EIA_PG4[1,]
EIA_PG4 <- tail(EIA_PG4, -1)

EIA_PG5 <- read_excel("EIA923_Schedules_2_3_4_5_M_10_2022_16DEC2022.xlsx", sheet = "Page 5 Fuel Receipts and Costs")
EIA_PG5 <- tail(EIA_PG5, -2)
names(EIA_PG5) <- EIA_PG5[1,]
EIA_PG5 <- tail(EIA_PG5, -1)
#####################################################################################################################################


#### Preparing EIA page 4 (net generation data) for merge
# clean column names of EIA page 4 data
names(EIA_PG4) <- gsub(x = names(EIA_PG4), pattern = "[\r\n]", "")
names(EIA_PG4) <- janitor::make_clean_names(names(EIA_PG4))

# create subsetted data frame with plant id, plant name, generator ID, and generation by month, only for SPP
EIA_4<- EIA_PG4 %>% 
  select(plant_name, generator_id, net_generation_january:net_generation_december, 
         ba_code, plant_state, plant_id) %>%
  # SWPP = Southwest Power Pool = SPP
  filter(ba_code == 'SWPP')

# transform EIA data to long format to match Dayzer
EIA_4 <- EIA_4 %>%
  pivot_longer(cols = net_generation_january:net_generation_december, 
               names_to = "month", values_to = "ActualGenerationMW")

# clean Month column
EIA_4$month <- str_to_title(gsub(x = EIA_4$month, pattern = "net_generation_", ""))

#### preparing Dayzer for Merge
# change date variable to have full month names

dayzer_clean <- monthly_dayzer_EIAID %>%
  mutate(Date = month.name[Date]) %>%
  rename('month' = 'Date')

names(dayzer_clean) <- janitor::make_clean_names(names(dayzer_clean))

# merge EIA data with Dayzer. join on plant_id, generator_id, and month
EIA_dayzer_merge <- merge(x = EIA_4, y = dayzer_clean, by.x = c('plant_id', 'generator_id', 'month'),
                          by.y = c('eia_id', 'generator_id', 'month'))

# calculating capacity factor gap:
# winter months for Engie are Dec, Jan, and Feb
winter_months <- c('December', 'January', 'February')

EIA_dayzer_merge$SummerMonthlyCapacity = EIA_dayzer_merge$summer_capacity_mw * 31 * 24
EIA_dayzer_merge$WinterMonthlyCapacity = EIA_dayzer_merge$winter_capacity_mw * 31 * 24

# clean data set for visualization. only contains data pertaining to net generation and has column for capacity factor gap
# NOTE: actual generation for nov & dec is NA since we only have jan-oct
EIA_dayzer_merge <- EIA_dayzer_merge %>%
  rename("DZRGenerationMW" = "generation_mw") %>%
  mutate(ActualGenerationMW = as.numeric(ActualGenerationMW)) %>%
  # calculate capacity factor gap using correct capacity corresponding to current month
  mutate(capacity_factor_gap = ifelse(month %in% winter_months,
                                      ActualGenerationMW/WinterMonthlyCapacity,
                                      ActualGenerationMW/SummerMonthlyCapacity)) %>%
  select(plant_id, plant_name.x, generator_id, month, ActualGenerationMW, DZRGenerationMW,
         SummerMonthlyCapacity, WinterMonthlyCapacity, capacity_factor_gap) %>%
  # fix plant_name name
  rename('plant_name' = 'plant_name.x') %>%
  # arrange months in chronological order, then have data arranged by plant name and generator ID
  mutate(month = factor(month, levels = month.name)) %>%
  arrange(month) %>%
  arrange(plant_name, generator_id)

write.csv(EIA_dayzer_merge, "EIA_dayzer_merge.csv")
