# Calculate and plot catch estimates and variance for SmartPass BSC
# 1/16/25
# JGM


library (tidyverse)

# CSVs downloaded from "BSC Total Catch Calculation Example" sheet, "Gillnet Data" and "Pot Trap Data" tabs
# https://edforg.sharepoint.com/:x:/r/sites/FisheriesOceans-FisherySolutionsCenter/_layouts/15/Doc.aspx?sourcedoc=%7BAB2655D6-08EC-44AC-85B1-8667BBE32157%7D&file=BSC%20Total%20Catch%20Calculation%20Example.xlsx&action=default&mobileredirect=true


# Read, clean, and compile catch and effort data ----

# read and clean Gillnet catch data 
gn_data <- read_csv ("Data/BSC Total Catch Calculation Example(Gillnet Data).csv",
                     # specify that first column is a date, format mm/dd/yyyy
                     col_types = cols (`Landing Date (MDY)` = col_date("%m/%d/%Y"))) %>%
  # get rid of calculation colums 
  select (c(`Landing Date (MDY)`, `Fishing Gear Type`, `Scientific Name`, `Catch Weight per Species (Kg)`)) %>%
  #rename remaining columns so R doesn't get mad
  rename (Landing_date = `Landing Date (MDY)`,
          Gear_type = `Fishing Gear Type`,
          Sci_name = `Scientific Name`,
          Catch_weight = `Catch Weight per Species (Kg)`) %>%
  # streamline gear name
  mutate (Gear_type = "Gillnet")

# check data structure
str (gn_data)

# read and clean Pot Trap catch data
pt_data <- read_csv ("Data/BSC Total Catch Calculation Example(Pot Trap Data).csv",
                     # specify that first column is a date, format mm/dd/yyyy
                     col_types = cols (`Landing Date (MDY)` = col_date("%m/%d/%Y"))) %>%
  # get rid of calculation colums 
  select (c(`Landing Date (MDY)`, `Fishing Gear Type`, `Scientific Name`, `Catch Weight per Species (Kg)`)) %>%
  #rename remaining columns so R doesn't get mad
  rename (Landing_date = `Landing Date (MDY)`,
          Gear_type = `Fishing Gear Type`,
          Sci_name = `Scientific Name`,
          Catch_weight = `Catch Weight per Species (Kg)`) %>%
  # remove rows with additional text/calculations
  filter (!is.na(Landing_date)) %>%
  # change gear name because slash will create R confusion
  mutate (Gear_type = "Trap_Pot")


str (pt_data)

# combine datasets and write new data frame
bsc_catch_combined <- rbind (gn_data, pt_data)

# optional, save interim dataframe
saveRDS(bsc_catch_combined, file = "Data/bsc_example_catch_compiled.Rds")

# extract sample effort data from "Combining Effort with Catch" tab
effort_data <- read_csv("Data/BSC Total Catch Calculation Example(Combining Effort with Catch).csv",
                        # specify that first column is a date, format mm/dd/yyyy
                        col_types = cols (Date = col_date("%m/%d/%Y"))) %>%
  select (Date, `SmartPass Gillnet Fishing Effort Estimate`, `SmartPass Trap/Pot Fishing Effort Estimate`) %>%
  # rename columns to match catch data
  rename (Landing_date = Date,
          Gillnet = `SmartPass Gillnet Fishing Effort Estimate`,
          Trap_Pot = `SmartPass Trap/Pot Fishing Effort Estimate`
  ) %>%
  # remove monthly totals 
  filter (!is.na(Landing_date)) %>%
  # convert to long format to match catch data
  pivot_longer (cols = c(Gillnet, Trap_Pot),
                names_to = "Gear_type",
                values_to = "Fishing_effort")


# Calculate daily catch and number of boats ----
bsc_daily_catch <- bsc_catch_combined %>%
  # Could skip Sci_name since only BSC data were collected, but could use to look at multiple species
  group_by (Landing_date, Gear_type, Sci_name) %>%
  summarise (Boats_surveyed = n(), # gives number of rows in each category--boats observed of each gear type per day
             Sampled_catch_kg = sum (Catch_weight), # total catch caught that day by gear type
             Avg_catch_kg = mean (Catch_weight), # mean catch caught that day by gear type
             # will use this parameter for calculating variance--deviation from mean, squared. This is SUM (pi - p_)2 in the ORBS variance equation
             Catch_dev_from_mean = sum ((Catch_weight - Avg_catch_kg)^2)) 


# Add effort data to estimate total daily catch
bsc_catch_est <- bsc_daily_catch %>%
  # combine catch and effort data
  left_join (effort_data, by = c("Landing_date", "Gear_type")) %>%
  mutate (
    # estimate of total daily catch by gear type
    Total_catch_est = Sampled_catch_kg / Boats_surveyed * Fishing_effort )


# quick plot
ggplot (bsc_catch_est) +
  geom_line (aes (x = Landing_date, y = Total_catch_est, col = Gear_type))

# Calculate variance and confidence intervals ----
# ORBS variance equation has 3 parts, with one parameter we haven't calculated yet: s^2
# Et^2--Total effort in number of boats, squared
# (1 - Sc/Et) -- Sc = # sampled boats, Et = total effort
# s^2 -- (1 / (Sc - 1)) * SUM (pi - p_)^2 -calculated SUM (pi - p_)^2 in the daily catch data (Catch_dev_from_mean)

bsc_catch_ci <- bsc_catch_est %>%
  mutate (
    # first calculate s^2 
    s_squared = (1 / (Boats_surveyed - 1)) * Catch_dev_from_mean, 
    # use ORBS equation to calculate variance
    catch_variance = Fishing_effort^2 * (1 - Boats_surveyed/Fishing_effort) * (s_squared / Boats_surveyed),
    # std dev is sqrt of variance
    catch_sd = sqrt (catch_variance),
    # 95% upper CI is catch estimate plus 2 standard deviations
    ci_upper = Total_catch_est + 2 * catch_sd,
    # 95% lower CI is catch estimate minus 2 standard deviations
    ci_lower = Total_catch_est - 2 * catch_sd
  )

# Create plot

ggplot (bsc_catch_ci, aes (x = Landing_date, y = Total_catch_est, col = Gear_type)) +
  geom_line (size = 1.5) +
  geom_ribbon (aes (ymin = ci_lower, ymax = ci_upper, fill = Gear_type), alpha = 0.2) +
  theme_bw() +
  labs (y = "Total catch estimate", x = "Date", fill = "Gear", col = "Gear") +
  ggtitle ("Estimated total BSC catch")
