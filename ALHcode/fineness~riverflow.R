rm(list = ls())

### Fineness Ratio FR ~ River flow regression looking at run timing group categories/times 2019-2021

fineness <- read.csv("ALHcode/AIC/lowerriver_bodycomp_fineness.csv")[-1]
riverflow <- read.csv("ALHcode/odata/million_dollar_bridge_discharge_data_1988-2025.csv")

#organize fineness data
fineness$Collection_Location <- as.factor(fineness$Collection_Location) 
fineness$RunTimingGroup <- as.factor(fineness$RunTimingGroup)
fineness$Sex<- as.factor(fineness$Sex)

fineness$RunTimingGroup <- factor(fineness$RunTimingGroup,
                                 levels = c("Early",
                                            "Middle",
                                            "Late"))

#create summary table of fineness means
library(dplyr)
fineness_summary <- fineness %>%
  filter(Year %in% c(2019, 2020, 2021)) %>%         # keep only 2019–2021
  group_by(Year, RunTimingGroup, Sex) %>%                # group by year and timing group and Sex
  summarise(
    mean_fineness = mean(fineness, na.rm = TRUE),  # calculate mean
    n = n()                                               # number of fish per group
  ) %>%
  arrange(Year, RunTimingGroup, Sex)  # sort nicely

fineness_summary <- as.data.frame(fineness_summary)




#pull specific flow averages for early, middle, late run timing for each year
str(riverflow)
library(lubridate)

riverflow <- riverflow %>%
  mutate(Date = mdy(time))  # convert "10/10/2025" etc. to real Date format

date_flow_groups <- list(
  "2019_Early"  = as.Date(c("2019-06-01", "2019-06-02", "2019-06-07")),
  "2019_Middle" = as.Date(c("2019-06-19")),
  "2019_Late"   = as.Date(c("2019-07-23", "2019-07-24")),
  
  "2020_Early"  = as.Date(c("2020-06-01", "2020-06-02")),
  "2020_Middle" = as.Date(c("2020-06-30", "2020-07-01")),
  "2020_Late"   = as.Date(c("2020-08-05", "2020-08-06")),
  
  "2021_Early"  = as.Date(c("2021-06-02")),
  "2021_Middle" = as.Date(c("2021-06-18")),
  "2021_Late"   = as.Date(c("2021-07-08"))
)


flow_summary <- lapply(names(date_flow_groups), function(name) {
  year  <- as.numeric(substr(name, 1, 4))
  group <- sub("^[0-9]+_", "", name)
  dates <- date_flow_groups[[name]]
  
  df <- riverflow %>%
    filter(Date %in% dates) %>%
    summarise(
      mean_flow_cfs = mean(value, na.rm = TRUE)
    ) %>%
    mutate(Year = year, RunTimingGroup = group)
  
  return(df)
}) %>%
  bind_rows() %>%
  select(Year, RunTimingGroup, mean_flow_cfs)




merged_data <- dplyr::left_join(
 fineness_summary,
  flow_summary,
  by = c("Year", "RunTimingGroup")
)

# Check the result
print(merged_data) #now data is all merged nicely together


ggplot(merged_data, aes(x = mean_flow_cfs, y = mean_fineness)) +
  geom_point() +
  facet_wrap(~Year, scales = "free_x")
  
