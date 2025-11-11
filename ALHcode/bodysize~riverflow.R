rm(list = ls())

### Body size ~ River flow regression looking at run timing group categories/times 2019-2021

bodydata <- read.csv("ALHcode/AIC/lowerriver_bodycomp_PCscores.csv")[-1]

riverflow <- read.csv("ALHcode/odata/million_dollar_bridge_discharge_data_1988-2025.csv")

#fineness vs body size PC score
bodydata <- bodydata %>%
  group_by(Sex) %>%
  mutate(fineness = Fish_Leng_1 / Fish_Wdth)

body_fineness <- lm(bodysize_pc_g_mm ~ fineness, data = bodydata)
summary(body_fineness)

ggplot(bodydata, aes(x = fineness, y = bodysize_pc_g_mm)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") #checking any relationship between fineness and body size are 
  
str(bodydata)
library(dplyr)


bodysize_summary <- bodydata %>%
  filter(Year %in% c(2019, 2020, 2021)) %>%         # keep only 2019–2021
  group_by(Year, RunTimingGroup) %>%                # group by year and timing group
  summarise(
    mean_bodysize = mean(bodysize_pc_g_mm, na.rm = TRUE),  # calculate mean
    n = n()                                               # number of fish per group
  ) %>%
  arrange(Year, RunTimingGroup)  # sort nicely

bodysize_summary <- as.data.frame(bodysize_summary)
str(bodysize_summary)

bodysize_summary$RunTimingGroup <- factor(
  bodysize_summary$RunTimingGroup,
  levels = c("Early", "Middle", "Late")
) #relevel run timing

str(bodysize_summary)



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

find("select")
#detach("package:raster", unload = TRUE)


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
  bodysize_summary,
  flow_summary,
  by = c("Year", "RunTimingGroup")
)

# Check the result
print(merged_data) #now data is all merged nicely together


#linear model, no year effect
model <- lm(mean_bodysize ~ mean_flow_cfs, data = merged_data)
summary(model) #multiple r squared = 0.3427, p = 0.0977

ggplot(merged_data, aes(x = mean_flow_cfs, y = mean_bodysize, color = factor(Year))) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Mean River Flow (cfs)",
    y = "Mean Body Size (pc_g_mm)",
    color = "Year",
    title = "Relationship between Mean Body Size and River Flow"
  ) +
  theme_minimal(base_size = 14) #this is without considering year as af factor in the linear regression




#model with year as factor
model2 <- lm(mean_bodysize ~ mean_flow_cfs + factor(Year) , data = merged_data)
summary(model2) #r squared = 0.8166

# Generate predictions from model2 and then graph them 
merged_data$predicted <- predict(model2)

library(ggplot2)
ggplot(merged_data, aes(x = mean_flow_cfs, y = mean_bodysize, color = factor(Year))) +
  geom_point(size = 3) +
  geom_line(aes(y = predicted, group = Year), linewidth = 1) +
  labs(
    x = "Mean River Flow (cfs)",
    y = "Mean Body Size (pc_g_mm)",
    color = "Year",
    title = "Mean Body Size vs River Flow, by Year"
  ) +
  theme_minimal(base_size = 14)



#model with year as factor and run timing group
model3 <- lm(mean_bodysize ~ mean_flow_cfs + factor(Year) + factor(RunTimingGroup), data = merged_data)
summary(model3) # Multiple R-squared:  0.9933

# Generate predictions from model3 and then graph them 
merged_data$predicted <- predict(model3)

library(ggplot2)

#plot for model 3
ggplot(merged_data, aes(x = mean_flow_cfs, y = mean_bodysize, color = factor(Year))) +
  geom_point(size = 3) +  # actual data points
  geom_line(aes(y = predicted, group = Year), linewidth = 1) +  # model-predicted lines
  geom_text(aes(label = RunTimingGroup),
            color = "black",
            hjust = 1.2,
            vjust = 0.5,# nudges label above the point
            size = 4,
            show.legend = FALSE) +   # label size
  labs(
    x = "Mean River Flow (cfs)",
    y = "Mean Body Size (pc_g_mm)",
    color = "Year",
    title = "Mean Body Size vs River Flow, by Year and Run Timing"
  ) +
  theme_minimal(14) + 
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  # title, bold, centered
    axis.title.x = element_text(size = 14, face = "bold"),             # x-axis title
    axis.title.y = element_text(size = 14, face = "bold"),             # y-axis title
    axis.text = element_text(size = 12),                                # axis tick labels
    legend.title = element_text(size = 12, face = "bold"),             # legend title
    legend.text = element_text(size = 11)                               # legend text
  )


#want to add model as well as multiple r-squared
# Get R-squared
r2 <- summary(model3)$r.squared

# Make a simple text string with model formula and R²
model_text <- paste0(
  "Model: mean_bodysize ~ mean_flow_cfs + Year + RunTimingGroup\n",
  "R² = ", round(r2, 3)
)

ggplot(merged_data, aes(x = mean_flow_cfs, y = mean_bodysize, color = factor(Year))) +
  geom_point(size = 3) +
  geom_line(aes(y = predicted, group = Year), linewidth = 1) +
  geom_text(aes(label = RunTimingGroup),
            color = "black",
            hjust = 1.2,
            vjust = 0.5,
            size = 4,
            show.legend = FALSE) +
  annotate("text", 
           x = max(merged_data$mean_flow_cfs),  # move to right side
           y = max(merged_data$mean_bodysize),  # top of plot
           label = model_text,
           hjust = 1,
           vjust = 1.35, # right-align
           size = 4.5,
           fontface = "bold") +
  labs(
    x = "Mean River Flow (cfs)",
    y = "Mean Body Size (pc_g_mm)",
    color = "Year",
    title = "Mean Body Size vs River Flow, by Year and Run Timing"
  ) +
  theme_minimal(14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 14)
  )
