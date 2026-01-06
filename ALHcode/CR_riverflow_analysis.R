### Copper River Flow Data Figures 2011-2021

rm(list = ls())
FlowData <- read.csv("ALHcode/odata/million_dollar_bridge_discharge_data_1988-2025.csv") #7523 observations, 12 variables

# Load data
library(dplyr)
library(ggplot2)
library(lubridate)

FlowAll <- FlowData %>%
  mutate(
    Date = mdy(time),
    Year = year(Date),
    DOY  = yday(Date)
  ) %>%
  filter(
    Year %in% 2012:2021,
    Date >= make_date(Year, 5, 1),
    Date <= make_date(Year, 11, 1)
  )

# Peak per year (based on actual date, but plotted on DOY)
PeakFlow <- FlowAll %>%
  group_by(Year) %>%
  filter(value == max(value)) %>%
  slice(1)


year_colors <- c(
  "#693829FF",
  "#894B33FF",
  "#A56A3EFF",
  "#CFB267FF",
  "#D9C5B6FF",
  "#9CA9BAFF",
  "#2F3E52FF",
  "#08306B",
  "#1F78B4",
  "#6BAED6"
)





riverdischarge_overlay <- ggplot(
  FlowAll,
  aes(x = DOY, y = value, color = factor(Year))
) +
  
  # Shaded sampling window (June 1 – Sept 1 on DOY axis)
  geom_rect(
    data = data.frame(
      xmin = yday(as.Date("2020-06-01")),
      xmax = yday(as.Date("2020-09-01")),
      ymin = -Inf, ymax = Inf
    ),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = "gray85", alpha = 0.5
  ) +
  scale_color_manual(values = year_colors) + 
  # ll years overlaid
  geom_line(linewidth = 2, alpha = 1) +
  
  # Peak points
  geom_point(
    data = PeakFlow,
    aes(x = DOY, y = value),
    inherit.aes = FALSE,
    size = 6,
    color = "red"
  ) +
  
  scale_x_continuous(
    breaks = yday(as.Date(paste0("2020-", c("04-01","05-01","06-01","07-01","08-01","09-01", "10-01", "11-01")))),
    labels = c("Apr","May","Jun","Jul","Aug","Sep", "Oct", "Nov")
  ) +
  
  scale_y_continuous(
    labels = function(x) format(x, scientific = FALSE),
    expand = expansion(mult = c(0, 0.12))
  ) +
  
  labs(
    x = "Month",
    y = "Discharge (ft³/s)",
    color = "Year"
  ) +
  
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 24),
    axis.title = element_text(size = 30, face = "bold"),
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 24),
    legend.title = element_text(size = 30, face = "bold"),
    legend.position = "right"
  ) + 
  guides(color = guide_legend(override.aes = list(linewidth = 4)))

riverdischarge_overlay

ggsave(
  filename = "ALHcode/AIC/AICFigures/CopperRiver_AnnualDischarge.jpeg",    # output file name
  plot = riverdischarge_overlay,              # the ggplot object
  width = 22,                     # width in inches
  height = 12,                    # height in inches
  dpi = 300                        # resolution (higher = better quality)
)




#mean flow from 2012-2021 in form of one single line
MeanFlow <- FlowAll %>%
  group_by(DOY) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE)
  )

PeakMean <- MeanFlow %>%
  filter(mean_value == max(mean_value)) %>%
  slice(1)

riverdischarge_mean <- ggplot(
  MeanFlow,
  aes(x = DOY, y = mean_value)
) +
  
  # Shaded sampling window (same as other plot)
  geom_rect(
    data = data.frame(
      xmin = yday(as.Date("2020-06-01")),
      xmax = yday(as.Date("2020-09-01")),
      ymin = -Inf, ymax = Inf
    ),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = "gray85", alpha = 0.5
  ) +
  
  # 0-year mean hydrograph
  geom_line(
    color = "navy",
    linewidth = 4
  ) +
  # Peak of the mean hydrograph
  geom_point(
    data = PeakMean,
    aes(x = DOY, y = mean_value),
    inherit.aes = FALSE,
    size = 7,
    color = "red"
  ) +
  
  scale_x_continuous(
    breaks = yday(as.Date(paste0(
      "2020-", c("04-01","05-01","06-01","07-01",
                 "08-01","09-01","10-01","11-01")
    ))),
    labels = c("Apr","May","Jun","Jul","Aug","Sep","Oct","Nov")
  ) +
  
  scale_y_continuous(
    labels = function(x) format(x, scientific = FALSE),
    expand = expansion(mult = c(0, 0.12))
  ) +
  
  labs(
    x = "Month",
    y = "Mean Discharge (ft³/s)"
  ) +
  
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 24),
    axis.title = element_text(size = 30, face = "bold"),
    plot.title = element_text(size = 30, face = "bold", hjust = 0.5)
  )

riverdischarge_mean

ggsave(
  filename = "ALHcode/AIC/AICFigures/CopperRiver_MeanDischarge_2012-2021.jpeg",    # output file name
  plot = riverdischarge_mean,              # the ggplot object
  width = 22,                     # width in inches
  height = 12,                    # height in inches
  dpi = 300                        # resolution (higher = better quality)
)





#### For just 2019-2021
#download data
rm(list = ls())
FlowData <- read.csv("ALHcode/odata/million_dollar_bridge_discharge_data_1988-2025.csv") #7523 observations, 12 variables


#structure
str(FlowData) #value is integer, time is character

#packages
library(dplyr)
library(ggplot2)
library(lubridate)

#convert time to data via lubridate
FlowData <- FlowData %>%
  mutate(Date = mdy(time))  # convert to Date format
str(FlowData)

#filter data for each year of interest
Flow2011 <- FlowData %>% filter(year(Date) == 2011)
Flow2012 <- FlowData %>% filter(year(Date) == 2012)
Flow2013 <- FlowData %>% filter(year(Date) == 2013)
Flow2014 <- FlowData %>% filter(year(Date) == 2014)
Flow2015 <- FlowData %>% filter(year(Date) == 2015)
Flow2016 <- FlowData %>% filter(year(Date) == 2016)
Flow2017 <- FlowData %>% filter(year(Date) == 2017)
Flow2018 <- FlowData %>% filter(year(Date) == 2018)
Flow2019 <- FlowData %>% filter(year(Date) == 2019)
Flow2020 <- FlowData %>% filter(year(Date) == 2020)
Flow2021 <- FlowData %>% filter(year(Date) == 2021)

#individual plots
ggplot(Flow2019, aes(x = Date, y = value)) +
  geom_line(color = "steelblue") +
  labs(title = "Discharge at Million Dollar Bridge - 2019",
       x = "Date", y = "Discharge (ft³/s)") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme_minimal()


ggplot(Flow2020, aes(x = Date, y = value)) +
  geom_line(color = "darkgreen") +
  labs(title = "Discharge at Million Dollar Bridge - 2020",
       x = "Date", y = "Discharge (ft³/s)") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme_minimal()

ggplot(Flow2021, aes(x = Date, y = value)) +
  geom_line(color = "firebrick") +
  labs(title = "Discharge at Million Dollar Bridge - 2021",
       x = "Date", y = "Discharge (ft³/s)") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme_minimal()



### Now visualize as facets with frozen y axis
library(dplyr)
library(ggplot2)

# Combine the data
FlowFacet <- FlowData %>%
  mutate(Date = mdy(time),
         Year = year(Date)) %>%
  filter(Year %in% c(2019, 2020, 2021)) %>%
  mutate(
    start_date = make_date(Year, 1, 1),
    end_date   = make_date(Year, 10, 1)
  )

# Faceted plot with shared (frozen) y-axis
ggplot(FlowFacet, aes(x = Date, y = value, color = as.factor(Year))) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~ Year, ncol = 1, scales = "free_x") +  # free_x fixes your issue
  labs(title = "Discharge at Million Dollar Bridge (2019–2021)",
       x = "Date", y = "Discharge (ft³/s)") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_color_manual(values = c("steelblue", "darkgreen", "firebrick")) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank()
  )


FlowFacet <- FlowData %>%
  mutate(Date = mdy(time),
         Year = year(Date)) %>%
  filter(
    Year %in% c(2019, 2020, 2021),
    Date >= make_date(Year, 4, 1),    # start April 1
    Date <= make_date(Year, 10, 25)   # end October 25
  )

ggplot(FlowFacet, aes(x = Date, y = value, color = as.factor(Year))) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~ Year, ncol = 1, scales = "free_x") +
  labs(title = "Discharge at Million Dollar Bridge (2019–2021)",
       x = "Date", y = "Discharge (ft³/s)") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_color_manual(values = c("steelblue", "darkgreen", "firebrick")) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank()
  )




### Plot with shaded regions for sampling season
ggplot(FlowFacet, aes(x = Date, y = value)) +
  # shaded region: June 1 – Aug 1 for each year
  geom_rect(
    data = distinct(FlowFacet, Year) %>%
      mutate(xmin = make_date(Year, 6, 1),
             xmax = make_date(Year, 8, 1),
             ymin = -Inf, ymax = Inf),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "gray85", alpha = 0.5, inherit.aes = FALSE
  ) +
  geom_line(aes(color = as.factor(Year)), linewidth = 1, show.legend = FALSE) +
  facet_wrap(~ Year, ncol = 1, scales = "free_x") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_color_manual(values = c("steelblue", "darkgreen", "firebrick")) +
  labs(
    title = "Discharge at Million Dollar Bridge (Apr 1 – Oct 25, 2019–2021)",
    y = "Discharge (ft³/s)",
    x = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold", size = 15),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 14),       # tick label size
    axis.title = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )






riverdischarge <- ggplot(FlowFacet, aes(x = Date, y = value)) +
  # shaded region: June 1 – Aug 1
  geom_rect(
    data = distinct(FlowFacet, Year) %>%
      mutate(xmin = make_date(Year, 6, 1),
             xmax = make_date(Year, 9, 1),
             ymin = -Inf, ymax = Inf),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "gray85", alpha = 0.5, inherit.aes = FALSE
  ) +
  geom_line(aes(color = as.factor(Year)), linewidth = 2, show.legend = FALSE) +
  # max point per year (take first if ties)
  geom_point(
    data = FlowFacet %>% group_by(Year) %>% filter(value == max(value)) %>% slice(1),
    aes(x = Date, y = value),
    color = "red", size = 6
  ) +
  geom_text(
    data = FlowFacet %>% group_by(Year) %>% filter(value == max(value)) %>% slice(1),
    aes(x = Date, y = value, label = value),
    vjust = -0.5, color = "red", size = 8
  ) +
  facet_wrap(~ Year, ncol = 1, scales = "free_x") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  scale_y_continuous(
    labels = function(x) format(x, scientific = FALSE),
    expand = expansion(mult = c(0, 0.15))  # add 10% space above
  ) +
  scale_color_manual(values = c("steelblue", "darkgreen", "firebrick")) +
  labs(
    y = "Discharge (ft³/s)",
    x = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold", size = 30),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 24),
    axis.title.y = element_text(size = 30, face = "bold", margin = margin(r = 10))
  )


 