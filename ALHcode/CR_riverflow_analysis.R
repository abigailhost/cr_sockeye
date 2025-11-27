### Copper River Flow Data Figures 2019-2021


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


ggsave(
  filename = "ALHcode/AIC/AICFigures/CopperRiver_AnnualDischarge.jpeg",    # output file name
  plot = riverdischarge,              # the ggplot object
  width = 18,                     # width in inches
  height = 12,                    # height in inches
  dpi = 300                        # resolution (higher = better quality)
)
 