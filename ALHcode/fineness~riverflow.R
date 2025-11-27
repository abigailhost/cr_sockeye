rm(list = ls())

### Fineness Ratio FR ~ River flow regression looking at run timing group categories/times 2019-2021

library(dplyr)
library(lubridate)

# Load data
fineness <- read.csv("ALHcode/AIC/lowerriver_bodycomp_fineness.csv")[-1]
riverflow <- read.csv("ALHcode/odata/million_dollar_bridge_discharge_data_1988-2025.csv")

# Clean fineness data
fineness <- fineness %>%
  mutate(
    Collection_Location = as.factor(Collection_Location),
    RunTimingGroup = factor(RunTimingGroup, levels = c("Early", "Middle", "Late")),
    Sex = as.factor(Sex),
    Date = as.Date(Collection_Date)  
  )

# Clean river flow data 
riverflow <- riverflow %>%
  mutate(Date = mdy(time))  # assuming `time` column looks like "06/01/2019"

# Merge by date 
# This will assign each fish the river flow measured on that same day
fineness_with_flow <- left_join(fineness, riverflow, by = "Date")

# Check what it looks like
glimpse(fineness_with_flow)

#make smaller data set
fineness_with_flow <- fineness_with_flow %>%
  rename(flow_cfs = value) %>%
  mutate(Year = lubridate::year(Date)) %>%   # <-- add this line
  select(
    Fish_ID_Revised,
    Collection_Location,
    RunTimingGroup,
    Sex,
    Date,
    Year,          # <-- keep Year
    fineness,
    flow_cfs
  )


#======= Fineness across genetic stock-associated migratory difficulty=======
rm(list = ls())
lowerriver_bodycomp_fineness <- "ALHcode/AIC/lowerriver_GSI_fineness.csv"

# Import the data. Be sure to check the structure of the data and that R is reading continuous and categorical variables appropriately.
fineness<-read.csv(lowerriver_bodycomp_fineness)[,-1]

#make response variables factors
fineness$Group_Assignment <- as.factor(fineness$Group_Assignment) 
fineness$RunTimingGroup <- as.factor(fineness$RunTimingGroup)
fineness$Year<- as.factor(fineness$Year)
fineness$Sex<- as.factor(fineness$Sex)
str(fineness)

library(ggplot2)

fineness_noGH <- fineness %>%
  filter(!Collection_Location %in% c("GulkanaHatchery"))

ggplot(fineness, aes(x = Work_km2, y = fineness)) +
  geom_point(alpha = 0.6) +
  geom_smooth(
    data = fineness_noGH,
    aes(x = Work_km2, y = fineness),
    method = "lm",
    se = TRUE,
    color = "black",
    fill = "lightgrey",
    alpha = 0.4,
    linewidth = 1.5,
    inherit.aes = FALSE,
    show.legend = FALSE
  )  +
  facet_wrap(~Year) +
  labs(
    x = "Migratory Work (km²)",
    y = "Fineness Ratio",
    title = "Fineness vs. Migratory Work Across Early Migratory Fish"
  ) +
  theme_minimal() +
  theme(
    panel.spacing.x = unit(2, "lines"),
    panel.spacing = unit(1.5, "lines"),
    # Facet header styling
    strip.text = element_text(size = 30, face = "bold", color = "white"),
    strip.background = element_rect(fill = "black", color = "black"),
    # Axis title margins
    axis.title.x = element_text(size = 30, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 30, face = "bold", margin = margin(r = 15)),
    #legend edits
    legend.title = element_text(size =24, face = "bold"),
    legend.text = element_text(size =20),
    # Axis text
    axis.text = element_text(size = 24, color = "black"),
    axis.text.x = element_text(size = 24, color = "black",
                               angle = 45, hjust = 1),
    # Panel border
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    # Grid tweaks
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    plot.title = element_text(size = 30, face = "bold")
  )

#plot with stats
# Global model 
global_model <- lm(fineness ~ Work_km2 + Year, data = fineness_noGH)
summary(global_model)
global_summary <- broom::glance(global_model)

global_R2 <- round(global_summary$r.squared, 3)
global_p <- global_summary$p.value
global_p_label <- ifelse(global_p < 0.001, "p < 0.001", paste0("p = ", signif(global_p, 3)))

# Year-specific Work p-values
year_stats <- fineness_noGH %>%
  group_by(Year) %>%
  do({
    mod <- lm(fineness ~ Work_km2, data = .)
    tidy_mod <- broom::tidy(mod)
    data.frame(
      p_work = signif(tidy_mod$p.value[tidy_mod$term == "Work"], 3)
    )
  })

# Add per-panel y-position 
year_stats <- fineness_noGH %>%
  group_by(Year) %>%
  summarize(
    p_work = signif(lm(fineness ~ Work_km2)$coefficients["Work_km2"], 3),
    .groups = "drop"
  ) %>%
  mutate(
    label = paste0("Global R² = ", global_R2, "\n",
                   "Global ", global_p_label, "\n",
                   "Year-specific p = ", p_work)
  )

# Plot 
ggplot(fineness_noGH, aes(x = Work_km2, y = fineness)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "black", fill = "lightgrey", alpha = 0.4, linewidth = 1.5) +
  facet_wrap(~Year) +
  geom_text(
    data = year_stats,
    aes(x = -Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = 0,      # left-align all lines
    vjust = 1.1,    # just above panel
    lineheight = 0.9, # optional: tighter spacing
    size = 4
  ) +
  labs(
    x = "Migratory Work (km²)",
    y = "Fineness Ratio",
    title = "Fineness vs. Migratory Work Across Early Migratory Sockeye"
  ) +
  theme_minimal() +
  theme(
    panel.spacing.x = unit(2, "lines"),
    panel.spacing = unit(1.5, "lines"),
    strip.text = element_text(size = 20, face = "bold", color = "white"),
    strip.background = element_rect(fill = "black", color = "black"),
    axis.title.x = element_text(size = 20, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 20, face = "bold", margin = margin(r = 15)),
    axis.text = element_text(size = 16, color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    plot.title = element_text(size = 22, face = "bold")
  )


#Year has a strong effect on fineness: 2021 fish have lower fineness than 2020.
#Migratory work has a small but statistically significant positive effect.
#About 51% of fineness variation is explained by this model.
#The model as a whole is highly significant.




# ======================== Fineness AIC, looking at effects of riverflow and discharge via AIC for EARLY MIGRATION FISH ============
rm(list = ls())
fineness_GSI <- read.csv("ALHcode/AIC/lowerriver_GSI_fineness.csv")
riverflow <- read.csv("ALHcode/odata/million_dollar_bridge_discharge_data_1988-2025.csv")

library(lubridate)
# Clean fineness data
fineness_GSI <- fineness_GSI %>%
  mutate(
    Collection_Location = as.factor(Collection_Location),
    Group_Assignment = as.factor(Group_Assignment),
    Sex = as.factor(Sex),
    Date = as.Date(Collection_Date)  
  )

#Clean river flow data
riverflow <- riverflow %>%
  mutate(Date = mdy(time))  # assuming `time` column looks like "06/01/2019"

# Merge by date
# This will assign each fish the river flow measured on that same day
fineness_with_flow <- left_join(fineness_GSI, riverflow, by = "Date")

#make smaller data set
fineness_with_flow <- fineness_with_flow %>%
  rename(flow_cfs = value) %>%
  mutate(Year = lubridate::year(Date)) %>%   # <-- add this line
  select(
    Fish_ID_Revised,
    Collection_Location,
    Sex,
    Date,
    Year,
    Work_km2,
    fineness,
    flow_cfs, 
    Group_Assignment
  )


library(AICcmodavg)

fineness_with_flow$Group_Assignment <-as.factor(fineness_with_flow$Group_Assignment)

levels(fineness_with_flow$Group_Assignment)[levels(fineness_with_flow$Group_Assignment) == "TanadaCopperLakes"] <- "Tanada Copper Lakes"
levels(fineness_with_flow$Group_Assignment)[levels(fineness_with_flow$Group_Assignment) == "GulkanaHatchery"] <- "Gulkana Hatchery"
levels(fineness_with_flow$Group_Assignment)[levels(fineness_with_flow$Group_Assignment) == "KlutinaTonsinaOutlet"] <- "Klutina Tonsina Outlet"
levels(fineness_with_flow$Group_Assignment)[levels(fineness_with_flow$Group_Assignment) == "KlutinaLake"] <- "Klutina Lake"

# Compare models to determine what has strongest effect on variation in fineness
fineness_with_flow_noGH <- fineness_with_flow %>%
  filter(!Group_Assignment %in% c("Gulkana Hatchery")) #cuts sample size by nearly 20, removes GH which corresponds with other work analyses

fineness1 <- lm(fineness ~ 1, data = fineness_with_flow_noGH)
fineness2 <- lm(fineness ~ Sex, data = fineness_with_flow_noGH)
fineness3 <- lm(fineness ~ Sex + flow_cfs, data = fineness_with_flow_noGH)
fineness4 <- lm(fineness ~ Sex + Work_km2, data = fineness_with_flow_noGH)

#table of AIC results
mynames1 <- paste("model", as.character(1:4), sep = "")
models <- list(fineness1, fineness2, fineness3, fineness4)

# Generate AICc table
myaicc1 <- aictab(models, modnames = mynames1)
print(myaicc1)

summary(fineness1)
summary(fineness2)
summary(fineness3) #Multiple R-squared:  0.4265
summary(fineness4)



#graphing for analysis
colors_GSI <- c(
  "Bremner" = "#E0B300",   
  "Chitina" = "#FFE5CCFF", #"black" if need to remove 
  "Gulkana" = "#A50021FF",   
  "Gulkana Hatchery" = "black", #D32826FF when included 
  "Tazlina" = "#FF5C5C",   # reddish
  "Slana" = "#CC5800FF",
  "Klutina Lake" = "#FF8E32FF", 
  "Klutina Tonsina Outlet" = "#E56B00", 
  "Tanada Copper Lakes" = "#993F00FF")


# FIGURE FOR FINESS ~ FLOW
fineness.flow <- ggplot() +
  
  geom_smooth(
    data = fineness_with_flow_noGH,
    aes(x = flow_cfs, y = fineness),
    method = "lm",
    se = TRUE,
    color = "black",
    fill = "lightgrey",
    alpha = 0.4,
    linewidth = 1.5,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  
  # All stocks except GulkanaHatchery
  geom_point(
    data = fineness_with_flow_noGH,
    aes(
      x = flow_cfs,
      y = fineness,
      color = Group_Assignment,
      shape = Group_Assignment
    ),
    size = 12,
    alpha = 1
  ) +
  
  # GulkanaHatchery: X will come from shape scale
  geom_point(
    data = fineness_with_flow %>% filter(Group_Assignment == "Gulkana Hatchery"),
    aes(
      x = flow_cfs,
      y = fineness,
      color = Group_Assignment,
      shape = Group_Assignment
    ),
    size = 10,
    stroke = 1.5
  ) +
  
  scale_color_manual(values = colors_GSI) +
  scale_shape_manual(values = c(
    "Bremner" = 16,
    "Chitina" = 16,
    "Gulkana" = 16,
    "Gulkana Hatchery" = 4,   # X shape
    "Tazlina" = 16,
    "Slana" = 16,
    "Klutina Lake" = 16,
    "Klutina Tonsina Outlet" = 16,
    "Tanada Copper Lakes" = 16
  )) +
  
  labs(
    x = "River discharge (cfs)",
    y = "Fineness",
    color = "Genetic Stock",
    shape = "Genetic Stock"
  ) +
  theme_minimal() +
  theme(
    panel.spacing.x = unit(2, "lines"),
    panel.spacing = unit(1.5, "lines"),
    strip.text = element_text(size = 30, face = "bold", color = "white"),
    strip.background = element_rect(fill = "black", color = "black"),
    axis.title.x = element_text(size = 30, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 30, face = "bold", margin = margin(r = 15)),
    legend.title = element_text(size = 24, face = "bold"),
    legend.text = element_text(size = 24),
    axis.text = element_text(size = 24, color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90")
  )

fineness.flow

#add stats for fineness.flow
library(dplyr)
library(broom)

# Fit linear model excluding Chitina
fineness3 <- lm(fineness ~ Sex + flow_cfs, data = fineness_with_flow_noGH)

# 2. Extract R² 
model_stats <- broom::glance(fineness3) %>%
  mutate(
    label = paste0("R² = ", round(r.squared, 3)),
    x = min(fineness_with_flow_noGH$flow_cfs),   # x position for label
    y = max(fineness_with_flow_noGH$fineness) # y position for label
  )

# 3. Add the label to your plot
fineness.flow.stats <- fineness.flow +
  geom_text(
    data = model_stats,
    aes(x = x, y = y, label = label),
    hjust = 0, vjust = 2,    # top-left corner
    size = 10
  )

fineness.flow.stats 

ggsave(
  filename = "ALHcode/AIC/AICFigures/EarlyMigr_Fineness_YearAndCfs.jpeg",    # output file name
  plot = fineness.flow.stats,              # the ggplot object
  width = 18,                     # width in inches
  height = 12,                    # height in inches
  dpi = 300                        # resolution (higher = better quality)
)






# FIGURE FOR FINENESS~WORK
fineness.work <- ggplot() +
  
  geom_smooth(
    data = fineness_with_flow_noGH,
    aes(x = Work_km2, y = fineness),
    method = "lm",
    se = TRUE,
    color = "black",
    fill = "lightgrey",
    alpha = 0.4,
    linewidth = 1.5,
    inherit.aes = FALSE,
    show.legend = FALSE
  )  + # All stocks except GulkanaHatchery 
geom_point(
  data = fineness_with_flow_noGH,
  aes(
    x = Work_km2,
    y = fineness,
    color = Group_Assignment,
    shape = Group_Assignment
  ),
  size = 12,
  alpha = 1
) +
  # GulkanaHatchery: X will come from shape scale
  geom_point(
    data = fineness_with_flow %>% filter(Group_Assignment == "Gulkana Hatchery"),
    aes(
      x = Work_km2,
      y = fineness,
      color = Group_Assignment,
      shape = Group_Assignment
    ),
    size = 10,
    stroke = 1.5
  ) +
  
  scale_color_manual(values = colors_GSI) +
  scale_shape_manual(values = c(
    "Bremner" = 16,
    "Chitina" = 16,
    "Gulkana" = 16,
    "Gulkana Hatchery" = 4,   # X shape
    "Tazlina" = 16,
    "Slana" = 16,
    "Klutina Lake" = 16,
    "Klutina Tonsina Outlet" = 16,
    "Tanada Copper Lakes" = 16
  )) +
  labs(
    x = expression(bold("Migratory Work (" * km^2 * ")")),
    y = "Fineness",
    color = "Genetic Stock",
    shape = "Genetic Stock"
  ) +
  theme_minimal() +
  theme(
    panel.spacing.x = unit(2, "lines"),
    panel.spacing = unit(1.5, "lines"),
    strip.text = element_text(size = 30, face = "bold", color = "white"),
    strip.background = element_rect(fill = "black", color = "black"),
    axis.title.x = element_text(size = 30, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 30, face = "bold", margin = margin(r = 15)),
    legend.title = element_text(size = 24, face = "bold"),
    legend.text = element_text(size = 24),
    axis.text = element_text(size = 24, color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90")
  )

fineness.work

#add stats for fineness.flow
library(dplyr)
library(broom)

# Fit linear model excluding Chitina
fineness4 <- lm(fineness ~ Sex + Work_km2, data = fineness_with_flow_noGH)

# 2. Extract R² 
model_stats <- broom::glance(fineness4) %>%
  mutate(
    label = paste0("R² = ", round(r.squared, 3)),
    x = min(fineness_with_flow_noGH$Work_km2),   # x position for label
    y = max(fineness_with_flow_noGH$fineness) # y position for label
  )

# 3. Add the label to your plot
fineness.work.stats <- fineness.work +
  geom_text(
    data = model_stats,
    aes(x = x, y = y, label = label),
    hjust = 0, vjust = 2,    # top-left corner
    size = 10
  )

fineness.work.stats 

ggsave(
  filename = "ALHcode/AIC/AICFigures/EarlyMigr_Fineness_YearAndWork.jpeg",    # output file name
  plot = fineness.work.stats,              # the ggplot object
  width = 18,                     # width in inches
  height = 12,                    # height in inches
  dpi = 300                        # resolution (higher = better quality)
)















# ======================== Fineness AIC, looking at effects of riverflow and discharge via AIC for EARLY MIGRATION FISH ============
rm(list = ls())
fineness_SP <- read.csv("ALHcode/AIC/upperriver_bodycomp_fineness.csv")
riverflow <- read.csv("ALHcode/odata/million_dollar_bridge_discharge_data_1988-2025.csv")

# Clean fineness data
fineness_SP <- fineness_SP %>%
  mutate(
    Collection_Location = as.factor(Collection_Location),
    Sex = as.factor(Sex),
    Date = as.Date(Collection_Date)  
  )

fineness_SP$Work_km2 <- round((fineness_SP$Collection_RiverMile_m * fineness_SP$Elevation_m) / (1000 * 1000), digits = 1)

#Clean river flow data
riverflow <- riverflow %>%
  mutate(Date = mdy(time))  # assuming `time` column looks like "06/01/2019"

# Merge by date
# This will assign each fish the river flow measured on that same day
fineness_with_flow <- left_join(fineness_SP, riverflow, by = "Date")

#make smaller data set
fineness_with_flow <- fineness_with_flow %>%
  rename(flow_cfs = value) %>%
  mutate(Year = lubridate::year(Date)) %>%   # <-- add this line
  select(
    Fish_ID_Revised,
    Collection_Location,
    Sex,
    Date,
    Year,
    Work_km2,
    fineness,
    flow_cfs
  )


library(AICcmodavg)

# Compare models to determine what has strongest effect on variation in fineness
fineness_with_flow_noGHnoLL <- fineness_with_flow %>%
  filter(!Collection_Location %in% c("Gulkana Hatchery", "Long Lake")) #cuts sample size by nearly 20, removes GH which corresponds with other work analyses

fineness1 <- lm(fineness ~ 1, data = fineness_with_flow_noGHnoLL)
fineness2 <- lm(fineness ~ Year + Sex, data = fineness_with_flow_noGHnoLL)
fineness3 <- lm(fineness ~ Year + Sex + Work_km2, data = fineness_with_flow_noGHnoLL)

#table of AIC results
mynames1 <- paste("model", as.character(1:3), sep = "")
models <- list(fineness1, fineness2, fineness3)

# Generate AICc table
myaicc1 <- aictab(models, modnames = mynames1)
print(myaicc1)


summary(fineness1) #Multiple R-squared:  0.07162
summary(fineness2) #Multiple R-squared:  0.1119

#PLOT
colors_SP <- c(
  "Long Lake" = "black",
  "Fish Creek Gulkana" = "#A50021FF",   
  "Gulkana Hatchery" = "#D82632FF", 
  "St Anne" = "#FFAD65FF",
  "Mahlo" = "#FF8E32FF",
  "Mentasta" = "#CC5800FF", 
  "Tanada" = "#993F00FF"
)

# Shapes: X for GH & LL, filled circle for everyone else
shapes_SP <- c(
  "Long Lake" = 4,
  "Gulkana Hatchery" = 4,
  "Fish Creek Gulkana" = 16,
  "St Anne" = 16,
  "Mahlo" = 16,
  "Mentasta" = 16,
  "Tanada" = 16
)

fineness.work <- ggplot() +
  geom_smooth(
    data = fineness_with_flow_noGHnoLL,
    aes(x = Work_km2, y = fineness),
    method = "lm",
    se = TRUE,
    color = "black",
    fill = "lightgrey",
    alpha = 0.4,
    linewidth = 1.5,
    inherit.aes = FALSE
  ) +
  # Circle points for NON-GH and NON-LL
geom_point(
  data = fineness_with_flow %>% 
    filter(!Collection_Location %in% c("Gulkana Hatchery", "Long Lake")),
  aes(
    x = Work_km2,
    y = fineness,
    color = Collection_Location,
    shape = Collection_Location
  ),
  size = 10,
  alpha = 0.85
) +
  # X points for GH 
geom_point(
  data = fineness_with_flow %>% filter(Collection_Location == "Gulkana Hatchery"),
  aes(
    x = Work_km2,
    y = fineness,
    color = Collection_Location,
    shape = Collection_Location
  ),
  size = 8,
  stroke = 1.5
) +
  # X points for LL 
geom_point(
  data = fineness_with_flow %>% filter(Collection_Location == "Long Lake"),
  aes(
    x = Work_km2,
    y = fineness,
    color = Collection_Location,
    shape = Collection_Location
  ),
  size = 8,
  stroke = 1.5
) +
  scale_color_manual(values = colors_SP) +
  scale_shape_manual(values = shapes_SP) +
  
  labs(
    x = "Migratory Work (km²)",
    y = "Fineness",
    color = "Spawning Population",
    shape = "Spawning Population"
  ) +
  facet_wrap(~ Year, strip.position = "top") +
  theme_minimal() +
  theme(
    panel.spacing.x = unit(2, "lines"),
    panel.spacing = unit(1.5, "lines"),
    strip.text = element_text(size = 30, face = "bold", color = "white"),
    strip.background = element_rect(fill = "black", color = "black"),
    axis.title.x = element_text(size = 30, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_text(size = 30, face = "bold", margin = margin(r = 15)),
    legend.title = element_text(size = 30, face = "bold"),
    legend.text = element_text(size = 24),
    axis.text = element_text(size = 24, color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90")
  )

fineness.work

#now add stats to figure
# global model
work_model <- lm(fineness ~ Year + Sex + Work_km2, 
                 data = fineness_with_flow_noGHnoLL)


# 2. Extract R² 
model_stats <- broom::glance(work_model) %>%
  mutate(
    label = paste0("R² = ", round(r.squared, 3)),
    x = min(fineness_with_flow_noGHnoLL$Work_km2),   # x position for label
    y = max(fineness_with_flow_noGHnoLL$fineness) # y position for label
  )

# 3. Add the label to your plot
fineness.work.stats <- fineness.work +
  geom_text(
    data = model_stats,
    aes(x = x, y = y, label = label),
    hjust = .25, vjust = 2,    # top-left corner
    size = 10
  )


fineness.work.stats


ggsave(
  filename = "ALHcode/AIC/AICFigures/PostMigr_Fineness_Work.jpeg",    # output file name
  plot = fineness.work.stats,              # the ggplot object
  width = 18,                     # width in inches
  height = 12,                    # height in inches
  dpi = 300                        # resolution (higher = better quality)
)

