rm(list = ls())

### Fineness Ratio FR ~ River flow regression looking at run timing group categories/times 2019-2021

library(dplyr)
library(lubridate)

# --- Load data ---
fineness <- read.csv("ALHcode/AIC/lowerriver_bodycomp_fineness.csv")[-1]
riverflow <- read.csv("ALHcode/odata/million_dollar_bridge_discharge_data_1988-2025.csv")

# --- Clean fineness data ---
fineness <- fineness %>%
  mutate(
    Collection_Location = as.factor(Collection_Location),
    RunTimingGroup = factor(RunTimingGroup, levels = c("Early", "Middle", "Late")),
    Sex = as.factor(Sex),
    Date = as.Date(Collection_Date)  
  )

# --- Clean river flow data ---
riverflow <- riverflow %>%
  mutate(Date = mdy(time))  # assuming `time` column looks like "06/01/2019"

# --- Merge by date ---
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


library(ggplot2)

ggplot(fineness_with_flow, aes(x = flow_cfs, y = fineness)) +
  geom_point(aes(color = RunTimingGroup), alpha = 0.6, size = 4) +
  geom_smooth(method = "lm", color = "black") +
  facet_wrap(~Year) +   # <-- facet by year
  labs(
    x = "River flow (cfs)",
    y = "Fineness ratio",
    title = "Fineness ratio vs. River flow across early migratory sockeye"
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

fineness_noCHnoGH <- fineness %>%
  filter(!Collection_Location %in% c("GulkanaHatchery", "Chitina"))

ggplot(fineness, aes(x = Work_km2, y = fineness)) +
  geom_point(alpha = 0.6) +
  geom_smooth(
    data = fineness_noCHnoGH,
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
# ---- Global model ----
global_model <- lm(fineness ~ Work_km2 + Year, data = fineness_noCHnoGH)
summary(global_model)
global_summary <- broom::glance(global_model)

global_R2 <- round(global_summary$r.squared, 3)
global_p <- global_summary$p.value
global_p_label <- ifelse(global_p < 0.001, "p < 0.001", paste0("p = ", signif(global_p, 3)))

# ---- Year-specific Work p-values ----
year_stats <- fineness_noCHnoGH %>%
  group_by(Year) %>%
  do({
    mod <- lm(fineness ~ Work_km2, data = .)
    tidy_mod <- broom::tidy(mod)
    data.frame(
      p_work = signif(tidy_mod$p.value[tidy_mod$term == "Work"], 3)
    )
  })

# ---- Add per-panel y-position ----
year_stats <- fineness_noCHnoGH %>%
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

# ---- Plot ----
ggplot(fineness_noCHnoGH, aes(x = Work_km2, y = fineness)) +
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











#========== Fineness across spawning population-associated migratory difficulty=======
rm(list = ls())
upperriver_bodycomp_fineness <- "ALHcode/AIC/upperriver_bodycomp_fineness.csv"

# Import the data. Be sure to check the structure of the data and that R is reading continuous and categorical variables appropriately.
fineness<-read.csv(upperriver_bodycomp_fineness)[,-1]

#make response variables factors
fineness$Group_Assignment <- as.factor(fineness$Group_Assignment) 
fineness$Year<- as.factor(fineness$Year)
fineness$Sex<- as.factor(fineness$Sex)
str(fineness)
fineness$Work <- round((fineness$Collection_RiverMile_m * fineness$Elevation_m) / (1000 * 1000), digits = 1)

finenessd_noLLnoGH <- fineness %>%
  filter(!Collection_Location %in% c("Gulkana Hatchery", "Long Lake"))

library(ggplot2)


summary(lm(fineness ~ Work + Year, data = finenessd_noLLnoGH)) #significant, but r-squared is LOW

ggplot(fineness, aes(x = Work, y = fineness)) +
  geom_point(alpha = 0.6) +
  geom_smooth(
    data = finenessd_noLLnoGH,
    aes(x = Work, y = fineness),
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
    title = "Fineness vs. Migratory Work Across Post-Migratory Sockeye"
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
# ---- Global model ----
global_model <- lm(fineness ~ Work + Year, data = finenessd_noLLnoGH)
global_summary <- broom::glance(global_model)

global_R2 <- round(global_summary$r.squared, 3)
global_p <- global_summary$p.value
global_p_label <- ifelse(global_p < 0.001, "p < 0.001", paste0("p = ", signif(global_p, 3)))

# ---- Year-specific Work p-values ----
year_stats <- finenessd_noLLnoGH %>%
  group_by(Year) %>%
  do({
    mod <- lm(fineness ~ Work, data = .)
    tidy_mod <- broom::tidy(mod)
    data.frame(
      p_work = signif(tidy_mod$p.value[tidy_mod$term == "Work"], 3)
    )
  })

# ---- Add per-panel y-position ----
year_stats <- year_stats %>%
  left_join(
    finenessd_noLLnoGH %>% group_by(Year) %>% summarize(y_max = max(fineness)),
    by = "Year"
  ) %>%
  mutate(
    y_pos = y_max * 1.05,  # slightly above the max for each panel
    label = paste0("Global R² = ", global_R2, "\n",
                   "Global ", global_p_label, "\n",
                   "Year-specific p = ", p_work)
  )

# ---- Plot ----
ggplot(finenessd_noLLnoGH, aes(x = Work, y = fineness)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "black", fill = "lightgrey", alpha = 0.4, linewidth = 1.5) +
  facet_wrap(~Year) +
  geom_text(
    data = year_stats,
    aes(x = max(finenessd_noLLnoGH$Work) * 0.2,
        y = max(finenessd_noLLnoGH$fineness)*1,
        label = label),
    inherit.aes = FALSE,
    size = 4,
    hjust = 0
  ) +
  labs(
    x = "Migratory Work (km²)",
    y = "Fineness Ratio",
    title = "Fineness vs. Migratory Work Across Post-Migratory Sockeye"
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
