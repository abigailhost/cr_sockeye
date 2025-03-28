rm(list = ls())
DataSet <- read.csv("ALHcode/AIC/upperriver_bodycomp_all.csv")
str(DataSet)
DataSet$SpawnStatus <- as.factor(DataSet$SpawnStatus)
DataSet$Gonad_Wt <- as.numeric(DataSet$Gonad_Wt)
str(DataSet)
max(DataSet$Gonad_Wt, na.rm = TRUE)
which(DataSet$Gonad_Wt == max(DataSet$Gonad_Wt, na.rm = TRUE))
DataSet <- DataSet[-146,] #eliminates the maximum 6.0 value that seems very out of place for weight
DataSet$Gonad_Wt_g <- (DataSet$Gonad_Wt*1000)


# Filter the data for SpawnStatus 0 (pre-spawn) and 2 (post-spawn)
pre_spawn_data <- DataSet[DataSet$SpawnStatus == 0, ]
partialpost_spawn_data <- DataSet[DataSet$SpawnStatus %in% c(1, 2), ]
post_spawn_data <- DataSet[DataSet$SpawnStatus == 2, ]

str(post_spawn_data)
mean(post_spawn_data$Gonad_Wt_g, na.rm = T) #mean weight is 20.66667 grams
mean(pre_spawn_data$Gonad_Wt_g, na.rm = T) #mean weight is 137.3151 grams
mean(partialpost_spawn_data$Gonad_Wt_g, na.rm = T) #mean weight is 35.375 grams

# Create histograms in base R
par(mfrow = c(1, 3))  # Arrange plots in one row and two columns

# Histogram for pre-spawn (SpawnStatus == 0)
hist(pre_spawn_data$Gonad_Wt_g, 
     main = "Pre-Spawn Gonad Weight", 
     xlab = "Gonad Weight (g)", 
     col = "lightblue", 
     border = "black", 
     breaks = 20)  # Adjust 'breaks' as needed

hist(partialpost_spawn_data$Gonad_Wt_g, 
     main = "Partial and Post-Spawn Gonad Weight", 
     xlab = "Gonad Weight (g)", 
     col = "lightgreen", 
     border = "black", 
     breaks = 20)

# Histogram for post-spawn (SpawnStatus == 2)
hist(post_spawn_data$Gonad_Wt_g, 
     main = "Post-Spawn Gonad Weight", 
     xlab = "Gonad Weight (g)", 
     col = "salmon", 
     border = "black", 
     breaks = 20)  # Adjust 'breaks' as needed

# Reset the plotting layout to default (single plot)
par(mfrow = c(1, 1))



##SEX Differences

# Get the min and max range for Gonad_Wt_g across all groups
all_data <- c(pre_spawn_data$Gonad_Wt_g, 
              partialpost_spawn_data$Gonad_Wt_g, 
              post_spawn_data$Gonad_Wt_g)

x_range <- range(all_data, na.rm=T)  # Get min and max values

# Set layout to 2 rows and 3 columns for 6 plots
par(mfrow = c(2, 3))

# Males - Pre-Spawn
hist(pre_spawn_data$Gonad_Wt_g[pre_spawn_data$Sex == "M"], 
     main = "Males: Pre-Spawn Gonad Weight", 
     xlab = "Gonad Weight (g)", 
     col = "lightblue", 
     border = "black", 
     breaks = 20, 
     xlim = x_range)

# Males - Partial and Post-Spawn
hist(partialpost_spawn_data$Gonad_Wt_g[partialpost_spawn_data$Sex == "M"], 
     main = "Males: Partial/Post-Spawn Gonad Weight", 
     xlab = "Gonad Weight (g)", 
     col = "lightgreen", 
     border = "black", 
     breaks = 20, 
     xlim = x_range)

# Males - Post-Spawn
hist(post_spawn_data$Gonad_Wt_g[post_spawn_data$Sex == "M"], 
     main = "Males: Post-Spawn Gonad Weight", 
     xlab = "Gonad Weight (g)", 
     col = "salmon", 
     border = "black", 
     breaks = 20, 
     xlim = x_range)

# Females - Pre-Spawn
hist(pre_spawn_data$Gonad_Wt_g[pre_spawn_data$Sex == "F"], 
     main = "Females: Pre-Spawn Gonad Weight", 
     xlab = "Gonad Weight (g)", 
     col = "lightblue", 
     border = "black", 
     breaks = 20, 
     xlim = x_range)

# Females - Partial and Post-Spawn
hist(partialpost_spawn_data$Gonad_Wt_g[partialpost_spawn_data$Sex == "F"], 
     main = "Females: Partial/Post-Spawn Gonad Weight", 
     xlab = "Gonad Weight (g)", 
     col = "lightgreen", 
     border = "black", 
     breaks = 20, 
     xlim = x_range)

# Females - Post-Spawn
hist(post_spawn_data$Gonad_Wt_g[post_spawn_data$Sex == "F"], 
     main = "Females: Post-Spawn Gonad Weight", 
     xlab = "Gonad Weight (g)", 
     col = "salmon", 
     border = "black", 
     breaks = 20, 
     xlim = x_range)