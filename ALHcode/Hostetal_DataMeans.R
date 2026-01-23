lower <- read.csv("ALHcode/AIC/lowerriver_bodycomp_PCscores.csv")

mean(lower$Fish_Leng_1_mm[lower$Sex == "F"], na.rm = TRUE)
mean(lower$Fish_Leng_1_mm[lower$Sex == "M"], na.rm = TRUE)

upper <- read.csv("ALHcode/AIC/upperriver_bodycomp_PCscores.csv")

mean(upper$Fish_Leng_1_mm[lower$Sex == "F"], na.rm = TRUE)
mean(upper$Fish_Leng_1_mm[lower$Sex == "M"], na.rm = TRUE)

mean(lower$EnergyPDry_1[lower$Sex == "F"], na.rm = TRUE)
mean(lower$EnergyPDry_1[lower$Sex == "M"], na.rm = TRUE)


mean(upper$EnergyPDry_1[upper$Sex == "F"], na.rm = TRUE)
mean(upper$EnergyPDry_1[upper$Sex == "M"], na.rm = TRUE)


lower_TE <- read.csv("ALHcode/AIC/lowerriver_bodycomp_TotalEnergy_1.12.2026.csv")
mean(lower_TE$TotalEnergy_carcass_g[lower$Sex == "F"], na.rm = TRUE)
mean(lower_TE$TotalEnergy_carcass_g[lower$Sex == "M"], na.rm = TRUE)