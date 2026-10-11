# Part 1

library(readr)
surveys <- read_csv("~/MyBiometryWork/biometry_course/data/Reef_fish/NCRMP_reef_fish_surveys.csv")

species <- read_csv("~/MyBiometryWork/biometry_course/data/Reef_fish/NCRMP_reef_fish_species.csv")



#lines 9-__ are code for Part 1 of the midterm: creating figures for the results 
# of the data.

library(tidyverse)

# The code below creates new datasets for two of the species of interest, the 
# white grunt (Haelmulon plumeirii) and the yellowtail snapper (Ocyurus chryurus),
# as well as a dataset that includes both: soi means species of interest.
white_grunt <- species|>
  filter(SPECIES_CD == "HAE PLUM")
yt_snapper <- species|>
  filter(SPECIES_CD == "OCY CHRY")
soi<- species|>
  filter(SPECIES_CD == "HAE PLUM" | SPECIES_CD == "OCY CHRY")

# The code below calculates the mean abundance of each of the species
wg_mean_abundance <- mean(white_grunt$abundance)
# mean is 5.186848
yts_mean_abundance <- mean(yt_snapper$abundance)
# mean is 7.761658

# Next, I wanted to compare the abundances between the species of interest, but the
# ranges of the abundances were really large and there was a large skew toward 0. To
# account for this, I created new variables in the dataset that was the natural log
# of the abundance plus one in order to keep from getting -Inf. I then removed the 
# zero values from the histogram by only plotting log abundances greater than zero.

white_grunt$log_abundance <- log(white_grunt$abundance+1)
yt_snapper$log_abundance <- log(yt_snapper$abundance+1)

par(mfrow= c(1,2))
hist(white_grunt$log_abundance[white_grunt$log_abundance > 0], main = "White Grunt",
     xlab = "Log Abundance", ylab = "Frequency", breaks = 24)
hist(yt_snapper$log_abundance[yt_snapper$log_abundance > 0], main = "Yellowtail Snapper",
     xlab = "Log Abundance", ylab = "Frequency", breaks = 24)
par(mfrow = c(1,1))

# The plot below creates a rough map of South Florida, showing the study area, with
# the most northern points being Martin County and the southern most being the Lower
# Keys and Dry Tortugas National Park.
plot(reef_fish$longitude, reef_fish$latitude,
     main = "Study Area",
     xlab = "Longitude",
     ylab = "Latitude")
reef_fish <- reef_fish|>
  arrange(desc(latitude))

# The plots below show the relationship between the species richness and the total
# abundance. As expected, the species richness increases as the total abundance
# increases. For the second plot, I used the log scale of the total abundance because
# there was significantly greater range in the abundances than in the species
# richness for this survey.
plot(reef_fish$total_abundance, reef_fish$species_richness,
     main = "Total Abundance and Species Richness",
     xlab = "Total Abundance",
     ylab = "Species Richness")

plot(log(reef_fish$total_abundance), reef_fish$species_richness,
     main = "Log of Abundance and Species Richness",
     xlab = "Log Total Abundance",
     ylab = "Species Richness")


# Part 2:
library(readr)
twoa <- read_rds("https://github.com/bmaitner/biometry_course/raw/refs/heads/main/midterm/b_2a.RDS")
twob <- read_rds("https://github.com/bmaitner/biometry_course/raw/refs/heads/main/midterm/b_2b.RDS")
twoc <- read_rds("https://github.com/bmaitner/biometry_course/raw/refs/heads/main/midterm/b_2c.RDS")
# Plot for Dataset A
plot(log(twoa$Adult.Body.Mass.g), log(twoa$Egg.Mass.g),
     main = "Body Mass and Egg Mass",
     xlab = "Adult Body Mass",
     ylab = "Egg Mass")
mean(twoa$Adult.Body.Mass.g)
# mean is 502.674
median(twoa$Adult.Body.Mass.g)
# median in 41.4

# Plot for Dataset B

plot(log(twob$Total.Fish.Abundance), log(twob$Species.Richness),
     main = "Log Abundance and Richness",
     xlab = "Log Total Fish Abundance",
     ylab = "log Species Richness")
plot(twob$Total.Fish.Abundance, twob$Species.Richness,
     main = "Abundance and Species Richness",
     xlab = "Total Abundance",
     ylab = "Species Richness")
hist(twob$Total.Fish.Abundance,
     main = "Fish Abundance",
     xlab = "Total Abundance",
     ylab = "Frequency",
     breaks = 50)
mean(twob$Total.Fish.Abundance)
# mean is 199.3104
median(twob$Total.Fish.Abundance)
# median is 140.55
hist(twob$Species.Richness, 
     main = "Species Richness",
     xlab = "Richness",
     ylab = "Frequency",
     breaks = 50)
mean(twob$Species.Richness)
# mean is 21.784
median(twob$Species.Richness)
# median is 19

# Plot for Dataset C
plot(twoc$Rugosity, twoc$Hard.Bottom.Cover.Pct,
     main = "Rugosity and Hard Bottom Cover Percentage",
     xlab = "Rugosity",
     ylab = "Hard Bottom Cover %")
hist(twoc$Rugosity, 
     main = "Rugosity Frequencies",
     xlab = "Rugosity",
     ylab = "Frequecies",
     breaks = 25)
mean(twoc$Rugosity)
# mean is 0.5158
median(twoc$Rugosity)
# median is 0.363


# Part 3

# Sample size of 20
sample_size <- 20
a <- 2
b <- 1

nsim <- 400
sdvec <- seq(1, 20, by = 1)
power.sd <- numeric(length(sdvec))
pval <- numeric(nsim)


for(j in 1:length(sdvec)){
  for(i in 1:nsim){
    
    x <- sample(x = 1:20,
                size = sample_size,
                replace = TRUE)
    
    sd <- sdvec[j]
    y_det <- a + b*x
    y <- rnorm(n = length(y_det),
               mean = y_det,
               sd = sd)
    
    m <- lm(y ~ x)
    
    pval[i] <- coef(summary(m))["x","Pr(>|t|)"]
    
  }
  power.sd[j] <- sum(pval < 0.05)/nsim
  
}

plot(power.sd ~ sdvec, main = sample_size, ylim = c(0, 1))

mean(pval)
# mean of the p-values is 0.303871, which is greater than 0.05, so these results
# are not significant.

# Sample size of 10

sample_size <- 10
a <- 2
b <- 1

nsim <- 400
sdvec <- seq(1, 10, by = 1)
power.sd <- numeric(length(sdvec))
pval <- numeric(nsim)


for(j in 1:length(sdvec)){
  for(i in 1:nsim){
    
    x <- sample(x = 1:10,
                size = sample_size,
                replace = TRUE)
    
    sd <- sdvec[j]
    y_det <- a + b*x
    y <- rnorm(n = length(y_det),
               mean = y_det,
               sd = sd)
    
    m <- lm(y ~ x)
    
    pval[i] <- coef(summary(m))["x","Pr(>|t|)"]
    
  }
  power.sd[j] <- sum(pval < 0.05)/nsim
  
}

plot(power.sd ~ sdvec, main = sample_size, ylim = c(0, 1))

mean(pval)
# mean p-value is 0.4272999, which is greater than 0.05 and 0.303871, so it is 
# not significant and is even further away from significance than a sample size 
# of 20 was.


# Sample size of 100 and
sample_size <- 100
a <- 2
b <- 1

nsim <- 200
sdvec <- seq(1, 100, by = 1)
power.sd <- numeric(length(sdvec))
pval <- numeric(nsim)


for(j in 1:length(sdvec)){
  for(i in 1:nsim){
    x <- sample(x = 1:100,
                size = sample_size,
                replace = TRUE)
    
    stand_dev <- sdvec[j]
    y_det <- a + b*x
    y <- rnorm(n = length(y_det),
               mean = y_det,
               sd = stand_dev)
    
    m <- lm(y ~ x)
    
    pval[i] <- coef(summary(m))["x","Pr(>|t|)"]
    
  }
  power.sd[j] <- sum(pval < 0.05)/nsim
  
}

plot(power.sd ~ sdvec, main = sample_size, ylim = c(0, 1))

mean(pval)
# mean is 0.04335932, which is significant








citation("readr")
citation("dplyr")
citation("ggplot2")

