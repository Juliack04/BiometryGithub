library(readr)
reef_fish <- read_csv("~/MyBiometryWork/biometry_course/data/Reef_fish/NCRMP_reef_fish_surveys.csv")
View(NCRMP_reef_fish_surveys)

# the following data is for the longitude at which the reefs are found,
# which is a continuous variable
hist(reef_fish$longitude, breaks = 25)
mean(reef_fish$longitude, na.rm = TRUE)
#mean = -81.27396
median(reef_fish$longitude, na.rm = TRUE)
#median = -80.6178
# I would use a normal distribution because the range includes all real numbers,
# and is continuous, but the values do not grow exponentially, so the lognormal 
# distribution would be inappropriate.

# the following data is for the depth at which the reef is found, which is a continuous variable
hist(reef_fish$DEPTH, breaks = 50)
hist(log(reef_fish$DEPTH), breaks = 25)
mean(reef_fish$DEPTH)
# mean = 13.08646
median(reef_fish$DEPTH)
# median = 11.9
# I would also use a normal distribution for this plot because the values are
# all positive numbers greater than 1, are not a proportion, and are not growing
# exponentially. However, this would not give you a very good fit. Using the
# log-scale of these values would create a better fitting, left-skewed, normal
# distribution, but thinking about the data, looking at the log-scale of depths
# is not very meaningful.

# the following data is for species richness, which is a discrete variable
hist(reef_fish$species_richness, breaks = 25)
mean(reef_fish$species_richness)
#mean = 23.44907
median(reef_fish$species_richness)
# median = 22
# The most appropriate distribution for this discrete data would be the Poisson
# distribution because species richness is a count of the number of arrivals in
# a given space. Poisson works better than other discrete models, like binomial
# or geometric because there is more than one possible outcome for the data; it
# is not a value based on "success" or "failure".

