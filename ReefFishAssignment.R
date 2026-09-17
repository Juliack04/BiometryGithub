# This data set was taken from the data of NOAA's NCRMP (National Coral Reef Monitoring Program)
# The data was found at https://github.com/bmaitner/biometry_course/tree/main/data/Reef_fish


fish <- read.csv("NCRMP_reef_fish_surveys.csv")
str(fish)
var<- fish
var<- fish[c("latitude", "DEPTH", "UNDERWATER_VISIBILITY", "total_abundance")]
round(cor(var),2)
# above provides the correlation between the variables latitude, depth, underwater visibility, and total abundance


pairs(x=var, pch=16, cex=0.3, col = rgb(red=0, green = 0, blue = 0, alpha = 0.15))
# above plots the correlation results on graphs


png("Cor_lat_depth_vis_ta.png", width = 900, height = 650)
pairs(x=var, pch=16, cex=0.3, col = rgb(red=0, green = 0, blue = 0, alpha = 0.15))
dev.off()
# above creates a png file for the correlation plot

boxplot(total_abundance ~ HABITAT_TYPE, data = fish, 
        ylab = "Total Abundance",
        xlab= NULL,
        las=2,
        cex.lab=1,
        cex.axis=.5)
# above makes a plot for the relationship between total abundance and the MPA status of the reef
# in the plot, I tried to make the x-axis labels small enough to fit on the file, but I do not know how to make the names so that they are readable and fit on the page.

png("Abundance_Habitat.png", width = 900, height = 650)
boxplot(total_abundance ~ HABITAT_TYPE, data = fish, 
        ylab = "Total Abundance",
        xlab= NULL,
        las=2,
        cex.lab=1,
        cex.axis=.5)
dev.off()
# above action saves the boxplot

par(mfrow= c(1,2))
hist(fish$species_richness, main = "Species Richness",
     xlab = "Species Richness", ylab = "Frequency")
hist(fish$UNDERWATER_VISIBILITY, main = "Visibility",
     xlab = "Underwater Visibility", ylab = "Frequency")
par(mfrow = c(1,1))
# above creates 2 side-by-side histograms showing the frequency of species richness data and visibility data respectively



plot(x= fish$longitude, y= fish$latitude, xlab = "Longitude", ylab = "Latitude", main = "Longitude and Latitude of Observations")
?plot
class(fish$species_richness)
# above creates a plot that works partially as a map showing the longitude and latitude of the observation sites

plot(x= fish$DEPTH, 
     y= fish$UNDERWATER_VISIBILITY,
     xlab = "Depth (Meters)",
     ylab = "Visibility (Meters)",
     main = "Changes in Visibility with Depth")
# above creates a plot that shows the visibility underwater at the depth it was measured

png("DepthVisScatterplot.png", width = 900, height = 650)
plot(x= fish$DEPTH, 
     y= fish$UNDERWATER_VISIBILITY,
     xlab = "Depth (Meters)",
     ylab = "Visibility (Meters)",
     main = "Changes in Visibility with Depth")
dev.off()


png("LogLat_Scatterplot.png", width = 900, height = 650)
plot(x= fish$longitude, y= fish$latitude, xlab = "Longitude", ylab = "Latitude", main = "Longitude and Latitude of Observations")
dev.off()


