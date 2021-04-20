#############################################################################
# Introduction to Programming and Geostatistics Assignment 
# Land Cover Change Detection in Inambaru, Peru
# due to illegal gold mining  
# Andrea Cardenas Reyes
#############################################################################

##### LOAD PACKAGES
library(raster)
library(RStoolbox)
library(ggplot2)
library(rgdal)
library(gridExtra)
library(tuneR)
library(randomForest)
library(e1071)
library(dplyr)
library(ggalluvial)
library(RColorBrewer)
library(mapview)
library(rasterVis)
library(reshape2)
library(maptools)
library(leaflet)
library(geojsonio)
library(ggpubr)

#### Define folder that contains the data
data.path <- 'C:/Users/andre/Documents/EAGLE_Msc/MB2_Intro_Programing_Geostatistics/Change_Detection'
list.files(data.path)

#############################################################################
# 1)  Data 
#############################################################################

# Study area
# Read Peru boundaries
Peru <- raster::getData("GADM",country = "Peru", level = 3)
Inambari <- Peru[Peru$NAME_3 == "Inambari",]
ilegalareas <- geojsonio::geojson_read("C:/Users/andre/Documents/EAGLE_Msc/MB2_Intro_Programing_Geostatistics/Change_Detection/SHP/ilegalareas.geojson", what = "sp")

mapInambari <- leaflet(data = Inambari) %>%  
  addTiles() %>% 
  addProviderTiles("Esri.WorldImagery", group = "Aerial") %>% 
  addProviderTiles("OpenTopoMap", group = "Topography")%>% 
  addPolygons(., stroke = TRUE, weight = 1, color = "black", popup = "Inambari",
    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) %>%
    addLayersControl(      
    baseGroups = c("Aerial", "Topography"), 
    options = layersControlOptions(collapsed = TRUE)) %>%
  addRectangles( 
    lng1=-70.0329710678581563, lat1=-13.0812406183933394,
    lng2=-69.8506281293290385, lat2=-12.8929948599727187,
    fillColor = "transparent", stroke = TRUE, color = "#03F"
    ) %>%
  addPolygons(data=ilegalareas,
              col = 'yellow',
              stroke = FALSE, 
              fillOpacity = 0.3, 
              smoothFactor = 0.5) %>%
  addLegend("bottomleft", colors= c("yellow", "blue"), labels=c("Ilegal mining area in 2013, RAISG", "Area of interest"), title="Leyend")

 mapInambari %>% addMiniMap() 

# a) Data handling 
## Upload raster files from 2014,2018

imag_2014 <- list.files(paste0(data.path, "/Raster/LC08_003069_20141013"), pattern = "B[2-7].TIF",full.names = T)
stack_2014 <- stack(imag_2014)
stack_2014
names(stack_2014) <- gsub(pattern = "RT_LC08_L1TP_003069_20141013_20170418_01_T1_", replace = "", x=names(stack_2014)) 
plot(stack_2014)

imag_2018 <- list.files(paste0(data.path, "/Raster/LC08_003069_20180906"), pattern = "B[2-7].TIF$",full.names = T)
stack_2018 <- stack(imag_2018)
names(stack_2018) <- gsub(pattern = "RT_LC08_L1TP_003069_20180906_20180912_01_T1_", replace = "", x=names(stack_2018))
plot(stack_2018)

## Crop and mask by a shapefile
shp <- readOGR(paste0(data.path, "/Shp/area.shp"))

study_area2014 <- mask(crop(stack_2014, shp), shp)
plot(study_area2014)

study_area2018 <- mask(crop(stack_2018, shp), shp)
plot(study_area2018)

compareRaster(study_area2014, study_area2018, extent=TRUE, crs = TRUE, res = TRUE)

## Save raster
writeRaster(study_area2014, filename = paste0("C:/Users/andre/Documents/EAGLE_Msc/MB2_Intro_Programing_Geostatistics/Change_Detection/Results/study_area2014.tif"), format = "GTiff", progress = 'text', overwrite = TRUE)
writeRaster(study_area2018, filename = paste0("C:/Users/andre/Documents/EAGLE_Msc/MB2_Intro_Programing_Geostatistics/Change_Detection/Results/study_area2018.tif"), format = "GTiff", progress = 'text', overwrite = TRUE)

# b) Inspecting the data 

img_2014_s <- stack(paste0(data.path, "/Results/study_area2014.tif"))
img_2018_s <- stack(paste0(data.path, "/Results/study_area2018.tif"))

## Plot images
### Natural color
ggRGB(img_2014_s, stretch = "lin")
ggRGB(img_2018_s, stretch = "lin")
### NIR
plotRGB(img_2014_s, r=4, g=3, b=2, stretch = "lin")
plotRGB(img_2018_s, r=4, g=3, b=2, stretch = "lin")

## Vegetation indices and data transforms
### NDVI
ndvi_formula <- function(nir, red) {
  (nir-red) / (nir+red)
}

img_2014s.ndvi <- overlay(img_2014_s$study_area2014.4, img_2014_s$study_area2014.3, fun = ndvi_formula) 
img_df_2014s.ndvi <- as.data.frame(img_2014s.ndvi, xy = TRUE)
ggplot() +
  geom_raster(data = img_df_2014s.ndvi, aes(x = x, y = y, fill = layer)) +
  coord_quickmap() + ggtitle("NDVI 2014") +
  scale_fill_gradient(high = "#087F28", low = "#CEE50E", name = "NDVI")

img_2018s.ndvi <- overlay(img_2018_s$study_area2018.4, img_2018_s$study_area2018.3, fun = ndvi_formula) 
img_df_2018s.ndvi <- as.data.frame(img_2018s.ndvi, xy = TRUE)
ggplot() +
  geom_raster(data = img_df_2018s.ndvi, aes(x = x, y = y, fill = layer)) +
  coord_quickmap() + ggtitle("NDVI 2018") +
  scale_fill_gradient(high = "#087F28", low = "#CEE50E", name = "NDVI")

# SAVI 
savi_formula <- function(nir, red) {
  ((nir-red) /  (nir + red + 0.5)) * (1.5)
}
img_2014s.savi <- overlay(img_2014_s$study_area2014.4, img_2014_s$study_area2014.3, fun = savi_formula)
par(mar=c(5,5,5,5))
plot(img_2014s.savi, main = "SAVI AOI 2014", axes = FALSE, box = FALSE)
img_2018s.savi <- overlay(img_2018_s$study_area2018.4, img_2018_s$study_area2018.3, fun = savi_formula)
plot(img_2018s.savi, main = "SAVI AOI 2018", axes = FALSE, box = FALSE)

# Plot spectral profiles grouped by factor date 
img_2014_2018 <- stack(img_2014_s, img_2018_s)
coordinate <- data.frame('x' = 398638, 'y' = -1435937) # Define coordinate (units = m)
cellvals <- extract(img_2014_2018, coordinate) # Extract cell values from image_stack
profile <- data.frame('wavelength'=rep(c(450, 525, 630, 845, 1560, 2100),2),   # Combine extracted values with band wavelengths
                      'date'=as.factor(c(rep('October 2014', 6), rep('September 2018', 6))),
                      'values'=as.numeric(cellvals))
print(profile)
ggplot(profile, aes(x=wavelength, y=values, group=date)) + 
  geom_line(aes(color=date)) + geom_point(aes(color=date))+
  scale_y_continuous(name='Reflectance') + scale_x_continuous(name='Wavelength (nm)') + 
  ggtitle("Profiles grouped by factor date") +
  scale_color_manual(values=c("#999999", "#56B4E9"))

#############################################################################
# 2) Classification
#############################################################################

### a) Training data for the classes "forest", "non-forest" and "water" for each of the two images

training_data2014 <- readOGR("C:/Users/andre/Documents/EAGLE_Msc/MB2_Intro_Programing_Geostatistics/Change_Detection/Shp/training_points_2014.shp")
training_data2018 <- readOGR("C:/Users/andre/Documents/EAGLE_Msc/MB2_Intro_Programing_Geostatistics/Change_Detection/Shp/training_points_2018.shp")

### b) Training a Random Forest model for image 2014
# Link classes with the corresponding spectral bands
train_df_2014 <- raster::extract(img_2014_s, training_data2014, sp = TRUE) # extract the pixel values under each sample point
train_df_2014 <- as.data.frame(train_df_2014)
train_df_2014$coords.x1 <- NULL
train_df_2014$coords.x2 <- NULL
train_df_2014$id <- na.omit(as.factor(train_df_2014$id)) 
spectra2014.df <- melt(train_df_2014, id.vars=c(1:2), measure.vars=c(3:8)) 
ggplot(spectra2014.df, aes(x=variable, y=value, color=class_name)) + geom_boxplot() + ggtitle("Spectral bands per class 2014") 

# RF classification
rf_2014 <- randomForest::randomForest(id ~ study_area2014.1 + study_area2014.2 + study_area2014.3 + study_area2014.4 + study_area2014.5 + study_area2014.6, 
                                      data = train_df_2014, ntree=500, mtry = 2, sampsize = 50, confusion = TRUE)
# Model performance
print(rf_2014)
print(importance(rf_2014, type = 2)) 

# Variable importance
varImpPlot(rf_2014,type=2) #mean decrease in node impurity

# Classification
Classification_2014 <- raster::predict(img_2014_s,rf_2014, progress = "text")
writeRaster(Classification_2014, "C:/Users/andre/Documents/EAGLE_Msc/MB2_Intro_Programing_Geostatistics/Change_Detection/Results/classification_2014.tif", 
                                                   format = "GTiff", progress = "text", sampsize = 50, overwrite = TRUE)

### c) Training a Random Forest model for image 2018
# Link classes with the corresponding spectral bands
train_df_2018 <- raster::extract(img_2018_s, training_data2018, sp = TRUE)
train_df_2018 <- as.data.frame(train_df_2018)
train_df_2018$coords.x1 <- NULL
train_df_2018$coords.x2 <- NULL
train_df_2018$id <- na.omit(as.factor(train_df_2018$id))
spectra2018.df <- melt(train_df_2018, id.vars=c(1:2), measure.vars=c(3:8))

ggplot(spectra2018.df, aes(x=variable, y=value, color=class_name)) + geom_boxplot() + ggtitle("Spectral bands per class 2018")# Boxplots of spectral bands per class

# RF classification
rf_2018 <- randomForest::randomForest(id ~ study_area2018.1 + study_area2018.2 + study_area2018.3 + study_area2018.4 + study_area2018.5 + study_area2018.6, 
                                      data = train_df_2018, ntree=500, mtry = 2,sampsize = 50, confusion = TRUE)
# Model performance
print(rf_2018)
print(importance(rf_2018, type = 2))   

# Variable importance
varImpPlot(rf_2018,type=2) #mean decrease in node impurity

# Classification
Classification_2018 <- raster::predict(img_2018_s,rf_2018, progress = "text")
writeRaster(Classification_2018, "C:/Users/andre/Documents/EAGLE_Msc/MB2_Intro_Programing_Geostatistics/Change_Detection/Results/classification_2018.tif",
            format = "GTiff", progress = "text", overwrite = TRUE)

### d) Landscape analysis

Classification_2014 <- raster("C:/Users/andre/Documents/EAGLE_Msc/MB2_Intro_Programing_Geostatistics/Change_Detection/Results/classification_2014.tif")
Classification_2018 <- raster("C:/Users/andre/Documents/EAGLE_Msc/MB2_Intro_Programing_Geostatistics/Change_Detection/Results/classification_2018.tif")

freq(Classification_2014)
freq(Classification_2018)

par(mar=c(2.1, 4.1, 4.1, 4.1), xpd=TRUE)
plot(Classification_2014, col = c("darkgreen","wheat3","blue"), 
     main = "Supervised Classification of Imagery (Year 2014)", legend = FALSE)
legend(x= 410500, y= ,-1430000, legend = c("Forest", "Non-forest", "Water"),
       fill = c("darkgreen","wheat3","blue"), cex = 0.7, inset= 0.9, xpd=TRUE)

par(mar=c(2.1, 4.1, 4.1, 4.1), xpd=TRUE)
plot(Classification_2018, col = c("darkgreen","wheat3","blue"), 
     main = "Supervised Classification of Imagery (Year 2018)", legend = FALSE)
legend(x= 410500, y= ,-1430000, legend = c("Forest", "Non-forest", "Water"),
       fill = c("darkgreen","wheat3","blue"), cex = 0.7, inset= 0.9, xpd=TRUE)

#### Area covered by forest, non-forest and water
Classification_2014.freq <- freq(Classification_2014, useNA = "no")
resLsat2014 <- res(Classification_2014)
area_km2_2014 <- Classification_2014.freq[, "count"] * prod(resLsat2014) * 1e-06
Total_2014 <- data.frame(landcover = c('forest', 'non-forest', 'water'), area_km2 = area_km2_2014)
plot2014 <- ggplot(Total_2014, aes(x=landcover, y=area_km2, fill = landcover)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=area_km2), vjust=-0.3, size=3.5)+
  ggtitle("Predicted area (km2) (Year 2014)")

Classification_2018.freq <- freq(Classification_2018, useNA = "no")
resLsat2018 <- res(Classification_2018)
area_km2_2018 <- Classification_2018.freq[, "count"] * prod(resLsat2018) * 1e-06
Total_2018 <- data.frame(landcover = c('forest', 'non-forest', 'water'), area_km2 = area_km2_2018)
plot2018 <- ggplot(Total_2018, aes(x=landcover, y=area_km2, fill = landcover)) +
  geom_text(aes(label=area_km2), vjust=-0.3, size=3.5)+
  geom_bar(stat="identity") +
  ggtitle("Predicted area (km2) (Year 2018)")

figure <- ggarrange(plot2014, plot2018)
                    
plot(figure)
#############################################################################
# 3)  Accuracy assessment using SuperClass
#############################################################################

### 2014
# RF in SuperClass
Classification_2014_sc <- superClass(img_2014_s, model = "rf",
                                     trainData = training_data2014,
                                     responseCol = "class_name",
                                     trainPartition = 0.7)
Classification_2014_sc$validation$performance

### 2018
# RF in SuperClass
Classification_2018_sc <- superClass(img_2018_s, model = "rf",
                                     trainData = training_data2018,
                                     responseCol = "class_name",
                                     trainPartition = 0.7)
Classification_2018_sc$validation$performance

#############################################################################
# 4) Change detection
#############################################################################

### POST CLASSIFICATION COMPARISON
## a) Final Classification 
map_2014_2018 <- (Classification_2018-Classification_2014) + Classification_2018

par(mar=c(5.1, 4.1, 4.1, 11.1), xpd=TRUE)
plot(map_2014_2018, col = c("mediumpurple","sienna1","darkgreen","wheat3","brown4","skyblue", "blue"), 
     main = "Changes between 2014_2018", legend = FALSE)
legend(x= 410500, y= ,-1430000, inset=c(0,0), legend = c("Non-forest - Forest", "Forest - Water","Forest - Forest","Non-forest - Non-forest","Forest - Non-forest", "Non-forest - Water", "Water - Water"),
       fill = c("mediumpurple","sienna1","darkgreen","wheat3","brown4","skyblue", "blue"), xpd=TRUE)

par(mar=c(5.1, 4.1, 4.1, 11.1), xpd=TRUE)
plot(map_2014_2018, col = c("mediumpurple", "brown4", "darkgreen","wheat3","brown4","blue", "blue"), 
     main = "Changes between 2014_2018", legend = FALSE)
legend(x= 410500, y= ,-1430000, inset=c(0,0), legend = c("Forest-Gain", "Forest-Loss","StableForest","Stable Non-forest","Water"),
       fill = c("mediumpurple","brown4","darkgreen","wheat3", "blue"), xpd=TRUE)

writeRaster(map_2014_2018, "C:/Users/andre/Documents/EAGLE_Msc/MB2_Intro_Programing_Geostatistics/Change_Detection/Results/map_2014_2018.tif",
            format = "GTiff", progress = "text", overwrite = TRUE)

map_2014_2018_2 <- (Classification_2018-Classification_2014)
writeRaster(map_2014_2018_2, "C:/Users/andre/Documents/EAGLE_Msc/MB2_Intro_Programing_Geostatistics/Change_Detection/Results/map_2014_2018_2.tif",
            format = "GTiff", progress = "text", overwrite = TRUE)

## b) Landscape statistics 

# Area estimation
freq_classes_2014_2018 <- freq(map_2014_2018, useNA="no")  
freq_classes_2014_2018_df <- as.data.frame(freq_classes_2014_2018) 

area_est_2014_2018 <- (freq_classes_2014_2018[,2]*30^2/10000)
dataframe_class <- as.data.frame(area_est_2014_2018)
rownames(dataframe_class) <- c("No forest - Forest", "No water 1 - Water","Forest - Forest","Non-forest - Non-forest","Forest - Non-forest", "No water 2 - Water", "Water - Water")
dataframe_class

#final_df <- as.data.frame(t(dtaframe))

## Proportions 85_15
totalarea <-sum(dataframe_class$area_est_2014_2018)
totalarea
Forest_gain <- ((dataframe_class$area_est_2014_2018*100)/totalarea)
Forest_gain

#alluvial plots
map_2014_2018 
freq(map_2014_2018)
classf <- freq(map_2014_2018, useNA="no")
classf <- as.data.frame(classf) 
names(classf)[names(classf) == "value"] <- "id" # Changing the column names "value" to "id"
names(classf)[names(classf) == "count"] <- "freq" # "count" to "freq"
Change_Detection_through_time <- rbind(classf)

## Km2 and percentage for each class

Change_Detection_through_time$Km2 <- ((Change_Detection_through_time$freq*900)/1000000) # We multiply each cell by the size of the pixel (30x30) and divide it by 1000000 (1000x1000) to get km2
Change_Detection_through_time$percentage <- ((Change_Detection_through_time$Km2*100)/sum(Change_Detection_through_time$Km2/2))
Change_Detection_through_time$Class <- c("Water", "No forest", "Forest", "No forest", "Forest", "No forest", "Forest") # based on changes observation in the maps in QGIS
Change_Detection_through_time$after <- c("Water", "Forest", "Forest", "No forest", "No Forest", "Water - extraction pool", "Water - extraction pool") # class and the time steps
Change_Detection_through_time$final <- c("River flow change", "Forest gain", "Stable forest", "Stable non forest", "Forest loss", "Forest loss", "Forest Loss") # class and the time steps
Change_Detection_through_time$Area_km2 <- ((Change_Detection_through_time$Km2/2))

ggplot(as.data.frame(Change_Detection_through_time),
       aes(y = Area_km2, axis1 = Class, axis2 = after, axis3 = final)) +
  geom_alluvium(aes(fill = Class), width = 1/10) +
  geom_stratum(width = 1/10) +
  geom_label(stat = "stratum", aes(label = after_stat(stratum), palette = "Set1")) +
  scale_x_discrete(limits = c("2014", "2015", "Final"), expand = c(.05, .05)) +
  #scale_fill_manual(values=c("forestgreen","darkorange3")) +
  ggtitle("Change Detection through time")

### MULTIDATE CLASSIFICATION
stack_2014 <- raster("C:/Users/andre/Documents/EAGLE_Msc/MB2_Intro_Programing_Geostatistics/Change_Detection/Raster/2014.tif")
stack_2018 <- raster("C:/Users/andre/Documents/EAGLE_Msc/MB2_Intro_Programing_Geostatistics/Change_Detection/Raster/2018.tif")
stack_2014_2018 <- stack(stack_2014, stack_2018)

change_classes <- readOGR("C:/Users/andre/Documents/EAGLE_Msc/MB2_Intro_Programing_Geostatistics/Change_Detection/Shp/validation_2014_2018/map.shp")
cc <- as.data.frame(change_classes)

change_stack_2014_2018_sc <- superClass(img_2014_2018, model = "rf",
                                     trainData = change_classes,
                                     responseCol = "class_nomb",
                                     trainPartition = 0.7)
change_stack_2014_2018_sc$validation$performance


#############################################################################
# 5) Accuracy assessment
#############################################################################
# Create validations points (25 per class) - validation <- sampleStratified(map_2014_2018, size = 25, sp= TRUE, na.rm = TRUE)
#writeOGR(validation, "C:/Users/andre/Documents/EAGLE_Msc/MB2_Intro_Programing_Geostatistics/Change_Detection/Shp/validation_2014_2018", layer = "map",
        # driver = "ESRI Shapefile" )

sample_points <- readOGR("C:/Users/andre/Documents/EAGLE_Msc/MB2_Intro_Programing_Geostatistics/Change_Detection/Shp/validation_2014_2018/map.shp")
sample_df <- as.data.frame(sample_points)
sample_df$coords.x1 <- NULL
sample_df$coords.x2 <- NULL
sample_df$cell <- NULL
sample_df$class_nomb <- NULL

confusion_matrix <- table(sample_df)
overall_acc <- sum(diag(confusion_matrix))/sum(confusion_matrix) 
producer_acc <- diag(confusion_matrix)/colSums(confusion_matrix) *100
user_acc <- diag(confusion_matrix)/rowSums(confusion_matrix) *100


