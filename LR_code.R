dir.create("C:/Users/user1/Desktop/workingDIR")
dir.create("C:/Users/user1/Desktop/workingDIR/MY Library")
setwd("C:/Users/user1/Desktop/workingDIR")
getwd()
.libPaths("C:/Users/user1/Desktop/workingDIR/MY Library")
.libPaths()

install.packages()
library(raster)

install.packages()
library(sf)

install.packages()
library(terra)

Elevation = raster("C:/Users/user1/Desktop/workingDIR/My data/Elevation.tif")
list.files("C:/Users/user1/Desktop/workingDIR/My data")

plot(Elevation, main="Elevation map")
plot(Slope, main="Slope map")
plot(NDVI, main="NDVI map")
plot(Curavture, main="Curvature map")
Elevation@crs

hist(Elevation, main = "Distrubution of Elvation Values",
     col = "yellow", maxpixels=2000000)

Elevation_by5 <- Elevation * 5

image(Elevation)
plot(Elevation)

image(Elevation, zlim=c(0,3000))
col<-terrain.colors(5)
image(Elevation , zlim=c(0,1000), main="Digital ELevation Model", col=col)

m=c(0,100,1,100,400,2,400,1000,3)
mat=matrix(m,ncol=3,byrow = TRUE)
elevationcat=reclassify(Elevation,mat)
plot(elevationcat)
writeRaster(elevationcat,"elevationcat.tif",overwrite=TRUE)

col=terrain.colors(5)
brk <- c(100,300,500,700,900,1200)
plot(Elevation,col=col,breaks=brk,main="Elevation with 6 breaks")


Elevation=raster("C:/Users/user1/Desktop/workingDIR/My data/Elevation.tif")
NDVI=raster("C:/Users/user1/Desktop/workingDIR/My data/NDVI.tif")
Slope=raster("C:/Users/user1/Desktop/workingDIR/My data/Slope Angel.tif")
Curavture=raster("C:/Users/user1/Desktop/workingDIR/My data/Curvature.tif")
Training_LR=raster("C:/Users/user1/Desktop/workingDIR/My data/Training_LR.tif")
list.files("C:/Users/user1/Desktop/workingDIR/My data")

Training_re<-resample(Training_LR,Elevation,resample='bilinear')
extent(Elevation)
extent(NDVI)
extent(Curavture)
extent(Training_re)
Elevation_re_re<-resample (Elevation,Elevation,resample='bilinear')

NDVI_re_re<-resample (NDVI,Elevation,resample='bilinear')
slope_re<-resample(Slope,Elevation,resample='bilinear')
Curvature_re<-resample(Curavture,Elevation,resample='bilinear')

dir.create("C:/Users/user1/Desktop/workingDIR/Resampled! Data")
writeRaster(Training_re,"C:/Users/user1/Desktop/workingDIR/Resampled! Data/Training_LR.tif",overwrite=TRUE)
writeRaster(NDVI_re_re,"C:/Users/user1/Desktop/workingDIR/Resampled! Data/NDVI.tif",overwrite=TRUE)
writeRaster(slope_re,"C:/Users/user1/Desktop/workingDIR/Resampled! Data/Slope.tif",overwrite=TRUE)
writeRaster(Curvature_re,"C:/Users/user1/Desktop/workingDIR/Resampled! Data/Curvature.tif",overwrite=TRUE)
writeRaster(Elevation_re_re ,"C:/Users/user1/Desktop/workingDIR/Resampled! Data/Elevation.tif",overwrite=TRUE)
hist(Training_LR)
plot(Slope)
plot(Training_LR, add=TRUE)
list.files("C:/Users/user1/Desktop/workingDIR/Resampled! Data")
stack_list1=list.files(path ="C:/Users/user1/Desktop/workingDIR/Resampled! Data",pattern = "tif$",full.names = TRUE )
Raster=stack(stack_list1)
names(Raster)
head(Raster)   ## our stack variable, we see NA values, #### 
##that needto be removed or replaced by -9999###

value_table=getValues(Raster)
head(value_table, n=6)

value_table=na.omit(value_table)
value_table=as.data.frame(value_table)
head(value_table, n=6)

#Export your values to text file, thats for double check or 
#use it in microsoft Excel for further calculations##
write.table(value_table, "mydata.txt", sep="\t") # export to text file
str(value_table)

# data= value_data, and choose the training column=Training
training.fit<-glm (Training_LR~., data=value_table, family=binomial)### use all independents
summary(training.fit) ### display results


anova(training.fit, test="Chisq")

##While no exact equivalent to the R2 
# of linear regression exists, the McFadden R2 
#index can be used to assess the model fit.
install.packages("pscl")
library(pscl)
pR2(training.fit)



### 3. Generate confidence interval for regression coefficients####
confint(training.fit)


### 4. Put the coefficient and confidence interval in useful scale##
exp(training.fit$coefficients)
exp(confint(training.fit))

#### 5. predicted values##
Predicted=predict(training.fit, type="response")
write.table(Predicted, "predicted values.txt", sep="\t")

#### 6. residuals ###
##Residual deviance indicates the response predicted 
# by a model on adding independent variables. Lower the value, better the model.
residuals(training.fit, type="deviance")

## 7.confusion matrix: a tabular representation of Actual vs Predicted values. 
#This helps us to find the accuracy of the model and avoid overfitting.
table(value_table$Training, Predicted > 0.5)

install.packages("ROCR")
library(ROCR)
ROCRpred <- prediction(Predicted,value_table$Training_LR)
ROCRpref <- performance(ROCRpred,'tpr','fpr')
plot(ROCRpref)


auc <- performance(ROCRpred, measure = "auc")
auc <-auc@y.values[[1]]
auc
plot(ROCRpref,main="AUC=0.614",colorize = TRUE,text.adj = c(-0,2,1.))


library(ggplot2)
ggplot(value_table, aes(x=Elevation,y=Training_LR))+geom_point()+
  stat_smooth(method="glm",method.args=list(family="binomial"),se=FALSE)


logr_elevation <- glm(Training_LR~Elevation,data = value_table,family=binomial)
par(mar = c(4,4,1,1))
plot(value_table$Elevation,value_table$Training_LR)
curve(predict(logr_elevation,data.frame(Elevation = x),type = "response"),add = TRUE)



library(ggplot2)
ggplot(value_table, aes(x=NDVI,y=Training_LR))+geom_point()+
  stat_smooth(method="glm",method.args=list(family="binomial"),se=FALSE)


logr_NDVI <- glm(Training_LR~NDVI,data = value_table,family=binomial)
par(mar = c(4,4,1,1))
plot(value_table$NDVI,value_table$Training_LR)
curve(predict(logr_NDVI,data.frame(NDVI = x),type = "response"),add = TRUE)


library(ggplot2)
ggplot(value_table, aes(x=Curvature,y=Training_LR))+geom_point()+
  stat_smooth(method="glm",method.args=list(family="binomial"),se=FALSE)


logr_Curvature <- glm(Training_LR~Curvature,data = value_table,family=binomial)
par(mar = c(4,4,1,1))
plot(value_table$Curvature,value_table$Training_LR)
curve(predict(logr_Curvature,data.frame(Curvature = x),type = "response"),add = TRUE)

summary(training.fit)
Y = (Elevation * (-0.0004688 ) + NDVI * (1.3010863) + Slope * (-0.0141709) + Curavture * (0.1279151) + 0.2341154)
P = 1/(1+exp(Y*(-1)))
summary(P)
Landslide_Susceptability_Index = P
plot(P)
//charan



