#################################################################################
## Course:   ST 501 (601) Fall 2017 Fundamentals of Statistical Inference I    ##
## GROUP 04                                                                    ##
##                                                                             ##
## ST 501 - R Project - Item 2                                                 ##
## Date: 11/16/2017                                                            ## 
##                                                                             ##
#################################################################################

# Define Weibull didtribution functions
WeibullPDF <-function(x,shape,scale){(shape/scale)*((x/scale)^(shape-1))*exp(-(x/scale)^shape)}
WeibullCDF <-function(x,shape,scale){1-exp(-(x/scale)^shape)}
WeibullCDFInv <-function(x,shape,scale){scale*(-log(1-x))^(1/shape)}
WeibullTrueMean<-function(shape,scale){scale*gamma(1+1/shape)}
WeibullTrueVariance<-function(shape,scale){scale^2*(gamma(1+2/shape)-(gamma(1+1/shape)^2))}
WeibullTrueSD<-function(shape,scale){(WeibullTrueVariance(shape,scale))^(1/2)}

# creating 6 areas of plotting to visualize the four functions
par(mfrow=c(2,3),mar=c(2,2,2,2), oma=c(2,2,2,2))
plot(function(x){WeibullPDF(x,0.75,1)},xlim=c(0,4),ylim=c(0,2),main="PDF Weibull(x,0.75,1)")
plot(function(x){WeibullCDF(x,0.75,1)},xlim=c(0,4),ylim=c(0,1),main="CDF Weibull(x,0.75,1)")
plot(function(x){WeibullCDFInv(x,0.75,1)},ylim=c(0,4),xlim=c(0,1),main="CDF Inverse Weibull(x,0.75,1)")
plot(function(x){WeibullPDF(x,1.75,1)},xlim=c(0,4),ylim=c(0,1),main="PDF Weibull(x,0.75,1)")
plot(function(x){WeibullCDF(x,1.75,1)},xlim=c(0,4),ylim=c(0,1),main="CDF Weibull(x,1.75,1)")
plot(function(x){WeibullCDFInv(x,1.75,1)},ylim=c(0,4),xlim=c(0,1),main="CDF Inverse Weibull(x,1.75,1)")
# General Title for the functions plotted
mtext("Visualizing the Weibull distribution/nfor shape=0.75/scale=1 and for shape=0.75/scale=1", side=3, line=1, outer=TRUE, font=2, cex=0.8)
mtext("plots not requested - added for visualization - ", side=3, line=0, outer=TRUE, font=1, cex=0.8)

# set seed for generation of RV
set.seed (6226)

# Function to generate sample means for Weibull sample based on parameters 
# size (of means sample), sizesample (size of each sample) , shape and scale = 1
WeibullSampleOfMeans<-function (mySize, mySizeSample, myShape, myScale)
                        {apply(MARGIN=2,FUN=mean,
                               X=replicate(mySize,
                                           (sapply(runif(mySizeSample,0,1),
                                            FUN=function(x){WeibullCDFInv(x,myShape,myScale)}))))}

# Function to generate sample means for Weibull sample based on parameters 
# size (of means sample), sizesample (size of each sample) , shape and scale = 1
# plot the histogram, plot the normal approximation and return the mean and the
# standard deviation for the sample and for the weibul
WeibullPlotAndNormal<-function (mySize, mySizeSample, myShape, myScale)
  {
  # Generate the (mySize) data sets of means for a 
  # Weibull( shape = myShape , scale = myScale), 
  # each data set of the requested size (mySizeSample)
  Weibull.samples.mean <- WeibullSampleOfMeans(mySize, mySizeSample, myShape, myScale)

  # sample mean (mean of means) and variance (variance of means)
  meanSample=mean( Weibull.samples.mean)
  sdSample=sd( Weibull.samples.mean)
  
  # True Normal approximation for Weibull
  ########  ---- Pending to be developed (this is a draft)
  normAprx<-function(x){dnorm(x,mean=WeibullTrueMean(myShape, myScale),
                              sd=WeibullTrueSD(myShape, myScale)/((mySize)^(1/2)))}
  
  # Sample Normal approximation for Weibull
  normSample<-function(x)
  {dnorm(x,meanSample,sdSample)}
  
  # Histagram for Weibull(myShape, myScale) generated
  hist (Weibull.samples.mean,freq=FALSE,
        border="dark red", col="lavenderblush",xlab="",ylab="",
        main=paste("Histogram of Means for generated RV for a Weibull(",myShape,",",myScale,")\n",
                   mySize,"means calsulated for samples of size", mySizeSample))
  title(xlab=paste("Mean Weibull(",myShape,",",myScale,")"),line=2,cex=0.5,col.lab="Dark Red")
  title(ylab="Frequency",line=2,cex=0.5)
  # overlapping the true normal approximation in blue
  curve(normAprx, type = "l",col="blue", lwd=2, add=TRUE)
  title(xlab="True Normal Aproximation",line=3,cex=0.4,col.lab="Blue")
  # overlapping the sample normal approximation in purple
   curve(normSample, type = "l",col="darkviolet", add=TRUE)
  title(xlab="Sample Normal Aproximation",line=4,cex=0.4,col.lab="darkviolet")
  c(meanSample,sdSample)
  }


# creating 6 areas of plotting for the histograms:
par(mfrow=c(2,3),mar=c(5,4,5,2), oma=c(2,2,2,2))

meansAndSD.075.100<-c(WeibullTrueMean(0.75,1),WeibullTrueSD(0.75,1))
# Generate and Plot N = 50000 data sets  of size 5 for a Weibull( shape = 0.75 , scale = 1)
meansAndSD.075.100<-c(meansAndSD.075.100,WeibullPlotAndNormal(50000,5,0.75,1))
# Generate and Plot N = 50000 data sets  of size 15 for a Weibull( shape = 0.75 , scale = 1)
meansAndSD.075.100<-c(meansAndSD.075.100,WeibullPlotAndNormal(50000,15,0.75,1))
# Generate and Plot N = 50000 data sets  of size 30 for a Weibull( shape = 0.75 , scale = 1)
meansAndSD.075.100<-c(meansAndSD.075.100,WeibullPlotAndNormal(50000,30,0.75,1))

meansAndSD.175.100<-c(WeibullTrueMean(1.75,1),WeibullTrueSD(1.75,1))
# Generate and Plot N = 50000 data sets  of size 5 for a Weibull( shape = 1.75 , scale = 1)
meansAndSD.175.100<-c(meansAndSD.175.100,WeibullPlotAndNormal(50000,5,1.75,1))
# Generate and Plot N = 50000 data sets  of size 15 for a Weibull( shape = 1.75 , scale = 1)
meansAndSD.175.100<-c(meansAndSD.175.100,WeibullPlotAndNormal(50000,15,1.75,1))
# Generate and Plot N = 50000 data sets  of size 30 for a Weibull( shape = 1.75 , scale = 1)
meansAndSD.175.100<-c(meansAndSD.175.100,WeibullPlotAndNormal(50000,30,1.75,1))

# General Title for the 6 histograms plotted
mtext("Histograms for generated Weibull sample mean  - red - and its approximation by a Normal  PDF - blue - ", side=3, line=1, outer=TRUE, font=2, cex=0.8)
mtext("R Project items 2.a-d", side=3, line=0, outer=TRUE, font=1, cex=0.8)

# Comparing samples mean and sd with true mean and sd
ComparsonMatrix.075.100 <- matrix(meansAndSD.075.100, ncol = 4)
ComparsonMatrixDF.075.100 <- as.data.frame(ComparsonMatrix.075.100)
colnames(ComparsonMatrixDF.075.100)<- c("True W(0.75,1)", 
                                        "5-Sample W(0.75,1)", 
                                        "15-Sample W(0.75,1)", 
                                        "30-Sample W(0.75,1)")
rownames(ComparsonMatrixDF.075.100) <- c("Mean","Standard Deviation")
ComparsonMatrixDF.075.100

ComparsonMatrix.175.100 <- matrix(meansAndSD.175.100, ncol = 4)
ComparsonMatrixDF.175.100 <- as.data.frame(ComparsonMatrix.175.100)
colnames(ComparsonMatrixDF.175.100)<- c("True W(1.75,1)", 
                                        "5-Sample W(1.75,1)", 
                                        "15-Sample W(1.75,1)", 
                                        "30-Sample W(1.75,1)")
rownames(ComparsonMatrixDF.175.100) <- c("Mean","Standard Deviation")
ComparsonMatrixDF.175.100



