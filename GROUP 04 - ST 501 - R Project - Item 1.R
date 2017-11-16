#################################################################################
## Course:   ST 501 (601) Fall 2017 Fundamentals of Statistical Inference I    ##
## GROUP 04                                                                    ##
##                                                                             ##
## ST 501 - R Project - Item 1                                                 ##
## Date: 11/16/2017                                                            ## 
##                                                                             ##
#################################################################################


# Define PMF/PDF functions for:
#     (a) Pois(lambda= 4) and Norm(lambda= 4,sigma^2= 4)
#     (b) Pois(lambda=10) and Norm(lambda=10,sigma^2=10)
#     (c) Pois(lambda=20) and Norm(lambda=20,sigma^2=20) 
#     (d) Pois(lambda=30) and Norm(lambda=30,sigma^2=30) 
PMFPois04<-function(x){dpois(x,lambda=4)}
PDFNorm04<-function(x){dnorm(x,mean=4,sd=sqrt(4))}
PMFPois10<-function(x){dpois(x,lambda=10)}
PDFNorm10<-function(x){dnorm(x,mean=10,sd=sqrt(10))}
PMFPois20<-function(x){dpois(x,lambda=20)}
PDFNorm20<-function(x){dnorm(x,mean=20,sd=sqrt(20))}
PMFPois30<-function(x){dpois(x,lambda=30)}
PDFNorm30<-function(x){dnorm(x,mean=30,sd=sqrt(30))}

# Define CDF functions for:
#     (a) Pois(lambda= 4) and Norm(lambda= 4,sigma^2= 4)
#     (b) Pois(lambda=10) and Norm(lambda=10,sigma^2=10)
#     (c) Pois(lambda=20) and Norm(lambda=20,sigma^2=20) 
#     (d) Pois(lambda=30) and Norm(lambda=30,sigma^2=30) 
CDFPois04<-function(x){ppois(x,lambda=4)}
CDFNorm04<-function(x){pnorm(x,mean=4,sd=sqrt(4))}
CDFPois10<-function(x){ppois(x,lambda=10)}
CDFNorm10<-function(x){pnorm(x,mean=10,sd=sqrt(10))}
CDFPois20<-function(x){ppois(x,lambda=20)}
CDFNorm20<-function(x){pnorm(x,mean=20,sd=sqrt(20))}
CDFPois30<-function(x){ppois(x,lambda=30)}
CDFNorm30<-function(x){pnorm(x,mean=30,sd=sqrt(30))}

# Finding the 10% quintile and 95% quintile to 
# get a good ploting range for x
#     (a) Pois(lambda=4), 
#     (b) Pois(lambda=10), 
#     (c) Pois(lambda=20) and 
#     (d) Pois(lambda=30) 
Pois04Left<-qpois(0.05,lambda=4)
Pois04Right<-qpois(0.95,lambda=4)
Pois10Left<-qpois(0.05,lambda=10)
Pois10Right<-qpois(0.95,lambda=10)
Pois20Left<-qpois(0.05,lambda=20)
Pois20Right<-qpois(0.95,lambda=20)
Pois30Left<-qpois(0.05,lambda=30)
Pois30Right<-qpois(0.95,lambda=30)

# creating 4 areas of plotting:
par(mfrow=c(2,2),mar=c(4,4,4,4), oma=c(2,2,2,2))

# Top left graph: PMF Poisson (4) and its normal approximation
at<-Pois04Left:Pois04Right
plot(at,PMFPois04(at),type="h",ylim=c(0,dpois(4,lambda=4)),xlim=c(Pois04Left-1,Pois04Right+1),
     xlab="x",ylab="PMF  -  PDF",col="dark red",
     main="Pois(lambda=4) and its Normal approximation", font=1,ps = 12, cex = 1, cex.main = 1)
# overlapping the normal in blue
curve(PDFNorm04, type = "l",col="blue", add=TRUE)

# Top right graph: PMF Poisson (10) and its normal approximation
at<-Pois10Left:Pois10Right
plot(at,PMFPois10(at),type="h",ylim=c(0,dpois(10,lambda=10)),xlim=c(Pois10Left-1,Pois10Right+1),
     xlab="x",ylab="PMF  -  PDF",col="dark red",
    main="Pois(lambda=10) and its Normal approximation", font=1,ps = 12, cex = 1, cex.main = 1)
# overlapping the normal in blue
curve(PDFNorm10, type = "l",col="blue", add=TRUE)

# Top right graph: PMF Poisson (20) and its normal approximation
at<-Pois20Left:Pois20Right
plot(at,PMFPois20(at),type="h",ylim=c(0,dpois(20,lambda=20)),xlim=c(Pois20Left-1,Pois20Right+1),
     xlab="x",ylab="PMF  -  PDF",col="dark red",
    main="Pois(lambda=20) and its Normal approximation", font=1,ps = 12, cex = 1, cex.main = 1)
# overlapping the normal in blue
curve(PDFNorm20, type = "l",col="blue", add=TRUE)

# Bottom right graph: PMF Poisson (30) and its normal approximation
at<-Pois30Left:Pois30Right
plot(at,PMFPois30(at),type="h",ylim=c(0,dpois(30,lambda=30)),xlim=c(Pois30Left-1,Pois30Right+1),
     xlab="x",ylab="PMF  -  PDF",col="dark red",
    main="Pois(lambda=30) and its Normal approximation", font=1,ps = 12, cex = 1, cex.main = 1)
# overlapping the normal in blue
curve(PDFNorm30, type = "l",col="blue", add=TRUE)

# General Title
mtext("Poisson PMF - red - and its approximation by a Normal  PDF - blue - ", side=3, line=1, outer=TRUE, font=2)
mtext("R Project items 1(a), 1(b), 1(c)", side=3, line=0, outer=TRUE, font=1, cex=0.8)

# creating 4 areas of plotting:
par(mfrow=c(2,2),mar=c(4,4,4,4), oma=c(2,2,2,2))

# Top left graph: CDF Poisson (4) and its normal approximation
plot(CDFPois04, type = "S",ylim=c(0,1),xlim=c(Pois04Left,Pois04Right),xlab="x",ylab="CDF",col="dark red",
     main="Pois(lambda=4) Pand its Normal approximation", font=1,ps = 12, cex = 1, cex.main = 1)
# overlapping the normal in blue
curve(CDFNorm04, type = "l",col="blue", add=TRUE)

# Top right graph: CDF Poisson (10) and its normal approximation
plot(CDFPois10, type = "S",ylim=c(0,1),xlim=c(Pois10Left,Pois10Right),xlab="x",ylab="CDF",col="dark red",
     main="Pois(lambda=10) and its Normal approximation", font=1,ps = 12, cex = 1, cex.main = 1)
# overlapping the normal in blue
curve(CDFNorm10, type = "l",col="blue", add=TRUE)

# Top right graph: CDF Poisson (20) and its normal approximation
plot(CDFPois20, type = "S",ylim=c(0,1),xlim=c(Pois20Left,Pois20Right),xlab="x",ylab="CDF",col="dark red",
     main="Pois(lambda=20) and its Normal approximation", font=1,ps = 12, cex = 1, cex.main = 1)
# overlapping the normal in blue
curve(CDFNorm20, type = "l",col="blue", add=TRUE)

# Bottom right graph: CDF Poisson (30) and its normal approximation
plot(CDFPois30, type = "S",ylim=c(0,1),xlim=c(Pois30Left,Pois30Right),xlab="x",ylab="CDF",col="dark red",
     main="CDF Pois(lambda=30) and its Normal approximation", font=1,ps = 12, cex = 1, cex.main = 1)
# overlapping the normal in blue
curve(CDFNorm30, type = "l",col="blue", add=TRUE)

# General Title
mtext("Poisson CDF - red - and its approximation by a Normal CDF - blue - ", side=3, outer=TRUE, font=2)

# The following is R Project  item 1(d)
#    For each of the settings for the mean parameter, 
#    use R to find P(Y>=lambda+sqrt(lambda) using the 
#    Poisson distribution and the normal approximation

#    using poison to calculate P(Y>=lambda+sqrt(lambda) for a given lambda
ProbPoisson<-function(x){1-ppois(x+sqrt(x),lambda=x)}
#    using normal to calculate P(Y>=lambda+sqrt(lambda) for a given lambda
ProbNormal<-function(x){1-pnorm(x+sqrt(x),mean=x,sd=sqrt(x))}
#    calculating P(Y>=lambda+sqrt(lambda) for lambda in (4,10,20,30)
Probabilities <- matrix(c(ProbPoisson(4), ProbNormal(4),
                          ProbPoisson(10),ProbNormal(10),
                          ProbPoisson(20),ProbNormal(20),
                          ProbPoisson(30),ProbNormal(30)),
                        nrow = 4, ncol = 2, byrow = TRUE,
                        dimnames = list(c("lambda=4","lambda=10","lambda=20","lambda=30"),
                                        c("Poisson", "Normal")))
                                        
Probabilities

# creating 1 areas of plotting 
par(mfrow=c(1,1),mar=c(5,5,5,5), oma=c(2,2,2,2))

# Calculation P(Y>=lambda+sqrt(lambda)) calculated with Poisson(lambda) vs. Normal(lambda,lambda^2) 
plot((1:399)/2 + 0.5, ProbPoisson((1:399)/2 + 0.5),ylim=c(0,0.2),xlim=c(0,399/2 + 0.5), 
     font=1,ps = 12, cex = 1, cex.main = 1,xlab="lambda",ylab="P(Y>=lambda+sqrt(lambda))",col="dark red",pch=20)
# overlapping the normal in blue
points((1:399)/2 + 0.5, ProbNormal((1:399)/2 + 0.5),col="blue", pch=18, cex=0.8)
# General Title
mtext("P(Y>=lambda+sqrt(lambda)) calculated with Poisson - red - vs. Normal -blue -", side=3, line=1, outer=TRUE, font=2)
mtext("R Project items 1(e)", side=3, line=0, outer=TRUE, font=1, cex=0.8)


