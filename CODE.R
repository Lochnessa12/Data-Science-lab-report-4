#Setting the working directory
#Place your own working directory to load the wanted files

#Loading the csv-file for salt calibration
SC<-read.csv("salt_calibration.csv", header = TRUE)
summary(SC)

#Creating the first plot for salt calibrations at each #wavelengths ----

#y1 describes wavelength intensity 3340 cm^-1 
#y2 describes wavelength intensity 3180 cm^-1 
y1<-SC$mean_intensity_3340
y2<-SC$mean_intensity_3180

#x describes salt concentration
x<-SC$salt_conc

#plot for wavelength 3340 cm^-1, (x,y1)
plot(x,y1,
     type = "p", 
     col="Green",
     main="Standard curves for salt concentration",
     xlab="Saltconcentration [W %]",
     ylab="Intensity [cm^-1]",
     xlim=c(1,20),
     ylim=c(38,100))

# Creating a model for the first plot, to generate a linear #fit
model1<-lm(y1~x)

#View the model, to get the coefficients for linear regression #at wavelength 3340 cm^-1
model1

#Naming coefficients A and B for wavelength 3340 cm^-1 linear #regression
A3340 <- coef(model1)[2]
B3340 <- coef(model1)[1]

#Plotting the linear regression in the first plot
abline(model1, col="Lightgreen")

par(new=TRUE)

#plot for wavelength 3180 cm^-1, (x,y2)
plot(x,y2,
     type = "p", 
     col="Red",
     main="",
     xlab="",
     xaxt="n",
     yaxt="n",
     ylab="",
     xlim=c(1,20),
     ylim=c(38,100))

# Creating a model for the second plot, to generate a linear #fit
model2<-lm(y2~x)

#Viewing the second plot to get coefficients
model2

#Naming coefficients A and B for wavelength 3180 cm^-1 linear #regression
A3180 <- coef(model2)[2]
B3180 <- coef(model2)[1]

#Plotting the linear regression in the second plot
abline(model2, col="Red")

#Naming each intensity plot
legend("bottomright",
       c("Intensity at 3340 cm^-1","Intensity at 3180 cm^-1"),
       col=c("Green","Red"), 
       lty=c(1,1))

#Salt concentration calculations ----
# Reading the 3 salt-concentration files 
NS<-read.csv("1-NS_droplets.csv", header=TRUE)
US<-read.csv("4-US_droplets.csv", header=TRUE)
HS<-read.csv("5-HS_droplets.csv", header=TRUE)

#Calculating salt concentrations in the droplets 

#Salt concentration for 1-Normal salted ----
#Normal salted gets the shortcut NS
#Controlling the data by giving it a view
View(NS)

#First calculating the relative intensity for NS
I_NS=NS$mean_intensity_3340/NS$mean_intensity_3180

#Calculate the concentration for 1-NS
#Use equation 1
C_salt_NS <- (B3180*I_NS-B3340)/(A3340-A3180*I_NS)
C_salt_NS

#Plot for NS concentration vs. droplet size without log
plot(NS$droplet_area_um2,C_salt_NS,
     main="Concentration at each droplet size for NS",
     xlab="Droplet size [um^2]",
     ylab="Concentration [W %]")

#Plot for NS concentration vs. droplet size with log
plot(log(NS$droplet_area_um2),C_salt_NS,
     main="Concentration at each logarithmic droplet size for NS",
     xlab="log(droplet size [um^2])",
     ylab="Concentration [W %]")

#Salt-concentration for 4-Unsalted ----
#All steps from 1-"Normal salted" are repeated
#Unsalted gets the shortcut US
View(US)

#Calculating the relative intensity for US
I_US=US$mean_intensity_3340/US$mean_intensity_3180

#Calculate the concentration for 4-US
C_salt_US <- (B3180*I_US-B3340)/(A3340-A3180*I_US)
C_salt_US

#Plot for US concentration vs. droplet size without log
plot(US$droplet_area_um2,C_salt_US,
     main="Concentration at each droplet size for US",
     xlab="Droplet size [um^2]",
     ylab="Concentration [W %]")

#Plot for US concentration vs. droplet size with log
plot(log(US$droplet_area_um2),C_salt_US,
     main="Concentration at each logarithmic droplet size for US",
     xlab="log(droplet size [um^2])",
     ylab="Saltconcentration [W %]")

#Salt-concentration for 5-Highly salted ----
#All steps from 1-"Normal salted" are repeated
#Highly salted gets the shortcut HS
View(HS)
#Calculating the relative intensity for HS
I_HS=HS$mean_intensity_3340/HS$mean_intensity_3180

#Calculate the concentration for 5-HS
C_salt_HS <- (B3180*I_HS-B3340)/(A3340-A3180*I_HS)
C_salt_HS

#Plot for HS concentration vs. droplet size without log
plot(HS$droplet_area_um2,C_salt_HS,
     main="Concentration at each droplet size for HS",
     xlab="log(droplet size [um^2])",
     ylab="Saltconcentration [W %]")

#Plot for HS concentration vs. droplet size with log
plot(log(HS$droplet_area_um2),C_salt_HS,
     main="Concentration at each logarithmic droplet size for HS",
     xlab="log(droplet size [um^2])",
     ylab="Saltconcentration [W %]")


#Barplot for mean salt concentration for 1-NS----
#Creating the mean and the standard deviation for NS


mean_salt_NS<-mean(C_salt_NS)
standard_d_NS<- sd(C_salt_NS)

#Plotting the barplot for the salt-concentration in NS
barplot(C_salt_NS, 
        main="Barplot for 1-NS salt-concentration, w. mean and standard deviations",
        xlab="1-NS samples",
        ylab="Saltconcentration [W %]")

#Plotting mean with standard deviations
abline(h=mean_salt_NS, col="Green", lwd=1.5) #Mean 
abline(h=mean_salt_NS+standard_d_NS, col="Red", lwd=1.5) #Maximum standard deviation
abline(h=mean_salt_NS-standard_d_NS, col="Red", lty=2, lwd=1.5) #Minimum standard deviation

legend("topright",
       c("Maximum standart diviation", "Mean", "Minimum standard diviation"),
       col=c("Red", "Green", "Red"), 
       lty=c(1,1,2))

#Barplot at the mean salt concentration for 4-US----
#Creating the mean and the standard deviation for US
mean_salt_US<-mean(C_salt_US)
standard_d_US<- sd(C_salt_US)

#Plotting the barplot for the salt-concentration in US
barplot(C_salt_US,
        main="Barplot for 4-US salt-concentration, w. mean and standard deviations",
        xlab="4-US samples",
        ylab="Saltconcentration [W %]")

#Plotting mean with standard deviations
abline(h=mean_salt_US, col="Green", lwd=1.5) #Mean
abline(h=mean_salt_US+standard_d_US, col="Red", lwd=1.5) #Maximum standard deviation
abline(h=mean_salt_US-standard_d_US, col="Red", lty=2, lwd=1.5) #Minimum standard deviation

legend("topright",
       c("Maximum standart diviation", "Mean", "Minimum standard diviation"),
       col=c("Red", "Green", "Red"), 
       lty=c(1,1,2))

#Barplot at the mean salt concentration for 5-HS----
#Creating the mean and the standard deviation for HS
mean_salt_HS<-mean(C_salt_HS)
standard_d_HS<- sd(C_salt_HS)

#Plotting the barplot for the salt-concentration in HS
barplot(C_salt_HS,
        main="Barplot for 5-HS salt-concentration, w. mean and standard deviations",
        xlab="5-HS samples",
        ylab="Saltconcentration [W %]")

#Plotting mean with standard deviations
abline(h=mean_salt_HS, col="Green", lwd=1.5) #Mean 
abline(h=mean_salt_HS+standard_d_HS, col="Red", lwd=1.5) #Maximum standard deviation
abline(h=mean_salt_HS-standard_d_HS, col="Red", lty=2, lwd=1.5) #Minimum standard deviation

legend("topright",
       c("Maximum standart diviation", "Mean", "Minimum standard diviation"),
       col=c("Red", "Green", "Red"), 
       lty=c(1,1,2))

#Trying boxplots instead of barplots ----
boxplot(C_salt_NS,
        main="Boxplot for 1-NS salt-concentration",
        xlab="1-NS samples",
        ylab="Saltconcentration [W %]")

boxplot(C_salt_US,
        main="Boxplot for 4-US salt-concentration",
        xlab="4-US samples",
        ylab="Saltconcentration [W %]")

boxplot(C_salt_HS,
        main="Boxplot for 5-HS salt-concentration",
        xlab="5-HS samples",
        ylab="Saltconcentration [W %]")

#Barplot for weighted mean 1-NS ----
#Loading weighted mean
WM_NS<-weighted.mean(C_salt_NS, NS$droplet_area_um2)

#Plotting the barplot
barplot(C_salt_NS,
        main="Barplot for 1-NS salt-concentration, w. weighted mean and standard deviations",
        xlab="1-NS samples",
        ylab="Saltconcentration [W %]")

#Plotting weighted mean with standard deviations
abline(h=WM_NS, col="Green", lwd=1.5) #Weighted mean 
abline(h=WM_NS+standard_d_NS, col="Red", lwd=1.5) #Maximum #standard deviation
abline(h=WM_NS-standard_d_NS, col="Red", lty=2, lwd=1.5) #Minimum #standard deviation

legend("topright",
       c("Maximum standart diviation", "Weighted mean", "Minimum standard diviation"),
       col=c("Red", "Green", "Red"), 
       lty=c(1,1,2))

#Barplot for weighted mean 4-US ----
#Loading weighted mean
WM_US<-weighted.mean(C_salt_US, US$droplet_area_um2)

#Plotting the barplot
barplot(C_salt_US,
        main="Barplot for 4-US salt-concentration, w. weighted mean and standard deviations",
        xlab="4-US samples",
        ylab="Saltconcentration [W %]")

#Plotting weighted mean with standard deviations
abline(h=WM_US, col="Green", lwd=1.5) #Weighted mean 
abline(h=WM_US+standard_d_US, col="Red", lwd=1.5) #Maximum #standard deviation
abline(h=WM_US-standard_d_US, col="Red", lty=2, lwd=1.5) #Minimum #standard deviation

legend("topright",
       c("Maximum standart diviation", "Weighted mean", "Minimum standard diviation"),
       col=c("Red", "Green", "Red"), 
       lty=c(1,1,2))

#Barplot for weighted mean 5-HS ----
#Loading weighted mean
WM_HS<-weighted.mean(C_salt_HS, HS$droplet_area_um2)

#Plotting the barplot
barplot(C_salt_HS,
        main="Barplot for 5-HS salt-concentration, w. weighted mean and standard deviations",
        xlab="5-HS samples",
        ylab="Saltconcentration [W %]")

#Plotting weighted mean with standard deviations
abline(h=WM_HS, col="Green", lwd=1.5) #Weighted mean 
abline(h=WM_HS+standard_d_HS, col="Red", lwd=1.5) #Maximum #standard deviation
abline(h=WM_HS-standard_d_HS, col="Red", lty=2, lwd=1.5) #Minimum #standard deviation

legend("topright",
       c("Maximum standart diviation", "Weighted mean", "Minimum standard diviation"),
       col=c("Red", "Green", "Red"), 
       lty=c(1,1,2))

#Creating histograms for 1-NS ----
#Histogram with 10 bins
Histogram_NS<-hist(C_salt_NS,breaks=10,
                   main="Histogram for 1-NS salt-concentration w. 10 bins",
                   xlab="Saltconcentration [W %]",
                   ylab="Frequency")

#Adding the normal curve
xfit <- seq(min(C_salt_NS),max(C_salt_NS), length=500)
yfit<- dnorm(xfit,mean(C_salt_NS), sd(C_salt_NS))
scaling <-Histogram_NS$counts[1]/Histogram_NS$density[1]
y=yfit*scaling
lines(xfit,y, col="Blue")

#Histogram with 100 bins
Histogram_NS<-hist(C_salt_NS,breaks=100,
                   main="Histogram for 1-NS salt-concentration w. 100 bins",
                   xlab="Saltconcentration [W %]",
                   ylab="Frequency")

#Adding the normal curve
xfit <- seq(min(C_salt_NS),max(C_salt_NS), length=500)
yfit<- dnorm(xfit,mean(C_salt_NS), sd(C_salt_NS))
scaling <-Histogram_NS$counts[1]/Histogram_NS$density[1]
y=yfit*scaling
lines(xfit,y, col="Blue")

#Density plot for 1-NS ----
#First, loading the density for salt concentration
d=density(C_salt_NS)

#Loading the plot for density
plot(d,
     main="Density for distribution of salt in 1-NS",
     xlab="Saltconcentration [W %]",
     ylab="Density [% pr. um^2]")

#Salt-concentration density for all three samples ----
#Creating a vector for all concentrations
All_Concentrations <-c(C_salt_NS,C_salt_US,C_salt_HS)

#Creating a vector for concentration labels
All_Concentrations_Labels <- c(rep(1,length(C_salt_NS)),rep(2,length(C_salt_US)),rep(3,length(C_salt_HS)))

#Using SM density compare to view the comparison                              
library(sm)

sm.density.compare(All_Concentrations,All_Concentrations_Labels,
                   main="Density comparison of saltconcentraions for all datasets",
                   xlab="Saltconcentraion [W %]",
                   ylab="Density [% pr. um^2]")

legend("topright",
       c("1-NS","4-US","5-HS"),col=c("Red","Green","Blue"),
       lty=c(1,2,2))

#Barplot for mean droplet size ----

#Loading the mean for droplet sizes(one for each dataset)
mean_droplet_NS <- mean(NS$droplet_area_um2)
mean_droplet_US <- mean(US$droplet_area_um2)
mean_droplet_HS <- mean(HS$droplet_area_um2)

#Loading the standard deviation for droplet sizes(one for each dataset)
sd_NS <- sd(NS$droplet_area_um2)
sd_US <- sd(US$droplet_area_um2)
sd_HS <- sd(HS$droplet_area_um2)

#Making a barplot for the dropletsize of NS
barplot(NS$droplet_area_um2,
        main="Barplot for 1-NS dropletsize, w. mean and standard deviations",
        xlab="1-NS samples",
        ylab="Saltconcentration [W %]",
        ylim=c(-50,250))
#Plotting mean with standard deviations
abline(h=mean_droplet_NS, col="green", lwd=1.5) #Mean
abline(h=mean_droplet_NS+sd_NS, col="Red", lwd=1.5) #Maximum #standard deviation
abline(h=mean_droplet_NS-sd_NS, col="Red", lty=2, lwd=1.5) #Minimum #standard deviation

legend("topright",
       c("Maximum standart diviation", "Mean", "Minimum standard diviation"),
       col=c("Red", "Green", "Red"), 
       lty=c(1,1,2))


#Making a barplot for the dropletsize of US
barplot(US$droplet_area_um2,
        main="Barplot for 4-US dropletsize, w. mean and standard deviations",
        xlab="4-US samples",
        ylab="Saltconcentration [W %]",
        ylim=c(-50,800))
#Plotting mean with standard deviations
abline(h=mean_droplet_US, col="green", lwd=1.5) #Mean
abline(h=mean_droplet_US+sd_US, col="Red", lwd=1.5) #Maximum #standard deviation
abline(h=mean_droplet_US-sd_US, col="Red", lty=2, lwd=1.5) #Minimum #standard deviation

legend("topright",
       c("Maximum standart diviation", "Mean", "Minimum standard diviation"),
       col=c("Red", "Green", "Red"), 
       lty=c(1,1,2))

#Making a barplot for the dropletsize of HS
barplot(HS$droplet_area_um2,
        main="Barplot for 5-HS dropletsize, w. mean and standard deviations",
        xlab="5-HS samples",
        ylab="Saltconcentration [W %]",
        ylim=c(-50,500))
#Plotting mean with standard deviations
abline(h=mean_droplet_HS, col="green", lwd=1.5) #Mean
abline(h=mean_droplet_HS+sd_HS, col="Red", lwd=1.5) #Maximum #standard deviation
abline(h=mean_droplet_HS-sd_HS, col="Red", lty=2, lwd=1.5) #Minimum #standard deviation

legend("topright",
       c("Maximum standart diviation", "Mean", "Minimum standard diviation"),
       col=c("Red", "Green", "Red"), 
       lty=c(1,1,2))

#Plot of size distribution ----
#Creating a plot for each of the droplet sizes
plot(NS$droplet_area_um2,
     main="1-NS droplet sizes",
     xlab="1-NS samples",
     ylab="Droplet size [um^2]")

plot(US$droplet_area_um2,
     main="4-US droplet sizes",
     xlab="4-US samples",
     ylab="Droplet size [um^2]")

plot(HS$droplet_area_um2,
     main="5-HS droplet sizes",
     xlab="5-HS samples",
     ylab="Droplet size [um^2]")

#Creating a log-plot for each of the droplet sizes
plot(log(NS$droplet_area_um2),
     main="Logarithmic 1-NS droplet sizes",
     xlab="1-NS samples",
     ylab="log(Droplet size [um^2])")

plot(log(US$droplet_area_um2),
     main="Logarithmic 4-US droplet sizes",
     xlab="4-US samples",
     ylab="log(Droplet size [um^2])")

plot(log(HS$droplet_area_um2),
     main="Logarithmic 5-HS droplet sizes",
     xlab="5-HS samples",
     ylab="log(Droplet size [um^2])")

#Total salt concentration ----
#This calculation uses equation 2
#Weighted mean is already loaded

#Loading water density and butter density 
Water_density <- 1
Butter_density <- 0.911

#Calculating total salt concentrations for NS
Total_area_NS <- 104661
Total_area_NS_pr_droplet <- sum(NS$droplet_area_um2)/Total_area_NS

(WM_NS*Water_density/Butter_density)*Total_area_NS_pr_droplet

#Calculating total salt concentrations for US
Total_area_US <- 338514
Total_area_US_pr_droplet <- sum(US$droplet_area_um2)/Total_area_US

(WM_US*Water_density/Butter_density)*Total_area_US_pr_droplet

#Calculating total salt concentrations for HS
Total_area_HS <- 41864
Total_area_HS_pr_droplet <- sum(HS$droplet_area_um2)/Total_area_HS

(WM_HS*Water_density/Butter_density)*Total_area_HS_pr_droplet
