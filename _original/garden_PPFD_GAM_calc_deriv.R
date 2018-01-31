### Calculate derivative
rm(list = ls())
setwd("C:/Users/lisanjie2/Dropbox/0_Brouwer_in_prep/Heberling_2016/Heberling_2016_final_workup/Nathans_final_stuff")

### Load raw data
par1 <- read.csv("garden_sensor_data_cleaned.csv")

### Load garden data that has been summarized by mid-day mean
par.mean <- read.csv("garden_sensor_data_10amTo2pm_mean.csv")



pdf("Appendix_PPFD_garden_sensor_daily_means.pdf",
    width=6,height=4)

### Plot midday mean vs. Julian w/error bars
library(ggplot2)
qplot(y = mean,
        x = (julian),
          geom = c("point","smooth"),
          data = par.mean) +
    geom_errorbar((aes(ymin = mean-2*SE, 
                       ymax = mean+2*SE))) +
  theme_bw() + ylab("Mid-day mean PPFD") +
   xlab("Date")

dev.off()



######################
### Model Data w/GAM
######################

### Use GAM to model how PAR decreases w/ time
### 
### This could be fanicer by adding autocorrelation to the errors
### but I don't think that accomplished anything really
### Also, instead of means for 10am to 2 PM we could model
### raw data w/ random effects with a gamm, but that wouldn't
### add anything to the anlysis.

library(mgcv) #mgcv 1.8-12
# Wood, S.N. (2006) Generalized Additive Models:
#   An Introduction with R. Chapman and Hall/CRC.


#Run gam 
gam1 <- gam(mean ~ s(julian), data = par.mean)

par(mfrow = c(1,1), mar = c(2,2,2,2)+2)

#extract predicted values from gam
preds.out <- predict(gam1)
par.mean$preds <- preds.out




####################################################################
## Differentiating the smooths in a model (with CIs for derivatives)
####################################################################

#Calculate derivative of gam smooth
#   see ?predict.gam for original code 
#   following code notes are copied from there with some
#   additional notes by NLB

## "evaluate derivatives of smooths with associated standard 
## errors, by finite differencing..."

# set up values to make predictions accorss
x.mesh <- seq(min(par.mean$julian),
              max(par.mean$julian),
              length=dim(par.mean)[1]) ## where to evaluate derivatives
newd <- data.frame(julian = x.mesh)
X0 <- predict(gam1,newd,type="lpmatrix") 


## set up value to perturb data by to approx the differentiation
## "finite difference interval to approximate differenatiation"
eps <- 1e-7 

## shift the evaluation mesh
x.mesh <- x.mesh + eps 

#make new data w/shifted values
#   key to this method is the use of 
#   type="lpmatrix" which 
#   out puts the coefficients for the udnerlying
#   GAM equations instead of actual predictiosn
newd <- data.frame(julian = x.mesh)
X1 <- predict(gam1,newd,type="lpmatrix")


#look at output of predicatio - note that there are 9 columns
# produced, not that 1, b/c the 9 underlying game equations are
# being called up
temp <- round(X1,2)
colnames(temp) <- gsub("julian","j",colnames(temp))
colnames(temp) <- gsub("Intercept","In",colnames(temp))
head(temp)
#    (Int) s(j).1 s(j).2 s(j).3 s(j).4 s(j).5 s(j).6 s(j).7 s(j).8 s(j).9
# 1    1  -1.11  -0.26   1.39  -0.35   1.51   0.41  -1.57  -1.12  -1.71
# 2    1  -1.11  -0.24   1.39  -0.33   1.51   0.39  -1.57  -1.08  -1.67
# 3    1  -1.11  -0.23   1.38  -0.31   1.51   0.37  -1.57  -1.04  -1.63
# 4    1  -1.11  -0.21   1.38  -0.29   1.50   0.35  -1.56  -0.99  -1.58
# 5    1  -1.11  -0.20   1.38  -0.28   1.49   0.32  -1.55  -0.95  -1.54
# 6    1  -1.11  -0.19   1.38  -0.26   1.48   0.29  -1.52  -0.90  -1.50


#divide perturbed and original coefficients 
Xp <- (X1-X0)/eps ## "maps coefficients to (fd approx.) derivatives" (?)


### Carry out differentiation
#  the value "9" is used b/c that "basis" for the GAM has 9 terms
summary(gam1)

Xi <- Xp*0 #empty dataframe

#Xi[,(i-1)*9+1:9+1] <- Xp[,(i-1)*9+1:9+1] ## Xi%*%coef(b) = smooth deriv i
Xi[,1:9+1] <- Xp[,1:9+1] ## Xi%*%coef(b) = smooth deriv i
df <- Xi%*%coef(gam1)                ##  smooth derivative 
df.sd <- rowSums(Xi%*%gam1$Vp*Xi)^.5 ## cheap diag(Xi%*%b$Vp%*%t(Xi))^.5






#####################################
### Plot derivative & CI for line ###
#####################################
### Set up to send output to PDF
pdf("Appendix_PPFD_garden_sensor_derivative.pdf",
    width=6,height=4)

par(mar = c(4,4,1,8.5),
    xpd=TRUE)

#xpd=TRUE prevents clipping of legend when plotted outside of figure
#http://stackoverflow.com/questions/3932038/plot-a-legend-outside-of-the-plotting-area-in-base-graphics
plot(x.mesh,df,
     type="l",
     ylim=range(c(df+2*df.sd,df-2*df.sd)),
     xlab = "Julian Date",
     ylab = "Derivative")

#Plot 95%CI of derivative
lines(x.mesh,df+2*df.sd,lty=2);lines(x.mesh,df-2*df.sd,lty=2)



leg <- c("GAM\nderivative\n",
         "95% CI",
         "Deriv = 0",
         "1st day\nDeriv = 0",
         "1st day\n95% CI = 0")

lty.x <- 2
col.x <- 3

legend("topright",bty = "n",#cex = 0.9,
       legend = leg,
       cex = 0.9,
       lty = c(1,2,1,lty.x,lty.x +1),
       col = c(1,1,2,col.x,col.x+1),inset=c(-0.385,0))


#Reset xpd to keep colored lines w/in plot
par(    xpd=F)

#plot deriv = 0 horizontal line 
abline(h = 0, col = 2)

#plot 1st day when deriv = 0
day.deriv.0 <- x.mesh[which(df> 0)[1]]
abline(v = day.deriv.0, col =col.x, lty = lty.x)

text(x = day.deriv.0,y = -30,
     col = 3,
     labels = round(day.deriv.0,0),
     pos = 4)

date1 <-paste(par1[which(par1$julian == 153)[1], c("month","day")],
              sep = "/",
              collapse = "/")

date1 <- paste(date1,"2016",
               sep = "/")
text(x = day.deriv.0,y = -35,
     col = 3,
     labels = date1,
     pos = 4)




#plot 1st day when deriv not sig different from zero
day.deriv.0b <- x.mesh[which((df+2*df.sd)> 0)[1]]
abline(v = day.deriv.0b, col = col.x+1, lty = lty.x +1)
text(x = day.deriv.0b-10,y = -30,
     col = 4,
     labels = round(day.deriv.0b,0),pos=4)
date2 <- paste(par1[which(par1$julian == 143)[1], c("month","day")],
               collapse = "/")
date2 <- paste(date2,"2016",sep = "/",
               collapse = "/")
text(x = day.deriv.0b-20,y = -35,
     col = 4,
     labels = date2,pos=4)



## turn off device for plotting
dev.off()






