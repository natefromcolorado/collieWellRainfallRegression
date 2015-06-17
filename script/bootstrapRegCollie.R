#################################################
## Date: 09/18/2014
## Title: Multiple linear regression block bootstrapping
#################################################

setwd("L:/Hydro Eval/Staff/Nathan/R/scripts/collieWellRainfallRegression/")

# install.packages("hydroGOF") ## all GOF functions
# install.packages("lubridate") ## date package
# install.packages("hydroGOF")

library(hydroGOF)
library(lubridate)

#All data file location.  Enter block size and change header to TRUE or FALSE
file.location = "data/dataCrookedLake.csv"

# Path for output files.  This directory must be existing
out.path <- "output/"
block_size <- 60 # (30 years 360, 20 years 240, 10 years 120, 168 for 2000-2014)
no_bootstrap_realiz <- 1
bootstrap = "N" # (Y/N) bootstrap the results or just go through different consecutive periods "N"
dep.var <-  "levelCrookedLake"  # Elev_C0120, Elev_M.0048, Discharge_Silver.River.German  y-variable in the text file
dep.type = "Water Level (NAVD88, ft)" #  "Water Level (NAVD88, ft)", "Discharge (cfs)"

calib.start = as.Date("1946-01-01")
calib.end = as.Date("1950-12-31")
resid.start = as.Date("2000-01-01")
resid.end = as.Date("2010-12-31")

alldata <- read.csv(file.location, header = TRUE)
alldata$monthYear = as.Date(alldata$monthYear, format = "%m/%d/%Y")
calib.data = subset(alldata, subset = alldata$monthYear < calib.end & alldata$monthYear> calib.start)
n_row = nrow(calib.data)
n_col<- ncol(alldata)

#convert all data to numeric so that it can be compared to pred.all
mat.alldata <- data.matrix(alldata[dep.var])
mat.calib.data <- data.matrix(calib.data[dep.var])
alldata.y <- as.numeric(mat.alldata)
calib.data.y <- as.numeric(mat.calib.data)
n_blocks <- as.integer(n_row/block_size)

#Make a data frame for gof stats so that all realizations can be easily compared
df.gof <- data.frame(matrix(nrow=21, ncol=no_bootstrap_realiz)) 
sum.stat.table = data.frame(date.start = as.Date(character()), 
                            date.end = as.Date(character()), 
                            R2 = numeric(), 
                            DF = numeric(), 
                            intercept = numeric(),
                            slope = numeric(),
                            RMSE = numeric())

#Loop for the number of bootstrap realizations
for (r in 1:no_bootstrap_realiz) {
  #Make a data frame to store the bootstrap sample in
  boot = data.frame(matrix(vector(), 0, length(names(calib.data)),
                           dimnames=list(c(), names(calib.data))), stringsAsFactors=F)
  df_boot = data.frame(matrix(vector(), 0, length(names(calib.data)),
                              dimnames=list(c(), names(calib.data))), stringsAsFactors=F)
  index = block_size
  
  if(bootstrap == "Y"){
    for (j in 1:n_blocks){
      block <- as.integer(runif(1, 0, n_blocks))
      boot = calib.data[seq((block*block_size),(block*block_size+block_size)),]
      df_boot = rbind(df_boot, boot)
    }
  }else{
    if ((r-1) > n_blocks){break}
    boot = calib.data[seq(((r-1)*block_size),((r-1)*block_size+block_size)),]
    df_boot = boot
  }
  #write the output bootstrap data frame to a text file
  #   out.file <- paste(out.path, "BlockbootRealiz", r, ".csv", sep= "")
  #   write.csv(df_boot, file=out.file)
  
  ####################Additional user input
  #Note: this equation will have to be changed based on the column names of the dependent variable and regressors
  
  fit_boot <- lm(levelCrookedLake ~ 
                   # Precip24_Month,  # for simple linear regression 24 month accumlated time series
                   #                    factor(month) +
                   rainfallFinal+
                   rainfallFinal1+
                   rainfallFinal2+
                   rainfallFinal3+
                   rainfallFinal4+
                   rainfallFinal5+
                   rainfallFinal6+
                   rainfallFinal7+
                   rainfallFinal8+
                   rainfallFinal9+
                   rainfallFinal10+
                   rainfallFinal11+
                   rainfallFinal12+
                   rainfallFinal13+
                   rainfallFinal14+
                   rainfallFinal15+
                   rainfallFinal16+
                   rainfallFinal17+
                   rainfallFinal18+
                   rainfallFinal19+
                   rainfallFinal20+
                   rainfallFinal21+
                   rainfallFinal22+
                   rainfallFinal23+
                   rainfallFinal24,
#                    rainfallFinal25+
#                    rainfallFinal26+
#                    rainfallFinal27+
#                    rainfallFinal28+
#                    rainfallFinal29+
#                    rainfallFinal30+
#                    rainfallFinal31+
#                    rainfallFinal32+
#                    rainfallFinal33+
#                    rainfallFinal34+
#                    rainfallFinal35+
#                    rainfallFinal36,
#                    rainfallFinal37+
#                    rainfallFinal38+
#                    rainfallFinal39+
#                    rainfallFinal40 +
#                    rainfallFinal41+
#                    rainfallFinal42+
#                    rainfallFinal43+
#                    rainfallFinal44+
#                    rainfallFinal45+
#                    rainfallFinal46+
#                    rainfallFinal47+
#                    rainfallFinal48,
                 #                    rainfallFinal49+
                 #                    rainfallFinal50+
                 #                    rainfallFinal51+
                 #                    rainfallFinal52+
                 #                    rainfallFinal53+
                 #                    rainfallFinal54+
                 #                    rainfallFinal55+
                 #                    rainfallFinal56+
                 #                    rainfallFinal57+
                 #                    rainfallFinal58+
                 #                    rainfallFinal59+
                 #                    rainfallFinal60,
                 #                                   rainfallFinal61+
                 #                                   rainfallFinal62+
                 #                                   rainfallFinal63+
                 #                                   rainfallFinal64+
                 #                                   rainfallFinal65+
                 #                                   rainfallFinal66+
                 #                                   rainfallFinal67+
                 #                                   rainfallFinal68+
                 #                                   rainfallFinal69+
                 #                                   rainfallFinal70,
                 na.action = na.omit, data=df_boot)
  
  ####################End additional user input
  reg.sum = summary(fit_boot)
  
  # Export summary of regression
  summary.file <- paste(out.path, "RegSummary_", r, ".csv", sep= "")
  out<-capture.output(summary(fit_boot))
  cat(out,file=summary.file,sep="\n",append=TRUE)
  
  sum.stat = data.frame(date.start = min(df_boot$monthYear), 
                        date.end = max(df_boot$monthYear), 
                        R2 = format(reg.sum$adj.r.squared, digits = 3), 
                        DF = format(reg.sum$df[2]), 
                        intercept = fit_boot$coefficients[1], 
                        slope = fit_boot$coefficients[2], 
                        RMSE = format(sqrt(mean(fit_boot$residuals^2)), digits = 3))
  sum.stat.table = rbind(sum.stat.table, sum.stat)
  
  ## Export the predictions on the entire data set
  pred.all <- predict.lm(fit_boot, alldata) #this is the prediction on the entire data set
  pred.calib <- predict.lm(fit_boot,calib.data) #this is the prediction on the calibration data set
  #   pred.all.file <- paste(out.path,"AllDataPred_", r, ".csv", sep="")
  #   write.csv(pred.all, file=pred.file)
  
  #### FIGURES ###
  
  
  ## FIGURE - MLR Coefficient graphic
  jpeg.out.name <- paste(out.path,"Coefficient_", r, ".jpg", sep= "")
  jpeg(jpeg.out.name, width=8, height=8, units= 'in', res= 300)
  par(mfrow =c(2,1))  
  reg.r2 = reg.sum$adj.r.squared
  reg.sum.p = 1-reg.sum$coefficients[,4]
  reg.sum.sum = reg.sum$coefficients[,1]
  #   barplot(height = as.vector(reg.sum.sum)[2:67], names.arg = c(month.abb,0,seq(2:54)), # 
  barplot(height = as.vector(reg.sum.sum)[2:67], names.arg = c(0:65), 
          xlab = "Lag", ylab = expression(beta), main = substitute(paste("Coefficient Estimate (", beta, ")")))
  #   barplot(height = as.vector(reg.sum.p)[2:67], names.arg = c(month.abb,0,seq(2:54)), xlab = "Lag", 
  barplot(height = as.vector(reg.sum.p)[2:67], names.arg = c(0:65), xlab = "Lag", 
          main = substitute(paste("Significance (1-", italic(p), ")", sep = "")), 
          ylab = substitute(paste("Significance (1-", italic(p), ")", sep = "")))
  abline(h = 0.90, col = "red")
  dev.off()
  
  ## FIGURE - Observed vs Simulated Times series
  jpeg.out.name <- paste(out.path,"ObsVSimCalib_", r, ".jpg", sep= "")
  jpeg(jpeg.out.name, width=8, height=4, units= 'in', res= 300)
  simCalib = zoo(as.vector(pred.calib[1:n_row]), calib.data$monthYear[1:n_row])
  obsCalib = zoo(calib.data.y, calib.data$monthYear[1:n_row])
  calib.resid = (obsCalib - simCalib)
  plot(calib.resid)  
  plot(obsCalib, calib.resid)
  ggof(simCalib,obsCalib, na.rm=TRUE, 
       ylab = dep.type, 
       xlab = "Date", tick.tstep = "years", lab.tstep = "years", lab.fmt = "%Y"
       #        ,cal.ini = c(min(df_boot$monthYear, na.rm = TRUE), max(df_boot$monthYear, na.rm = TRUE))
  )
  dev.off()
  
  ## FIGURE - Residuals time series with average resdual between resid.start and resid.end
  jpeg.out.name <- paste(out.path,"Residual_",r,"_", year(calib.start),".", year(calib.end), ".jpg", sep= "")
  jpeg(jpeg.out.name, width=8, height=4, units= 'in', res= 300)
  sim = zoo(as.vector(pred.all), alldata$monthYear)
  obs = zoo(alldata.y, alldata$monthYear)
  resid = (obs - sim)
  avg.resid = round(median(resid,  na.rm = TRUE), digits = 2)
  resid.dates = seq(resid.start, resid.end, by = "day")  
  resid1 = resid[resid.dates]
  avg.resid1 = round(median(resid1,  na.rm = TRUE), digits = 2)
  plot(na.trim(resid), ylab = "Residual(Obs - Sim)", xlab = "Year")
  legend("bottomright", 
         legend = c(
           paste("Avg Resid (", year(resid.start), "-", year(resid.end),") =", avg.resid1, sep = ""),
           "Lowess f = 0.3", "Lowess f = 0.6"), col = c("red", "purple", "blue"), lty = c(2,3,4),
         bty = "n",cex = 0.75, merge = F, border=F)
  abline(h = c(0), col = c("black"), lty = c(1))
  lines(x = c(resid.start, resid.end), y = c(avg.resid1, avg.resid1), type = "l", col = c("red"), lty = c(2))
  abline(v = c(resid.start,resid.end), col = c("red", "red"), lty = c(1,1))
  lo.3 = lowess(na.trim(resid), f = 0.3)
  lo.6 = lowess(na.trim(resid), f = 0.6)
  lines(index(na.trim(resid)), lo.3$y, lty = 3, col = 'purple')
  lines(index(na.trim(resid)), lo.6$y, lty = 4, col = 'blue')
  dev.off()
  
  ## FIGURE - CHECK RESDIDUALS FOR NON-LINEARITY
  plot(obsCalib,calib.resid, main = "Residuals vs. Observed") # shows non-linear relationship
  resid.check = lm(obsCalib~calib.resid)
  abline(resid.check)
  
  
  ## FIGURE - Observed vs simulated over whole period of record
  jpeg.out.name <- paste(out.path,"ObsVSim_", r, ".jpg", sep= "")
  jpeg(jpeg.out.name, width=7, height=4, units= 'in', res= 300)
  ggof(sim,obs, na.rm=TRUE, ylab=dep.type, xlab = "Date", 
       tick.tstep = 'years', lab.tstep = "years", lab.fmt = "%Y", 
       gof.leg = FALSE, cex = c(0.25,0.25), cex.axis = 0.75, cex.lab = 0.8, leg.cex = 1,
       cal.ini = c(min(df_boot$monthYear, na.rm = TRUE), 
                   max(df_boot$monthYear, na.rm = TRUE)))  # vert red line calibration period
  dev.off()
  
  gof.stats = gof(simCalib, obsCalib, na.rm=TRUE) # gof stats for individual model
  df.gof[r]<- rbind(gof.stats, avg.resid) # creates data batle of all gof.stats for each model
  #   write.csv(gof.stats, file=gof.file)
  #   gof.file <- paste(out.path,"AllDataGOF_", r, ".csv", sep="")
}

## FIGURE - R2 model metric. The R2 can be that of only the bootstrap data "as.numeric(as.vector(sum.stat.table$R2))" or over the calibration period "R2.num"
mat.R2 <- data.matrix(df.gof[17, 1:no_bootstrap_realiz])
R2.num <- as.numeric(mat.R2)
jpeg.out.name <- paste(out.path,"Stats_R2.jpg", sep= "")
jpeg(jpeg.out.name, width=8, height=4, units= 'in', res= 300)
# hist(as.numeric(as.vector(sum.stat.table$R2)), 
#      xlab = substitute(paste("Coefficient of Determination ",R^2)), 
#      main = substitute(paste("Distribution ", R^2)))
hist(x = R2.num, xlab = substitute(paste("Coefficient of Determination ",R^2)), 
     main = substitute(paste("Distribution ", R^2)))
dev.off()

## FIGURE - Distirubtion of average residual between  resid.start and resid.end
jpeg.out.name <- paste(out.path,"Stats_avg.resid.jpg", sep= "")
jpeg(jpeg.out.name, width=12, height=4, units= 'in', res= 300)
par(mfrow = c(1,2))
t.df.gof = t(df.gof) # transpose the df.gof for next step
resid.good = as.numeric(subset(t.df.gof, subset = t.df.gof[,7] > 0.7)[,21]) # average residuals of models with R2 greater than 0.7
avg.resid.num <- as.numeric(data.matrix(df.gof[21, 1:no_bootstrap_realiz])) # residual of all models
hist(x = avg.resid.num, xlab = "Average Residual", 
     main = paste("Distribution Residual (", year(resid.start),"-", year(resid.end), ")", sep = ""))
boxplot(x = avg.resid.num, ylab = "Average Residual", 
        main = paste("Distribution Residual (", year(resid.start),"-", year(resid.end), ")", sep = ""))
dev.off()

# quantile(resid.good, c(1, 0.95, .75,.5,.25,0.05,0))
# quantile(avg.resid.num, c(1, 0.95, .75,.5,.25,0.05,0)) # quantile of residuals

## FIGURES OF METRICS
# mat.PBIAS <- data.matrix(df.gof[6, 1:no_bootstrap_realiz])
# PBIAS.num <- as.numeric(mat.PBIAS)
# jpeg.out.name <- paste(out.path,"Stats_PBIAS.jpg", sep= "")
# jpeg(jpeg.out.name, width=8, height=4, units= 'in', res= 300)
# hist(x = PBIAS.num,xlab = "Percent Bias (PBIAS)", main = "Distribution PBIAS")
# dev.off()
# 
# mat.MAE <- data.matrix(df.gof[2, 1:no_bootstrap_realiz])
# MAE.num <- as.numeric(mat.MAE)
# jpeg.out.name <- paste(out.path,"Stats_MAE.jpg", sep= "")
# jpeg(jpeg.out.name, width=8, height=4, units= 'in', res= 300)
# hist(x = MAE.num,xlab = "Mean Absolute Error (MAE)", main = "Distribution MAE")
# dev.off()
# 
# mat.MSE <- data.matrix(df.gof[3, 1:no_bootstrap_realiz])
# MSE.num <- as.numeric(mat.MSE)
# jpeg.out.name <- paste(out.path,"Stats_MSE.jpg", sep= "")
# jpeg(jpeg.out.name, width=8, height=4, units= 'in', res= 300)
# hist(x = MSE.num,xlab = "Mean Squared Error (MSE)", main = "Distribution MSE")
# dev.off()
# 
# mat.RMSE <- data.matrix(df.gof[4, 1:no_bootstrap_realiz])
# RMSE.num <- as.numeric(mat.RMSE)
# jpeg.out.name <- paste(out.path,"Stats_RMSE.jpg", sep= "")
# jpeg(jpeg.out.name, width=8, height=4, units= 'in', res= 300)
# hist(x = RMSE.num,xlab = "Root Mean Squared Error (RMSE)", main = "Distribution RMSE")
# dev.off()
# 
# mat.NSE <- data.matrix(df.gof[9, 1:no_bootstrap_realiz])
# NSE.num <- as.numeric(mat.NSE)
# jpeg.out.name <- paste(out.path,"Stats_NSE.jpg", sep= "")
# jpeg(jpeg.out.name, width=8, height=4, units= 'in', res= 300)
# hist(x = NSE.num,xlab="Nash Sutcliffe Efficiency (NSE)", main = "Distribution NSE")
# dev.off()
# 
# gof.file <- paste(out.path,"AllDataGOF_Summary.csv", sep="")
# write.csv(df.gof, file=gof.file)

## FIGURE - regression plot of one variable
# jpeg.out.name <- paste(out.path,"Regression.jpg", sep= "")
# jpeg(jpeg.out.name, width=7, height=7, units= 'in', res= 300)
# plot(calib.data[["Elev_M.0048"]] ~ calib.data[["Precip24_Month"]], xlab = "Precip 24 Month Accumulation", ylab = "Elev M-0048")
# abline(fit_boot)
# final.reg = fit_boot
# legend(x = "topleft", bty= "n",
#          legend = c(paste("R2 =", format(summary(final.reg)$adj.r.squared, digits = 3)), 
#                     paste("DF =", format(summary(final.reg)$df[2])),
#                     paste("RMSE =", format(summary(final.reg)$sigma, digits = 2))
#                     )
#   )
# dev.off()