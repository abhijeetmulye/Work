####################################################################################################
# Purpose: 
# Check if <Important business metric> is driven by FX rates
# Creates linear models of the form metric1 ~ metric2 + FX rate pair 1 + FX rate pair 2 
# and plots the results
# Repeats for 2 types of business sources Type 1 and Type 2
# All terms are YoY %
#
# Input data: <Datafile 1> or <Datafile 2>, fxData.csv
# Output: PDF containing color-coded plots, Text file containing summary of Regression models 
# (Green if FX rate matters to metric1, Red otherwise, Purple if metric2 doesn't matter to metric1)
# 
# Instructions:
# Set directory pointing to input data, populate lists corridor, sends, rcvrs, Fx pairs and run code
# Repeat after removing any insignificant secondary Fx pairs
####################################################################################################

rm(list=ls())
library(xts)

# This pdf contains results

pdf(paste("Linear model plots ", format(Sys.Date(), format = '%Y-%m-%d'), ".pdf", sep = ""))
report = ""

setwd("C:/Users/abmulye/Desktop/CA-US TPV~FX adhoc")

# !Manual! 
# Set ppData to contain required Datafile

ppData = read.table("<Datafile 1>", sep="\t", header=TRUE, stringsAsFactors = FALSE)
ppData$wkEndgDate= as.Date(ppData$wkEndgDate,"%m/%d/%Y")


fxData = read.table("fxData.csv", sep=",", header=TRUE)
fxData$wkEndgDate = as.Date(fxData$X,"%m/%d/%Y")
fxData$wkEndgDate = as.Date(fxData$wkEndgDate-1,"%m/%d/%Y")


ppDataType1 = ppData[ppData$business_segment %in% "Type1",]
ppDataType2 = ppData[ppData$business_segment %in% "Type2",]

############# Function creating linear models and plotting results ##################################

RegressPlot <- function (sndr, rcvr, currencyPairs, corridor, raw) {

  # Computes the sample covariance between two vectors.
  #
  # Args:
  #   sndr: Source of trade
  #   rcvr: Destination of trade
  #   currencyPairs: Fx rate pairs
  #   corridor: Name given to the route
  #   raw: Type of data to be used
  #
  # Returns:
  #   Summary of Regression Model
  # Error handling:
  # If less than 1 year's worth of data is available, prints a message
  # Marks insignificant coefficients in Red
  #
  
  currency_pair1 = currencyPairs[1]
  currency_pair2 = currencyPairs[2]
  
  rawxb = raw[raw$sndr_cntryCode %in% sndr & raw$rcvr_cntryCode %in% rcvr,]
  rawib = raw[raw$sndr_cntryCode %in% sndr & raw$rcvr_cntryCode %in% sndr,]
  
  # Preparing intra-country transfer dataframe
  
  rawxbAgg = aggregate(rawxb$metric_Spot, by=list(wkEndgDate=rawxb$wkEndgDate), FUN = sum, na.rm=TRUE)
  
  rawxbAgg = rawxbAgg[order(as.Date(rawxbAgg$wkEndgDate, format="%m/%d/%Y")),]
  
  names(rawxbAgg) = c("wkEndgDate", "metric_Spot_xb")
  
  rawxbAgg = xts(rawxbAgg$metric_Spot_xb, order.by=rawxbAgg$wkEndgDate)
  
  rawxbAgg_m = apply.monthly(rawxbAgg, "sum")
  
  metric_Spot_xbYoY = (rawxbAgg_m/lag(rawxbAgg_m, 12) -1)*100
  
  rawxbAggXts = na.omit(data.frame(
    cbind(data.frame(wkEndgDate=index(rawxbAgg_m), coredata(rawxbAgg_m)),data.frame(metric_Spot_xbYoY))
  ))
  
  names(rawxbAggXts) = c("wkEndgDate", "metric_Spot_xb", "SR")
  
  # Preparing inter-country transfer dataframe
  
  rawibAgg = aggregate(rawib$metric_Spot, by=list(wkEndgDate=rawib$wkEndgDate), FUN = sum, na.rm=TRUE)
  
  rawibAgg = rawibAgg[order(as.Date(rawibAgg$wkEndgDate, format="%m/%d/%Y")),]
  
  names(rawibAgg) = c("wkEndgDate", "metric_Spot_ib")
  
  rawibAgg = xts(rawibAgg$metric_Spot_ib, order.by=rawibAgg$wkEndgDate)
  
  rawibAgg_m = apply.monthly(rawibAgg, "sum")
  
  metric_Spot_ibYoY = (rawibAgg_m/lag(rawibAgg_m, 12) -1)*100
  
  rawibAggXts = na.omit(data.frame(
    cbind(data.frame(wkEndgDate=index(rawibAgg_m), coredata(rawibAgg_m)),data.frame(metric_Spot_ibYoY))
  ))
  
  names(rawibAggXts) = c("wkEndgDate", "metric_Spot_ib", "SS")
  
  
  # Preparing FX rates YoY
  
  fxData.dates = fxData$wkEndgDate
  
  fxDataXts = xts(fxData[,2:41], order.by=fxData$wkEndgDate)
  
  fxDataYoY1 = apply.monthly(fxDataXts, "mean")
  
  fxDataYoY2 = (fxDataYoY1/lag(fxDataYoY1, 12) -1)*100
  
  fxDataYoY = na.omit(data.frame(
    cbind(data.frame(wkEndgDate=index(fxDataYoY2), coredata(fxDataYoY2)))
  ))
  
  
  # Combining both
  
  rawAggXts = merge(x=rawxbAggXts, y=rawibAggXts, by="wkEndgDate", all.x=TRUE)
  
  if(nrow(rawAggXts<12)){
    message("Not enough data")
  }
  
  tpv_fx = merge(x=rawAggXts, y=fxDataYoY, by="wkEndgDate", all.x=TRUE)
  
  tpv_fx = na.omit(tpv_fx)
  
  
  # Calling linear model
    
  if (is.na(currency_pair2)) {
    fit = lm(paste("SR~SS",currency_pair1, sep="+"),tpv_fx)
  } else{
    fit = lm(paste("SR~SS",currency_pair1, currency_pair2, sep="+"),tpv_fx)
  }
  
      
  print(summary(fit))
  
  # Title of plot
  
  title_fit =  paste0("SR ~ ", round(coefficients(fit)[1],2), "", 
                      paste(sprintf(" %+.2f*%s ", 
                                    coefficients(fit)[-1],  
                                    names(coefficients(fit)[-1])), 
                            collapse="")
  )
  
  # Color coding the title
  
  # Green if the lone FX term is significant else Red
  
  if (is.na(currency_pair2)) {
    if(summary(fit)$coefficients[3,4] < 0.05){
      title_color = "green"
    }
    else{
      title_color="red"
    }
    
  } 
    
  # Green only if both Fx pairs are significant
  
  if(!is.na(currency_pair2)) {
    if(max(summary(fit)$coefficients[3,4],summary(fit)$coefficients[4,4]) < 0.05){
      title_color = "green"
    }
    else{
      title_color="red"
    }
  }
  
  
  # Purple if SS term is not significant. Over-rides Red/Green rules.
  
  if(summary(fit)$coefficients[2,4] >= 0.05){
    title_color = "purple"
  }
  
  # Plotting...
  
  par(mar = c(5, 4, 4, 4) + 0.3)
  
  plot(tpv_fx$wkEndgDate, tpv_fx$SR, type="l", col="red",
       ylim=c(min(tpv_fx$SR-10),max(tpv_fx$SR+10)), ylab="", xlab="",xaxt="n")
  title(main=c(title_fit,"\n",corridor), ylab="metric YoY %", col.main=title_color)
  
  axis(1, at=tpv_fx$wkEndgDate, labels=tpv_fx$wkEndgDate, las=2, cex.axis=0.6)
  
  par(new = TRUE)
  
  plot(tpv_fx$wkEndgDate, tpv_fx[,currency_pair1], type = "l", xaxt="n", yaxt="n", xlab = "", ylab = "", col="black")
  
  axis(4)
  
  if (!is.na(currency_pair2)) {
    lines(tpv_fx$wkEndgDate, tpv_fx[,currency_pair2], type = "l", bty = "n", xlab = "", ylab = "", col="green")
  }
  
  
  mtext("FX rate YoY %", side=4, line=3)
  
  if (is.na(currency_pair2)) {
    legend("topright", inset=.01, c("SR", "Primary FX"), lty=c(1,1,1), lwd=c(2.5,2.5,2.5),
           col=c("red","black"),bty = "n")
    
  } else{
    legend("topright", inset=.01, c("SR", "Primary FX", "Secondary FX"), lty=c(1,1,1), lwd=c(2.5,2.5,2.5),
           col=c("red","black","green"),bty = "n")
    
  }
  
  
  return (print(summary(fit)))
  
  
}

################################# End Function definition ##########################################



# !Manual!
# Set input as list of sender countries, list of receiver counties, list of currency pairs
# Set raw data as Type2 or Type1
#


sndrsType1 = list(c("CA "),
                  c("AU "),
                  c("GB "),
                  c("US "),
                  c("US "),
                  c("RU ")
                  
)

rcvrsType1 = list(c("US "),
                  c("US "),
                  c("US "),
                  c("CA "),
                  c("GB "),
                  c("US ")
)


currencyPairsType1 = list(c("CAD.USD"),
                          c("AUD.USD"),
                          c("GBP.USD"),
                          c("CAD.USD"),
                          c("GBP.USD"),
                          c("RUB.USD","EUR.USD")
)



# Type1

for (i in 1:5)
{
  summary = RegressPlot(sndrsType1[[i]], rcvrsType1[[i]], currencyPairsType1[[i]], corridorsType1[i], ppDataType1)
  capture.output(noquote(corridorsType1[i]), file="Type1 (monthly) result.txt", append=TRUE)
  capture.output(summary, file="Type1 (monthly) result.txt", append=TRUE)
}


corridorsType2 =    c("Type2: CA to US",
                    "Type2: US to CA",
                    "Type2: UK to US",
                    "Type2: US to UK",
                    "Type2: AU to US",
                    "Type2: DEATCH to UK",
                    "Type2: DEATCH to US",
                    "Type2: DEATCH to NL",
                    "Type2: AU to UK")


sndrsType2 = list(c("CA "),
                c("US "),
                c("GB "),
                c("US "),
                c("AU "),
                c("DE ","AT ","CH "),
                c("DE ","AT ","CH "),
                c("DE ","AT ","CH "),
                c("AU ")
)

rcvrsType2 = list(c("US "),
                c("CA "),
                c("US "),
                c("GB "),
                c("US "),
                c("GB "),
                c("US "),
                c("NL "),
                c("GB ")
)


currencyPairsType2 = list(c("CAD.USD"),
                         c("CAD.USD"),
                         c("GBP.USD"),
                         c("GBP.USD"),
                         c("AUD.USD"),
                         c("EUR.USD"),
                         c("EUR.USD"),
                         c("EUR.USD"),
                         c("AUD.GBP")
)


# Type2   

for (i in 1:9)
{
summary = RegressPlot(sndrsType2[[i]], rcvrsType2[[i]], currencyPairsType2[[i]], corridorsType2[i], ppDataType2)
capture.output(noquote(corridorsType2[i]), file="Type2 (monthly) result.txt", append=TRUE)
capture.output(summary, file="Type2 (monthly) result.txt", append=TRUE)
}

corridorsType1 =    c("Type1: CA to US",
                    "Type1: AU to US",
                    "Type1: UK to US",
                    "Type1: US to CA",
                    "Type1: US to UK",
                    "Type1: RU to US"
)


# Sign off plots
dev.off()

