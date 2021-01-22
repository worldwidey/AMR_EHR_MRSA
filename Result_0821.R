rm(list=ls())
##########RESULTS
Data_v3<-readRDS("Data_v3.RDS")

##########ROC curve
#install.packages("pROC")
library(pROC)
#install.packages("verification")
library(verification)

#Probability Vector
Data_v3$Prob1

#Death Vector
Data_v3$Mortality30
set.seed(20200728)
par(pty="s")
rocobj1<-plot.roc(Data_v3$Mortality30,
                 Data_v3$Prob1,
                 main="Confidence intervals",
                 percent=TRUE,
                 ci=TRUE, # compute AUC (of AUC by default)
                 #print.auc=TRUE,
                 thresholds="best",
                 print.thres="best",
                 legacy.axes=TRUE, xlim=c(100,0))
ciobj1 <- ci.se(rocobj1, # CI of sensitivity
               specificities=seq(0, 100, 5)) # over a select set of specificities=
plot(ciobj1, type="shape", col="#1c61b6AA") # plot as a blue shape
plot(ci(rocobj1, of="thresholds", thresholds="best",print.thres="best")) # add one threshold
text(25, 23.5, labels=paste("AUC:65.8% (52.4% - 79.3%)"))
text(32, 20, labels=paste("p-value =", format.pval(pvalue$p.value)), adj=c(0, .5))

pvalue<-roc.area(Data_v3$Mortality30, Data_v3$Prob1)

Random140<-runif(n = 140, min = 0, max = 1)

rocobj1 <- plot.roc(Data_v3$Mortality30,
                    Data_v3$Prob1,
                    main="Statistical comparison", 
                    percent=TRUE,
                    ci=TRUE, 
                    #print.auc=TRUE,
                    thresholds="best",
                    print.thres="best",
                    col="#1c61b6")
rocobj2 <- lines.roc(Data_v3$Mortality30, Random140, percent=TRUE, col="#008600")

testobj <- roc.test(rocobj1, rocobj2)
?roc.test
text(5, 23.5, labels=paste("AUC:65.8% (52.4% - 79.3%)"))
text(10, 20, labels=paste("p-value =", format.pval(testobj$p.value)), adj=c(0, .5))
?plot.roc
legend("bottomright", legend=c("Yang et al. Model", "Random Prediction"), col=c("#1c61b6", "#008600"), lwd=2)



########Youden's cutpoint
install.packages("cutpointr")
library(cutpointr)
#Method:
#Predictor:
#Outcome: Mortality30
#Direction >=

#Optimal cutpoint for Prob1
opt_cut<-cutpointr(Data_v3, Prob1, Mortality30,metric = youden)
s_opt_cut<-summary(opt_cut)
plot(opt_cut)
plot_metric(opt_cut, conf_lvl = 0.9)

#write.csv(s_opt_cut,"s_opt_cut_Prob.csv")

