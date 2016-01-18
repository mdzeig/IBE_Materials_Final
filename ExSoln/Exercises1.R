### R code from vignette source 'Exercises1.Rnw'

library(mirt)


# 1) Load the data using a load statement
load("TimssData.Rdata")


# 2) Fit the PCM, GPCM and graded response models
pcmFit <- mirt(timssData, 1, "Rasch")
gpcmFit <- mirt(timssData, 1, "gpcm")
grmFit <- mirt(timssData, 1, "graded")


# 3) Plot the category probability curves. The curves for the GPCM and
# graded response models look similar, but both have higher slopes than
# the PCM model. This suggests that the GPCM model provides a better fit 
# to the data, because the data suggest a discrimination parameter that
# is not equal to 1.
itemplot(pcmFit, 4)
itemplot(gpcmFit, 4)
itemplot(grmFit, 4)


# 4) Compare the item parameter estimates. The following code prints
# the estimates of the PCM and GPCM as rows in a matrix.  This makes
# it easy to compare estimates. We see that the GPCM's discrimination
# parameter is estimated to be much different than 1 and the category
# thresholds much closer to 0.
rbind(coef(pcmFit, IRTpar = TRUE)[[4]],
      coef(gpcmFit, IRTpar = TRUE)[[4]])


# 5) Abilities can be estimates using the 'fscores' function. The
# third line of creates a scatterplot of the WLE and EAP estimates
# for the test takers. When the two methods estimate the same value
# the corresponding point will fall on the line y = x. The farther
# an estimate falls from this point, the more different the estimates
# are. To facilitate seeing differences, the fourth line adds the
# line y = x to the plot as a reference. We see that WLE estimates
# tend to be lower than EAP estimates for test takers whose WLE 
# estimate is < -1, WLE estimates tend to be higher than EAP
# estimates for test takers whose WLE estimate is between -1 and 0,
# and, with a few exceptions, WLE estimates tend to be higher than
# EAP estimates for test takers whose WLE estimate is > 0.
abilEstWLE <- fscores(gpcmFit, method = "WLE")
abilEstEAP <- fscores(gpcmFit, method = "EAP")
plot(abilEstWLE, abilEstEAP, pch = 19, xlab = "WLE", ylab = "EAP")
abline(0, 1, lty = 2)


# 6) The histograms look very different, as there are no Taiwanese
# students with ability estimates below 0. This suggests that the
# MML assumption that the test takers come from the same normally
# distributed population is incorrect.  This suggests a closer look
# at the data is needed...
hist(abilEstEAP[timssCovar$country == "Australia"],
     xlab = "Ability Estimate", main = "Australia")
hist(abilEstEAP[timssCovar$country == "Taiwan"],
     xlab = "Ability Estimate", main = "Taiwan")


