### R code from vignette source 'Exercises2.Rnw'

# Load the TIMSS data and recreate the fits
library(mirt)
load("TimssData.Rdata")
pcmFit <- mirt(timssData, 1, "Rasch")
gpcmFit <- mirt(timssData, 1, "gpcm")


# 1) An LRT between the two models indicates that the PCM is
# not sufficiently flexible to account for the data. Each of the
# information criteria agrees with this interpretation (recall that
# lower information criterion = better fit). The information criteria
# indicate that the GPCM provides a more parsimonious account of the
# test data.
anova(pcmFit, gpcmFit)


# 2) The p-value (approx. 0) indicates that the observed data are
# very unlikely to have occured by chance under the GPCM. This
# suggests that the GPCM provides a poor fit to the data.
M2(gpcmFit)


# 3) The response patterns observed for items 'M032760B' and 'M032692'
# (indices 5 and 8) are unlikely to be generated by the GPCM with the
# fitted item parameters. This suggests GPCM does not provide a good
# account of these items. Removing these items improves the fit of
# the GPCM, but the remaining responses are still unlikely to have
# been generated by the GPCM.
itemfit(gpcmFit)
gpcmFit2 <- mirt(timssData[, -c(5,8)], 1, "gpcm")
M2(gpcmFit2)


# 4) The uniform DIF analysis suggests that the category threshold
# parameters of items 6:9 differ between Australian and Taiwanese
# students.
anchors <- c(1,3,4,10)
invar <- c("free_means", "free_var", names(timssData)[anchors])
gpcmFitMG <- multipleGroup(timssData, 1, timssCovar$country,
                           invariance = invar, itemtype = "gpcm")
DIF(gpcmFitMG, c("d1", "d2"), items2test = (1:11)[-anchors])


