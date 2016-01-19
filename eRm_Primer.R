library(eRm)

# fits the Rasch model using CML with item parameters summing to 0
rmFit1 <- RM(raschdat1) 
# fits the Rasch model with eta_1 = 0
rmFit2 <- RM(raschdat1, sum0 = FALSE)

#display a summary of the fit
summary(rmFit2)

# get item parameter estimates
coef(rmFit2) # easiness
# both are difficulty
coef(rmFit2, parm = "eta")
-coef(rmFit2)

# get ability estimates
abilFit <- person.parameter(rmFit2)
#summarize
summary(abilFit)
coef(abilFit)

# plot ICCs
plotICC(rmFit2, ask = FALSE)
plotINFO(rmFit2)

# LRT-based DIF analysis of FIMS data
library(TAM)

# format the data
data("data.fims.Aus.Jpn.scored")
fims <- data.fims.Aus.Jpn.scored
fimsX <- fims[, -c(1, 16)]
fims$country <- factor(fims$country, 1:2, c("Australia", "Japan"))

# fit the items using CML
cmlItem <- RM(fimsX, sum0 = FALSE)

# perform an LRT
fimsLRT <- LRtest(cmlItem)
fimsLRT # print test results at console

# plot the individual estimates
plotGOF(fimsLRT, tlab = "item",
        conf = list(ia = FALSE, col = "blue", lty = 2))
