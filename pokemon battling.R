## Importing packages

# This R environment comes with all of CRAN and many other helpful packages preinstalled.
# You can see which packages are installed by checking out the kaggle/rstats docker image: 
# https://github.com/kaggle/docker-rstats

library(tidyverse) # metapackage with lots of helpful functions

list.files(path = "../input")

pokemon = read.csv("../input/pokemon.csv")
combats = read.csv("../input/combats.csv")
options(repr.matrix.max.rows=600, repr.matrix.max.cols=200)


firstpokemon = as.data.frame(combats$First_pokemon) 
colnames(firstpokemon) = "X."
secondpokemon = as.data.frame(combats$Second_pokemon)
colnames(secondpokemon) = "X."

library(plyr)

mergepokemon1 = join(firstpokemon, pokemon, by= "X.")
mergepokemon2 = join(secondpokemon, pokemon, by = "X.")

colnames(mergepokemon2) = c("X2", "Name2", "Type.1_2", "Type.2_2", "HP2", "Attack2","Defense2", "Sp.Atk2", "Sp.Def2", "Speed2", "Generation2", "Legendary2")

newcombatdata = cbind(mergepokemon1, mergepokemon2, combats$Winner)
colnames(newcombatdata)[colnames(newcombatdata) == "combats$Winner"] = "Winner"

newcombatdata$classtype1 = as.numeric(newcombatdata$Type.1)
newcombatdata$classtype.2 = as.numeric(newcombatdata$Type.2)  #1 is no pokemon data type
newcombatdata$classtype.1_2 = as.numeric(newcombatdata$Type.1_2)
newcombatdata$classtype.2_2 = as.numeric(newcombatdata$Type.2_2)
newcombatdata$LegendaryClass = as.numeric(newcombatdata$Legendary)
newcombatdata$LegendaryClass2 = as.numeric(newcombatdata$Legendary2)

head(newcombatdata)

binarywinner = rep(1, 50000)
binarywinner[newcombatdata$Winner == newcombatdata$X2] = 0

newcombatdata$binarywinner = binarywinner

#binarywinner 0 means first pokemon lost
head(newcombatdata)

checkingformc = lm(binarywinner ~ classtype1 + classtype.2 + HP + Attack + Defense + Sp..Atk + Sp..Def + Speed + LegendaryClass + classtype.1_2 + classtype.2_2 + HP2 + Attack2 + Defense2 + Sp.Atk2 + Sp.Def2 + Speed2 + LegendaryClass2, data = newcombatdata)
library(car)
vif(checkingformc)

#No indication of any multicollinearity at all according to VIF score.  
#Meaning the data variables passes the assumption that we can further proceed with Logistic Regression.

#Log regression model using the full model (DID NOT SPLIT DATA TO TEST AND TRAINING SET).  This is just a snapshot of how accurate it is according to our prediction table.
fit = glm(binarywinner ~ classtype1 + classtype.2 + HP + Attack + Defense + Sp..Atk + Sp..Def + Speed + LegendaryClass + classtype.1_2 + classtype.2_2 + HP2 + Attack2 + Defense2 + Sp.Atk2 + Sp.Def2 + Speed2 + LegendaryClass2, data = newcombatdata, family = "binomial")

summary(fit)

#Crossvalidation #10-k fold cross validation
k = 10
n = dim(newcombatdata)[1]
groups = c(rep(1:k,floor(n/k)),1:(n-floor(n/k) * k))

set.seed(1)
cvgroups = sample(groups,n)

allpredictedcv = rep(0,n)

colnames(newcombatdata)

for (i in 1:k){
  groupi = (cvgroups == i)
  glmfitCV = glm(binarywinner ~ classtype1 + classtype.2 + HP + Attack + Defense+ Sp..Atk + Sp..Def + Speed + LegendaryClass + classtype.1_2 + classtype.2_2 + HP2 + Attack2 + Defense2 + Sp.Atk2 + Sp.Def2 + Speed2 + LegendaryClass2, data = newcombatdata[!groupi,], family = "binomial")
  allpredictedcv[groupi] = predict(glmfitCV, newcombatdata[groupi,], type = "response")
}

#assessment of Log regression
table(allpredictedcv > .5, newcombatdata$binarywinner)

#true positive rate
20717/(20717 + 2884)

#the true positive rate is 88% of the power of this model using a .5 threshold.

#perform the roc curve to determine if the thresholds are very accurate or not.
library(pROC)
myroc = roc(response= newcombatdata$binarywinner, predictor = allpredictedcv)
plot.roc(myroc)

#According to the ROC curve, this curve's upper left corner is much further away from the gray diagonal line.  Meaning, this model is pretty accurate.  
#if the model was closer to the gray line
#chances are that the model is closer and closer to random guessing...which is bad.

#Ultimate data prediction. using our glm model.

#predictions data set.
finalpredicteddataset = cbind(newcombatdata$Name, newcombatdata$Name2, allpredictedcv)
head(allpredictedcv)

#testing our data with real data.  (Jigglypuff vs Oddish)
realdata = data.frame("classtype1" = 13, "classtype.2" = 6, "HP" = 45, "Attack" = 45, "Defense" = 20,"Sp..Atk" = 45, "Sp..Def" = 25, "Speed" = 20, "LegendaryClass" = 1, "classtype.1_2" = 16, "classtype.2_2" = 3, "HP2" = 45, "Attack2" = 50,"Defense2" = 55, "Sp.Atk2" = 75,"Sp.Def2" = 65,"Speed2" = 30, "LegendaryClass2" = 1)
realdata

#Jigglypuff (player1) vs Oddish (player2)
predict(fit, realdata, type = "response")

#Jigglypuff only has 26% chance of winning this battle.  According to the Log regression.  with 88% accuracy.

#decision trees.