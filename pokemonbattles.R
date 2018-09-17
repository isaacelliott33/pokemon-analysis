setwd("/home/badugi/Desktop/KNN project/Pokemon")

pokemon = read.csv("pokemon.csv")
pokemoncombat = read.csv("combats.csv")
tests = read.csv("tests.csv")
head(pokemon)
head(pokemoncombat)
dim(pokemoncombat)
attach(pokemoncombat)
attach(pokemon)

#the objective is to predict the winners of pokemon.



#######################EDA#######################################################################################
#it seems like the dataset consists of a specific type of pokemon vs another specific pokemon type.  
#So each number is a unique identification number that represents a specific pokemon.
#questions - is pokemon type based on the specific race or individual pokemon.



#dirty method of merging and cbinding
library(plyr)
pokemoncombat1 = as.data.frame(pokemoncombat$First_pokemon)
colnames(pokemoncombat1) = "X."
head(pokemoncombat1)
pokemoncombat2 = as.data.frame(pokemoncombat$Second_pokemon)
colnames(pokemoncombat2) = "X."

mergedata1 = join(pokemoncombat1, pokemon, by = "X.")
colnames(mergedata1)[1] = "First_pokemon"
head(mergedata1)

mergedata2 = join(pokemoncombat2, pokemon, by = "X.")
colnames(mergedata2)[1] = "Second_pokemon"
head(mergedata2)
newpokemoncombat = cbind(mergedata1, mergedata2)
head(newpokemoncombat)
head(pokemoncombat)
winner = pokemoncombat$Winner
newpokemoncombat = cbind(newpokemoncombat, winner)


colnames(newpokemoncombat) = c("First_pokemon", "first_name", "First_Type.1", "First_Type.2", "First_hp", "First_attack",
                               "First_Defense", "First_SP..Atk", "First_SP..Def", "First_Speed", "First_Generation", 
                               "First_legendary", "Second_pokemon","Second_Name","Second_Type.1","Second_Type.2",
                               "Second_HP", "Second_Attack", "Second_Defense", "Second_Sp..Atk", "Second_Sp..Def",
                               "Second_Speed", "Second_Generation","Second_Legendary","Winner")

head(newpokemoncombat)
