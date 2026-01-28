# first i will clean the data I guess and do some exploration of it

library(tidyverse)

nestlings <- read_csv("nestlings.csv")

mean(nestlings$fledged, na.rm = T)

# on average, 82% of blue tits fledged in the project 

adults <- read_csv("adults.csv")

veterans <- left_join(adults, nestlings, by = "ring")

# of the 9422 blue tits fledging, 2263 of them were found as adults in the future in the same area. 

2263/ 9422

# (so about 24 percent!) ### 

site <- read_csv("site_details.csv")


# combining site and box into a single variable for a specific site seems smart. 

bird_phen <- read_csv("bird_phenology.csv")

# clutch swap treatment is a key variable - these nests should be excluded as they are manipulations of the natural conditions 

bird_phen <- bird_phen |> filter(clutch_swap_treatment == "unmanipulated" | clutch_swap_treatment == "odd" | is.na(clutch_swap_treatment) == T)

# there are slightly more nests examined each year except for a dip in 2019 (covid??)

bird_phen |> group_by(year) |> count()

temp <- read_csv("temperatures.csv")

# this seems to be in the wide format so might need to be pivoted to be usable? 
# Its also using a very strange timescale which could maybe be converted to something more digestible

catpills <- read_csv("branch_beating.csv")

# this uses a slightly different timescale again which could be converted ... 

trees <- read_csv("tree_phenology.csv")

# this gives each tree an ID, allowing them to be joined together also. 


boots_n_cats <- left_join(catpills, trees, by = c("site", "treeID", "year"))
boots_n_cats$uniqueID = paste(boots_n_cats$site, boots_n_cats$treeID, sep = " ")
#cut duplicate
boots_n_cats <- boots_n_cats |> select(-taxon.y)
### ok we've looked at the data, time to ...  


## exploration #####

boots_n_cats$taxon.x <- as.factor(boots_n_cats$taxon.x)

#this plot initially showed a lot of outliers ... perhaps it would be better to log transform? hmmm




boots_n_cats$logged_cats <- log(boots_n_cats$caterpillars)

ggplot(data = boots_n_cats, aes(x = taxon.x, y = logged_cats)) + geom_jitter()

# this still looks messed up. I guess its because its count data, and follows a poisson distribution! 

# trying to model the count of caterpillars based on tree species or more simple initially, seeing if weight and count work together 

library(DHARMa)
library(StatisticalModels)
library(lme4)
library(glmmTMB)
library(ggeffects)

boots_n_cats$caterpillar.mass <- as.numeric(boots_n_cats$caterpillar.mass)

# i think using nbinom2 makes the most sense as the data isn't normally distributed and also is overdispersed. 

model <- glmmTMB(caterpillars ~ caterpillar.mass, data = boots_n_cats, family = "nbinom2")
summary(model)




predictions <- boots_n_cats 
predictions$Pred <- predict(model, predictions |> dplyr::select(-"caterpillars"))

# a good step would be to simplify the tree data down to taxa rather than having a mix of species data and taxon level data which don't work as comparisons? (though almost all seem to be genus tbf) 
       
unique(boots_n_cats$taxon.x)

boots_n_cats |> filter(taxon.x == "Damson")

# cut faulty entries

boots_n_cats <- boots_n_cats |> filter(taxon.x != "?" & taxon.x != "Field maple?")



model <- glmmTMB(caterpillars ~ taxon.x, data = boots_n_cats, family = "poisson")
summary(model)




predictions <- boots_n_cats 
predictions$Pred <- predict(model, predictions |> dplyr::select(-"caterpillars"), type = "response")

# data is zero inflated ie there are way to many zeros which is fucking up the model. next i tried to model using 
# a zero inflated model 

library(pscl)

summary(m1 <- zeroinfl(caterpillars ~ taxon.x | taxon.x, data = boots_n_cats, dist = "poisson", link = "logit", na.action = "na.omit"))

predictions <- boots_n_cats 
predictions$zero_pred <- predict(m1, predictions |> dplyr::select(-"caterpillars"), type = "response")

# this zero issue could be solved by using two folds - one to classify if zero is present, one to then predict on where it isn't


boots_n_cats <- boots_n_cats |> mutate(
  zero_or_no = case_when(
    caterpillars > 0 ~ 1,
  T ~ 0)
  
)

first_fold <- zeroinfl(zero_or_no ~ taxon.x + site, data = boots_n_cats)

predictions <- boots_n_cats
predictions$zero_pred <- predict(first_fold, predictions, type = "response")

second_fold <- glmmTMB(caterpillars ~ taxon.x + site + date, data = predictions, family = "poisson")

predictions$final_pred <- predict(second_fold, predictions, type = "response")

plot(y = predictions$final_pred, x = predictions$caterpillars)

## ok this stuff is silly, I'm getting sidetracked - what is the goal of the project? 

# ideas

# spatial data - make some kind of visualisation using GIS, shape files, shiny app etc
# machine learning = learn some more key libraries and try to model something in the data 
# tracking individuals - with the ring ID we could investigate how they move around, how long they are in the dataset etc

# look to augment the data somehow? 

# do birds that stay around for multiple years raise more successful chicks? 
# are some nests or sites better for raising the fledglings successfully? 

