
## Project:  Final SOC302 Project 
# Located:   Class folder in ELSA Cluster
# File Name: edu-attain-inequality-ortiz.R*
# Date:      May 15th, 2025 (updated 5/15 at home)
# Who:       Jordan Ortiz

################################################################################################
############              Pre-Analysis: settings, packages, and data    ########################
################################################################################################
### Settings + Packages
setwd("/courses/SOC302/ortizj9")
install.packages("dplyr")
install.packages("psych")
library(dplyr)
library(psych)

### Load data 
GSS <- read.csv("GSS2022.csv")

################################################################################################
############              PHASE 1: CLEAN DATA FOR ANALYSIS              ########################
################################################################################################
# Steps of cleaning variables Clear vars
# Step 1: Examine variable and coding schema: Table() / summary()
# Step 2: Recode (if necessary/warrented): mutate(), ifelse(), etc
# Step 3: Confirm: table() / summary()

############            DEPENDENT VARIABLE                           ###########
############     Differences due to lack of education                ###########
#STEP 1 : Examine variable and coding schema
table(GSS$racdif3)

#STEP 2 : Re-code if necessary or justify if not neccesary
GSS <- mutate(GSS, yes_belf_edudiff = ifelse(racdif3 == 1, 1, 0))
GSS <- mutate(GSS, no_belf_edudiff  = ifelse(racdif3 == 2, 1, 0))

# STEP 3: Confirm creation (if necessary)
table(GSS$racdif3, GSS$yes_belf_edudiff)
table(GSS$racdif3, GSS$no_belf_edudiff)

############              INDEPENDENT VARIABLE          ########################
############              Religious Affiliation          #######################
#STEP 1 : Examine variable and coding schema 
table(GSS$relig16)

#STEP 2 : Re-code if necessary or justify if not neccesary
GSS <- mutate(GSS, protestant_relig = ifelse(relig16 == 1, 1, 0))
GSS <- mutate(GSS, catholic_relig = ifelse(relig16 == 2, 1, 0))
GSS <- mutate(GSS, jewish_relig = ifelse(relig16 == 3, 1, 0))
GSS <- mutate(GSS, other_relig = ifelse(relig16 == 4 & relig16 <= 13, 1, 0))

#STEP 3 : Confirm creation (if necessary)
table(GSS$relig16, GSS$protestant_relig)
table(GSS$relig16, GSS$catholic_relig)
table(GSS$relig16, GSS$jewish_relig)
table(GSS$relig16, GSS$other_relig)

#####################          CONTROL VARIABLE         ########################
#####################             Gender(SEX)           ########################        
##STEP 1 : examine
table(GSS$sex)

##STEP 2 : create dummy
GSS <- mutate(GSS, man = ifelse(sex == 1, 1, 0))
GSS <- mutate(GSS, woman = ifelse(sex == 2, 1, 0))

##STEP 3 : test dummy
table(GSS$sex, GSS$man)
table(GSS$sex, GSS$woman)

######################          CONTROL VARIABLE         #######################
######################                Race               #######################        
##STEP 1 : examine
table(GSS$racecen1)

##STEP 2 : create dummy
GSS <- mutate(GSS, white  = ifelse(racecen1 == 1, 1, 0))
GSS <- mutate(GSS, black = ifelse(racecen1 == 2, 1, 0))
GSS <- mutate(GSS, other_race = ifelse(racecen1 >= 3 & racecen1 <= 16, 1, 0))

##STEP 3 : test dummy
table(GSS$racecen1, GSS$white)
table(GSS$racecen1, GSS$black)
table(GSS$racecen1, GSS$other_race)

#######################       CONTROL VARIABLE           #######################
############                   Political views               ###################
##STEP 1 : examine
table(GSS$polviews)

##STEP 2 : create dummy
GSS <- mutate(GSS, liberal  = ifelse(polviews >= 1 & polviews <= 3, 1, 0))
GSS <- mutate(GSS, moderate = ifelse(polviews == 4, 1, 0))
GSS <- mutate(GSS, conservative = ifelse(polviews >= 5 & polviews <= 7, 1, 0))

##STEP 3 : test dummy
table(GSS$polviews, GSS$liberal)
table(GSS$polviews, GSS$moderate)
table(GSS$polviews, GSS$conservative)

#################              CONTROL VARIABLE              ###################
##############                       Age                        ################        
##STEP 1 : examine
table(GSS$age)

##STEP 2 : create dummy
GSS <- mutate(GSS, gen_z = ifelse(age == 18 & age <= 28, 1, 0))
GSS <- mutate(GSS, millennial = ifelse(age == 29 & age <= 44, 1, 0))
GSS <- mutate(GSS, gen_x = ifelse(age == 45 & age <= 60, 1, 0))
GSS <- mutate(GSS, baby_boomer = ifelse(age == 61 & age <= 79, 1, 0))
GSS <- mutate(GSS, silent_gen = ifelse(age == 80 & age <= 89, 1, 0))

##STEP 3 : test dummy
table(GSS$age, GSS$gen_z)
table(GSS$age, GSS$millennial)
table(GSS$age, GSS$gen_x)
table(GSS$age, GSS$baby_boomer)
table(GSS$age, GSS$silent_gen)

######################          CONTROL VARIABLE         #######################
######################              Income               #######################        
##STEP 1 : examine
table(GSS$realinc)

################################################################################################
############              PHASE 2: CREATE MY DATASET                    ########################
################################################################################################
### STEP 1: Create a list of variables to keep

#trimmed Varlist for final paper
my_varlist <- c("racdif3", "yes_belf_edudiff" ,
                "relig16" , "protestant_relig" , "catholic_relig" , "jewish_relig",
                "sex" , "man" ,
                "racecen1" ,"black" , 
                "polviews" , "liberal" , "moderate" , "conservative" , 
                "age", "gen_z", "millennial", "gen_x", "baby_boomer",
                "realinc")

### STEP 2: Create a new dataset with only your variables and complete case
my_dataset <- GSS %>%
  select(all_of(my_varlist)) %>%
  filter(complete.cases(.))

### STEP 3: Gather Summary Statistics and confirm valid dataset construction
describe(my_dataset)

################################################################################################
############              PHASE 3: Descriptive Statistics               ########################
################################################################################################
#TABLE 1: DESCRIPTIVE STATISTICS HERE
describe(my_dataset)
View(my_dataset)

################################################################################################
############              PHASE 4: Correlation Matrix             ########################
################################################################################################
cor(my_dataset)

################################################################################################
############              PHASE 5: Regression                        ###########################
################################################################################################
##STEP 1 :
model1a <- glm(yes_belf_edudiff ~ realinc +
                 man + woman +
                 millennial + gen_x + baby_boomer + silent_gen
                 black + other_race  , 
               data = my_dataset, family = binomial)
summary(model1a)

model1b <- glm(yes_belf_edudiff ~ realinc +
                 man +
                 millennial + gen_x + baby_boomer + 
                 black ,  
               data = my_dataset, family = binomial)
summary(model1b)

model2a <- glm(yes_belf_edudiff ~ 
                 liberal + moderate + conservative +
                 protestant_relig + catholic_relig + jewish_relig + other_relig , 
               data = my_dataset, family = binomial)
summary(model2a)

model2b <- glm(yes_belf_edudiff ~
                 liberal + moderate +
                 protestant_relig + catholic_relig + jewish_relig , 
               data = my_dataset, family = binomial)
summary(model2b)

model3a <- glm(yes_belf_edudiff ~ realinc +
                 liberal + moderate + conservative +
                 man +
                 black, 
               data = my_dataset, family = binomial)
summary(model3a)

model3b <- glm(yes_belf_edudiff ~ realinc +
                 liberal + moderate +
                 man +
                 black, 
               data = my_dataset, family = binomial)
summary(model3b)

## 1a and 1b 'social position'
## 2a and 2b 'institutional'
## 3b and 3b 'mix of both'

