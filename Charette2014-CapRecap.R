library(VGAM)
library(data.table)

## Data for Serious offenses 1972-1974 ##
## Data extracted from Blumstein, Alfred, and Jacqueline Cohen. ADULT CRIMINAL CAREERS IN NEW YORK, 1972-1983. ICPSR version. Pittsburgh, PA: Carnegie-Mellon University, School of Urban and Public Affairs [producer], 1989. Ann Arbor, MI: Inter-university Consortium for Political and Social Research [distributor], 1994. http://doi.org/10.3886/ICPSR09353.v1 ##

AllArrest74.dta <- read.csv("FILE PATH/file.csv", header=TRUE, sep=",")

## Quadratic and cubic effect Age in 1973 ##
AllArrest74.dta$Age <- AllArrest74.dta$Age1972 + 1
AllArrest74.dta$Agesq <- AllArrest74.dta$Age * AllArrest74.dta$Age
AllArrest74.dta$Agecu <- AllArrest74.dta$Age * AllArrest74.dta$Age * AllArrest74.dta$Age

## Quadratic and cubic effect for Mobility and its quartiles ##
AllArrest74.dta$Mobility2sq <- AllArrest74.dta$Mobility2 * AllArrest74.dta$Mobility2
AllArrest74.dta$Mobility2cu <- AllArrest74.dta$Mobility2 * AllArrest74.dta$Mobility2 * AllArrest74.dta$Mobility2
AllArrest74.dta$Mobility2Q <- quantcut(AllArrest74.dta$Mobility2, q=seq(0,1,by=0.25))

## Quadratic and cubic effect for Versatility and its quartiles ##
AllArrest74.dta$Versatility2sq <- AllArrest74.dta$Versatility2 * AllArrest74.dta$Versatility2
AllArrest74.dta$Versatility2cu <- AllArrest74.dta$Versatility2 * AllArrest74.dta$Versatility2 * AllArrest74.dta$Versatility2
AllArrest74.dta$Versatility2qu <- AllArrest74.dta$Versatility2 * AllArrest74.dta$Versatility2 * AllArrest74.dta$Versatility2 * AllArrest74.dta$Versatility2
AllArrest74.dta$Versatility2Q <- quantcut(AllArrest74.dta$Versatility2, q=seq(0,1,by=0.25))

## Confinement data (subsample of convicted individual) ##
AllConviction74.dta <-  AllArrest74.dta[ which(AllArrest74.dta$Convicted72_74_sum > 0), ]
AllConviction74.dta$One <- 1

## Ratio of conviction per arrest ##
AllConviction74.dta$RatioConvArr <- AllConviction74.dta$RatioConvArr$Convicted72_74_sum / AllConviction74.dta$AllArrest72_74_sum


####################################################################
##                           Models                               ##
####################################################################

####################################################################
##            Model predicting the number of arrests              ##
####################################################################


#### Zero-truncated negative binomial NULL model predicting arrests ####

nb0 <- vglm(AllArrest72_74_sum ~ 1, family = posnegbinomial(), data = AllArrest74.dta)
summary(nb0)
AICc(nb0)

## Estimation of probabilities of counts for the null model##

AllArrest74.pred0 <- predict(nb0)
AllArrest74.pred10 <- as.data.frame(AllArrest74.pred0)

AllArrest74.dta$nb10p1 <- dposnegbin(1, mu = exp(AllArrest74.pred10$'log(munb)'), size = exp(AllArrest74.pred10$'log(size)'))
AllArrest74.dta$nb10p2 <- dposnegbin(2, mu = exp(AllArrest74.pred10$'log(munb)'), size = exp(AllArrest74.pred10$'log(size)'))
AllArrest74.dta$nb10p3 <- dposnegbin(3, mu = exp(AllArrest74.pred10$'log(munb)'), size = exp(AllArrest74.pred10$'log(size)'))
AllArrest74.dta$nb10p4 <- dposnegbin(4, mu = exp(AllArrest74.pred10$'log(munb)'), size = exp(AllArrest74.pred10$'log(size)'))
AllArrest74.dta$nb10p5 <- dposnegbin(5, mu = exp(AllArrest74.pred10$'log(munb)'), size = exp(AllArrest74.pred10$'log(size)'))
AllArrest74.dta$nb10p5M <- 1 - AllArrest74.dta$nb10p1 - AllArrest74.dta$nb10p2 - AllArrest74.dta$nb10p3 - AllArrest74.dta$nb10p4 - AllArrest74.dta$nb10p5

## Estimation of probabilities of zero counts (hidden population) using negative binomial distribution and the zero-truncated negative binomial model estimates ##

AllArrest74.dta$nb10p0 <- dnbinom(0, mu = exp(AllArrest74.pred10$'log(munb)'), size = exp(AllArrest74.pred10$'log(size)'))
AllArrest74.dta$nb10p0Pop <- AllArrest74.dta$nb10p0 * (1 / (1-AllArrest74.dta$nb10p0))
AllArrest74.dta$nb10p1Pop <- (1 - AllArrest74.dta$nb10p0) * (1 / (1-AllArrest74.dta$nb10p0))
AllArrest74.dta$One <- 1

sum(AllArrest74.dta$nb10p0Pop)     ## Population estimated to be arrested 0 time (Hidden population)
sum(AllArrest74.dta$nb10p1)        ## Population estimated to be arrested 1 time
sum(AllArrest74.dta$nb10p2)        ## Population estimated to be arrested 2 times
sum(AllArrest74.dta$nb10p3)        ## Population estimated to be arrested 3 times
sum(AllArrest74.dta$nb10p4)        ## Population estimated to be arrested 4 times
sum(AllArrest74.dta$nb10p5)        ## Population estimated to be arrested 5 times
sum(AllArrest74.dta$nb10p5M)       ## Population estimated to be arrested more than 5 times

## Estimated risk of arrest ##
sum(AllArrest74.dta$One) / (sum(AllArrest74.dta$nb10p0Pop) + sum(AllArrest74.dta$One))




#### Zero-truncated negative binomial FULL model predicting arrests ####
## NEWRACE == 2 (Black individuals); NEWSEX == 2 (Women); TYPEOFCRIME_max == 1 (presence of a type of crime); COUNTY_max == 1 (presence of an offense is this county); Outofstate_max == 1 (presence of an offense in another state) ## 

nb1 <- vglm(AllArrest72_74_sum ~ scale(I(NEWRCE == 2)) + scale(I(NEWSEX == 2)) + 
              scale(Age) + scale(Agesq) + scale(Agecu) +
              scale(Mobility2) + scale(Mobility2sq) + scale(Mobility2cu) +
              scale(Versatility2) + scale(Versatility2sq) + scale(Versatility2cu) + scale(Versatility2qu) +
              scale(Intensity) + 
              scale(Homicide_max) + scale(Rape_max) + scale(Robbery_max) + scale(AggAss_max) + scale(Kidnap_max) + scale(Burglary_max) + scale(Theft_max) + scale(Auto_max) + scale(Assault_max) + scale(Arson_max) + scale(Fraud_max) + scale(Justice_max) + scale(Weapons_max) + scale(SexInd_max) + scale(Drugs_max) + scale(Gambling_max) + scale(Traffic_max) + scale(OtherOffenses_max) + 
              scale(Albany_max) + scale(Allegany_max) + scale(Broome_max) + scale(Cattaraugus_max) + scale(Cayuga_max) + scale(Chatauqua_max) + scale(Chemung_max) + scale(Chenango_max) + scale(Clinton_max) + scale(Columbia_max) + scale(Cortland_max) + scale(Delaware_max) + scale(Dutchess_max) + scale(Erie_max) + scale(Essex_max) + scale(Franklin_max) + scale(Fulton_max) + scale(Genesee_max) + scale(Greene_max) + scale(Hamilton_max) + scale(Herkimer_max) + scale(Jefferson_max) + scale(Kings_max) + scale(Lewis_max) + scale(Livingston_max) + scale(Madison_max) + scale(Monroe_max) + scale(Montgomery_max) + scale(Nassau_max) + scale(NewYork_max) + scale(Niagara_max) + scale(Oneida_max) + scale(Onondaga_max) + scale(Ontario_max) + scale(Orange_max) + scale(Orleans_max) + scale(Oswego_max) + scale(Otsego_max) + scale(Putnam_max) + scale(Queens_max) + scale(Rensselaer_max) + scale(Richmond_max) + scale(Rockland_max) + scale(StLawrence_max) + scale(Saratoga_max) + scale(Schenectady_max) + scale(Schoharie_max) + scale(Schuyler_max) + scale(Seneca_max) + scale(Steuben_max) + scale(Suffolk_max) + scale(Sullivan_max) + scale(Tioga_max) + scale(Tompkins_max) + scale(Ulster_max) + scale(Warren_max) + scale(Washington_max) + scale(Wayne_max) + scale(Westchester_max) + scale(Wyoming_max) + scale(Yates_max) + scale(Bronx_max) + 
              scale(Outofstate_max)
            , family = posnegbinomial(), data = AllArrest74.dta)
summary(nb1)
AICc(nb1)

## Calculating p-values from the model coefficients ##

nb1.pvalue<-1-pnorm(abs(coef(nb1)/sqrt(diag(vcov(nb1))))) 
nb1.pvalue


## Estimation of probabilities of counts ##

AllArrest74.pred <- predict(nb1)
AllArrest74.pred1 <- as.data.frame(AllArrest74.pred)

AllArrest74.dta$nb1p1 <- dposnegbin(1, mu = exp(AllArrest74.pred1$'log(munb)'), size = exp(AllArrest74.pred1$'log(size)'))
AllArrest74.dta$nb1p2 <- dposnegbin(2, mu = exp(AllArrest74.pred1$'log(munb)'), size = exp(AllArrest74.pred1$'log(size)'))
AllArrest74.dta$nb1p3 <- dposnegbin(3, mu = exp(AllArrest74.pred1$'log(munb)'), size = exp(AllArrest74.pred1$'log(size)'))
AllArrest74.dta$nb1p4 <- dposnegbin(4, mu = exp(AllArrest74.pred1$'log(munb)'), size = exp(AllArrest74.pred1$'log(size)'))
AllArrest74.dta$nb1p5 <- dposnegbin(5, mu = exp(AllArrest74.pred1$'log(munb)'), size = exp(AllArrest74.pred1$'log(size)'))
AllArrest74.dta$nb1p5M <- 1 - AllArrest74.dta$nb1p1 - AllArrest74.dta$nb1p2 - AllArrest74.dta$nb1p3 - AllArrest74.dta$nb1p4 - AllArrest74.dta$nb1p5

## Estimation of probabilities of zero counts (hidden population) using negative binomial distribution and the zero-truncated negative binomial model estimates ##

AllArrest74.dta$nb1p0 <- dnbinom(0, mu = exp(AllArrest74.pred1$'log(munb)'), size = exp(AllArrest74.pred1$'log(size)'))
AllArrest74.dta$nb1p0Pop <- AllArrest74.dta$nb1p0 * (1 / (1-AllArrest74.dta$nb1p0))
AllArrest74.dta$nb1p1Pop <- (1 - AllArrest74.dta$nb1p0) * (1 / (1-AllArrest74.dta$nb1p0))
AllArrest74.dta$One <- 1

sum(AllArrest74.dta$nb1p0Pop) ## Population estimated to be arrested 0 time (Hidden population)
sum(AllArrest74.dta$nb1p1)    ## Population estimated to be arrested 1 time
sum(AllArrest74.dta$nb1p2)    ## Population estimated to be arrested 2 times
sum(AllArrest74.dta$nb1p3)    ## Population estimated to be arrested 3 times
sum(AllArrest74.dta$nb1p4)    ## Population estimated to be arrested 4 times
sum(AllArrest74.dta$nb1p5)    ## Population estimated to be arrested 5 times
sum(AllArrest74.dta$nb1p5M)   ## Population estimated to be arrested more than 5 times

## Estimated risk of arrest ##
sum(AllArrest74.dta$One) / (sum(AllArrest74.dta$nb1p0Pop) + sum(AllArrest74.dta$One))


## Estimated risk of arrest in function of the characteristics ##
dt <- data.table(AllArrest74.dta)         ## Convert data into a data.table

## Estimated risk of arrest in function of Age ##
setkey(dt, "Age")                         ## Set the key variable (Age)
X <- dt[, list(hidden=sum(nb1p0Pop), Obs=sum(One), Risk=(sum(One) / (sum(nb1p0Pop) + sum(One)))), by=key(dt)]
X

## Estimated risk of arrest in function of Gender ##
setkey(dt, "NEWSEX")                      ## Set the key variable (Gender)
X <- dt[, list(zero=sum(nb1p0Pop), pop=sum(One), Risk=(sum(One) / (sum(nb1p0Pop) + sum(One)))), by=key(dt)] 
X

## [...] same thing for the other variables [...] ##



########################################################################
##            Model predicting the number of convictions              ##
########################################################################


## Zero-truncated negative binomial NULL model predicting convictions ##

nb20 <- vglm(Convicted72_74_sum ~ 1, family = negbinomial(), data = AllArrest74.dta)
summary(nb20)
AICc(nb20)

## Estimation of probabilities of counts for the null model##

AllArrest74.pred20 <- predict(nb20)
AllArrest74.pred201 <- as.data.frame(AllArrest74.pred20)

AllArrest74.dta$nb20p0 <- dnbinom(0, mu = exp(AllArrest74.pred201$'log(mu)'), size = exp(AllArrest74.pred201$'log(size)'))
AllArrest74.dta$nb20p1 <- dnbinom(1, mu = exp(AllArrest74.pred201$'log(mu)'), size = exp(AllArrest74.pred201$'log(size)'))
AllArrest74.dta$nb20p2 <- dnbinom(2, mu = exp(AllArrest74.pred201$'log(mu)'), size = exp(AllArrest74.pred201$'log(size)'))
AllArrest74.dta$nb20p3 <- dnbinom(3, mu = exp(AllArrest74.pred201$'log(mu)'), size = exp(AllArrest74.pred201$'log(size)'))
AllArrest74.dta$nb20p4 <- dnbinom(4, mu = exp(AllArrest74.pred201$'log(mu)'), size = exp(AllArrest74.pred201$'log(size)'))
AllArrest74.dta$nb20p5 <- dnbinom(5, mu = exp(AllArrest74.pred201$'log(mu)'), size = exp(AllArrest74.pred201$'log(size)'))
AllArrest74.dta$nb20p5M <- 1 - AllArrest74.dta$nb20p0 - AllArrest74.dta$nb20p1 - AllArrest74.dta$nb20p2 - AllArrest74.dta$nb20p3 - AllArrest74.dta$nb20p4 - AllArrest74.dta$nb20p5

sum(AllArrest74.dta$nb20p0)     ## Population estimated to be convicted 0 time
sum(AllArrest74.dta$nb20p1)     ## Population estimated to be convicted 1 time
sum(AllArrest74.dta$nb20p2)     ## Population estimated to be convicted 2 times
sum(AllArrest74.dta$nb20p3)     ## Population estimated to be convicted 3 times
sum(AllArrest74.dta$nb20p4)     ## Population estimated to be convicted 4 times
sum(AllArrest74.dta$nb20p5)     ## Population estimated to be convicted 5 times
sum(AllArrest74.dta$nb20p5M)    ## Population estimated to be convicted more than 5 times

## Estimated risk of arrest ##
(sum(1 - AllArrest74.dta$nb20p0) / sum(AllArrest74.dta$One))




#### Zero-truncated negative binomial FULL model predicting convictions ####
## NEWRACE == 2 (Black individuals); NEWSEX == 2 (Women); TYPEOFCRIME_max == 1 (presence of a type of crime); COUNTY_max == 1 (presence of an offense is this county); Outofstate_max == 1 (presence of an offense in another state) ## 

nb2 <- vglm(Convicted72_74_sum ~ scale(I(NEWRCE == 2)) + scale(I(NEWSEX == 2)) + 
              scale(Age) + scale(Agesq) + scale(Agecu) +
              scale(Mobility2) + scale(Mobility2sq) + scale(Mobility2cu) +
              scale(Versatility2) + scale(Versatility2sq) + scale(Versatility2cu) +
              scale(Intensity) + 
              scale(Homicide_max) + scale(Rape_max) + scale(Robbery_max) + scale(AggAss_max) + scale(Kidnap_max) + scale(Burglary_max) + scale(Theft_max) + scale(Auto_max) + scale(Assault_max) + scale(Arson_max) + scale(Fraud_max) + scale(Justice_max) + scale(Weapons_max) + scale(SexInd_max) + scale(Drugs_max) + scale(Gambling_max) + scale(Traffic_max) + scale(OtherOffenses_max) + 
              scale(Albany_max) + scale(Allegany_max) + scale(Broome_max) + scale(Cattaraugus_max) + scale(Cayuga_max) + scale(Chatauqua_max) + scale(Chemung_max) + scale(Chenango_max) + scale(Clinton_max) + scale(Columbia_max) + scale(Cortland_max) + scale(Delaware_max) + scale(Dutchess_max) + scale(Erie_max) + scale(Essex_max) + scale(Franklin_max) + scale(Fulton_max) + scale(Genesee_max) + scale(Greene_max) + scale(Hamilton_max) + scale(Herkimer_max) + scale(Jefferson_max) + scale(Kings_max) + scale(Lewis_max) + scale(Livingston_max) + scale(Madison_max) + scale(Monroe_max) + scale(Montgomery_max) + scale(Nassau_max) + scale(NewYork_max) + scale(Niagara_max) + scale(Oneida_max) + scale(Onondaga_max) + scale(Ontario_max) + scale(Orange_max) + scale(Orleans_max) + scale(Oswego_max) + scale(Otsego_max) + scale(Putnam_max) + scale(Queens_max) + scale(Rensselaer_max) + scale(Richmond_max) + scale(Rockland_max) + scale(StLawrence_max) + scale(Saratoga_max) + scale(Schenectady_max) + scale(Schoharie_max) + scale(Schuyler_max) + scale(Seneca_max) + scale(Steuben_max) + scale(Suffolk_max) + scale(Sullivan_max) + scale(Tioga_max) + scale(Tompkins_max) + scale(Ulster_max) + scale(Warren_max) + scale(Washington_max) + scale(Wayne_max) + scale(Westchester_max) + scale(Wyoming_max) + scale(Yates_max) + scale(Bronx_max) + 
              scale(Outofstate_max) + scale(AllArrest72_74_sum)
            , family = negbinomial(), data = AllArrest74.dta)
summary(nb2)
AICc(nb2)

## Calculating p-values from the model coefficients ##

nb2.pvalue<-1-pnorm(abs(coef(nb2)/sqrt(diag(vcov(nb2))))) 
nb2.pvalue


#Estimation of probabilities of counts

AllArrest74.pred2 <- predict(nb2)
AllArrest74.pred21 <- as.data.frame(AllArrest74.pred2)

AllArrest74.dta$nb2p0 <- dnbinom(0, mu = exp(AllArrest74.pred21$'log(mu)'), size = exp(AllArrest74.pred21$'log(size)'))
AllArrest74.dta$nb2p1 <- dnbinom(1, mu = exp(AllArrest74.pred21$'log(mu)'), size = exp(AllArrest74.pred21$'log(size)'))
AllArrest74.dta$nb2p2 <- dnbinom(2, mu = exp(AllArrest74.pred21$'log(mu)'), size = exp(AllArrest74.pred21$'log(size)'))
AllArrest74.dta$nb2p3 <- dnbinom(3, mu = exp(AllArrest74.pred21$'log(mu)'), size = exp(AllArrest74.pred21$'log(size)'))
AllArrest74.dta$nb2p4 <- dnbinom(4, mu = exp(AllArrest74.pred21$'log(mu)'), size = exp(AllArrest74.pred21$'log(size)'))
AllArrest74.dta$nb2p5 <- dnbinom(5, mu = exp(AllArrest74.pred21$'log(mu)'), size = exp(AllArrest74.pred21$'log(size)'))
AllArrest74.dta$nb2p5M <- 1 - AllArrest74.dta$nb2p0 - AllArrest74.dta$nb2p1 - AllArrest74.dta$nb2p2 - AllArrest74.dta$nb2p3 - AllArrest74.dta$nb2p4 - AllArrest74.dta$nb2p5

sum(AllArrest74.dta$nb2p0)
sum(AllArrest74.dta$nb2p1)
sum(AllArrest74.dta$nb2p2)
sum(AllArrest74.dta$nb2p3)
sum(AllArrest74.dta$nb2p4)
sum(AllArrest74.dta$nb2p5)
sum(AllArrest74.dta$nb2p5M)

## Estimated risk of arrest ##
(sum(1 - AllArrest74.dta$nb2p0) / sum(AllArrest74.dta$One))

#### Estimated risk of arrest in function of the characteristics ####
dt <- data.table(AllArrest74.dta)         ## Convert data into a data.table 

## Estimated risk of arrest in function of Age ##
setkey(dt, "Age")                         ## Set the key variable (Age) 
X <- dt[, list(zero=sum(nb2p0), OneMore=sum(1 - nb2p0), Risk=(sum(1 - nb2p0) / sum(One))), by=key(dt)] 
X

setkey(dt, "NEWSEX")                      ## Set the key variable (Gender)
X <- dt[, list(zero=sum(nb2p0), OneMore=sum(1 - nb2p0), Risk=(sum(1 - nb2p0) / sum(One))), by=key(dt)] 
X

## [...] same thing for the other variables [...] ##



###########################################################################
##            Model predicting the number of incarcerations              ##
###########################################################################


#### Zero-truncated negative binomial NULL model predicting incarcerations ####

nb30 <- vglm(Confinement72_74_sum ~ 1, family = negbinomial(), data = AllConviction74.dta)
summary(nb30)
AICc(nb30)

## Estimation of probabilities of counts for the null model##

AllArrest74.pred30 <- predict(nb30)
AllArrest74.pred301 <- as.data.frame(AllArrest74.pred30)

AllConviction74.dta$nb30p0 <- dnbinom(0, mu = exp(AllArrest74.pred301$'log(mu)'), size = exp(AllArrest74.pred301$'log(size)'))
AllConviction74.dta$nb30p1 <- dnbinom(1, mu = exp(AllArrest74.pred301$'log(mu)'), size = exp(AllArrest74.pred301$'log(size)'))
AllConviction74.dta$nb30p2 <- dnbinom(2, mu = exp(AllArrest74.pred301$'log(mu)'), size = exp(AllArrest74.pred301$'log(size)'))
AllConviction74.dta$nb30p3 <- dnbinom(3, mu = exp(AllArrest74.pred301$'log(mu)'), size = exp(AllArrest74.pred301$'log(size)'))
AllConviction74.dta$nb30p4 <- dnbinom(4, mu = exp(AllArrest74.pred301$'log(mu)'), size = exp(AllArrest74.pred301$'log(size)'))
AllConviction74.dta$nb30p5 <- dnbinom(5, mu = exp(AllArrest74.pred301$'log(mu)'), size = exp(AllArrest74.pred301$'log(size)'))
AllConviction74.dta$nb30p5M <- 1 - AllConviction74.dta$nb30p0 - AllConviction74.dta$nb30p1 - AllConviction74.dta$nb30p2 - AllConviction74.dta$nb30p3 - AllConviction74.dta$nb30p4 - AllConviction74.dta$nb30p5

sum(AllConviction74.dta$nb30p0)     ## Population estimated to be incarcerated 0 time
sum(AllConviction74.dta$nb30p1)     ## Population estimated to be incarcerated 1 time
sum(AllConviction74.dta$nb30p2)     ## Population estimated to be incarcerated 2 times
sum(AllConviction74.dta$nb30p3)     ## Population estimated to be incarcerated 3 times
sum(AllConviction74.dta$nb30p4)     ## Population estimated to be incarcerated 4 times
sum(AllConviction74.dta$nb30p5)     ## Population estimated to be incarcerated 5 times
sum(AllConviction74.dta$nb30p5M)    ## Population estimated to be incarcerated more than 5 times

## Estimated risk of arrest ##
(sum(1 - AllConviction74.dta$nb30p0) / sum(AllConviction74.dta$One))




#### Zero-truncated negative binomial FULL model predicting incarceration ####
## NEWRACE == 2 (Black individuals); NEWSEX == 2 (Women); TYPEOFCRIME_max == 1 (presence of a type of crime); COUNTY_max == 1 (presence of an offense is this county); Outofstate_max == 1 (presence of an offense in another state) ## 

nb3 <- vglm(Confinement72_74_sum ~ scale(I(NEWRCE == 2)) + scale(I(NEWSEX == 2)) + 
              scale(Age) + scale(Agesq) + scale(Agecu) +
              scale(Mobility2) + scale(Mobility2sq) + scale(Mobility2cu) +
              scale(Versatility2) + scale(Versatility2sq) + scale(Versatility2cu) +
              scale(Intensity) + 
              scale(Homicide_max) + scale(Rape_max) + scale(Robbery_max) + scale(AggAss_max) + scale(Kidnap_max) + scale(Burglary_max) + scale(Theft_max) + scale(Auto_max) + scale(Assault_max) + scale(Arson_max) + scale(Fraud_max) + scale(Justice_max) + scale(Weapons_max) + scale(SexInd_max) + scale(Drugs_max) + scale(Gambling_max) + scale(Traffic_max) + scale(OtherOffenses_max) + 
              scale(Albany_max) + scale(Allegany_max) + scale(Broome_max) + scale(Cattaraugus_max) + scale(Cayuga_max) + scale(Chatauqua_max) + scale(Chemung_max) + scale(Chenango_max) + scale(Clinton_max) + scale(Columbia_max) + scale(Cortland_max) + scale(Delaware_max) + scale(Dutchess_max) + scale(Erie_max) + scale(Essex_max) + scale(Franklin_max) + scale(Fulton_max) + scale(Genesee_max) + scale(Greene_max) + scale(Hamilton_max) + scale(Herkimer_max) + scale(Jefferson_max) + scale(Kings_max) + scale(Lewis_max) + scale(Livingston_max) + scale(Madison_max) + scale(Monroe_max) + scale(Montgomery_max) + scale(Nassau_max) + scale(NewYork_max) + scale(Niagara_max) + scale(Oneida_max) + scale(Onondaga_max) + scale(Ontario_max) + scale(Orange_max) + scale(Orleans_max) + scale(Oswego_max) + scale(Otsego_max) + scale(Putnam_max) + scale(Queens_max) + scale(Rensselaer_max) + scale(Richmond_max) + scale(Rockland_max) + scale(StLawrence_max) + scale(Saratoga_max) + scale(Schenectady_max) + scale(Schoharie_max) + scale(Schuyler_max) + scale(Seneca_max) + scale(Steuben_max) + scale(Suffolk_max) + scale(Sullivan_max) + scale(Tioga_max) + scale(Tompkins_max) + scale(Ulster_max) + scale(Warren_max) + scale(Washington_max) + scale(Wayne_max) + scale(Westchester_max) + scale(Wyoming_max) + scale(Yates_max) + scale(Bronx_max) + 
              scale(Outofstate_max) + scale(AllArrest72_74_sum) + scale(Convicted72_74_sum) + scale(RatioConvArr) 
            , family = negbinomial(), data = AllConviction74.dta)
summary(nb3)
AICc(nb3)

## Calculating p-values from the model coefficients ##
nb3.pvalue<-1-pnorm(abs(coef(nb3)/sqrt(diag(vcov(nb3))))) 
nb3.pvalue

## Estimation of probabilities of counts ##

AllArrest74.pred3 <- predict(nb3)
AllArrest74.pred31 <- as.data.frame(AllArrest74.pred3)

AllConviction74.dta$nb3p0 <- dnbinom(0, mu = exp(AllArrest74.pred31$'log(mu)'), size = exp(AllArrest74.pred31$'log(size)'))
AllConviction74.dta$nb3p1 <- dnbinom(1, mu = exp(AllArrest74.pred31$'log(mu)'), size = exp(AllArrest74.pred31$'log(size)'))
AllConviction74.dta$nb3p2 <- dnbinom(2, mu = exp(AllArrest74.pred31$'log(mu)'), size = exp(AllArrest74.pred31$'log(size)'))
AllConviction74.dta$nb3p3 <- dnbinom(3, mu = exp(AllArrest74.pred31$'log(mu)'), size = exp(AllArrest74.pred31$'log(size)'))
AllConviction74.dta$nb3p4 <- dnbinom(4, mu = exp(AllArrest74.pred31$'log(mu)'), size = exp(AllArrest74.pred31$'log(size)'))
AllConviction74.dta$nb3p5 <- dnbinom(5, mu = exp(AllArrest74.pred31$'log(mu)'), size = exp(AllArrest74.pred31$'log(size)'))
AllConviction74.dta$nb3p5M <- 1 - AllConviction74.dta$nb3p0 - AllConviction74.dta$nb3p1 - AllConviction74.dta$nb3p2 - AllConviction74.dta$nb3p3 - AllConviction74.dta$nb3p4 - AllConviction74.dta$nb3p5

sum(AllConviction74.dta$nb3p0)    ## Population estimated to be incarcerated 0 time
sum(AllConviction74.dta$nb3p1)    ## Population estimated to be incarcerated 1 time
sum(AllConviction74.dta$nb3p2)    ## Population estimated to be incarcerated 2 times
sum(AllConviction74.dta$nb3p3)    ## Population estimated to be incarcerated 3 times
sum(AllConviction74.dta$nb3p4)    ## Population estimated to be incarcerated 4 times
sum(AllConviction74.dta$nb3p5)    ## Population estimated to be incarcerated 5 times
sum(AllConviction74.dta$nb3p5M)   ## Population estimated to be incarcerated more than 5 times

## Estimated risk of arrest ##
(sum(1 - AllConviction74.dta$nb3p0) / sum(AllConviction74.dta$One))

#### Estimated risk of arrest in function of the characteristics ####
dt <- data.table(AllConviction74.dta)         ## Convert data into a data.table

## Estimated risk of arrest in function of Age ##
setkey(dt, "Age")                             ## Set the key variable (Age)
X <- dt[, list(zero=sum(nb3p0), OneMore=sum(1 - nb3p0), Risk=(sum(1 - nb3p0) / sum(One))), by=key(dt)] 
X

## Estimated risk of arrest in function of Gender ##
setkey(dt, "NEWSEX")                      ## Set the key variable (Gender) 
X <- dt[, list(zero=sum(nb3p0), OneMore=sum(1 - nb3p0), Risk=(sum(1 - nb3p0) / sum(One))), by=key(dt)] 
X

## [...] same thing for the other variables [...] ##



## [END] ##