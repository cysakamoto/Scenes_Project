# Setting the folder 
setwd("/home/cysakamoto/TRUMP_Data")

# Libraries
library(readstata13)
library(foreign)
library(stargazer)


# Loading data and selecting variables from each file.
mydata <- read.dta13(file = "ScenesWithVotingFixed.dta", convert.factors = TRUE, 
                                  generate.factors = TRUE, 
                                  encoding = "UTF-8",
                                  fromEncoding = NULL, 
                                  convert.underscore = FALSE,
                                  missing.type = FALSE, convert.dates = TRUE, replace.strl = TRUE,
                                  add.rownames = FALSE, nonint.factors = TRUE, select.rows = NULL )

mydataE <- readstata13::read.dta13(file = "ScenesWithVotingFixedE.dta", convert.factors = TRUE, 
                                   generate.factors = TRUE, 
                                   encoding = "UTF-8",
                                   fromEncoding = NULL, 
                                   convert.underscore = FALSE,
                                   missing.type = FALSE, convert.dates = TRUE, replace.strl = TRUE,
                                   add.rownames = FALSE, nonint.factors = TRUE, select.rows = NULL )

gini <- mydataE[,c("zipcode", "estimateginiindex")]

trumpdata1 <- mydataE[,c("zipcode", "countcen", "donaldjtrump_prop16", 
                         "percent_nonwhite2k", "estimateginiindex")]

Merge34 <- readstata13::read.dta13(file = "Merge34.DS Hindex.STATA.Format..dta", convert.factors = TRUE, 
                                   generate.factors = TRUE, 
                                   encoding = "UTF-8",
                                   fromEncoding = NULL, 
                                   convert.underscore = FALSE,
                                   missing.type = FALSE, convert.dates = TRUE, replace.strl = TRUE,
                                   add.rownames = FALSE, nonint.factors = TRUE, select.rows = NULL )

Merge34_1 <- Merge34[,c("zipcode", "pctfemale2k", "MedAge2k", "pctchild2k", "NonWhite_2000", "LevelNonWhite_2000", "ZYP_FactorScore", "pctba2k")]

crimemerge <- Merge34[,c("zipcode", "CrimeRate1999county")]

hyesun <- read.csv("nhgis0030_ts_geog2010_zcta.csv", header = TRUE, sep = ",", skipNul = TRUE, blank.lines.skip = TRUE, dec = ".")

finalvars <- read.spss("FinalVariables_plusYP_clean1.sav", to.data.frame = TRUE)

bizzip <- read.spss("bizzip_part_arts11.sav", to.data.frame = TRUE)

dan_new <- read.csv("DanFile_NEW.csv", header = TRUE, sep = ",", skipNul = TRUE, blank.lines.skip = TRUE, dec = ".")

newtrump <- merge(dan_new, trumpdata1, by = "countcen")

#zip code (mydataE or mydata)
#pctba2k (34)
#percent_nonwhite2k (mydataE)
#TotalRepubVotes2008 (Dan's File)
#crime rate (mydataE)
#ZYP_FactorScore (34)
#pctfemale2k (34)
#MedAge2k (34)
#pctchild2k (34)
#zgini

hansel_final <- merge(Merge34_1, newtrump)
hansel_final <- merge(crimemerge, hansel_final)

#And finally create a global core including all data needed. 
#You can print this data to double check that it includes all the factors necessary. 

globalcore <- hansel_final[,c("zipcode", "countcen", "donaldjtrump_prop16","pctba2k", 
                              "CrimeRate1999county", "TotalRepubVotes2008", 
                              "percent_nonwhite2k", "ZYP_FactorScore", "pctfemale2k", 
                              "MedAge2k", "pctchild2k" )]

#Core regression
corereg <- lm(donaldjtrump_prop16 ~ pctba2k + CrimeRate1999county + TotalRepubVotes2008 
              + percent_nonwhite2k + ZYP_FactorScore + pctfemale2k + MedAge2k + 
                pctchild2k, data = globalcore, na.action=na.omit)  

#Core regression with standardization requires the download of a package "lm.beta"
#the lm function is R's syntax for a regression. When dealing with multiple variables, you need to separate
#using the addition sign above. 

coreregbeta <- lm.beta::lm.beta(corereg)
summary (coreregbeta)

##Output a table using the stargazer package. 

stargazer(corereg,type = "html", out = "corereg.htm")
stargazer(coreregbeta,type = "html", out = "coreregbeta.htm")

#Now, we get into the hypotheses. 

##H1Strong levels of religiosity interact with low population density 
#and lack of non-white diversity

#Start by creating the changeinreligiousorgsneg from the DO file.
tot_estb813110 <- mydata [,c("tot_estb813110")] 
totest13_813110 <- mydata [,c("totest13_813110")]
religneg <- tot_estb813110- totest13_813110

density2k <- mydata[,c("density2000_K")]
pctnw2k <- mydata[,c("percent_nonwhite2k")]
zipcode <- mydataE[,c("zipcode")]

summary(density2k)
summary(pctnw2k)
summary(religneg)

#Standardization of your data. 

zdensity <- scale(density2k) 
zpctnw2k <- scale(pctnw2k) # This variable shouldn't be standardized, as it already starts in 0 and ends in 1.
zreligneg <- scale(religneg)

testh1 <- data.frame(zdensity, zpctnw2k, zreligneg, zipcode, row.names = NULL) # I disagree with this line. Also, it crashes. This is as far as I could get. 
h1data <- merge(globalcore, testh1, by = "zipcode")

h1m1regcore <- coreregbeta

#Hyp 1 Model 2

h1m2reldenreg <- lm(donaldjtrump_prop16 ~ pctba2k + CrimeRate1999county + TotalRepubVotes2008 + 
                      percent_nonwhite2k + ZYP_FactorScore + pctfemale2k + MedAge2k + 
                      pctchild2k + zdensity + zpctnw2k + zreligneg, data =h1data, na.rm = TRUE)

h1m2regbeta <- lm.beta::lm.beta(h1m2reldenreg)

#Hyp. 1 Model 3

h1m3reldennwreg <- lm(donaldjtrump_prop16 ~ pctba2k + CrimeRate1999county + TotalRepubVotes2008 + 
                        percent_nonwhite2k + ZYP_FactorScore + pctfemale2k + MedAge2k + 
                        pctchild2k+ zdensity + zreligneg + zpctnw2k*zreligneg+
                        zpctnw2k*zdensity + zdensity*zreligneg, data =h1data, na.rm = TRUE)

h1m3regbeta <- lm.beta::lm.beta(h1m3reldennwreg)

library(stargazer)

stargazer(h1m1regcore, type = "html", out = "h1m1.htm")
stargazer(h1m2regbeta, type = "html", out = "h1m2.htm")
stargazer(h1m3regbeta, type = "html", out = "h1m3.htm")

#H2: Increases in racial diversity interact with persistent poverty, 
#low diversity scenes, and traditionalism

#Pull to data frame
racediv2k <- mydata[,c("racediversity2k")]
pctpov2k <- mydata[,c("pctpov2k")]
BZ01TradPerf_cty_impu <- mydata[,c("BZ01TradPerf_cty_impu")]
BZ01EthnPerf_cty_impu <- mydata[,c("BZ01EthnPerf_cty_impu")]

h2data <-hansel_final[,c("zipcode", "countcen", "donaldjtrump_prop16","pctba2k", 
                "CrimeRate1999county", "TotalRepubVotes2008", 
                "percent_nonwhite2k", "ZYP_FactorScore", "pctfemale2k", 
                "MedAge2k", "pctchild2k" )]

#Standardize your data
zracediv2k <- scale(racediv2k, center = TRUE, scale = TRUE)
zpctpov2k <- scale(pctpov2k, center = TRUE, scale = TRUE)
zBZ01TradPerf <- scale(BZ01TradPerf_cty_impu, center = TRUE, scale = TRUE)
zBZ01EthnPerf <- scale(BZ01EthnPerf_cty_impu, center = TRUE, scale = TRUE)

testh2 <- data.frame(zracediv2k, zpctpov2k, zBZ01EthnPerf, zBZ01TradPerf, 
                     zipcode, row.names = NULL)
h2data <- merge(h2data, testh2, by = "zipcode")

h2m1regcore <- coreregbeta
h2m2regcore <- coreregbeta

#Hyp 2 Without Interactions
h2m1reg <- lm(donaldjtrump_prop16 ~ pctba2k + CrimeRate1999county + TotalRepubVotes2008 + 
                percent_nonwhite2k + ZYP_FactorScore + pctfemale2k + MedAge2k + 
                pctchild2k+ zracediv2k, data =h2data)
h2m1regbeta <- lm.beta::lm.beta(h2m1reg)

h2m2reg <- lm(donaldjtrump_prop16 ~ pctba2k + CrimeRate1999county + TotalRepubVotes2008 + 
                percent_nonwhite2k + ZYP_FactorScore + pctfemale2k + MedAge2k + 
                pctchild2k+ zracediv2k + zpctpov2k, data =h2data)
h2m2regbeta <- lm.beta::lm.beta(h2m2reg)

h2m3reg <- lm(donaldjtrump_prop16 ~ pctba2k + CrimeRate1999county + TotalRepubVotes2008 + 
                percent_nonwhite2k + ZYP_FactorScore + pctfemale2k + MedAge2k + 
                pctchild2k + zracediv2k + zBZ01TradPerf, data =h2data)
h2m3regbeta <- lm.beta::lm.beta(h2m3reg)

h2m4reg <- lm(donaldjtrump_prop16 ~ pctba2k + CrimeRate1999county + TotalRepubVotes2008 + 
                percent_nonwhite2k + ZYP_FactorScore + pctfemale2k + MedAge2k + 
                pctchild2k+ zracediv2k + zBZ01EthnPerf, data =h2data)
h2m4regbeta <- lm.beta::lm.beta(h2m4reg)

h2m5reg <- lm(donaldjtrump_prop16 ~ pctba2k + CrimeRate1999county + TotalRepubVotes2008 + 
                percent_nonwhite2k + ZYP_FactorScore + pctfemale2k + MedAge2k + 
                pctchild2k + zracediv2k + zpctpov2k +zBZ01TradPerf, data =h2data)
h2m5regbeta <- lm.beta::lm.beta(h2m5reg)

h2m6reg <- lm(donaldjtrump_prop16 ~ pctba2k + CrimeRate1999county + TotalRepubVotes2008 + 
                percent_nonwhite2k + ZYP_FactorScore + pctfemale2k + MedAge2k + 
                pctchild2k+ zracediv2k + zpctpov2k +zBZ01EthnPerf, data =h2data)
h2m6regbeta <- lm.beta::lm.beta(h2m6reg)

h2m7reg <- lm(donaldjtrump_prop16 ~ pctba2k + CrimeRate1999county + TotalRepubVotes2008 + 
                percent_nonwhite2k + ZYP_FactorScore + pctfemale2k + MedAge2k + 
                pctchild2k + zracediv2k + zBZ01TradPerf + zBZ01EthnPerf, data =h2data)
h2m7regbeta <- lm.beta::lm.beta(h2m7reg)


#Hyp 2 (int.) With Interactions

h2m0intreg <- lm(donaldjtrump_prop16 ~ pctba2k + CrimeRate1999county + TotalRepubVotes2008 + 
                   percent_nonwhite2k + ZYP_FactorScore + pctfemale2k + MedAge2k + 
                   pctchild2k + zracediv2k, data =h2data)
h2m0intregbeta <- lm.beta::lm.beta(h2m0intreg)

h2m1intregbeta <- coreregbeta

h2m2intreg <- lm(donaldjtrump_prop16 ~ pctba2k + CrimeRate1999county + TotalRepubVotes2008 + 
                   percent_nonwhite2k + ZYP_FactorScore + pctfemale2k + MedAge2k + 
                   pctchild2k + zracediv2k + zpctpov2k + zracediv2k*zpctpov2k
                 ,data =h2data)
h2m2intregbeta <- lm.beta::lm.beta(h2m2intreg)

h2m3intreg <- lm(donaldjtrump_prop16 ~ pctba2k + CrimeRate1999county + TotalRepubVotes2008 + 
                   percent_nonwhite2k + ZYP_FactorScore + pctfemale2k + MedAge2k + 
                   pctchild2k + zracediv2k + zBZ01TradPerf + zracediv2k*zBZ01TradPerf
                 ,data =h2data)
h2m3intregbeta <- lm.beta::lm.beta(h2m3intreg)

h2m4intreg <- lm(donaldjtrump_prop16 ~ pctba2k + CrimeRate1999county + TotalRepubVotes2008 + 
                   percent_nonwhite2k + ZYP_FactorScore + pctfemale2k + MedAge2k + 
                   pctchild2k + zracediv2k + zBZ01EthnPerf + zracediv2k*zBZ01EthnPerf
                 ,data =h2data)
h2m4intregbeta <- lm.beta::lm.beta(h2m4intreg)

h2m5intreg <- lm(donaldjtrump_prop16 ~ pctba2k + CrimeRate1999county + TotalRepubVotes2008 + 
                   percent_nonwhite2k + ZYP_FactorScore + pctfemale2k + MedAge2k + 
                   pctchild2k + zracediv2k + zpctpov2k +zBZ01TradPerf +
                   zracediv2k*zpctpov2k + zracediv2k*zBZ01TradPerf + zpctpov2k*zBZ01TradPerf + 
                   zracediv2k*zpctpov2k*zBZ01TradPerf,data =h2data)
h2m5intregbeta <- lm.beta::lm.beta(h2m5intreg)

h2m6intreg <- lm(donaldjtrump_prop16 ~ pctba2k + CrimeRate1999county + TotalRepubVotes2008 + 
                   percent_nonwhite2k + ZYP_FactorScore + pctfemale2k + MedAge2k + 
                   pctchild2k + zracediv2k + zpctpov2k +zBZ01EthnPerf +
                   zracediv2k*zpctpov2k + zracediv2k*zBZ01EthnPerf + zpctpov2k*zBZ01EthnPerf + 
                   zracediv2k*zpctpov2k*zBZ01EthnPerf,data =h2data)
h2m6intregbeta <- lm.beta::lm.beta(h2m6intreg)

h2m7intreg <- lm(donaldjtrump_prop16 ~ pctba2k + CrimeRate1999county + TotalRepubVotes2008 + 
                   percent_nonwhite2k + ZYP_FactorScore + pctfemale2k + MedAge2k + 
                   pctchild2k+ zracediv2k + zBZ01TradPerf +zBZ01EthnPerf +
                   zracediv2k*zBZ01TradPerf + zracediv2k*zBZ01EthnPerf + zBZ01TradPerf*zBZ01EthnPerf + 
                   zracediv2k*zBZ01TradPerf*zBZ01EthnPerf,data =h2data)
h2m7intregbeta <- lm.beta::lm.beta(h2m7intreg)

h2m8intreg <- lm(donaldjtrump_prop16 ~ pctba2k + CrimeRate1999county + TotalRepubVotes2008 + 
                   percent_nonwhite2k + ZYP_FactorScore + pctfemale2k + MedAge2k + 
                   pctchild2k + zracediv2k + zpctpov2k + zBZ01TradPerf +
                   zBZ01EthnPerf + zracediv2k*zpctpov2k + zracediv2k*zBZ01TradPerf + zracediv2k*zBZ01EthnPerf +
                   zpctpov2k*zBZ01TradPerf + zpctpov2k*zBZ01EthnPerf+ zBZ01TradPerf*zBZ01EthnPerf + 
                   zracediv2k*zpctpov2k*zBZ01TradPerf + zracediv2k*zBZ01TradPerf*zBZ01EthnPerf + 
                   zracediv2k*zpctpov2k*zBZ01EthnPerf + zpctpov2k*zBZ01TradPerf*zBZ01EthnPerf
                 ,data =h2data)
h2m8intregbeta <- lm.beta::lm.beta(h2m8intreg)

#Regression not run in Emily, but important to run

h2m9intreg <- lm(donaldjtrump_prop16 ~ pctba2k + CrimeRate1999county + TotalRepubVotes2008 + 
                  percent_nonwhite2k + ZYP_FactorScore + pctfemale2k + MedAge2k + 
                  pctchild2k + zracediv2k + zpctpov2k + zBZ01TradPerf +
                  zBZ01EthnPerf + zracediv2k*zpctpov2k + zracediv2k*zBZ01TradPerf + zracediv2k*zBZ01EthnPerf +
                  zpctpov2k*zBZ01TradPerf + zpctpov2k*zBZ01EthnPerf+ zBZ01TradPerf*zBZ01EthnPerf + 
                  zracediv2k*zpctpov2k*zBZ01TradPerf + zracediv2k*zBZ01TradPerf*zBZ01EthnPerf + 
                  zracediv2k*zpctpov2k*zBZ01EthnPerf + zpctpov2k*zBZ01TradPerf*zBZ01EthnPerf +
                  zracediv2k*zpctpov2k*zBZ01TradPerf*zBZ01EthnPerf,data =h2data)
h2m9intregbeta <- lm.beta::lm.beta(h2m9intreg)

##H3 Increased income inequality in more local, 
#transgressive, more anti-state communities

#Pull to data frame
BZ01TransPerf_cty_impu <- mydataE[,c("BZ01TransPerf_cty_impu")]
BZ01StatePerf_cty_impu <- mydataE[,c("BZ01StatePerf_cty_impu")]
BZ01LocalPerf_cty_impu <- mydataE[,c("BZ01LocalPerf_cty_impu")]
gini <- mydataE[,c("estimateginiindex")]

testtrans <- mydata[,c("BZ01TransPerf_cty_impu")]
teststate <- mydata[,c("BZ01StatePerf_cty_impu")]
testlocal <- mydata[,c("BZ01LocalPerf_cty_impu")]

summary(gini)

summary(BZ01TransPerf_cty_impu)
summary(testtrans)

summary(BZ01StatePerf_cty_impu)
summary(teststate)

summary(BZ01LocalPerf_cty_impu)
summary(testlocal)


h3data <- hansel_final[,c("zipcode", "countcen", "donaldjtrump_prop16","pctba2k", 
                          "CrimeRate1999county", "TotalRepubVotes2008", 
                          "percent_nonwhite2k", "ZYP_FactorScore", "pctfemale2k", 
                          "MedAge2k", "pctchild2k" )]

#Standardize your data
zTransPerfcty <- scale(BZ01TransPerf_cty_impu, center = TRUE, scale = TRUE)
zStatePerfcty <- scale(BZ01StatePerf_cty_impu, center = TRUE, scale = TRUE)
zLocalPerfcty <- scale(BZ01LocalPerf_cty_impu, center = TRUE, scale = TRUE)
zgini <- scale(gini, center = TRUE, scale = TRUE)

testh3 <- data.frame(zTransPerfcty, zStatePerfcty, zLocalPerfcty, zgini,
                     zipcode, row.names = NULL)
h3data <- merge(h3data, testh3, by = "zipcode")

#Run Regressions 

h3m1regbeta <- coreregbeta

h3m2reg <- lm(donaldjtrump_prop16 ~ pctba2k + CrimeRate1999county + TotalRepubVotes2008 + 
                percent_nonwhite2k + ZYP_FactorScore + pctfemale2k + MedAge2k + 
                pctchild2k + zTransPerfcty + zStatePerfcty+ 
                zLocalPerfcty + zgini, data =h3data)
h3m2regbeta <- lm.beta::lm.beta(h3m2reg)

h3m3reg <- lm(donaldjtrump_prop16 ~ pctba2k + CrimeRate1999county + TotalRepubVotes2008 + 
                percent_nonwhite2k + ZYP_FactorScore + pctfemale2k + MedAge2k + 
                pctchild2k + zTransPerfcty*zgini + zStatePerfcty*zgini + zLocalPerfcty*zgini
              , data = h3data)
h3m3regbeta <- lm.beta::lm.beta(h3m3reg)

#H4:Declining neighborliness interacts with 
#declining religiosity and increasing population density within rural areas

YPNeighPerf_cty_impu <- mydata[,c("YPNeighPerf_cty_impu")]

h4data <- hansel_final[,c("zipcode", "countcen", "donaldjtrump_prop16","pctba2k", 
                          "CrimeRate1999county", "TotalRepubVotes2008", 
                          "percent_nonwhite2k", "ZYP_FactorScore", "pctfemale2k", 
                          "MedAge2k", "pctchild2k" )]

zYPNeighPerf <- scale(YPNeighPerf_cty_impu, center = TRUE, scale = TRUE)
testh4 <- data.frame(zYPNeighPerf, zreligneg, zdensity, zipcode, row.names = NULL)
h4data <- merge(h4data, testh4, by = "zipcode")

h4m1regbeta <- coreregbeta

h4m2reg <- lm(donaldjtrump_prop16 ~ pctba2k + CrimeRate1999county + TotalRepubVotes2008 + 
                percent_nonwhite2k + ZYP_FactorScore + pctfemale2k + MedAge2k + 
                pctchild2k+ zdensity + zreligneg + zYPNeighPerf, data =h4data)

h4m2regbeta <- lm.beta::lm.beta(h4m2reg)

h4m3reg <- lm(donaldjtrump_prop16 ~ pctba2k + CrimeRate1999county + TotalRepubVotes2008 + 
                percent_nonwhite2k + ZYP_FactorScore + pctfemale2k + MedAge2k + 
                pctchild2k+ zdensity + zreligneg + zYPNeighPerf*zreligneg +
                zYPNeighPerf*zdensity + zdensity*zreligneg, data =h4data)
h4m3regbeta <- lm.beta::lm.beta(h4m3reg)

##DONE WITH HYPOTHESIS TESTING. 

#Next, 
#For Mkcorr, we will use the cor() function in R 
#For cor(), complete.obs is listwise 
#For cor(), pairwise.complete.obs is pairwise. 

cordatacore <- globalcore
cordatacore$zipcode = NULL
cordatah1 <- h1data
cordatah1$zipcode = NULL
cordatah2 <- h2data
cordatah2$zipcode = NULL
cordatah3 <- h3data
cordatah3$zipcode = NULL
cordatah4 <- h4data
cordatah4$zipcode = NULL

#H0 
h0lwcor <- round(cor(cordatacore, use = "complete.obs", method = "pearson"),4)
h0pwcor <- round(cor(cordatacore, use = "pairwise.complete.obs", method = "pearson"),4)

#H1 
h1lwcor <- round(cor(cordatah1, use = "complete.obs", method = "pearson"),4)
h1pwcor <- round(cor(cordatah1, use = "pairwise.complete.obs", method = "pearson"),4)

#H2
h2lwcor <- round(cor(cordatah2, use = "complete.obs", method = "pearson"),4)
h2pwcor <- round(cor(cordatah2, use = "pairwise.complete.obs", method = "pearson"),4)

#H3
h3lwcor <- round(cor(cordatah3, use = "complete.obs", method = "pearson"),4)
h3pwcor <- round(cor(cordatah3, use = "pairwise.complete.obs", method = "pearson"),4)

#H4
h4lwcor <- round(cor(cordatah4, use = "complete.obs", method = "pearson"),4)
h4pwcor <- round(cor(cordatah4, use = "pairwise.complete.obs", method = "pearson"),4)

#making small changes with high-multicollinearity variables / for variables <-.45 or >.45 
#in CASEWISE or LISTWISE collinearity table
write.csv(h0lwcor, "h0lwcor.csv")
write.csv(h0pwcor, "h0pwcor.csv")
write.csv(h1lwcor, "h1lwcor.csv")
write.csv(h1pwcor, "h1pwcor.csv")
write.csv(h2lwcor, "h2lwcor.csv")
write.csv(h2pwcor, "h2pwcor.csv")
write.csv(h3lwcor, "h3lwcor.csv")
write.csv(h3pwcor, "h3pwcor.csv")
write.csv(h4lwcor, "h4lwcor.csv")
write.csv(h4pwcor, "h4pwcor.csv")

library(stargazer)
stargazer(h1m1regcore,h1m2regbeta,h1m3regbeta,type = "html", out = "h1reg.htm")
stargazer(h2m1regbeta,h2m2regbeta,h2m3regbeta,h2m4regbeta,h2m5regbeta,h2m6regbeta,
          h2m7regbeta,h2m8regbeta,type = "html", out = "h2regNOINT.htm")
stargazer(h2m1intregbeta,h2m2intregbeta,h2m3intregbeta,h2m4intregbeta,h2m5intregbeta,
          h2m6intregbeta,h2m7intregbeta,h2m8intregbeta, type = "html", out = "h2regINT.htm")
stargazer(h3m1regbeta,h3m2regbeta,h3m3regbeta, type = "html", out = "h3reg.htm")
stargazer(h4m1regbeta,h4m2regbeta,h4m3regbeta, type = "html", out = "h4reg.htm")