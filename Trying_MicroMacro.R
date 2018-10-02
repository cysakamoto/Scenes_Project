# Micro Macro Multilevel
# For the Trump team 

library(MicroMacroMultilevel)

#########################################
# From the package documentation
#adjusted.predictors

######## SETUP: DATA GENERATING PROCESSES ########
set.seed(123)
# Step 1. Generate a G-by-q data frame of group-level predictors (e.g., control variables), z.data
# In this example, G = 40, q = 2
group.id = seq(1, 40)
z.var1 = rnorm(40, mean=0, sd=1)
z.var2 = rnorm(40, mean=100, sd=2)
z.data = data.frame(group.id, z.var1, z.var2)
# Step 2. Generate a G-by-p data frame of group-level means for the predictors that will be used to
# generate x.data
# In this example, there are 3 individual-level predictors, thus p = 3
x.var1.means = rnorm(40, mean=50, sd = .05)
x.var2.means = rnorm(40, mean=20, sd = .05)
x.var3.means = rnorm(40, mean=-10, sd = .05)
x.data.means = data.frame(group.id, x.var1.means, x.var2.means, x.var3.means)
# Step 3. Generate two N-by-p data frames of individual-level predictors, x.data
# One of these two data frames assumes unequal-sized groups (Step 3a),
# whereas the other assumes equal-sized groups (Step 3b):
# Step 3a. Generate the individual-level predictors
# In this example, N = 200 and group size is unequal
x.data.unequal = data.frame( group.id=rep(1:40, times=sample( c(4,5,6), 40, replace=TRUE) )[1:200] ) 
x.data.unequal = merge( x.data.unequal, data.frame( group.id, x.var1.means, x.var2.means, x.var3.means ), by="group.id" ) 
x.data.unequal = within( x.data.unequal, {x.var1 = x.var1.means + rnorm(200, mean=0, sd = 2)
x.var2 = x.var2.means + rnorm(200, mean=0, sd = 6)
x.var3 = x.var3.means + rnorm(200, mean=0, sd = 1.5)
})

# Step 3b. Generate the individual-level predictors
# In this example, N = 200 and group size is equal
x.data.equal = data.frame( group.id=rep(1:40, each=5) )
x.data.equal = merge( x.data.equal, x.data.means, by="group.id" )
x.data.equal = within( x.data.equal, {
  x.var1 = x.var1.means + rnorm(200, mean=0, sd = 2)
  x.var2 = x.var2.means + rnorm(200, mean=0, sd = 6)
  x.var3 = x.var3.means + rnorm(200, mean=0, sd = 1.5)
})
# Step 3. Generate a G-by-1 data frame of group-level outcome variable, y
# In this example, G = 40
y = rnorm(40, mean=6, sd=5)
apply(x.data.equal,2,mean)
# group.id x.var1.means x.var2.means x.var3.means # 20.500000 50.000393 19.994708 -9.999167 
apply(x.data.unequal,2,mean)
# group.id x.var1.means x.var2.means x.var3.means # 20.460000 50.002286 19.994605 -9.997034 
apply(z.data,2,mean)
# z.var1 z.var2
# 0.04518332 99.98656817
mean(y)
# 6.457797 

######## EXAMPLE 1. GROUP SIZE IS DIFFERENT ACROSS GROUPS ########
######## Need to use adjusted.predictors() in the same package ###
# Step 4a. Generate a G-by-1 matrix of group ID, z.gid. Then generate an N-by-1 matrix of # each individual's group ID, x.gid, where the group sizes are different
z.gid = seq(1:40)
x.gid = x.data.unequal$group.id
# Step 5a. Generate the best linear unbiased predictors that are calcualted from
# individual-level data
x.data = x.data.unequal[,c("x.var1","x.var2","x.var3")]
results = adjusted.predictors(x.data, z.data, x.gid, z.gid)
# Note: Given the fixed random seed, the output should be as below
results$unequal.groups
# TRUE
names(results$adjusted.group.means)
# "BLUP.x.var1" "BLUP.x.var2" "BLUP.x.var3" "z.var1"
head(results$adjusted.group.means)

######## EXAMPLE 2. GROUP SIZE IS THE SAME ACROSS ALL GROUPS ########
######## Need to use adjusted.predictors() in the same package ###
# Step 4b. Generate a G-by-1 matrix of group ID, z.gid. Then generate an N-by-1 matrix of # each individual's group ID, x.gid, where group size is the same across all groups z.gid = seq(1:40)
x.gid = x.data.equal$group.id
# Step 5b. Generate the best linear unbiased predictors that are calcualted from
# individual-level data
x.data = x.data.equal[,c("x.var1","x.var2","x.var3")]
results = adjusted.predictors(x.data, z.data, x.gid, z.gid)
results$unequal.groups
# FALSE
names(results$adjusted.group.means)
# "BLUP.x.var1" "BLUP.x.var2" "BLUP.x.var3" "z.var1"
results$adjusted.group.means[1:5, ]


# micromacro.lm
# for example 1
# Step 6a. Fit a micro-macro multilevel model when group sizes are different
model.formula = as.formula(y ~ BLUP.x.var1 + BLUP.x.var2 + BLUP.x.var3 + z.var1 + z.var2) 
model.output = micromacro.lm(model.formula, results$adjusted.group.means, y, results$unequal.groups) 
micromacro.summary(model.output)
model.output$statistics
model.output$rsquared
model.output$rsquared.adjusted

# for example 2
# Step 6b. Fit a micro-macro multilevel model when group size is the same across groups
model.output2 = micromacro.lm(model.formula, results$adjusted.group.means, y,
                              results$unequal.groups)
micromacro.summary(model.output2)
model.output2$statistics
model.output2$rsquared
model.output2$rsquared.adjusted

# for example 3: interaction term
model.formula3 = as.formula(y ~ BLUP.x.var1 * BLUP.x.var2 + BLUP.x.var3 + z.var1 + z.var2) 
model.output3 = micromacro.lm(model.formula3, results$adjusted.group.means, y, results$unequal.groups)
micromacro.summary(model.output3)
model.output3$statistics
model.output3$rsquared
model.output3$rsquared.adjusted

#############################################

# countyID = countycen
# zipcodeID = zipcode
#donaldjtrump_prop16    county
#CrimeRate1999county    county  - a lot of missing cases 
#PropRepub2008          county
#pctba2k                zipcode
#percent_nonwhite2k     zipcode
#ZYP_FactorScore        zipcode
#pctfemale2k            zipcode
#MedAge2k               zipcode
#pctchild2k             zipcode - 100% children in a zip code?
#zdensity               zipcode - what does it mean?
#zpctnw2k               zipcode - what does it mean?
#zreligneg              zipcode

#higher level is of different sizes 

library(readstata13)

#Trump data
df <- read.dta13('merge.dta')

z.gid = df$countcen
x.gid = df$zipcode
x.data = x.data.unequal[,c("pctba2k","percent_nonwhite2k","ZYP_FactorScore", "pctfemale2k", 
                           "MedAge2k", "pctchild2k", "zdensity", "zpctnw2k", "zreligneg")]










