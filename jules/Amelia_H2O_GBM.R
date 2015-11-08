library(data.table)
library(fastmatch)
library(zoo)
library(Metrics)
library(dplyr)
library(h2o)

h2o.init(nthreads=-2) #Option #1: Connects to H2O servers. I get curl error on this one
localH2O = h2o.init(nthreads=-1,max_mem_size="16g") #Option #2: runs on local machine no issues

setwd("/Users/jmalin/Documents/Personal/School/Capstone/R working directory")

train <-fread("train.csv")
train.df <- data.frame(train)
train.n <- train.df[,c("Id", "minutes_past", "radardist_km", "Ref","RefComposite","RhoHV","Zdr","Kdp", "Expected")]

test <-fread("test.csv")
test.df <- data.frame(test)
test.n <- test.df[,c("Id",  "minutes_past", "radardist_km", "Ref","RefComposite","RhoHV","Zdr", "Kdp")]

##### Amelia imputation #####
install.packages("Amelia")
library(Amelia)

# Imputation method #1: Multiple Imputation
# EMB (expectation-maximization with bootstrapping) algorithm
# EMB algorithm combines the classic EM algorithm with a 
# bootstrap approach to take draws from this posterior.
# EM algorithm is a general iterative algorithm for parameter estimation

# impute train
a.out.train <- amelia(train.n, m = 1, ts = "minutes_past")
write.amelia(obj = a.out.train, file.stem = "amelia_train_3")

#impute test 
a.out.test <- amelia(test.n, m = 1, ts = "minutes_past")
write.amelia(obj = a.out.test, file.stem = "amelia_test_2")

# Imputation method #2: Time-series cross-sectional data
# Builds a general model of patterns within variables across time by 
# creating a sequence of polynomials of the time index

# impute train
a.out.train.poly <- amelia(train.n, ts = "minutes_past", polytime = 2)
write.amelia(obj = a.out.train.poly, file.stem = "amelia_train_poly")

# impute test 
a.out.test.poly <- amelia(test.n, ts = "minutes_past", polytime = 2)
write.amelia(obj = a.out.test.poly, file.stem = "amelia_test_poly")

### Amelia Diagnostics
disperse(a.out.train, dims = 1, m = 5)
plot(a.out.train, which.vars = 6)
compare.density(a.out.train, var = "Ref")
overimpute(a.out.train.poly, var = "Ref")
missmap(a.out.train)
tscsPlot(a.out.test.poly, cs = "minutes_past", var = "Ref",
         +    ylim = c(-10, 60), main = "Ref (with time settings)")

#### Variable Correlation Charting
library(PerformanceAnalytics)
train.n.sub <- train.n[,c("Ref","RhoHV","Zdr","Kdp")]
train.n.sub.samp <- sample_n(train.n.sub, 1000)
chart.Correlation(train.n.sub.samp)

##################################
#### Marshal Palmer + H2O GBM ###
#################################

##########FUNCTIONS###########
#Fast %in%
`%fin%` <- function(x, lkup) {
    fmatch(x, lkup, nomatch = 0L) > 0L
}

#Get the time differences between each measure
time_difference <- function(times, num_per_segment = 60) {
    n <- length(times)
    valid_time <- vector(mode="numeric", length = n)
    valid_time[1] <- times[1]
    valid_time[-1] <- diff(times, 1)
    valid_time[n] <- valid_time[n] + num_per_segment - sum(valid_time)
    valid_time <- valid_time / num_per_segment
    valid_time
}

#Convert reflectivity (dbz) to mm/hr
marshall_palmer <- function(dbz) {
    ((10**(dbz/10))/200) ** 0.625
}



##Valid values based on 0.01in measurements
valid_vals <- 0.254 * 1:300

######Training Data############
tr_raw <- fread("amelia_train_poly5.csv", select = c(
    "Id", 
    "minutes_past", 
    "radardist_km",
    "Ref", 
    "Expected")
)


tr_raw <- tr_raw[round(Expected, 4) %fin% valid_vals] 
tr_raw$dt <- time_difference(tr_raw$minutes_past)
tr_raw$mp <- marshall_palmer(tr_raw$Ref)

#Collapse to one record per Id
tr <- tr_raw[, .(
    target = log1p(mean(Expected, na.rm = T)),
    ref = mean(dt * Ref, na.rm = T),
    mp = sum(dt * mp, na.rm = T),
    rd = mean(radardist_km, na.rm = T),
    records = .N,
    naCounts = sum(is.na(Ref))
), Id]


print("training model...")
trainHex<-as.h2o(tr,destination_frame="train.hex")

feature_cols <- c("ref", "mp", "rd", "records")
gbmHex<-h2o.gbm(x=feature_cols,
                y="target",training_frame=trainHex, model_id="gbmStarter.hex",
                distribution="AUTO",
                nfolds = 0,
                seed = 23887, 
                ntrees = 1130, 
                max_depth = 7,
                min_rows = 10,
                learn_rate = 0.015)

rm(tr_raw)
gbmHex

print("Processing test data...")
te_raw<-fread("amelia_test_poly5.csv",select=c(    
    "Id", 
    "minutes_past", 
    "radardist_km",
    "Ref", 
    "Expected")
)
te_raw$dt <- time_difference(te_raw$minutes_past)
te_raw$mp <- marshall_palmer(te_raw$Ref)

te <- te_raw[, .(
    ref = mean(dt * Ref, na.rm = T),
    mp = sum(dt * mp, na.rm = T),
    rd = mean(radardist_km),
    records = .N
),Id]

testHex<-as.h2o(te,destination_frame="test.hex")
sample_sol <-fread("sample_solution.csv")
gmb_prediction <- expm1(as.data.frame(h2o.predict(gbmHex,testHex)))

res <- data.frame(
    Id = te$Id,
    Expected = 0.75 * gmb_prediction$predict + 0.25 * sample_sol$Expected
)

#convert expected values to 0.01in values
res$Expected <- round(res$Expected / 0.254) * 0.254

write.csv(res, "submission_poly.csv") #, row.names = FALSE, col.names = TRUE) 
