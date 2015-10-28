library(data.table)
library(fastmatch)
library(zoo)
library(xgboost)
library(Metrics)

source( "../team/rain_utils.R")
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
rdata_file <- "train_full.RData" # 
rdata_file <- "train_10pct.RData" # 
tcheck(0)
cat("loading train from RData file ", rdata_file, "\n")
load( rdata_file )
zzz <- tcheck( desc='load data'); print(zzz)

ids <- unique(train$Id)
cv_frac_trn <- .7
set.seed( 1999)
cv_ids_trn <- sample( ids, round(cv_frac_trn * length(ids)) )
cv_ix_trn <- train$Id %in% cv_ids_trn
train <- train[ cv_ix_trn,  ] ;zzz <- tcheck( desc='partition cv_train'); print(zzz)

tr_raw <- train %>%  select(
    Id, 
    minutes_past, 
    radardist_km,
    Ref, 
    RefComposite, 
    Expected)

tr_raw <- subset(tr_raw, Expected <= 65)
tr_raw <- tr_raw[round(Expected, 4) %fin% valid_vals]
tr_raw$dt <- time_difference(tr_raw$minutes_past)
tr_raw$mp <- marshall_palmer(tr_raw$Ref)

#Collapse to one record per Id
tr <- tr_raw[, .(
    target = log1p(mean(Expected, na.rm = T)),
    ref = mean(dt * Ref, na.rm = T),
    ref1 = mean(dt * RefComposite, na.rm = T),
    mp = sum(dt * mp, na.rm = T),
    rd = mean(radardist_km, na.rm = T),
    records = .N,
    naCounts = sum(is.na(Ref))
), Id]


print("training model...")
cs <- c("ref", "ref1",   "mp", "rd", "records")
y<-tr$target
tr<-as.data.frame(tr)
tr<-tr[,cs]
param0 <- list("objective"  = "reg:linear" 
               , "eval_metric" = "rmse"
               , "eta" = 0.007
               , "subsample" = 0.8
               , "min_child_weight" =10    
               , "max_depth" = 8
               , "nthreads" = 4
)
xgtrain = xgb.DMatrix(as.matrix(tr), label = y, missing = NA);zzz <- tcheck( desc='construct train matrix'); print(zzz)
rm(tr,tr_raw,train)
gc()

x.mod.t  <- xgb.train(params = param0, data = xgtrain , nrounds =1955);zzz <- tcheck( desc='xgb.train cv_train'); print(zzz)

pr_trn  <- predict(x.mod.t,xgtrain);zzz <- tcheck( desc='predict logvals on cv_train'); print(zzz)
mae_xgb_trn <- mae( expm1(pr_trn), expm1(y) )

load( rdata_file ) ; zzz <- tcheck( desc='reload data'); print(zzz)
train <- train[ ! cv_ix_trn,  ] ;zzz <- tcheck( desc='partition cv_test'); print(zzz)

te_raw<-train %>%  select(
    Id, 
    minutes_past, 
    radardist_km,
    Ref, 
    RefComposite, 
    Expected)

te_raw$dt <- time_difference(te_raw$minutes_past)
te_raw$mp <- marshall_palmer(te_raw$Ref)

te <- te_raw[, .(
    ref = mean(dt * Ref, na.rm = T),
    ref1 = mean(dt * RefComposite, na.rm = T),
    mp = sum(dt * mp, na.rm = T),
    rd = mean(radardist_km),
    records = .N,
    y = max(Expected)
),Id]
te<-as.data.frame(te)

xgtest = xgb.DMatrix(as.matrix(te[,cs ]), missing = NA);zzz <- tcheck( desc='construct test matrix'); zzz

pr  <- predict(x.mod.t,xgtest);zzz <- tcheck( desc='predict logvals on cv_test'); zzz
te$xgb_prediction <- expm1(pr)
mae_xgb <- mae( te$y, te$xgb_prediction )

res <- te %>% select( Id, Expected = xgb_prediction )

#convert expected values to 0.01in values
res$Expected <- round(res$Expected / 0.254) * 0.254

print( get_tcheck())

#write.csv(res, "xgboost2210.csv", row.names = FALSE, col.names = TRUE)