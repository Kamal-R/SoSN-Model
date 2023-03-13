
###########################
###
### Import data 
###
###########################

nCov_Merged <- readRDS( "Data_File.RDS" )
nCov_Merged = nCov_Merged[!is.na(nCov_Merged$dead), ]

### We consider the six regions listed below
theRegions <- c( "Japan", "Italy", "Ontario", "Belgium", "Texas", "Peru" )
nCov_Merged <- nCov_Merged[ nCov_Merged$reporting_unit %in% theRegions,]


###########################
###
### Set the starting date 
### for each region
### 
###########################

startOfEpidemic1 <- c(
  "Japan" = format( nCov_Merged[nCov_Merged$reporting_unit == "Japan",][ 
    nCov_Merged[nCov_Merged$reporting_unit == "Japan",]$dead != 0, ]$time[1]-14, "%m/%d"),
  "Italy" = format( nCov_Merged[nCov_Merged$reporting_unit == "Italy",][ 
    nCov_Merged[nCov_Merged$reporting_unit == "Italy",]$dead != 0, ]$time[1]-14, "%m/%d"),
  "Ontario" = format( nCov_Merged[nCov_Merged$reporting_unit == "Ontario",][ 
    nCov_Merged[nCov_Merged$reporting_unit == "Ontario",]$dead != 0, ]$time[1]-14, "%m/%d"),
  "Belgium" = format( nCov_Merged[nCov_Merged$reporting_unit == "Belgium",][ 
    nCov_Merged[nCov_Merged$reporting_unit == "Belgium",]$dead != 0, ]$time[1]-14, "%m/%d"), 
  "Texas" = format( nCov_Merged[nCov_Merged$reporting_unit == "Texas",][ 
    nCov_Merged[nCov_Merged$reporting_unit == "Texas",]$dead != 0, ]$time[1]-14, "%m/%d"),
  "Peru" = format( nCov_Merged[nCov_Merged$reporting_unit == "Peru",][ 
    nCov_Merged[nCov_Merged$reporting_unit == "Peru",]$dead != 0, ]$time[1]-14, "%m/%d") )

startOfEpidemic <- as.Date(paste0( "2020/", startOfEpidemic1 ) )
names(startOfEpidemic) <- names(startOfEpidemic1)

# Remove rows before start of epidemic for location, via index
idx_Japan <- which(nCov_Merged$reporting_unit == "Japan" &
                     nCov_Merged$time < startOfEpidemic["Japan"])
idx_Italy <- which(nCov_Merged$reporting_unit == "Italy" &
                     nCov_Merged$time < startOfEpidemic["Italy"])
idx_Belgium <- which(nCov_Merged$reporting_unit == "Belgium" &
                     nCov_Merged$time < startOfEpidemic["Belgium"])
idx_Ontario <- which(nCov_Merged$reporting_unit == "Ontario" &
                     nCov_Merged$time < startOfEpidemic["Ontario"])
idx_Texas <- which(nCov_Merged$reporting_unit == "Texas" &
                     nCov_Merged$time < startOfEpidemic["Texas"])
idx_Peru <- which(nCov_Merged$reporting_unit == "Peru" &
                     nCov_Merged$time < startOfEpidemic["Peru"])

idx_theRegions <- c( idx_Japan, idx_Italy, idx_Belgium, 
                     idx_Ontario, idx_Texas, idx_Peru )
nCov_Merged <- nCov_Merged[-idx_theRegions, ]



###########################
###
### Set initial values 
### for Stan, by region
###
###########################

Japan_init <- list(
  "A" = unname( sapply( c( "2020/04/01", "2020/08/01", "2020/12/01",
           "2021/04/01", "2021/08/01", "2021/12/01", "2022/04/01",
           "2022/07/01" ), 
           function(xx) { as.numeric(as.Date(xx, origin = "1970-01-01") ) } ) ),
  "B" = rep(25,8), "C" = c( rep(1.5,8) ), "K" = c( rep(0,8) ),
  "eta" = 1e-12/22961.86, "inv_sqrt_phi" = 0.10, "betaDay" = rep(1,6) )


Italy_init <- list( 
  "A" = unname( sapply( c( "2020/04/01", "2020/08/01", "2020/12/01",
           "2021/04/01", "2021/08/01", "2021/12/01", "2022/04/01",
           "2022/07/01" ), 
           function(xx) { as.numeric(as.Date(xx, origin = "1970-01-01") ) } ) ),
  "B" = rep(25,8), "C" = c( rep(4,8) ), "K" = c( rep(0,8) ),
  "eta" = 2e-4/9218, "inv_sqrt_phi" = 0.2, "betaDay" = rep(1,6) )


Belgium_init <- list( 
  "A" = unname( sapply( c( "2020/04/01", "2020/08/01", "2020/12/01",
           "2021/08/01", "2021/08/01", "2021/12/01", "2022/03/01",
           "2022/07/01" ), 
           function(xx) { as.numeric(as.Date(xx, origin = "1970-01-01") ) } ) ),
  "B" = c( rep(15,8) ), "C" = c( 10,1,8,rep(4,5) ), "K" = c( 6,0,2,rep(0,5) ),
  "eta" = 1e-8/1424.437, "inv_sqrt_phi" = 0.30, "betaDay" = rep(1,6) )


Ontario_init <- list(  
  "A" = unname( sapply( c( "2020/03/31", "2020/07/01", "2020/10/01",
           "2020/12/01", "2021/02/01", "2021/04/01",
           "2021/06/01", "2021/09/01", "2021/12/01", 
           "2022/03/01" ), function(xx) { 
             as.numeric(as.Date(xx, origin = "1970-01-01") ) } ) ),
  "B" = rep(30, 8), "C" = rep(2,8), "K" = c( 3.0, rep(0,7) ), 
  "eta" = 1e-12/1555.529, "inv_sqrt_phi" = 0.20, "betaDay" = rep(1,6) )


Texas_init <- list( 
  "A" = unname( sapply( c( "2020/04/01", "2020/08/01", "2020/12/01",
           "2021/04/01", "2021/08/01", "2021/12/01", "2022/04/01",
           "2022/07/01" ), 
           function(xx) { as.numeric(as.Date(xx, origin = "1970-01-01") ) } ) ),
  "B" = rep(15,8), "C" = c( 5, rep(4,7) ), "K" = c( 2, rep(0,7) ),
  "eta_1" = 4e-4/2192.584, "eta_2" = 1e-4/2192.584, "inv_sqrt_phi" = 0.10, 
  "betaDay_1" = rep(1,6), "betaDay_2" = rep(1,6) )


Peru_init <- list( 
  "A" = unname( sapply( c( "2020/04/01", "2020/08/01", "2020/12/01",
           "2021/04/01", "2021/08/01", "2021/12/01", "2022/04/01",
           "2022/07/01" ), 
           function(xx) { as.numeric(as.Date(xx, origin = "1970-01-01") ) } ) ),
  "B" = rep(15,8), "C" = c( 6, rep(4,7) ), "K" = c( 4, rep(0,7) ),
  "eta_1" = 2e-4/1753.598, "eta_2" = 1e-4/1753.598, "inv_sqrt_phi" = 0.20, 
  "betaDay_1" = rep(1,6), "betaDay_2" = rep(1,6) )



###########################
###
### Set initial values 
### for Stan, by parameter
###
###########################

forStartingValues = list(
A = c(
  "Japan" = Japan_init$A[1],
  "Italy" = Italy_init$A[1],
  "Belgium" = Belgium_init$A[1],
  "Ontario" = Ontario_init$A[1],
  "Texas" = Texas_init$A[1],
  "Peru" = Peru_init$A[1] ), 
A2 = c(
  "Japan" = Japan_init$A[2],
  "Italy" = Italy_init$A[2],
  "Belgium" = Belgium_init$A[2],
  "Ontario" = Ontario_init$A[2],
  "Texas" = Texas_init$A[2],
  "Peru" = Peru_init$A[2] ),
A3 = c(
  "Japan" = Japan_init$A[3],
  "Italy" = Italy_init$A[3],
  "Belgium" = Belgium_init$A[3],
  "Ontario" = Ontario_init$A[3],
  "Texas" = Texas_init$A[3],
  "Peru" = Peru_init$A[3] ),
A4 = c(
  "Japan" = Japan_init$A[4],
  "Italy" = Italy_init$A[4],
  "Belgium" = Belgium_init$A[4],  
  "Ontario" = Ontario_init$A[4],
  "Texas" = Texas_init$A[4],
  "Peru" = Peru_init$A[4] ), 
A5 = c(
  "Japan" = Japan_init$A[5],
  "Italy" = Italy_init$A[5],
  "Belgium" = Belgium_init$A[5],
  "Ontario" = Ontario_init$A[5],
  "Texas" = Texas_init$A[5],
  "Peru" = Peru_init$A[5] ),
A6 = c(
  "Japan" = Japan_init$A[6],
  "Italy" = Italy_init$A[6],
  "Belgium" = Belgium_init$A[6],
  "Ontario" = Ontario_init$A[6],
  "Texas" = Texas_init$A[6],
  "Peru" = Peru_init$A[6] ), 
A7 = c(
  "Japan" = Japan_init$A[7],
  "Italy" = Italy_init$A[7],
  "Belgium" = Belgium_init$A[7],  
  "Ontario" = Ontario_init$A[7],
  "Texas" = Texas_init$A[7],
  "Peru" = Peru_init$A[7] ), 
A8 = c(
  "Belgium" = Belgium_init$A[8],
  "Italy" = Italy_init$A[8],
  "Japan" = Japan_init$A[8],
  "Texas" = Texas_init$A[8],
  "Ontario" = Ontario_init$A[8],
  "Peru" = Peru_init$A[8] ), 
B = c(
  "Japan" = Japan_init$B[1],
  "Italy" = Italy_init$B[1],
  "Belgium" = Belgium_init$B[1],
  "Ontario" = Ontario_init$B[1],
  "Texas" = Texas_init$B[1],
  "Peru" = Peru_init$B[1] ), 
B2 = c(
  "Japan" = Japan_init$B[2],
  "Italy" = Italy_init$B[2],
  "Belgium" = Belgium_init$B[2],  
  "Ontario" = Ontario_init$B[2],
  "Texas" = Texas_init$B[2],
  "Peru" = Peru_init$B[2] ), 
B3 = c(
  "Japan" = Japan_init$B[3],
  "Italy" = Italy_init$B[3],
  "Belgium" = Belgium_init$B[3],
  "Ontario" = Ontario_init$B[3],
  "Texas" = Texas_init$B[3],
  "Peru" = Peru_init$B[3] ), 
B4 = c(
  "Japan" = Japan_init$B[4],
  "Italy" = Italy_init$B[4],
  "Belgium" = Belgium_init$B[4],
  "Ontario" = Ontario_init$B[4],
  "Texas" = Texas_init$B[4],
  "Peru" = Peru_init$B[4] ), 
B5 = c(
  "Japan" = Japan_init$B[5],
  "Italy" = Italy_init$B[5],
  "Ontario" = Ontario_init$B[5],
  "Belgium" = Belgium_init$B[5],
  "Texas" = Texas_init$B[5],
  "Peru" = Peru_init$B[5] ), 
B6 = c(
  "Japan" = Japan_init$B[6],
  "Italy" = Italy_init$B[6],
  "Belgium" = Belgium_init$B[6],
  "Ontario" = Ontario_init$B[6],
  "Texas" = Texas_init$B[6],
  "Peru" = Peru_init$B[6] ), 
B7 = c(
  "Japan" = Japan_init$B[7],
  "Italy" = Italy_init$B[7],
  "Belgium" = Belgium_init$B[7],
  "Ontario" = Ontario_init$B[7],
  "Texas" = Texas_init$B[7],
  "Peru" = Peru_init$B[7] ), 
B8 = c(
  "Japan" = Japan_init$B[8],
  "Italy" = Italy_init$B[8],
  "Belgium" = Belgium_init$B[8],
  "Ontario" = Ontario_init$B[8],
  "Texas" = Texas_init$B[8],
  "Peru" = Peru_init$B[8] ), 
C = c(
  "Japan" = Japan_init$C[1],
  "Italy" = Italy_init$C[1],
  "Texas" = Texas_init$C[1],
  "Belgium" = Belgium_init$C[1],
  "Ontario" = Ontario_init$C[1],
  "Peru" = Peru_init$C[1] ), 
C2 = c(
  "Japan" = Japan_init$C[2],
  "Italy" = Italy_init$C[2],
  "Belgium" = Belgium_init$C[2],
  "Ontario" = Ontario_init$C[2],
  "Texas" = Texas_init$C[2],
  "Peru" = Peru_init$C[2] ), 
C3 = c(
  "Japan" = Japan_init$C[3],
  "Italy" = Italy_init$C[3],
  "Belgium" = Belgium_init$C[3],
  "Ontario" = Ontario_init$C[3],
  "Texas" = Texas_init$C[3],
  "Peru" = Peru_init$C[3] ), 
C4 = c(
  "Japan" = Japan_init$C[4],
  "Italy" = Italy_init$C[4],
  "Belgium" = Belgium_init$C[4],
  "Ontario" = Ontario_init$C[4],
  "Texas" = Texas_init$C[4],
  "Peru" = Peru_init$C[4] ), 
C5 = c(
  "Japan" = Japan_init$C[5],
  "Italy" = Italy_init$C[5],
  "Belgium" = Belgium_init$C[5],
  "Ontario" = Ontario_init$C[5],
  "Texas" = Texas_init$C[5],
  "Peru" = Peru_init$C[5] ),
C6 = c(
  "Japan" = Japan_init$C[6],
  "Italy" = Italy_init$C[6],
  "Belgium" = Belgium_init$C[6],
  "Ontario" = Ontario_init$C[6],
  "Texas" = Texas_init$C[6],
  "Peru" = Peru_init$C[6] ), 
C7 = c(
  "Japan" = Japan_init$C[7],
  "Italy" = Italy_init$C[7],
  "Belgium" = Belgium_init$C[7],
  "Ontario" = Ontario_init$C[7],
  "Texas" = Texas_init$C[7],
  "Peru" = Peru_init$C[7] ), 
C8 = c(
  "Japan" = Japan_init$C[8],
  "Italy" = Italy_init$C[8],
  "Belgium" = Belgium_init$C[8],
  "Ontario" = Ontario_init$C[8],
  "Texas" = Texas_init$C[8],
  "Peru" = Peru_init$C[8] ), 
K = c(
  "Japan" = Japan_init$K[1],
  "Italy" = Italy_init$K[1],
  "Belgium" = Belgium_init$K[1],
  "Ontario" = Ontario_init$K[1],
  "Texas" = Texas_init$K[1],
  "Peru" = Peru_init$K[1] ), 
K2 = c(
  "Japan" = Japan_init$K[2],
  "Italy" = Italy_init$K[2],
  "Belgium" = Belgium_init$K[2],
  "Ontario" = Ontario_init$K[2],
  "Texas" = Texas_init$K[2],
  "Peru" = Peru_init$K[2] ), 
K3 = c(
  "Japan" = Japan_init$K[3],
  "Italy" = Italy_init$K[3],
  "Belgium" = Belgium_init$K[3],
  "Ontario" = Ontario_init$K[3],
  "Texas" = Texas_init$K[3],
  "Peru" = Peru_init$K[3] ), 
K4 = c(
  "Japan" = Japan_init$K[4],
  "Italy" = Italy_init$K[4],
  "Belgium" = Belgium_init$K[4],
  "Ontario" = Ontario_init$K[4],
  "Texas" = Texas_init$K[4],
  "Peru" = Peru_init$K[4] ), 
K5 = c(
  "Japan" = Japan_init$K[5],
  "Italy" = Italy_init$K[5],
  "Belgium" = Belgium_init$K[5],
  "Ontario" = Ontario_init$K[5],
  "Texas" = Texas_init$K[5],
  "Peru" = Peru_init$K[5] ), 
K6 = c(
  "Japan" = Japan_init$K[6],
  "Italy" = Italy_init$K[6],
  "Belgium" = Belgium_init$K[6],
  "Ontario" = Ontario_init$K[6],
  "Texas" = Texas_init$K[6],
  "Peru" = Peru_init$K[6] ), 
K7 = c(
  "Japan" = Japan_init$K[7],
  "Italy" = Italy_init$K[7],
  "Belgium" = Belgium_init$K[7],
  "Ontario" = Ontario_init$K[7],
  "Texas" = Texas_init$K[7],
  "Peru" = Peru_init$K[7] ), 
K8 = c(
  "Japan" = Japan_init$K[8],
  "Italy" = Italy_init$K[8],
  "Belgium" = Belgium_init$K[8],
  "Ontario" = Ontario_init$K[8],
  "Texas" = Texas_init$K[8],
  "Peru" = Peru_init$K[8] ), 
eta = c(
  "Japan" = Japan_init$eta,
  "Italy" = Italy_init$eta,
  "Belgium" = Belgium_init$eta,
  "Ontario" = Ontario_init$eta ),
eta_1 = c(
  "Texas" = Texas_init$eta_1,
  "Peru" = Peru_init$eta_1 ),
eta_2 = c( 
  "Texas" = Texas_init$eta_2,
  "Peru" = Peru_init$eta_2 ),
inv_sqrt_phi = c(
  "Japan" = Japan_init$inv_sqrt_phi,  
  "Italy" = Italy_init$inv_sqrt_phi,
  "Belgium" = Belgium_init$inv_sqrt_phi,
  "Ontario" = Ontario_init$inv_sqrt_phi,
  "Texas" = Texas_init$inv_sqrt_phi,
  "Peru" = Peru_init$inv_sqrt_phi )
)



###########################
###
### Construct data set with 
### mortality observations 
### up to May 31, 2022
###
###########################

nCov_Merged <- nCov_Merged[nCov_Merged[["time"]] <= "2022-05-31", ]
nCov_Merged$timeNumeric <- as.numeric(nCov_Merged$time)
nCov_Merged$logExpected <- log(nCov_Merged$Expected)

nCov_Merged <- nCov_Merged[!is.na(nCov_Merged$dead), ]
nCov_Merged$dead = pmax(0, nCov_Merged$dead)

### Day-of-the-week effects
nCov_Merged$dow <- factor( nCov_Merged$dow, c("Mon",'Tue','Wed','Thu','Fri','Sat','Sun') )
nCov_Merged$dowInt = as.integer(nCov_Merged$dow)

nCov_MergedList = split(nCov_Merged[, 
    c('timeNumeric','dowInt','Expected','dead')], 
  nCov_Merged$reporting_unit)

### Expected deaths
nCov_MergedSub = lapply(nCov_MergedList[theRegions], as.list)
nCov_MergedSub = lapply(nCov_MergedSub, function(xx) c(xx[setdiff(names(xx), 'Expected')], 
  list(Nwaves = 8, N = length(xx$dead), Expected = xx$Expected[[1]])))

### Format data for input into Stan
forStartingValues2 = as.data.frame( lapply(forStartingValues, function(xx) xx[theRegions]) )
forStartingValues2$reporting_unit = rownames(forStartingValues2)
forStartingValues3 = reshape2::melt(forStartingValues2, id.vars='reporting_unit')
forStartingValues3$par = gsub("[[:digit:]]+$", "", forStartingValues3$variable)
forStartingValues3$wave = gsub("^([[:alpha:]]|_)+", "", forStartingValues3$variable)
forStartingValues3$wave = gsub("^$", "1", forStartingValues3$wave)

svDf = reshape2::dcast(forStartingValues3, reporting_unit + wave ~ par, value.var = 'value')
svList = lapply(split(svDf, svDf$reporting_unit), as.list)
svList = lapply(svList, function(xx)  lapply(xx[c('A','B','C','K')], na.omit))


### Initial values for inv_sqrt_phi 
svList$Japan$inv_sqrt_phi = Japan_init$inv_sqrt_phi
svList$Italy$inv_sqrt_phi = Italy_init$inv_sqrt_phi
svList$Belgium$inv_sqrt_phi = Belgium_init$inv_sqrt_phi
svList$Ontario$inv_sqrt_phi = Ontario_init$inv_sqrt_phi
svList$Texas$inv_sqrt_phi = Texas_init$inv_sqrt_phi
svList$Peru$inv_sqrt_phi = Peru_init$inv_sqrt_phi

### Initial values for beta 
svList$Japan$betaDay = Japan_init$betaDay
svList$Italy$betaDay = Italy_init$betaDay
svList$Belgium$betaDay = Belgium_init$betaDay
svList$Ontario$betaDay = Ontario_init$betaDay
# Texas and Peru have two periods
svList$Texas$betaDay_1 = Texas_init$betaDay
svList$Texas$betaDay_2 = Texas_init$betaDay
svList$Peru$betaDay_1 = Peru_init$betaDay
svList$Peru$betaDay_2 = Peru_init$betaDay

### Initial values for eta
svList$Japan$eta = Japan_init$eta
svList$Italy$eta = Italy_init$eta
svList$Belgium$eta = Belgium_init$eta
svList$Ontario$eta = Ontario_init$eta
# Texas and Peru have two periods
svList$Texas$eta_1 = Texas_init$eta_1
svList$Texas$eta_2 = Texas_init$eta_2
svList$Peru$eta_1 = Peru_init$eta_1
svList$Peru$eta_2 = Peru_init$eta_2



###########################
###
### Run the 8 wave DOW 
### model in Stan
###
###########################

### Convert Stan model to C++ code
nCov_code_3 <- rstan::stanc(file = "Models/Wave_8.stan") 
nCov_code_4 <- rstan::stanc(file = "Models/Wave_8_Peru.stan") 
nCov_code_5 <- rstan::stanc(file = "Models/Wave_8_Texas.stan") 

### Compile C++ code
nCov_model_3 <- rstan::stan_model(stanc_ret = nCov_code_3)
nCov_model_4 <- rstan::stan_model(stanc_ret = nCov_code_4)
nCov_model_5 <- rstan::stan_model(stanc_ret = nCov_code_5)


### Set values for Stan call
chains = 2
thin = 1
warmup = 6000 
iter = 6000 + 6000 
adapt_delta = 0.9999
max_treedepth = 24


### Models used by each region 
object_list = list( "Japan" = nCov_model_3,
                    "Italy" = nCov_model_3, 
                    "Ontario" = nCov_model_3, 
                    "Belgium" = nCov_model_3,
                    "Texas" = nCov_model_5,
                    "Peru" = nCov_model_4 )

### Create cluster, run Stan models
theCores = 4
theCluster = parallel::makeCluster( spec = theCores, type = 'PSOCK', 
                                    methods = TRUE, outfile = "" )
parallel::setDefaultCluster(theCluster)

parallel::clusterEvalQ(theCluster, library('rstan'))
stanRaw <- parallel::clusterMap(
  theCluster, 
  rstan::sampling, 
  data = nCov_MergedSub[theRegions],
  init = lapply(svList, function(xx) list(xx)[rep(1,chains)])[theRegions],
  object = object_list[theRegions],
  MoreArgs = list(
    iter = iter, warmup = warmup,
    chains = chains, cores = chains, thin = thin,
    open_progress = TRUE,
    control = list(
      adapt_delta = adapt_delta,
      max_treedepth = max_treedepth
    )
  ),
  .scheduling = 'dynamic'
)

if ( !(interactive()) ) { parallel::stopCluster(theCluster) }
rm(theCluster) 

### Save results to the file below
nCov_fit <- lapply(stanRaw, rstan::extract)
nCov_fit$data <- nCov_Merged
nCov_fit$chains <- chains

saveRDS( nCov_fit, "8_Waves.RDS" )
rm(nCov_fit)