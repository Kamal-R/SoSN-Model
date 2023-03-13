
###########################
###
### Import data 
###
###########################

nCov_Merged <- readRDS( "Data_File.RDS")
nCov_Merged = nCov_Merged[!is.na(nCov_Merged$dead), ]

### We consider the regions listed below
theRegions <- c( "Italy", "Ontario", "Japan", "Belgium", "Peru", "Texas" ) 
nCov_Merged <- nCov_Merged[nCov_Merged$reporting_unit %in% theRegions,]



###########################
###
### Set the starting date 
### for each region
### 
###########################

startOfEpidemic1 <- c(
  "Italy" = format( nCov_Merged[nCov_Merged$reporting_unit == "Italy",][ 
    nCov_Merged[nCov_Merged$reporting_unit == "Italy",]$dead != 0, ]$time[1]-14, "%m/%d"),
  "Ontario" = format( nCov_Merged[nCov_Merged$reporting_unit == "Ontario",][ 
    nCov_Merged[nCov_Merged$reporting_unit == "Ontario",]$dead != 0, ]$time[1]-14, "%m/%d"),
  "Japan" = format( nCov_Merged[nCov_Merged$reporting_unit == "Japan",][ 
    nCov_Merged[nCov_Merged$reporting_unit == "Japan",]$dead != 0, ]$time[1]-14, "%m/%d"),
  "Belgium" = format( nCov_Merged[nCov_Merged$reporting_unit == "Belgium",][ 
    nCov_Merged[nCov_Merged$reporting_unit == "Belgium",]$dead != 0, ]$time[1]-14, "%m/%d"),
  "Peru" = format( nCov_Merged[nCov_Merged$reporting_unit == "Peru",][ 
    nCov_Merged[nCov_Merged$reporting_unit == "Peru",]$dead != 0, ]$time[1]-14, "%m/%d"), 
  "Texas" = format( nCov_Merged[nCov_Merged$reporting_unit == "Texas",][ 
    nCov_Merged[nCov_Merged$reporting_unit == "Texas",]$dead != 0, ]$time[1]-14, "%m/%d") )

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

Italy_init <- list( 
  "A" = unname( sapply( c( "2020/04/01", "2020/08/01", "2020/12/01" ), 
           function(xx) { as.numeric(as.Date(xx, origin = "1970-01-01") ) } ) ),
  "B" = rep(25,3), "C" = c( rep(4,3) ), "K" = c( rep(0,3) ),
  "eta" = 7e-4 / 9218, "inv_sqrt_phi" = 0.20, "betaDay" = rep(1,6) )

Japan_init <- list(
  "A" = unname( sapply( c( "2020/04/01", "2020/09/01", "2021/01/01" ), 
           function(xx) { as.numeric(as.Date(xx, origin = "1970-01-01") ) } ) ),
  "B" = rep(25,25,40), "C" = c( rep(1,2), 4 ), "K" = c( rep(0,3) ),
  "eta" = 7e-3 / 22961.86, "inv_sqrt_phi" = 0.10, "betaDay" = rep(1,6) )


Ontario_init <- list(  
  "A" = unname( sapply( c( "2020/04/01", "2020/12/01", "2021/03/01" ), 
          function(xx) { as.numeric(as.Date(xx, origin = "1970-01-01") ) } ) ),
  "B" = c( 30,20,30 ), "C" = c( 4.0, 4.0, 6.0 ), "K" = c( 0,0,0 ), 
  "eta" = 7e-2 / 1555.529, "inv_sqrt_phi" = 0.10, "betaDay" = rep(1,6) )


Belgium_init <- list( 
  "A" = unname( sapply( c( "2020/04/01", "2020/09/01", "2020/12/01" ), 
           function(xx) { as.numeric(as.Date(xx, origin = "1970-01-01") ) } ) ),
  "B" = c(20,40,40), "C" = c( 6,2,6 ), "K" = c( 4,0,1 ),
  "eta" = 7e-6 / 1424.437, "inv_sqrt_phi" = 0.10, "betaDay" = rep(1,6) )


Texas_init <- list(  
  "A" = unname( sapply( c( "2020/05/01", "2020/09/01", "2021/06/01" ), 
          function(xx) { as.numeric(as.Date(xx, origin = "1970-01-01") ) } ) ),
  "B" = c( 20,20,20 ), "C" = c( 2,6,8 ), "K" = c( 4,4,0 ),
  "eta" = 1e-1 / 2192.584, "inv_sqrt_phi" = 0.10, "betaDay" = rep(1,6) )


Peru_init <- list(
  "A" = unname( sapply( c( "2020/04/01", "2020/08/01", "2021/01/01" ), 
          function(xx) { as.numeric(as.Date(xx, origin = "1970-01-01") ) } ) ),
  "B" = c( 20,20,40 ), "C" = c( rep(4,3) ), "K" = c( 0,0,1 ),
  "eta" = 7e-5 / 1753.598, "inv_sqrt_phi" = 0.20, "betaDay" = rep(1,6) )


###########################
###
### Set initial values 
### for Stan, by parameter
###
###########################

forStartingValues = list(
A = c(
  "Belgium" = Belgium_init$A[1],
  "Italy" = Italy_init$A[1],
  "Japan" = Japan_init$A[1],
  "Texas" = Texas_init$A[1],
  "Ontario" = Ontario_init$A[1],
  "Peru" = Peru_init$A[1] ), 
A2 = c(
  "Belgium" = Belgium_init$A[2],
  "Italy" = Italy_init$A[2],
  "Japan" = Japan_init$A[2],
  "Texas" = Texas_init$A[2],
  "Ontario" = Ontario_init$A[2],
  "Peru" = Peru_init$A[2] ), 
A3 = c(
  "Belgium" = Belgium_init$A[3],
  "Italy" = Italy_init$A[3],
  "Japan" = Japan_init$A[3],
  "Texas" = Texas_init$A[3],
  "Ontario" = Ontario_init$A[3],
  "Peru" = Peru_init$A[3] ), 
B = c(
  "Belgium" = Belgium_init$B[1],
  "Italy" = Italy_init$B[1],
  "Japan" = Japan_init$B[1],
  "Texas" = Texas_init$B[1],
  "Ontario" = Ontario_init$B[1],
  "Peru" = Peru_init$B[1] ), 
B2 = c(
  "Belgium" = Belgium_init$B[2],
  "Italy" = Italy_init$B[2],
  "Japan" = Japan_init$B[2],
  "Texas" = Texas_init$B[2],
  "Ontario" = Ontario_init$B[2],
  "Peru" = Peru_init$B[2] ), 
B3 = c(
  "Belgium" = Belgium_init$B[3],
  "Italy" = Italy_init$B[3],
  "Japan" = Japan_init$B[3],
  "Texas" = Texas_init$B[3],
  "Ontario" = Ontario_init$B[3],
  "Peru" = Peru_init$B[3] ),  
C = c(
  "Belgium" = Belgium_init$C[1],
  "Italy" = Italy_init$C[1],
  "Japan" = Japan_init$C[1],
  "Texas" = Texas_init$C[1],
  "Ontario" = Ontario_init$C[1],
  "Peru" = Peru_init$C[1] ), 
C2 = c(
  "Belgium" = Belgium_init$C[2],
  "Italy" = Italy_init$C[2],
  "Japan" = Japan_init$C[2],
  "Texas" = Texas_init$C[2],
  "Ontario" = Ontario_init$C[2],
  "Peru" = Peru_init$C[2] ), 
C3 = c(
  "Belgium" = Belgium_init$C[3],
  "Italy" = Italy_init$C[3],
  "Japan" = Japan_init$C[3],
  "Texas" = Texas_init$C[3],
  "Ontario" = Ontario_init$C[3],
  "Peru" = Peru_init$C[3] ), 
K = c(
  "Belgium" = Belgium_init$K[1],
  "Italy" = Italy_init$K[1],
  "Japan" = Japan_init$K[1],
  "Texas" = Texas_init$K[1],
  "Ontario" = Ontario_init$K[1],
  "Peru" = Peru_init$K[1] ), 
K2 = c(
  "Belgium" = Belgium_init$K[2],
  "Italy" = Italy_init$K[2],
  "Japan" = Japan_init$K[2],
  "Texas" = Texas_init$K[2],
  "Ontario" = Ontario_init$K[2],
  "Peru" = Peru_init$K[2] ), 
K3 = c(
  "Belgium" = Belgium_init$K[3],
  "Italy" = Italy_init$K[3],
  "Japan" = Japan_init$K[3],
  "Texas" = Texas_init$K[3],
  "Ontario" = Ontario_init$K[3],
  "Peru" = Peru_init$K[3] ), 
eta = c(
  "Belgium" = Belgium_init$eta,
  "Italy" = Italy_init$eta,
  "Japan" = Japan_init$eta,
  "Texas" = Texas_init$eta,
  "Ontario" = Ontario_init$eta,
  "Peru" = Peru_init$eta ),
inv_sqrt_phi = c(
  "Belgium" = Belgium_init$inv_sqrt_phi,
  "Italy" = Italy_init$inv_sqrt_phi,
  "Japan" = Japan_init$inv_sqrt_phi,
  "Texas" = Texas_init$inv_sqrt_phi,
  "Ontario" = Ontario_init$inv_sqrt_phi,
  "Peru" = Peru_init$inv_sqrt_phi )
)


###########################
###
### Prepare data for Stan
### 
###########################

# ### Convert sequence of dates (for location parameter) to numeric values
# Swave = c('', sort(as.numeric(gsub("^A", "", grep("^A[[:digit:]]+$", 
#                     names(forStartingValues), value=TRUE)))))
# for(Dwave in seq(length(Swave), 2)) {
#   Ahere = paste0('A', Swave[Dwave])
#   Aprev = paste0('A', Swave[Dwave-1])
#   xx = as.numeric(as.Date(forStartingValues[[Ahere]], origin = "1970-01-01"))
#   names(xx) = names(forStartingValues[[Ahere]])
#   forStartingValues[[Ahere]] = xx
# }; theNames = names(forStartingValues$A)
# 
# ### Set initial values for the day-of-the-week effect (multiplicative)
# forStartingValues$betaDay = matrix(1.0, 7, length(theNames))
# colnames(forStartingValues$betaDay) = theNames



###########################
###
### Construct data set with 
### mortality observations 
### up to November 12, 2020
###
###########################

nCov_Merged <- nCov_Merged[nCov_Merged[["time"]] <= "2020-11-12", ]
if ( as.numeric( max( nCov_Merged$time ) ) > as.numeric( as.Date("2020-11-12") ) ) { stop("
  Dates being forecasted are (mistakenly!) included in the data input into Stan.") }

nCov_Merged$timeNumeric <- as.numeric(nCov_Merged$time)
nCov_Merged$logExpected <- log(nCov_Merged$Expected)
nCov_Merged$dead = pmax(0, nCov_Merged$dead)

### Day-of-the-week effects
nCov_Merged$dow <- factor( nCov_Merged$dow, c("Mon",'Tue','Wed','Thu','Fri','Sat','Sun') )
nCov_Merged$dowInt = as.integer(as.factor(nCov_Merged$dow))

nCov_MergedList = split(nCov_Merged[, 
    c('timeNumeric','dowInt','Expected','dead')], 
  nCov_Merged$reporting_unit)

### Expected deaths
nCov_MergedSub = lapply(nCov_MergedList[theRegions], as.list)
nCov_MergedSub = lapply(nCov_MergedSub, function(xx) c(xx[setdiff(names(xx), 'Expected')], 
  list(Nwaves = 3, N = length(xx$dead), Expected = xx$Expected[[1]])))

### Format data for input into Stan
forStartingValues2 = as.data.frame( lapply(forStartingValues, function(xx) xx[theRegions]) )
forStartingValues2$reporting_unit = rownames(forStartingValues2)
forStartingValues3 = reshape2::melt(forStartingValues2, id.vars='reporting_unit')
forStartingValues3$par = gsub("[[:digit:]]+$", "", forStartingValues3$variable)
forStartingValues3$wave = gsub("^([[:alpha:]]|_)+", "", forStartingValues3$variable)
forStartingValues3$wave = gsub("^$", "1", forStartingValues3$wave)

svDf = reshape2::dcast(forStartingValues3, reporting_unit + wave ~ par, value.var = 'value')
svList = lapply(split(svDf, svDf$reporting_unit), as.list)
svList = lapply(svList, function(xx)  lapply(xx[c('A','B','C','K','eta')], na.omit))
svList = lapply(svList, function(xx) {xx$betaDay = rep(1, 6);xx})

### Initial values for inv_sqrt_phi 
svList$Japan$inv_sqrt_phi = Japan_init$inv_sqrt_phi
svList$Italy$inv_sqrt_phi = Italy_init$inv_sqrt_phi
svList$Belgium$inv_sqrt_phi = Belgium_init$inv_sqrt_phi
svList$Ontario$inv_sqrt_phi = Ontario_init$inv_sqrt_phi
svList$Texas$inv_sqrt_phi = Texas_init$inv_sqrt_phi
svList$Peru$inv_sqrt_phi = Peru_init$inv_sqrt_phi




###########################
###
### Run the 3 wave DOW 
### model in Stan
###
###########################

# Convert to C++ code
nCov_code <- rstan::stanc(file = "Models/Wave_3.stan")
# Compile generated code
nCov_model <- rstan::stan_model(stanc_ret = nCov_code)

### Set values for Stan call
chains = 2
thin = 1
warmup = 6000
iter = 6000 + 6000
adapt_delta = 0.9999
max_treedepth = 24

theRegions = c( "Italy", "Ontario", "Japan", "Belgium", "Texas", "Peru" ) 

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
  MoreArgs = list(
    object = nCov_model,
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

if( !interactive() ) { parallel::stopCluster(theCluster) }
rm(theCluster)

### Save results to the file below
nCov_fit <- lapply(stanRaw, rstan::extract)
nCov_fit$data <- nCov_Merged
nCov_fit$chains <- chains

saveRDS(nCov_fit, "3_Waves_DOW.RDS")
rm(nCov_fit)



###########################
###
### Run the 3 wave no-DOW 
### model in Stan
###
###########################

# Convert to C++ code
nCov_code_2 <- rstan::stanc(file ="Models/Wave_3_No_DOW.stan") 
# Compile generated code
nCov_model_2 <- rstan::stan_model(stanc_ret = nCov_code_2) 

chains = 2
thin = 1
warmup = 6000
iter = 6000 + 6000
adapt_delta = 0.9999
max_treedepth = 24

theRegions = c( "Italy", "Ontario", "Japan", "Belgium", "Texas", "Peru" ) 

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
  MoreArgs = list(
    object = nCov_model_2,
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

if ( !interactive() ) { parallel::stopCluster(theCluster) }
rm(theCluster)

### Save results to the file below
nCov_fit <- lapply(stanRaw, rstan::extract)
nCov_fit$data <- nCov_Merged
nCov_fit$chains <- chains

saveRDS(nCov_fit, "3_Waves_No_DOW.RDS" )
rm(nCov_fit)