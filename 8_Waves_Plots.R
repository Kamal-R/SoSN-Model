
###########################
###
### Load libraries 
###
###########################
library("knitr")
library("ggplot2")
library("RColorBrewer")
library("tidyverse")
library("ggbrace")
library("colorspace")


###########################
###
### Load saved model and
### process for forecasts
###
###########################
nCov_fit <- readRDS( "8_Waves.RDS" )  

fDPM <- data.frame( "reporting_unit" = c( "Belgium", "Italy", "Japan", 
                                          "Ontario", "Peru", "Texas" ),
                    "mMult" = c( 0.08628413572977742, 0.01653936099980305,
                                 0.00790661003193527, 0.06866818400463920,
                                 0.03032890545467185, 0.03484096579854013 ) )
rownames(fDPM) <- fDPM$reporting_unit
Regions <- fDPM$reporting_unit


###########################
###
### Plot waves by region
###
###########################

for ( D2 in Regions ) {
  
  num_extra_obs <- c( nCov_fit$data[nCov_fit$data$reporting_unit == D2,]$timeNumeric - 
                        as.numeric( as.Date( "2020-01-28", origin = "1970-01-01" ) ) )[1] 
  
  forecasts_dates_1 <- seq( from = as.Date("2020-01-28", origin = "1970-01-01"), by = 1, 
                            to = as.Date("2020-01-28", origin = "1970-01-01") + num_extra_obs - 1 )
  
  if( as.numeric( nCov_fit$data[nCov_fit$data$reporting_unit == D2,]$time[1] - 
                  forecasts_dates_1[length(forecasts_dates_1)] ) != 1 ) { 
    stop( paste0( "The wrong dates are being added to the beginning of the graph for region ", D2) ) }
  
  forecast_dates <- as.Date( c( 
     "2022-06-01", "2022-06-02", "2022-06-03", "2022-06-04", "2022-06-05", "2022-06-06", 
     "2022-06-07", "2022-06-08", "2022-06-09", "2022-06-10", "2022-06-11", "2022-06-12", 
     "2022-06-13", "2022-06-14", "2022-06-15", "2022-06-16", "2022-06-17", "2022-06-18", 
     "2022-06-19", "2022-06-20", "2022-06-21", "2022-06-22", "2022-06-23", "2022-06-24", 
     "2022-06-25", "2022-06-26", "2022-06-27", "2022-06-28", "2022-06-29", "2022-06-30", 
     "2022-07-01", "2022-07-02" ) )
  
  extra_observed_times <- as.numeric( forecasts_dates_1, origin = "1970-01-01" )
  extra_observed_dead <- rep(0,num_extra_obs) 
  extra_observed_data_mean <- apply( nCov_fit[[D2]][["lambda_smooth_rep_1"]][,1:num_extra_obs], 2, mean )
  extra_observed_data_median <- apply( nCov_fit[[D2]][["lambda_smooth_rep_1"]][,1:num_extra_obs], 2, median ) 
  extra_observed_data_025 <- apply( nCov_fit[[D2]][["lambda_smooth_rep_1"]][,1:num_extra_obs], 2, quantile, probs = 0.025 ) 
  extra_observed_data_25 <- apply( nCov_fit[[D2]][["lambda_smooth_rep_1"]][,1:num_extra_obs], 2, quantile, probs = 0.25 )
  extra_observed_data_40 <- apply( nCov_fit[[D2]][["lambda_smooth_rep_1"]][,1:num_extra_obs], 2, quantile, probs = 0.40 )
  extra_observed_data_60 <- apply( nCov_fit[[D2]][["lambda_smooth_rep_1"]][,1:num_extra_obs], 2, quantile, probs = 0.60 )
  extra_observed_data_75 <- apply( nCov_fit[[D2]][["lambda_smooth_rep_1"]][,1:num_extra_obs], 2, quantile, probs = 0.75 )
  extra_observed_data_975 <- apply( nCov_fit[[D2]][["lambda_smooth_rep_1"]][,1:num_extra_obs], 2, quantile, probs = 0.975 )
  
  c( length( extra_observed_times ), length( extra_observed_dead ), 
     length( extra_observed_data_mean ), length( extra_observed_data_median ),
     length( extra_observed_data_025 ), length( extra_observed_data_975 ),
     length( extra_observed_data_25 ), length( extra_observed_data_75 ), 
     length( extra_observed_data_40 ), length( extra_observed_data_60 ) )
  
  observed_rows <- which( nCov_fit$data$reporting_unit == D2 )
  observed_data <- nCov_fit$data[observed_rows, ]
  observed_times <- nCov_fit$data[observed_rows, ]$timeNumeric
  observed_dead <- nCov_fit$data[observed_rows, ]$dead
  
  observed_data_mean <- apply( nCov_fit[[D2]][["lambda_smooth"]], 2, mean )
  observed_data_median <- apply( nCov_fit[[D2]][["lambda_smooth"]], 2, median ) 
  observed_data_025 <- apply( nCov_fit[[D2]][["lambda_smooth"]], 2, quantile, probs = 0.025 ) 
  observed_data_25 <- apply( nCov_fit[[D2]][["lambda_smooth"]], 2, quantile, probs = 0.25 )
  observed_data_40 <- apply( nCov_fit[[D2]][["lambda_smooth"]], 2, quantile, probs = 0.40 )
  observed_data_60 <- apply( nCov_fit[[D2]][["lambda_smooth"]], 2, quantile, probs = 0.60 )
  observed_data_75 <- apply( nCov_fit[[D2]][["lambda_smooth"]], 2, quantile, probs = 0.75 )
  observed_data_975 <- apply( nCov_fit[[D2]][["lambda_smooth"]], 2, quantile, probs = 0.975 )
  
  c( length( observed_times ), length( observed_dead ), 
     length( observed_data_mean ), length( observed_data_median ),
     length( observed_data_025 ), length( observed_data_975 ),
     length( observed_data_25 ), length( observed_data_75 ), 
     length( observed_data_40 ), length( observed_data_60 ) )
  
  forecasts_times <- as.numeric( forecast_dates, origin = "1970-01-01" )
  forecasts_dead <- apply( nCov_fit[[D2]]$dead_rep_2, 2, median ) 
  forecasts_mean <- apply( nCov_fit[[D2]][["lambda_smooth_rep_2"]], 2, mean )
  forecasts_median <- apply( nCov_fit[[D2]][["lambda_smooth_rep_2"]], 2, median )
  forecasts_025 <- apply( nCov_fit[[D2]][["lambda_smooth_rep_2"]], 2, quantile, probs = 0.025 ) 
  forecasts_25 <- apply( nCov_fit[[D2]][["lambda_smooth_rep_2"]], 2, quantile, probs = 0.25 ) 
  forecasts_40 <- apply( nCov_fit[[D2]][["lambda_smooth_rep_2"]], 2, quantile, probs = 0.40 ) 
  forecasts_60 <- apply( nCov_fit[[D2]][["lambda_smooth_rep_2"]], 2, quantile, probs = 0.60 ) 
  forecasts_75 <- apply( nCov_fit[[D2]][["lambda_smooth_rep_2"]], 2, quantile, probs = 0.75 ) 
  forecasts_975 <- apply( nCov_fit[[D2]][["lambda_smooth_rep_2"]], 2, quantile, probs = 0.975 ) 
  
  c( length( forecasts_times ), length( forecasts_dead ),
     length( forecasts_mean ), length( forecasts_median ),
     length( forecasts_025 ), length( forecasts_975 ),
     length( forecasts_25 ), length( forecasts_75 ), 
     length( forecasts_40 ), length( forecasts_60 ) )
  
  
  first_wave <- cbind( nCov_fit[[D2]][["waves_rep_1"]][,1:num_extra_obs,1],
                       nCov_fit[[D2]][["waves"]][,,1], 
                       nCov_fit[[D2]][["waves_rep_2"]][,,1] )
  second_wave <- cbind( nCov_fit[[D2]][["waves_rep_1"]][,1:num_extra_obs,2], 
                        nCov_fit[[D2]][["waves"]][,,2], 
                        nCov_fit[[D2]][["waves_rep_2"]][,,2] )
  third_wave <- cbind( nCov_fit[[D2]][["waves_rep_1"]][,1:num_extra_obs,3], 
                       nCov_fit[[D2]][["waves"]][,,3], 
                       nCov_fit[[D2]][["waves_rep_2"]][,,3] )
  fourth_wave <- cbind( nCov_fit[[D2]][["waves_rep_1"]][,1:num_extra_obs,4], 
                        nCov_fit[[D2]][["waves"]][,,4], 
                        nCov_fit[[D2]][["waves_rep_2"]][,,4] )
  fifth_wave <- cbind( nCov_fit[[D2]][["waves_rep_1"]][,1:num_extra_obs,5], 
                       nCov_fit[[D2]][["waves"]][,,5], 
                       nCov_fit[[D2]][["waves_rep_2"]][,,5] )
  sixth_wave <- cbind( nCov_fit[[D2]][["waves_rep_1"]][,1:num_extra_obs,6], 
                       nCov_fit[[D2]][["waves"]][,,6], 
                       nCov_fit[[D2]][["waves_rep_2"]][,,6] )
  seventh_wave <- cbind( nCov_fit[[D2]][["waves_rep_1"]][,1:num_extra_obs,7], 
                         nCov_fit[[D2]][["waves"]][,,7], 
                         nCov_fit[[D2]][["waves_rep_2"]][,,7] )
  eighth_wave <- cbind( nCov_fit[[D2]][["waves_rep_1"]][,1:num_extra_obs,8], 
                       nCov_fit[[D2]][["waves"]][,,8], 
                       nCov_fit[[D2]][["waves_rep_2"]][,,8] )

  
  points_color <- c( rep( " ", num_extra_obs ),
                     rep( "Observed Mortality", length(observed_times) ), 
                     rep( "  ", length(forecasts_times) ) )
  
  alpha_curves_95_CI <- 0.20
  alpha_curves_50_CI <- 0.30
  alpha_curves_20_CI <- 0.40
  alpha_intensity_95_CI <- 0.30
  alpha_intensity_50_CI <- 0.50
  alpha_intensity_20_CI <- 0.70

  
  intensity_tibble <- tibble::tibble( 
  
    time_interval = c( extra_observed_times, observed_times, forecasts_times ), 
    mortality_points = c( extra_observed_dead, observed_dead, forecasts_dead ) * fDPM[D2,'mMult'],
    mortality_color = points_color,
    
    intensity_mean = c( extra_observed_data_mean, observed_data_mean, forecasts_mean ) * fDPM[D2,'mMult'],
    intensity_median = c( extra_observed_data_median, observed_data_median, forecasts_median ) * fDPM[D2,'mMult'],
    
    intensity_Q025 = c( extra_observed_data_025, observed_data_025, forecasts_025 ) * fDPM[D2,'mMult'],
    intensity_Q25 = c( extra_observed_data_25, observed_data_25, forecasts_25 ) * fDPM[D2,'mMult'],
    intensity_Q40 = c( extra_observed_data_40, observed_data_40, forecasts_40 ) * fDPM[D2,'mMult'],
    intensity_Q60 = c( extra_observed_data_60, observed_data_60, forecasts_60 ) * fDPM[D2,'mMult'],
    intensity_Q75 = c( extra_observed_data_75, observed_data_75, forecasts_75 ) * fDPM[D2,'mMult'],
    intensity_Q975 = c( extra_observed_data_975, observed_data_975, forecasts_975 ) * fDPM[D2,'mMult'],
    
    first_wave_mean = apply( first_wave, 2, mean ) * fDPM[D2,'mMult'],
    first_wave_median = apply( first_wave, 2, median ) * fDPM[D2,'mMult'],
    first_wave_Q025 = apply( first_wave, 2, quantile, probs = 0.025 ) * fDPM[D2,'mMult'],
    first_wave_Q25 = apply( first_wave, 2, quantile, probs = 0.25 ) * fDPM[D2,'mMult'],
    first_wave_Q40 = apply( first_wave, 2, quantile, probs = 0.40 ) * fDPM[D2,'mMult'],
    first_wave_Q60 = apply( first_wave, 2, quantile, probs = 0.60 ) * fDPM[D2,'mMult'],
    first_wave_Q75 = apply( first_wave, 2, quantile, probs = 0.75 ) * fDPM[D2,'mMult'],
    first_wave_Q975 = apply( first_wave, 2, quantile, probs = 0.975 ) * fDPM[D2,'mMult'],
    
    second_wave_mean = apply( second_wave, 2, mean ) * fDPM[D2,'mMult'],
    second_wave_median = apply( second_wave, 2, median ) * fDPM[D2,'mMult'],
    second_wave_Q025 = apply( second_wave, 2, quantile, probs = 0.025 ) * fDPM[D2,'mMult'],
    second_wave_Q25 = apply( second_wave, 2, quantile, probs = 0.25 ) * fDPM[D2,'mMult'],
    second_wave_Q40 = apply( second_wave, 2, quantile, probs = 0.40 ) * fDPM[D2,'mMult'],
    second_wave_Q60 = apply( second_wave, 2, quantile, probs = 0.60 ) * fDPM[D2,'mMult'],
    second_wave_Q75 = apply( second_wave, 2, quantile, probs = 0.75 ) * fDPM[D2,'mMult'],
    second_wave_Q975 = apply( second_wave, 2, quantile, probs = 0.975 ) * fDPM[D2,'mMult'],
  
    third_wave_mean = apply( third_wave, 2, mean ) * fDPM[D2,'mMult'],
    third_wave_median = apply( third_wave, 2, median ) * fDPM[D2,'mMult'],
    third_wave_Q025 = apply( third_wave, 2, quantile, probs = 0.025 ) * fDPM[D2,'mMult'],
    third_wave_Q25 = apply( third_wave, 2, quantile, probs = 0.25 ) * fDPM[D2,'mMult'],
    third_wave_Q40 = apply( third_wave, 2, quantile, probs = 0.40 ) * fDPM[D2,'mMult'],
    third_wave_Q60 = apply( third_wave, 2, quantile, probs = 0.60 ) * fDPM[D2,'mMult'],
    third_wave_Q75 = apply( third_wave, 2, quantile, probs = 0.75 ) * fDPM[D2,'mMult'],
    third_wave_Q975 = apply( third_wave, 2, quantile, probs = 0.975 ) * fDPM[D2,'mMult'],
    
    fourth_wave_mean = apply( fourth_wave, 2, mean ) * fDPM[D2,'mMult'],
    fourth_wave_median = apply( fourth_wave, 2, median ) * fDPM[D2,'mMult'],
    fourth_wave_Q025 = apply( fourth_wave, 2, quantile, probs = 0.025 ) * fDPM[D2,'mMult'],
    fourth_wave_Q25 = apply( fourth_wave, 2, quantile, probs = 0.25 ) * fDPM[D2,'mMult'],
    fourth_wave_Q40 = apply( fourth_wave, 2, quantile, probs = 0.40 ) * fDPM[D2,'mMult'],
    fourth_wave_Q60 = apply( fourth_wave, 2, quantile, probs = 0.60 ) * fDPM[D2,'mMult'],
    fourth_wave_Q75 = apply( fourth_wave, 2, quantile, probs = 0.75 ) * fDPM[D2,'mMult'],
    fourth_wave_Q975 = apply( fourth_wave, 2, quantile, probs = 0.975 ) * fDPM[D2,'mMult'],

    fifth_wave_mean = apply( fifth_wave, 2, mean ) * fDPM[D2,'mMult'],
    fifth_wave_median = apply( fifth_wave, 2, median ) * fDPM[D2,'mMult'],
    fifth_wave_Q025 = apply( fifth_wave, 2, quantile, probs = 0.025 ) * fDPM[D2,'mMult'],
    fifth_wave_Q25 = apply( fifth_wave, 2, quantile, probs = 0.25 ) * fDPM[D2,'mMult'],
    fifth_wave_Q40 = apply( fifth_wave, 2, quantile, probs = 0.40 ) * fDPM[D2,'mMult'],
    fifth_wave_Q60 = apply( fifth_wave, 2, quantile, probs = 0.60 ) * fDPM[D2,'mMult'],
    fifth_wave_Q75 = apply( fifth_wave, 2, quantile, probs = 0.75 ) * fDPM[D2,'mMult'],
    fifth_wave_Q975 = apply( fifth_wave, 2, quantile, probs = 0.975 ) * fDPM[D2,'mMult'],
    
    sixth_wave_mean = apply( sixth_wave, 2, mean ) * fDPM[D2,'mMult'],
    sixth_wave_median = apply( sixth_wave, 2, median ) * fDPM[D2,'mMult'],
    sixth_wave_Q025 = apply( sixth_wave, 2, quantile, probs = 0.025 ) * fDPM[D2,'mMult'],
    sixth_wave_Q25 = apply( sixth_wave, 2, quantile, probs = 0.25 ) * fDPM[D2,'mMult'],
    sixth_wave_Q40 = apply( sixth_wave, 2, quantile, probs = 0.40 ) * fDPM[D2,'mMult'],
    sixth_wave_Q60 = apply( sixth_wave, 2, quantile, probs = 0.60 ) * fDPM[D2,'mMult'],
    sixth_wave_Q75 = apply( sixth_wave, 2, quantile, probs = 0.75 ) * fDPM[D2,'mMult'],
    sixth_wave_Q975 = apply( sixth_wave, 2, quantile, probs = 0.975 ) * fDPM[D2,'mMult'],
    
    seventh_wave_mean = apply( seventh_wave, 2, mean ) * fDPM[D2,'mMult'],
    seventh_wave_median = apply( seventh_wave, 2, median ) * fDPM[D2,'mMult'],
    seventh_wave_Q025 = apply( seventh_wave, 2, quantile, probs = 0.025 ) * fDPM[D2,'mMult'],
    seventh_wave_Q25 = apply( seventh_wave, 2, quantile, probs = 0.25 ) * fDPM[D2,'mMult'],
    seventh_wave_Q40 = apply( seventh_wave, 2, quantile, probs = 0.40 ) * fDPM[D2,'mMult'],
    seventh_wave_Q60 = apply( seventh_wave, 2, quantile, probs = 0.60 ) * fDPM[D2,'mMult'],
    seventh_wave_Q75 = apply( seventh_wave, 2, quantile, probs = 0.75 ) * fDPM[D2,'mMult'],
    seventh_wave_Q975 = apply( seventh_wave, 2, quantile, probs = 0.975 ) * fDPM[D2,'mMult'],

    eighth_wave_mean = apply( eighth_wave, 2, mean ) * fDPM[D2,'mMult'],
    eighth_wave_median = apply( eighth_wave, 2, median ) * fDPM[D2,'mMult'],
    eighth_wave_Q025 = apply( eighth_wave, 2, quantile, probs = 0.025 ) * fDPM[D2,'mMult'],
    eighth_wave_Q25 = apply( eighth_wave, 2, quantile, probs = 0.25 ) * fDPM[D2,'mMult'],
    eighth_wave_Q40 = apply( eighth_wave, 2, quantile, probs = 0.40 ) * fDPM[D2,'mMult'],
    eighth_wave_Q60 = apply( eighth_wave, 2, quantile, probs = 0.60 ) * fDPM[D2,'mMult'],
    eighth_wave_Q75 = apply( eighth_wave, 2, quantile, probs = 0.75 ) * fDPM[D2,'mMult'],
    eighth_wave_Q975 = apply( eighth_wave, 2, quantile, probs = 0.975 ) * fDPM[D2,'mMult'],
    
    fill_curves_95_CI = rep( colorspace::adjust_transparency("grey50", alpha_curves_95_CI), 
                             length(points_color) ),
    fill_curves_50_CI = rep( colorspace::adjust_transparency("grey50", alpha_curves_50_CI), 
                             length(points_color) ),
    fill_curves_20_CI = rep( colorspace::adjust_transparency("grey50", alpha_curves_20_CI), 
                             length(points_color) ),
      
    fill_intensity_95_CI = rep( colorspace::adjust_transparency("skyblue", alpha_intensity_95_CI), 
                                length(points_color) ),
    fill_intensity_50_CI = rep( colorspace::adjust_transparency("skyblue", alpha_intensity_50_CI), 
                                length(points_color) ),
    fill_intensity_20_CI = rep( colorspace::adjust_transparency("skyblue", alpha_intensity_20_CI), 
                                length(points_color) )
      
  )
  
  if ( sum( is.na( intensity_tibble ) ) != 0 ) {
    stop( paste0("Region ", D2, " has 'NA' values.") )
  }
  
  temp <- ggplot( data = intensity_tibble, aes( x = as.Date( time_interval, origin = "1970-01-01" ) ) ) + 
    
    geom_point( aes( y = mortality_points, color = mortality_color ), 
                alpha = 0.8, shape = 21, stroke = 1.15 ) +
  
    geom_ribbon( aes( ymin = first_wave_median, ymax = first_wave_Q975, 
                      fill = fill_curves_95_CI ) ) +
    geom_ribbon( aes( ymin = first_wave_Q025, ymax = first_wave_median, 
                      fill = fill_curves_95_CI ) ) +

    geom_ribbon( aes( ymin = first_wave_median, ymax = first_wave_Q75, 
                      fill = fill_curves_50_CI ) ) +
    geom_ribbon( aes( ymin = first_wave_Q25, ymax = first_wave_median, 
                      fill = fill_curves_50_CI ) ) +

    geom_ribbon( aes( ymin = first_wave_median, ymax = first_wave_Q60, 
                      fill = fill_curves_20_CI ) ) +
    geom_ribbon( aes( ymin = first_wave_Q40, ymax = first_wave_median, 
                      fill = fill_curves_20_CI ) ) +


    ### Second Wave
    geom_ribbon( aes( ymin = second_wave_median, ymax = second_wave_Q975, 
                      fill = fill_curves_95_CI ) ) +
    geom_ribbon( aes( ymin = second_wave_Q025, ymax = second_wave_median, 
                      fill = fill_curves_95_CI ) ) +

    geom_ribbon( aes( ymin = second_wave_median, ymax = second_wave_Q75, 
                      fill = fill_curves_50_CI ) ) +
    geom_ribbon( aes( ymin = second_wave_Q25, ymax = second_wave_median, 
                      fill = fill_curves_50_CI ) ) +

    geom_ribbon( aes( ymin = second_wave_median, ymax = second_wave_Q60, 
                      fill = fill_curves_20_CI ) ) +
    geom_ribbon( aes( ymin = second_wave_Q40, ymax = second_wave_median, 
                      fill = fill_curves_20_CI ) ) +


    ### Third Wave
    geom_ribbon( aes( ymin = third_wave_median, ymax = third_wave_Q975, 
                      fill = fill_curves_95_CI ) ) +
    geom_ribbon( aes( ymin = third_wave_Q025, ymax = third_wave_median, 
                      fill = fill_curves_95_CI ) ) +

    geom_ribbon( aes( ymin = third_wave_median, ymax = third_wave_Q75, 
                      fill = fill_curves_50_CI ) ) +
    geom_ribbon( aes( ymin = third_wave_Q25, ymax = third_wave_median, 
                      fill = fill_curves_50_CI ) ) +

    geom_ribbon( aes( ymin = third_wave_median, ymax = third_wave_Q60, 
                      fill = fill_curves_20_CI ) ) +
    geom_ribbon( aes( ymin = third_wave_Q40, ymax = third_wave_median, 
                      fill = fill_curves_20_CI ) ) +

    
    ### Fourth Wave
    geom_ribbon( aes( ymin = fourth_wave_median, ymax = fourth_wave_Q975, 
                      fill = fill_curves_95_CI ) ) +
    geom_ribbon( aes( ymin = fourth_wave_Q025, ymax = fourth_wave_median, 
                      fill = fill_curves_95_CI ) ) +

    geom_ribbon( aes( ymin = fourth_wave_median, ymax = fourth_wave_Q75, 
                      fill = fill_curves_50_CI ) ) +
    geom_ribbon( aes( ymin = fourth_wave_Q25, ymax = fourth_wave_median, 
                      fill = fill_curves_50_CI ) ) +

    geom_ribbon( aes( ymin = fourth_wave_median, ymax = fourth_wave_Q60, 
                      fill = fill_curves_20_CI ) ) +
    geom_ribbon( aes( ymin = fourth_wave_Q40, ymax = fourth_wave_median, 
                      fill = fill_curves_20_CI ) ) +

    
    ### Fifth Wave
    geom_ribbon( aes( ymin = fifth_wave_median, ymax = fifth_wave_Q975, 
                      fill = fill_curves_95_CI ) ) +
    geom_ribbon( aes( ymin = fifth_wave_Q025, ymax = fifth_wave_median, 
                      fill = fill_curves_95_CI ) ) +

    geom_ribbon( aes( ymin = fifth_wave_median, ymax = fifth_wave_Q75, 
                      fill = fill_curves_50_CI ) ) +
    geom_ribbon( aes( ymin = fifth_wave_Q25, ymax = fifth_wave_median, 
                      fill = fill_curves_50_CI ) ) +

    geom_ribbon( aes( ymin = fifth_wave_median, ymax = fifth_wave_Q60, 
                      fill = fill_curves_20_CI ) ) +
    geom_ribbon( aes( ymin = fifth_wave_Q40, ymax = fifth_wave_median, 
                      fill = fill_curves_20_CI ) ) +


    ### Sixth Wave
    geom_ribbon( aes( ymin = sixth_wave_median, ymax = sixth_wave_Q975, 
                      fill = fill_curves_95_CI ) ) +
    geom_ribbon( aes( ymin = sixth_wave_Q025, ymax = sixth_wave_median, 
                      fill = fill_curves_95_CI ) ) +

    geom_ribbon( aes( ymin = sixth_wave_median, ymax = sixth_wave_Q75, 
                      fill = fill_curves_50_CI ) ) +
    geom_ribbon( aes( ymin = sixth_wave_Q25, ymax = sixth_wave_median, 
                      fill = fill_curves_50_CI ) ) +

    geom_ribbon( aes( ymin = sixth_wave_median, ymax = sixth_wave_Q60, 
                      fill = fill_curves_20_CI ) ) +
    geom_ribbon( aes( ymin = sixth_wave_Q40, ymax = sixth_wave_median, 
                      fill = fill_curves_20_CI ) ) +
    

    ### Seventh Wave
    geom_ribbon( aes( ymin = seventh_wave_median, ymax = seventh_wave_Q975, 
                      fill = fill_curves_95_CI ) ) +
    geom_ribbon( aes( ymin = seventh_wave_Q025, ymax = seventh_wave_median, 
                      fill = fill_curves_95_CI ) ) +

    geom_ribbon( aes( ymin = seventh_wave_median, ymax = seventh_wave_Q75, 
                      fill = fill_curves_50_CI ) ) +
    geom_ribbon( aes( ymin = seventh_wave_Q25, ymax = seventh_wave_median, 
                      fill = fill_curves_50_CI ) ) +

    geom_ribbon( aes( ymin = seventh_wave_median, ymax = seventh_wave_Q60, 
                      fill = fill_curves_20_CI ) ) +
    geom_ribbon( aes( ymin = seventh_wave_Q40, ymax = seventh_wave_median, 
                      fill = fill_curves_20_CI ) ) +

    
    ### Eighth Wave
    geom_ribbon( aes( ymin = eighth_wave_median, ymax = eighth_wave_Q975, 
                      fill = fill_curves_95_CI ) ) +
    geom_ribbon( aes( ymin = eighth_wave_Q025, ymax = eighth_wave_median, 
                      fill = fill_curves_95_CI ) ) +

    geom_ribbon( aes( ymin = eighth_wave_median, ymax = eighth_wave_Q75, 
                      fill = fill_curves_50_CI ) ) +
    geom_ribbon( aes( ymin = eighth_wave_Q25, ymax = eighth_wave_median, 
                      fill = fill_curves_50_CI ) ) +

    geom_ribbon( aes( ymin = eighth_wave_median, ymax = eighth_wave_Q60, 
                      fill = fill_curves_20_CI ) ) +
    geom_ribbon( aes( ymin = eighth_wave_Q40, ymax = eighth_wave_median, 
                      fill = fill_curves_20_CI ) ) +
    
    ### Intensity
    geom_ribbon( aes( ymin = intensity_median, ymax = intensity_Q975, 
                      fill = fill_intensity_95_CI ) ) +
    geom_ribbon( aes( ymin = intensity_Q025, ymax = intensity_median, 
                      fill = fill_intensity_95_CI ) ) +

    geom_ribbon( aes( ymin = intensity_median, ymax = intensity_Q75, 
                      fill = fill_intensity_50_CI ) ) +
    geom_ribbon( aes( ymin = intensity_Q25, ymax = intensity_median, 
                      fill = fill_intensity_50_CI ) ) +

    geom_ribbon( aes( ymin = intensity_median, ymax = intensity_Q60, 
                      fill = fill_intensity_20_CI ) ) +
    geom_ribbon( aes( ymin = intensity_Q40, ymax = intensity_median, 
                      fill = fill_intensity_20_CI ) ) +

    geom_line( aes( y = intensity_median, color = "Intensity: Median" ), size = 1.5 ) +

    ### Waves: medians
    geom_line( aes(  y = first_wave_median, color = "Curve 1: Median" ), size = 1.25, alpha = 0.75 ) + 
    geom_line( aes(  y = second_wave_median, color = "Curve 2: Median" ), size = 1.25, alpha = 0.75 ) +
    geom_line( aes(  y = third_wave_median, color = "Curve 3: Median" ), size = 1.25, alpha = 0.75 ) +
    geom_line( aes(  y = fourth_wave_median, color = "Curve 4: Median" ), size = 1.25, alpha = 0.75 ) +
    geom_line( aes(  y = fifth_wave_median, color = "Curve 5: Median" ), size = 1.25, alpha = 0.75 ) +
    geom_line( aes(  y = sixth_wave_median, color = "Curve 6: Median" ), size = 1.25, alpha = 0.75 ) +
    geom_line( aes(  y = seventh_wave_median, color = "Curve 7: Median" ), size = 1.25, alpha = 0.75 ) +
    geom_line( aes(  y = eighth_wave_median, color = "Curve 8: Median" ), size = 1.25, alpha = 0.75 ) +

    ### Legend
    scale_color_manual( values = c( "Observed Mortality" = "grey50", " " = "white",
                                    "  " = "white",
                                    "Intensity: Median" = "skyblue2",
                                    "Curve 1: Median" = scales::hue_pal()(8)[1], 
                                    "Curve 2: Median" = scales::hue_pal()(8)[2], 
                                    "Curve 3: Median" = scales::hue_pal()(8)[3], 
                                    "Curve 4: Median" = scales::hue_pal()(8)[4], 
                                    "Curve 5: Median" = scales::hue_pal()(8)[5], 
                                    "Curve 6: Median" = scales::hue_pal()(8)[6], 
                                    "Curve 7: Median" = scales::hue_pal()(8)[7], 
                                    "Curve 8: Median" = scales::hue_pal()(8)[8] ),
                        guide = guide_legend( direction = "horizontal", 
                          override.aes = list( ncol = 2,
                            linetype = c(rep("blank", 3), rep("solid",9) ),
                            shape = c(rep(1, 3), rep(NA,9) ) ) ) ) +
    
    ### Legend labels
    scale_fill_identity( 
      labels = c( "Skew Normal Curves: 95%, 50%, and 20% Credible Intervals (Light to Dark)", 
                  "Intensity: 95%, 50%, and 20% Credible Intervals (Light to Dark)", rep("",4) ),
      guide = guide_legend( override.aes = list( fill = c("#7F7F7F", "#87CEEB", rep(NA,4) ) ),
                            byrow = FALSE, direction = "vertical", ncol = 3 ) ) +
  
    xlab("") + ylab( paste0("Daily Deaths per Million (",D2,")") ) + 
    
    ### Axes
    scale_x_date( breaks = as.Date( c("2020-03-01", "2020-09-01",
                                      "2021-03-01", "2021-09-01",
                                      "2022-03-01") ), 
                  date_labels =  "%b %Y",
                  limits = c( as.Date("2020-01-15", origin = "1970-01-01"), 
                              as.Date("2022-07-02", origin = "1970-01-01") ) ) + 
    coord_cartesian( ylim = c( 0, if_else( D2 == "Japan", 5, 35 ) ), 
                     clip = "on" ) +
    coord_cartesian( ylim = c( 0, 35 ), clip = "on" ) + 

    ### Theme 
    theme_classic() + 
    theme( axis.text = element_text(size = 16, color = "black"),
           axis.title = element_text(size = 18),
           axis.title.x = element_text(margin = margin(15, 0, 0, 0)),
           axis.title.y = element_text(margin = margin(0, 12.5, 0, 0)),
           plot.margin = unit(c(0.25, 0.11, 0.11, 0.11), units = "npc") ) + 
    theme( legend.position = "none" ) 

    ### Set y axes label and scale 
    temp <- temp + 
      scale_y_continuous(
        labels = scales::label_comma(), 
        trans = scales::pseudo_log_trans(),
        breaks = c( 1, 2, 5, 10, 25 ) ) 

  ### Add annotations to plot of Ontario
  if ( D2 == "Ontario" ) {
    
    temp <- temp + 
      
      geom_brace( aes( c( as.Date( "2020-02-26", origin = "1970-01-01" ), 
                          as.Date( "2020-08-31", origin = "1970-01-01" ) ),
                       c(50, 65), label = "Wave 1" ), 
                  inherit.data = FALSE, labelsize = 3 ) + 
      geom_brace( aes( c( as.Date( "2020-09-01", origin = "1970-01-01" ), 
                          as.Date( "2021-02-28", origin = "1970-01-01" ) ),
                       c(50, 65), label = "Wave 2" ), 
                  inherit.data = FALSE, labelsize = 3 ) + 
      geom_brace( aes( c( as.Date( "2021-03-01", origin = "1970-01-01" ), 
                          as.Date( "2021-07-31", origin = "1970-01-01" ) ),
                       c(50, 65), label = "Wave 3" ), 
                  inherit.data = FALSE, labelsize = 3 ) + 
      geom_brace( aes( c( as.Date( "2021-08-01", origin = "1970-01-01" ), 
                          as.Date( "2021-12-14", origin = "1970-01-01" ) ),
                       c(50, 65), label = "Wave 4" ), 
                  inherit.data = FALSE, labelsize = 3 ) + 
      geom_brace( aes( c( as.Date( "2021-12-15", origin = "1970-01-01" ), 
                          as.Date( "2022-02-28", origin = "1970-01-01" ) ),
                       c(50, 65), label = "Wave 5" ), 
                  inherit.data = FALSE, labelsize = 3 ) + 
      geom_brace( aes( c( as.Date( "2022-03-01", origin = "1970-01-01" ), 
                          as.Date( "2022-06-18", origin = "1970-01-01" ) ),
                       c(50, 65), label = "Wave 6" ), 
                  inherit.data = FALSE, labelsize = 3 ) +
      
      geom_brace( aes( c( as.Date( "2021-03-23", origin = "1970-01-01" ), 
                          as.Date( "2021-06-19", origin = "1970-01-01" ) ),
                       c(95, 110), label = "Alpha" ), 
                  inherit.data = FALSE, labelsize = 4, labeldistance = 0.125 ) + 
      geom_brace( aes( c( as.Date( "2021-06-20", origin = "1970-01-01" ), 
                          as.Date( "2021-12-11", origin = "1970-01-01" ) ),
                       c(95, 110), label = "Delta" ), 
                  inherit.data = FALSE, labelsize = 4, labeldistance = 0.125 ) +
      geom_brace( aes( c( as.Date( "2021-12-12", origin = "1970-01-01" ), 
                          as.Date( "2022-06-30", origin = "1970-01-01" ) ),
                       c(95, 110), label = "Omicron" ), 
                  inherit.data = FALSE, labelsize = 4, labeldistance = 0.125 ) + 
      
      annotation_custom(grid::textGrob(label = "Designated\nWave",
                                       x = unit(-0.075, "npc"),
                                       y = unit(1.110, "npc") ) ) + 
      annotation_custom(grid::textGrob(label = "Dominant\nVariant",
                                       x = unit(-0.075, "npc"),
                                       y = unit(1.275, "npc") ) ) + 
      
      scale_y_continuous(
        labels = scales::label_comma(), 
        trans = scales::pseudo_log_trans(),
        breaks = c( 1, 2, 5, 10, 25 ) ) + 

      coord_cartesian( ylim = c(0,35), 
                       clip = "off" )

  }
  
    
  ### Add annotations to plot of Belgium 
  if ( D2 == "Belgium" ) { 
    
    temp <- temp + 
      
      geom_brace( aes( c( as.Date( "2020-03-01", origin = "1970-01-01" ), 
                          as.Date( "2020-06-22", origin = "1970-01-01" ) ),
                       c(50, 65), label = "Wave 1" ), 
                  inherit.data = FALSE, labelsize = 3 ) + 
      geom_brace( aes( c( as.Date( "2020-08-31", origin = "1970-01-01" ), 
                          as.Date( "2021-02-14", origin = "1970-01-01" ) ),
                       c(50, 65), label = "Wave 2" ), 
                  inherit.data = FALSE, labelsize = 3 ) + 
      geom_brace( aes( c( as.Date( "2021-02-15", origin = "1970-01-01" ), 
                          as.Date( "2021-06-27", origin = "1970-01-01" ) ),
                       c(50, 65), label = "Wave 3" ), 
                  inherit.data = FALSE, labelsize = 3 ) + 
      geom_brace( aes( c( as.Date( "2021-10-04", origin = "1970-01-01" ), 
                          as.Date( "2021-12-26", origin = "1970-01-01" ) ),
                       c(50, 65), label = "Wave 4" ), 
                  inherit.data = FALSE, labelsize = 3 ) + 
      geom_brace( aes( c( as.Date( "2021-12-27", origin = "1970-01-01" ), 
                          as.Date( "2022-02-27", origin = "1970-01-01" ) ),
                       c(50, 65), label = "Wave 5" ), 
                  inherit.data = FALSE, labelsize = 3 ) + 
      geom_brace( aes( c( as.Date( "2022-02-28", origin = "1970-01-01" ), 
                          as.Date( "2022-05-29", origin = "1970-01-01" ) ),
                       c(50, 65), label = "Wave 6" ), 
                  inherit.data = FALSE, labelsize = 3 ) +
      
      geom_brace( aes( c( as.Date( "2021-02-15", origin = "1970-01-01" ), 
                          as.Date( "2021-06-27", origin = "1970-01-01" ) ),
                       c(95, 110), label = "Alpha" ), 
                  inherit.data = FALSE, labelsize = 4, labeldistance = 0.125 ) + 
      geom_brace( aes( c( as.Date( "2021-06-28", origin = "1970-01-01" ), 
                          as.Date( "2021-12-26", origin = "1970-01-01" ) ),
                       c(95, 110), label = "Delta" ), 
                  inherit.data = FALSE, labelsize = 4, labeldistance = 0.125 ) +
      geom_brace( aes( c( as.Date( "2021-12-27", origin = "1970-01-01" ), 
                          as.Date( "2022-06-30", origin = "1970-01-01" ) ),
                       c(95, 110), label = "Omicron" ), 
                  inherit.data = FALSE, labelsize = 4, labeldistance = 0.125 ) + 
      
      annotation_custom(grid::textGrob(label = "Designated\nWave",
                                       x = unit(-0.075, "npc"),
                                       y = unit(1.110, "npc") ) ) + 
      annotation_custom(grid::textGrob(label = "Dominant\nVariant",
                                       x = unit(-0.075, "npc"),
                                       y = unit(1.275, "npc") ) ) + 
      
      scale_y_continuous(
        labels = scales::label_comma(), 
        trans = scales::pseudo_log_trans(),
        breaks = c( 1, 2, 5, 10, 25 ) ) + 

      
      coord_cartesian( ylim = c(0,35), 
                       clip = "off" )
    
  }
  
  ### Add legend to plot of Japan
  if ( D2 == "Japan" ) {
    
    temp = temp + 
      theme( legend.title = element_blank(),
             legend.position = c( 0.525, 0.95 ),
             legend.text = element_text(size = 12) )  
  
  }
  
  ### Save plot
  png( filename = paste0("Figures/8_Waves_",sub(" ", "_", D2),".png"), 
       res = 300, width = 1.5 * 1928, height = 1.5 * 1514, unit = "px" )
  print(temp)
  dev.off()

}




###########################
###
### Location parameter plot
###
###########################


for (ii in 1:8) { 
  
  assign( paste0( "A_waves_",ii), tibble::tibble( 
    "Japan" = c( nCov_fit[["Japan"]]$A[,ii] ),
    "Italy" = c( nCov_fit[["Italy"]]$A[,ii] ), 
    "Belgium" = c( nCov_fit[["Belgium"]]$A[,ii] ),
    "Ontario" = c( nCov_fit[["Ontario"]]$A[,ii] ),
    "Texas" = c( nCov_fit[["Texas"]]$A[,ii] ),
    "Peru" = c( nCov_fit[["Peru"]]$A[,ii] ) ) )
  
  assign( paste0( "A_waves_CI_",ii), tibble( 
    "Mean" = apply( get( paste0( "A_waves_",ii) ), 2, mean ),
    "LCL" = apply( get( paste0( "A_waves_",ii) ), 2, quantile, probs = 0.025 ),
    "UCL" = apply( get( paste0( "A_waves_",ii) ), 2, quantile, probs = 0.975 ),
    "Region" = c( "Japan", "Italy", "Belgium", "Ontario", "Texas", "Peru" ) ) )

  assign(  paste0( "A_waves_",ii), reshape2::melt( get( paste0( "A_waves_",ii) ) ) )

}

A_waves_1$wave <- "Wave 1"
A_waves_2$wave <- "Wave 2"
A_waves_3$wave <- "Wave 3"
A_waves_4$wave <- "Wave 4"
A_waves_5$wave <- "Wave 5"
A_waves_6$wave <- "Wave 6"
A_waves_7$wave <- "Wave 7"
A_waves_8$wave <- "Wave 8"

A_waves <- rbind( A_waves_1, A_waves_2, A_waves_3, A_waves_4, 
                  A_waves_5, A_waves_6, A_waves_7, A_waves_8 )

temp <- ggplot( ) + 
  xlab("") + ylab("Location") +
  stat_summary( geom = 'pointrange',
    data = A_waves,
    mapping = aes(x = variable, y = as.Date( value, origin = "1970-01-01" ), 
                  color = wave, group = wave), 
    position = position_dodge2( width = 0.75, padding = 0.5 ),
    fun = mean,
    fun.min = function(x) { quantile(x, probs = 0.025) },
    fun.max = function(x) { quantile(x, probs = 0.975) } ) + 
  
    scale_y_date( breaks = as.Date( c("2020-03-01", "2020-09-01",
                                      "2021-03-01", "2021-09-01",
                                      "2022-03-01"), origin = "1970-01-01" ), 
                  date_labels =  "%b %Y" ) + 

    scale_color_manual( values = scales::hue_pal()(8),
                        name = "", 
                        labels = c( "Curve 1", "Curve 2", 
                                    "Curve 3", "Curve 4",
                                    "Curve 5", "Curve 6", 
                                    "Curve 7", "Curve 8" ) ) + 
    guides(color = guide_legend(byrow = TRUE) ) + 
    coord_flip() + 
  
    theme_classic() + 
    theme( axis.text = element_text(size = 18, color = "black"),
           axis.title = element_text(size = 20),
           axis.title.x = element_text(margin = margin(15, 0, 0, 0)),
           axis.title.y = element_text(margin = margin(0, 12.5, 0, 0)),
           legend.title.align = 0.5,
           legend.text = element_text(size = 17),
           legend.position = "top",
           legend.spacing.x = unit(0.5, 'cm'),
           legend.spacing.y = unit(0.5, 'cm') )

png( filename = paste0("Figures/8_Waves_A_Selected_Error_Bars.png"), 
     res = 300, width = 1.5 * 1928, height = 1.5 * 1514, unit = "px" )
temp
dev.off()



### Scale parameter plot
for (ii in 1:8) { 
  
  assign( paste0( "B_waves_",ii), tibble::tibble( 
    "Japan" = c( nCov_fit[["Japan"]]$B[,ii] ),
    "Italy" = c( nCov_fit[["Italy"]]$B[,ii] ), 
    "Belgium" = c( nCov_fit[["Belgium"]]$B[,ii] ),
    "Ontario" = c( nCov_fit[["Ontario"]]$B[,ii] ),
    "Texas" = c( nCov_fit[["Texas"]]$B[,ii] ),
    "Peru" = c( nCov_fit[["Peru"]]$B[,ii] ) ) )
  
  assign( paste0( "B_waves_CI_",ii), tibble( 
    "Mean" = apply( get( paste0( "B_waves_",ii) ), 2, mean ),
    "LCL" = apply( get( paste0( "B_waves_",ii) ), 2, quantile, probs = 0.025 ),
    "UCL" = apply( get( paste0( "B_waves_",ii) ), 2, quantile, probs = 0.975 ),
    "Region" = c( "Japan", "Italy", "Belgium", "Ontario", "Texas", "Peru" ) ) )
  
  assign(  paste0( "B_waves_",ii), reshape2::melt( get( paste0( "B_waves_",ii) ) ) )

}

B_waves_1$wave <- "Wave 1"
B_waves_2$wave <- "Wave 2"
B_waves_3$wave <- "Wave 3"
B_waves_4$wave <- "Wave 4"
B_waves_5$wave <- "Wave 5"
B_waves_6$wave <- "Wave 6"
B_waves_7$wave <- "Wave 7"
B_waves_8$wave <- "Wave 8"

B_waves <- rbind( B_waves_1, B_waves_2, B_waves_3, B_waves_4, 
                  B_waves_5, B_waves_6, B_waves_7, B_waves_8 )

temp <- ggplot( ) + 
  xlab("") + ylab("Scale") +
  stat_summary( geom = 'pointrange',
    data = B_waves,
    mapping = aes(x = variable, y = value, 
                  color = wave, group = wave), 
    position = position_dodge2( width = 0.75, padding = 0.50 ),
    fun = mean,
    fun.min = function(x) { quantile(x, probs = 0.025) },
    fun.max = function(x) { quantile(x, probs = 0.975) } ) + 
  
    scale_color_manual( values = scales::hue_pal()(8),
                        name = "", 
                        labels = c( "Curve 1", "Curve 2", 
                                    "Curve 3", "Curve 4",
                                    "Curve 5", "Curve 6", 
                                    "Curve 7", "Curve 8" ) ) + 
    coord_flip() +

  theme_classic() + 
  theme( axis.text = element_text(size = 18, color = "black"),
         axis.title = element_text(size = 20),
         axis.title.x = element_text(margin = margin(15, 0, 0, 0)),
         axis.title.y = element_text(margin = margin(0, 12.5, 0, 0)),
         legend.title.align = 0.5,
         legend.text = element_text(size = 17),
         legend.position = "none",
         legend.spacing.x = unit(0.5, 'cm'),
         legend.spacing.y = unit(0.5, 'cm') )

png( filename = paste0("Figures/8_Waves_B_Selected_Error_Bars.png"), 
     res = 300, width = 1.5 * 1928, height = 1.5 * 1514, unit = "px" )
temp
dev.off()



###########################
###
### Severity parameter plot
###
###########################
for (ii in 1:8) { 
  
  assign( paste0( "C_waves_",ii), tibble::tibble( 
    "Japan" = c( nCov_fit[["Japan"]]$C[,ii] ),
    "Italy" = c( nCov_fit[["Italy"]]$C[,ii] ), 
    "Belgium" = c( nCov_fit[["Belgium"]]$C[,ii] ),
    "Ontario" = c( nCov_fit[["Ontario"]]$C[,ii] ),
    "Texas" = c( nCov_fit[["Texas"]]$C[,ii] ),
    "Peru" = c( nCov_fit[["Peru"]]$C[,ii] ) ) )
  
  assign( paste0( "C_waves_CI_",ii), tibble( 
    "Mean" = apply( get( paste0( "C_waves_",ii) ), 2, mean ),
    "LCL" = apply( get( paste0( "C_waves_",ii) ), 2, quantile, probs = 0.025 ),
    "UCL" = apply( get( paste0( "C_waves_",ii) ), 2, quantile, probs = 0.975 ),
    "Region" = c( "Japan", "Italy", "Belgium", "Ontario", "Texas", "Peru" ) ) )

  assign(  paste0( "C_waves_",ii), reshape2::melt( get( paste0( "C_waves_",ii) ) ) )

}

C_waves_1$wave <- "Wave 1"
C_waves_2$wave <- "Wave 2"
C_waves_3$wave <- "Wave 3"
C_waves_4$wave <- "Wave 4"
C_waves_5$wave <- "Wave 5"
C_waves_6$wave <- "Wave 6"
C_waves_7$wave <- "Wave 7"
C_waves_8$wave <- "Wave 8"

C_waves <- rbind( C_waves_1, C_waves_2, C_waves_3, C_waves_4, 
                  C_waves_5, C_waves_6, C_waves_7, C_waves_8 )

temp <- ggplot( ) + 
  xlab("") + ylab("Severity") +
  stat_summary( geom = 'pointrange',
    data = C_waves,
    mapping = aes(x = variable, y = value, 
                  color = wave, group = wave), 
    position = position_dodge2( width = 0.75, padding = 0.5 ),
    fun = mean,
    fun.min = function(x) { quantile(x, probs = 0.025) },
    fun.max = function(x) { quantile(x, probs = 0.975) } ) + 
    
    scale_y_continuous( breaks = c( 0.25, 1, 3, 7.5, 15, 30, 60 ),
                        trans = scales::pseudo_log_trans(sigma = 1.5) ) +
  
    scale_color_manual( values = scales::hue_pal()(8),
                        name = "", 
                        labels = c( "Curve 1", "Curve 2", 
                                    "Curve 3", "Curve 4",
                                    "Curve 5", "Curve 6", 
                                    "Curve 7", "Curve 8" ) ) + 
  coord_flip() + 

  theme_classic() + 
  theme( axis.text = element_text(size = 18, color = "black"),
         axis.title = element_text(size = 20),
         axis.title.x = element_text(margin = margin(15, 0, 0, 0)),
         axis.title.y = element_text(margin = margin(0, 12.5, 0, 0)),
         legend.title.align = 0.5,
         legend.text = element_text(size = 17),
         legend.position = "none",
         legend.spacing.x = unit(0.5, 'cm'),
         legend.spacing.y = unit(0.5, 'cm') )

png( filename = paste0("Figures/8_Waves_C_Selected_Error_Bars.png"), 
     res = 300, width = 1.5 * 1928, height = 1.5 * 1514, unit = "px" )
temp
dev.off()




###########################
###
### Shape parameter plot
###
###########################

for (ii in 1:8) { 
  
  assign( paste0( "K_waves_",ii), tibble::tibble( 
    "Japan" = c( nCov_fit[["Japan"]]$K[,ii] ),
    "Italy" = c( nCov_fit[["Italy"]]$K[,ii] ), 
    "Belgium" = c( nCov_fit[["Belgium"]]$K[,ii] ),
    "Ontario" = c( nCov_fit[["Ontario"]]$K[,ii] ),
    "Texas" = c( nCov_fit[["Texas"]]$K[,ii] ),
    "Peru" = c( nCov_fit[["Peru"]]$K[,ii] ) ) )
  
  assign( paste0( "K_waves_CI_",ii), tibble( 
    "Mean" = apply( get( paste0( "K_waves_",ii) ), 2, mean ),
    "LCL" = apply( get( paste0( "K_waves_",ii) ), 2, quantile, probs = 0.025 ),
    "UCL" = apply( get( paste0( "K_waves_",ii) ), 2, quantile, probs = 0.975 ),
    "Region" = c( "Japan", "Italy", "Belgium", "Ontario", "Texas", "Peru" ) ) )
  
  assign(  paste0( "K_waves_",ii), reshape2::melt( get( paste0( "K_waves_",ii) ) ) )

}

K_waves_1$wave <- "Wave 1"
K_waves_2$wave <- "Wave 2"
K_waves_3$wave <- "Wave 3"
K_waves_4$wave <- "Wave 4"
K_waves_5$wave <- "Wave 5"
K_waves_6$wave <- "Wave 6"
K_waves_7$wave <- "Wave 7"
K_waves_8$wave <- "Wave 8"

K_waves <- rbind( K_waves_1, K_waves_2, K_waves_3, K_waves_4, 
                  K_waves_5, K_waves_6, K_waves_7, K_waves_8 )

temp <- ggplot( ) + 
  xlab("") + ylab("Shape") +
  stat_summary( geom = 'pointrange',
    data = K_waves,
    mapping = aes(x = variable, y = value, color = wave, group = wave), 
    position = position_dodge2( width = 0.75, padding = 0.5 ),
    fun = mean,
    fun.min = function(x) { quantile(x, probs = 0.025) },
    fun.max = function(x) { quantile(x, probs = 0.975) } ) + 

  scale_color_manual( values = scales::hue_pal()(8),
                      name = "", 
                      labels = c( "Curve 1", "Curve 2", "Curve 3", "Curve 4",
                                  "Curve 5", "Curve 6", "Curve 7", "Curve 8" ) ) + 
  coord_cartesian( ylim = c(-7.5,15) ) + 
  coord_flip() + 

  theme_classic() + 
  theme( axis.text = element_text(size = 18, color = "black"),
         axis.title = element_text(size = 20),
         axis.title.x = element_text(margin = margin(15, 0, 0, 0)),
         axis.title.y = element_text(margin = margin(0, 12.5, 0, 0)),
         legend.title.align = 0.5,
         legend.text = element_text(size = 17),
         legend.position = "none", # c(0.15,0.85),
         legend.spacing.y = unit(0.25, 'cm') )

png( filename = paste0("Figures/8_Waves_K_Selected_Error_Bars.png"), 
     res = 300, width = 1.5 * 1928, height = 1.5 * 1514, unit = "px" )
temp
dev.off()




###########################
###
### DOW parameter plot
### Japan, Italy, Belgium, 
### Ontario
###
###########################

Japan_DOW <- nCov_fit[["Japan"]]$betaDayTrans
Italy_DOW <- nCov_fit[["Italy"]]$betaDayTrans
Belgium_DOW <- nCov_fit[["Belgium"]]$betaDayTrans
Ontario_DOW <- nCov_fit[["Ontario"]]$betaDayTrans

assign( paste0( "Japan_DOW" ), tibble::tibble( 
  "Monday" = Japan_DOW[,1],
  "Tuesday" = Japan_DOW[,2],
  "Wednesday" = Japan_DOW[,3],
  "Thursday" = Japan_DOW[,4],
  "Friday" = Japan_DOW[,5], 
  "Saturday" = Japan_DOW[,6], 
  "Sunday" = Japan_DOW[,7] ) )

assign( paste0( "Italy_DOW" ), tibble::tibble( 
  "Monday" = Italy_DOW[,1],
  "Tuesday" = Italy_DOW[,2],
  "Wednesday" = Italy_DOW[,3],
  "Thursday" = Italy_DOW[,4],
  "Friday" = Italy_DOW[,5],
  "Saturday" = Italy_DOW[,6],
  "Sunday" = Italy_DOW[,7] ) )

assign( paste0( "Belgium_DOW" ), tibble::tibble( 
  "Monday" = Belgium_DOW[,1],
  "Tuesday" = Belgium_DOW[,2],
  "Wednesday" = Belgium_DOW[,3],
  "Thursday" = Belgium_DOW[,4],
  "Friday" = Belgium_DOW[,5], 
  "Saturday" = Belgium_DOW[,6], 
  "Sunday" = Belgium_DOW[,7] ) )

assign( paste0( "Ontario_DOW" ), tibble::tibble( 
  "Monday" = Ontario_DOW[,1],
  "Tuesday" = Ontario_DOW[,2],
  "Wednesday" = Ontario_DOW[,3],
  "Thursday" = Ontario_DOW[,4],
  "Friday" = Ontario_DOW[,5],
  "Saturday" = Ontario_DOW[,6],
  "Sunday" = Ontario_DOW[,7] ) )

Japan_DOW <- reshape2::melt( Japan_DOW ) 
Japan_DOW$region <- "Japan"

Italy_DOW <- reshape2::melt( Italy_DOW ) 
Italy_DOW$region <- "Italy"

Belgium_DOW <- reshape2::melt( Belgium_DOW ) 
Belgium_DOW$region <- "Belgium"

Ontario_DOW <- reshape2::melt( Ontario_DOW ) 
Ontario_DOW$region <- "Ontario"


RegionSub_DOW <- rbind( Japan_DOW, Italy_DOW, Belgium_DOW, Ontario_DOW )
RegionSub_DOW$region <- factor( RegionSub_DOW$region, 
                                levels = c( "Japan", "Italy", "Belgium", "Ontario" ) )


temp <- ggplot( ) + 
  xlab("") + ylab("Day-of-the-Week Effect") +
  stat_summary( geom = 'pointrange',
    data = RegionSub_DOW,
    mapping = aes(x = region, y = value, color = variable, group = variable), 
    position = position_dodge2( width = 0.75, padding = 0.5 ),
    fun = mean,
    fun.min = function(x) { quantile(x, probs = 0.025) },
    fun.max = function(x) { quantile(x, probs = 0.975) } ) + 
  labs( color = "" ) + 
  coord_cartesian( ylim = c(0,1.25) ) + 
  guides( color = guide_legend(byrow = TRUE) ) + 
  coord_flip() + 
  
  scale_color_manual( values = c( "Monday" = unname( scales::hue_pal()(8)[1] ), 
                                "Tuesday" = unname( scales::hue_pal()(8)[2] ), 
                                "Wednesday" = unname( scales::hue_pal()(8)[3] ), 
                                "Thursday" = unname( scales::hue_pal()(8)[4] ), 
                                "Friday" = unname( scales::hue_pal()(8)[5] ), 
                                "Saturday" = unname( scales::hue_pal()(8)[6] ), 
                                "Sunday" = unname( scales::hue_pal()(8)[7] ) ) ) + 

  theme_classic() + 
  theme( axis.text = element_text(size = 18, color = "black"),
         axis.title = element_text(size = 20),
         axis.title.x = element_text(margin = margin(15, 0, 0, 0)),
         axis.title.y = element_text(margin = margin(0, 12.5, 0, 0)),
         strip.background = element_blank(),
         strip.text = element_text(size = 18),
         strip.placement = 'outside',
         legend.title.align = 0.5,
         legend.text = element_text(size = 17),
         legend.position = "top", # c(0.50,0.25),
         plot.margin = margin(0.5,0,0,0.25, "cm"),
         legend.spacing.x = unit(0.25, 'cm'),
         legend.spacing.y = unit(0.25, 'cm') )

png( filename = paste0("Figures/8_Waves_RegionSub_DOW.png"), 
     res = 300, width = 1.5 * 1928, height = 1.5 * 1514, unit = "px" )
temp
dev.off()





###########################
###
### DOW parameter plot
### Texas and Peru
###
###########################

Texas_DOW_1 <- nCov_fit[["Texas"]]$betaDayTrans_1
Texas_DOW_2 <- nCov_fit[["Texas"]]$betaDayTrans_2
Peru_DOW_1 <- nCov_fit[["Peru"]]$betaDayTrans_1
Peru_DOW_2 <- nCov_fit[["Peru"]]$betaDayTrans_2

assign( paste0( "Texas_DOW_1" ), tibble::tibble( 
  "Monday" = Texas_DOW_1[,1],
  "Tuesday" = Texas_DOW_1[,2],
  "Wednesday" = Texas_DOW_1[,3],
  "Thursday" = Texas_DOW_1[,4],
  "Friday" = Texas_DOW_1[,5], 
  "Saturday" = Texas_DOW_1[,6], 
  "Sunday" = Texas_DOW_1[,7] ) )

assign( paste0( "Peru_DOW_1" ), tibble::tibble( 
  "Monday" = Peru_DOW_1[,1],
  "Tuesday" = Peru_DOW_1[,2],
  "Wednesday" = Peru_DOW_1[,3],
  "Thursday" = Peru_DOW_1[,4],
  "Friday" = Peru_DOW_1[,5],
  "Saturday" = Peru_DOW_1[,6],
  "Sunday" = Peru_DOW_1[,7] ) )

assign( paste0( "Texas_DOW_2" ), tibble::tibble( 
  "Monday" = Texas_DOW_2[,1],
  "Tuesday" = Texas_DOW_2[,2],
  "Wednesday" = Texas_DOW_2[,3],
  "Thursday" = Texas_DOW_2[,4],
  "Friday" = Texas_DOW_2[,5], 
  "Saturday" = Texas_DOW_2[,6], 
  "Sunday" = Texas_DOW_2[,7] ) )

assign( paste0( "Peru_DOW_2" ), tibble::tibble( 
  "Monday" = Peru_DOW_2[,1],
  "Tuesday" = Peru_DOW_2[,2],
  "Wednesday" = Peru_DOW_2[,3],
  "Thursday" = Peru_DOW_2[,4],
  "Friday" = Peru_DOW_2[,5],
  "Saturday" = Peru_DOW_2[,6],
  "Sunday" = Peru_DOW_2[,7] ) )

Texas_DOW_1 <- reshape2::melt( Texas_DOW_1 ) 
Texas_DOW_1$region <- "Texas"
Texas_DOW_1$period <- "Day-of-the-Week Effect\n1st Period"

Texas_DOW_2 <- reshape2::melt( Texas_DOW_2 ) 
Texas_DOW_2$region <- "Texas"
Texas_DOW_2$period <- "Day-of-the-Week Effect\n2nd Period"

Peru_DOW_1 <- reshape2::melt( Peru_DOW_1 ) 
Peru_DOW_1$region <- "Peru"
Peru_DOW_1$period <- "Day-of-the-Week Effect\n1st Period"

Peru_DOW_2 <- reshape2::melt( Peru_DOW_2 ) 
Peru_DOW_2$region <- "Peru"
Peru_DOW_2$period <- "Day-of-the-Week Effect\n2nd Period"


Texas_Peru_DOW <- rbind( Texas_DOW_1, Texas_DOW_2, Peru_DOW_1, Peru_DOW_2 )
Texas_Peru_DOW$region <- factor( Texas_Peru_DOW$region, levels = c( "Texas", "Peru" ) )


temp <- ggplot( ) + 
  ylab("") + xlab("") +
  stat_summary( geom = 'pointrange',
    data = Texas_Peru_DOW,
    mapping = aes(x = value, y = region, color = variable, group = variable), 
    position = position_dodge2( width = 0.75, padding = 0.5 ),
    fun = mean,
    fun.min = function(x) { quantile(x, probs = 0.025) },
    fun.max = function(x) { quantile(x, probs = 0.975) } ) + 
  facet_wrap( ~ period, strip.position = "bottom" ) + 
  labs( color = "" ) + 
  guides( color = guide_legend(byrow = TRUE) ) + 
  
  scale_color_manual( values = c( "Monday" = unname( scales::hue_pal()(8)[1] ),
                                "Tuesday" = unname( scales::hue_pal()(8)[2] ), 
                                "Wednesday" = unname( scales::hue_pal()(8)[3] ), 
                                "Thursday" = unname( scales::hue_pal()(8)[4] ), 
                                "Friday" = unname( scales::hue_pal()(8)[5] ), 
                                "Saturday" = unname( scales::hue_pal()(8)[6] ), 
                                "Sunday" = unname( scales::hue_pal()(8)[7] ) ) ) +

  theme_classic() + 
  theme( axis.text = element_text(size = 18, color = "black"),
         axis.title.y = element_text(margin = margin(0, 12.5, 0, 0), size = 20),
         axis.title.x = element_text(margin = margin(15, 0, 0, 0), size = 20),
         strip.background = element_blank(),
         strip.text = element_text(size = 18),
         strip.placement = 'outside',
         legend.title.align = 0.5,
         legend.text = element_text(size = 17),
         plot.margin = margin(2.5,0.50,-1.1,0.50, "cm"),
         legend.position = "none", # c(0.75,0.30),
         legend.spacing.y = unit(0.25, 'cm') )

png( filename = paste0("Figures/8_Waves_Texas_Peru_DOW.png"), 
     res = 300, width = 1.5 * 1928, height = 1.5 * 1514, unit = "px" )
temp
dev.off()