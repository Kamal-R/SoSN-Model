
###########################
###
### Load libraries 
###
###########################
library("knitr")
library("ggplot2")
library("RColorBrewer")
library("tidyverse")
library("colorspace")



###########################
###
### Load saved model and 
### process for forecasts
###
###########################
nCov_fit <- readRDS( "3_Waves_DOW.RDS" ) 
nCov_data <- readRDS( "Data_File.RDS" )

fDPM <- data.frame( "reporting_unit" = c( "Belgium", "Italy", "Japan", 
                                          "Ontario", "Peru", "Texas" ),
                    "mMult" = c( 0.08628413572977742, 0.01653936099980305,
                                 0.00790661003193527, 0.06866818400463920,
                                 0.03032890545467185, 0.03484096579854013 ) )
rownames(fDPM) <- fDPM$reporting_unit
Regions <- fDPM$reporting_unit




###########################
###
### Plot waves by regions 
### (with forecasts)
###
###########################

for (D2 in Regions) {
  
  num_extra_obs <- c( nCov_fit$data[nCov_fit$data$reporting_unit == D2,]$timeNumeric - 
                        as.numeric( as.Date( "2020-01-28", origin = "1970-01-01" ) ) )[1] 
  
  forecasts_dates_1 <- seq( from = as.Date("2020-01-28", origin = "1970-01-01"), by = 1, 
                            to = as.Date("2020-01-28", origin = "1970-01-01") + num_extra_obs - 1 )
  
  if( as.numeric( nCov_fit$data[nCov_fit$data$reporting_unit == D2,]$time[1] - 
                  forecasts_dates_1[length(forecasts_dates_1)] ) != 1 ) { 
    stop( paste0( "The wrong dates are being added to the beginning of the graph for region ", D2) ) }
  
  forecast_dates <- as.Date( c( 
    "2020-11-13", "2020-11-14", "2020-11-15", "2020-11-16",
    "2020-11-17", "2020-11-18", "2020-11-19", "2020-11-20", "2020-11-21", 
    "2020-11-22", "2020-11-23", "2020-11-24", "2020-11-25", "2020-11-26",
    "2020-11-27", "2020-11-28", "2020-11-29", "2020-11-30", "2020-12-01",
    "2020-12-02", "2020-12-03", "2020-12-04", "2020-12-05", "2020-12-06", 
    "2020-12-07", "2020-12-08", "2020-12-09", "2020-12-10" ) )
  
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
  thisReportingUnit <- nCov_data[nCov_data$reporting_unit == D2,]
  observed_dead_forecast_dates <- thisReportingUnit[ thisReportingUnit$time %in% forecasts_times, ]$dead

  forecasts_dead <- nCov_fit[[D2]]$dead_rep_2[2,][1:length(forecast_dates)]
  forecasts_mean <- apply( nCov_fit[[D2]][["lambda_smooth_rep_2"]], 2, mean )[1:length(forecast_dates)]
  forecasts_median <- apply( nCov_fit[[D2]][["lambda_smooth_rep_2"]], 2, median )[1:length(forecast_dates)]
  forecasts_025 <- apply( nCov_fit[[D2]][["lambda_smooth_rep_2"]], 2, quantile, probs = 0.025 )[1:length(forecast_dates)] 
  forecasts_25 <- apply( nCov_fit[[D2]][["lambda_smooth_rep_2"]], 2, quantile, probs = 0.25 )[1:length(forecast_dates)]
  forecasts_40 <- apply( nCov_fit[[D2]][["lambda_smooth_rep_2"]], 2, quantile, probs = 0.40 )[1:length(forecast_dates)] 
  forecasts_60 <- apply( nCov_fit[[D2]][["lambda_smooth_rep_2"]], 2, quantile, probs = 0.60 )[1:length(forecast_dates)] 
  forecasts_75 <- apply( nCov_fit[[D2]][["lambda_smooth_rep_2"]], 2, quantile, probs = 0.75 )[1:length(forecast_dates)] 
  forecasts_975 <- apply( nCov_fit[[D2]][["lambda_smooth_rep_2"]], 2, quantile, probs = 0.975 )[1:length(forecast_dates)] 
  
  forecasts_025_DOW <- apply( nCov_fit[[D2]][["lambda_rep_2"]], 2, quantile, probs = 0.025 )[1:length(forecast_dates)] 
  forecasts_975_DOW <- apply( nCov_fit[[D2]][["lambda_rep_2"]], 2, quantile, probs = 0.975 )[1:length(forecast_dates)] 
  
  c( length( forecasts_times ), length( forecasts_dead ),
     length( forecasts_mean ), length( forecasts_median ),
     length( forecasts_025 ), length( forecasts_975 ),
     length( forecasts_25 ), length( forecasts_75 ), 
     length( forecasts_40 ), length( forecasts_60 ),
     length( forecasts_025_DOW ), length( forecasts_975_DOW ) )
  
  
  first_wave <- cbind( nCov_fit[[D2]][["waves_rep_1"]][,1:num_extra_obs,1],
                       nCov_fit[[D2]][["waves"]][,,1], 
                       nCov_fit[[D2]][["waves_rep_2"]][,1:length(forecast_dates),1] )
  second_wave <- cbind( nCov_fit[[D2]][["waves_rep_1"]][,1:num_extra_obs,2], 
                        nCov_fit[[D2]][["waves"]][,,2], 
                        nCov_fit[[D2]][["waves_rep_2"]][,1:length(forecast_dates),2] )
  third_wave <- cbind( nCov_fit[[D2]][["waves_rep_1"]][,1:num_extra_obs,3], 
                       nCov_fit[[D2]][["waves"]][,,3], 
                       nCov_fit[[D2]][["waves_rep_2"]][,1:length(forecast_dates),3] )
  
  points_color <- c( rep( " ", num_extra_obs ),
                     rep( "Observed Mortality", length(observed_times) ), 
                     rep( "Observed Mortality, Forecast Period", length(forecasts_times) ) )
  
  DOW_color <- c( rep( "extra_observed", num_extra_obs ),
                  rep( "extra_observed", length(observed_times) ), 
                  rep( "not_observed_DOW", length(forecasts_times) ) )
  

  alpha_curves_95_CI <- 0.20
  alpha_curves_50_CI <- 0.30
  alpha_curves_20_CI <- 0.40
  alpha_intensity_95_CI <- 0.30
  alpha_intensity_50_CI <- 0.50
  alpha_intensity_20_CI <- 0.70
  
  
  intensity_tibble <- tibble::tibble( 
  
    time_interval = c( extra_observed_times, observed_times, forecasts_times ), 
    mortality_points = c( extra_observed_dead, observed_dead, observed_dead_forecast_dates ) * fDPM[D2,'mMult'],
    mortality_color = points_color,
    
    intensity_mean = c( extra_observed_data_mean, observed_data_mean, forecasts_mean ) * fDPM[D2,'mMult'],
    intensity_median = c( extra_observed_data_median, observed_data_median, forecasts_median ) * fDPM[D2,'mMult'],
    
    intensity_Q025 = c( extra_observed_data_025, observed_data_025, forecasts_025 ) * fDPM[D2,'mMult'],
    intensity_Q25 = c( extra_observed_data_25, observed_data_25, forecasts_25 ) * fDPM[D2,'mMult'],
    intensity_Q40 = c( extra_observed_data_40, observed_data_40, forecasts_40 ) * fDPM[D2,'mMult'],
    intensity_Q60 = c( extra_observed_data_60, observed_data_60, forecasts_60 ) * fDPM[D2,'mMult'],
    intensity_Q75 = c( extra_observed_data_75, observed_data_75, forecasts_75 ) * fDPM[D2,'mMult'],
    intensity_Q975 = c( extra_observed_data_975, observed_data_975, forecasts_975 ) * fDPM[D2,'mMult'],
    
    intensity_DOW_Q025 = c( extra_observed_data_025, observed_data_025, forecasts_025_DOW ) * fDPM[D2,'mMult'],
    intensity_DOW_Q975 = c( extra_observed_data_975, observed_data_975, forecasts_975_DOW ) * fDPM[D2,'mMult'],

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
    
    fill_curves_95_CI = rep( adjust_transparency("grey50", alpha_curves_95_CI), 
                             length(points_color) ),
    fill_curves_50_CI = rep( adjust_transparency("grey50", alpha_curves_50_CI), 
                             length(points_color) ),
    fill_curves_20_CI = rep( adjust_transparency("grey50", alpha_curves_20_CI), 
                             length(points_color) ),
      
    fill_intensity_95_CI = rep( adjust_transparency("skyblue", alpha_intensity_95_CI), 
                                length(points_color) ),
    fill_intensity_50_CI = rep( adjust_transparency("skyblue", alpha_intensity_50_CI), 
                                length(points_color) ),
    fill_intensity_20_CI = rep( adjust_transparency("skyblue", alpha_intensity_20_CI), 
                                length(points_color) )

  
  )
  
  if ( sum( is.na( intensity_tibble ) ) != 0 ) {
    stop( paste0("Region ", D2, " has 'NA' values.") )
  }
  
  temp <- ggplot( data = intensity_tibble, aes( x = as.Date( time_interval, origin = "1970-01-01" ) ) ) + 
  
    geom_point( aes(  y = mortality_points, color = mortality_color ), 
                alpha = 0.8, shape = 21, stroke = 1.15 ) +
    
    ### First Wave
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

    geom_line( aes(  y = first_wave_median, color = "Curve 1: Median" ), size = 1.25, alpha = 0.75 ) + 
    geom_line( aes(  y = second_wave_median, color = "Curve 2: Median" ), size = 1.25, alpha = 0.75 ) +
    geom_line( aes(  y = third_wave_median, color = "Curve 3: Median" ), size = 1.25, alpha = 0.75 ) +
  
    ### Legend colors
    scale_color_manual( values = c( "Observed Mortality, Training Period" = "gray50", 
                                    "Observed Mortality, Forecast Period" = "red3", 
                                    " " = "white", "  " = "white",
                                    "Intensity: Median" = "skyblue2",
                                    "Curve 1: Median" = scales::hue_pal()(8)[1], 
                                    "Curve 2: Median" = scales::hue_pal()(8)[2], 
                                    "Curve 3: Median" = scales::hue_pal()(8)[3] ),
                        guide = guide_legend( direction = "horizontal", 
                          order = 2, ncol = 2,
                          override.aes = list( 
                            linetype = c(rep("blank", 4), rep("solid",4) ),
                            shape = c(rep(1, 4), rep(NA,4) ) ) ) ) +
    
    ### Legend labels
    scale_fill_identity( 
      labels = c( "Skew Normal Curves: 95%, 50%, and 20% Credible Intervals (Light to Dark)", 
                  "Intensity: 95%, 50%, and 20% Credible Intervals (Light to Dark)", rep("",4) ),
      guide = guide_legend( override.aes = list( fill = c("#7F7F7F", "#87CEEB", rep(NA,4) ) ),
                            order = 1, byrow = FALSE, direction = "vertical", ncol = 3 ) ) +

    xlab("") + ylab( paste0( "Daily Deaths per Million (",D2,")" ) ) + 
    scale_x_date( breaks = as.Date( c("2020-03-01", "2020-06-01", "2020-09-01") ), 
                  date_labels =  "%b %Y",
                  limits = c( as.Date("2020-01-15"), as.Date("2020-12-15") ) ) + 
    coord_cartesian( ylim = c( 0, if_else( D2 == "Japan", 2, 28 ) ) ) +
    
    scale_y_continuous(
      labels = scales::label_comma(), 
      trans = scales::pseudo_log_trans(base = exp(1)),
      breaks = c( 1, 2, 5, 15, 30 ) ) +
  
    theme_classic() + 
    theme( axis.text = element_text(size = 18, color = "black"),
         axis.title = element_text(size = 20),
         axis.title.x = element_text(margin = margin(15, 0, 0, 0)),
         axis.title.y = element_text(margin = margin(0, 12.5, 0, 0)) ) + 
    theme(legend.position = "none") 
  
    if ( D2 == "Japan" ) {
    
    temp = temp + 
      theme( legend.title = element_blank(),
             legend.position = c( 0.50, 0.85 ),
             legend.text = element_text(size = 13) )  
  
    }
  
    ### Save plot 
    png( filename = paste0("Figures/3_Waves_",sub(" ", "_", D2),".png"), 
      res = 300, width = 1.5 * 1928, height = 1.5 * 1514, unit = "px" )
    print(temp)   
    dev.off()
    
}




###########################
###
### Shape parameter plots
###
###########################

for (ii in 1:3) { 
  
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
K_waves <- rbind( K_waves_1, K_waves_2, K_waves_3 )

temp <- ggplot( ) + 
  xlab("") + ylab("Skewness Parameter") +
  stat_summary( geom = 'pointrange',
    data = K_waves,
    mapping = aes(x = variable, y = value, color = wave, group = wave), 
    position = position_dodge2( width = 0.50 ),
    fun = median,
    fun.min = function(x) { quantile(x, probs = 0.025) },
    fun.max = function(x) { quantile(x, probs = 0.975) } ) + 

  scale_color_manual( values = scales::hue_pal()(3),
                      name = "", 
                      labels = c( "Curve 1", "Curve 2", "Curve 3" ) ) + 
  coord_cartesian( ylim = c(-7.5,15) ) + 
  coord_flip() + 

  theme_classic() + 
  theme( axis.text = element_text(size = 16, color = "black"),
         axis.title = element_text(size = 18),
         axis.title.x = element_text(margin = margin(15, 0, 0, 0)),
         axis.title.y = element_text(margin = margin(0, 12.5, 0, 0)),
         legend.title.align = 0.5,
         legend.text = element_text(size = 16),
         legend.position = c(0.15,0.90),
         legend.spacing.y = unit(0.25, 'cm'))

png( filename = paste0("Figures/3_Waves_K_Selected_Error_Bars.png"), 
     res = 300, width = 1.5 * 1928, height = 1.5 * 1514, unit = "px" )
print( temp )
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
  xlab("") + ylab("Relative Risk") +
  stat_summary( geom = 'pointrange',
    data = RegionSub_DOW,
    mapping = aes(x = region, y = value, color = variable, group = variable), 
    position = position_dodge2( width = 1.00 ),
    fun = median,
    fun.min = function(x) { quantile(x, probs = 0.025) },
    fun.max = function(x) { quantile(x, probs = 0.975) } ) + 
  labs( color = "" ) + 
  coord_cartesian( ylim = c(0,2.25) ) + 
  guides( color = guide_legend(byrow = TRUE) ) + 
  
  scale_color_manual( values = c( "Monday" = unname( scales::hue_pal()(8)[1] ), # Monday
                                "Tuesday" = unname( scales::hue_pal()(8)[2] ), # Tuesday
                                "Wednesday" = unname( scales::hue_pal()(8)[3] ), # Wednesday
                                "Thursday" = unname( scales::hue_pal()(8)[4] ), # Thursday
                                "Friday" = unname( scales::hue_pal()(8)[5] ), # Friday
                                "Saturday" = unname( scales::hue_pal()(8)[6] ), # Saturday
                                "Sunday" = unname( scales::hue_pal()(8)[7] ) ) ) + # Sunday

  theme_classic() + 
  theme( axis.text = element_text(size = 16, color = "black"),
         axis.title = element_text(size = 18),
         axis.title.x = element_text(margin = margin(15, 0, 0, 0)),
         axis.title.y = element_text(margin = margin(0, 12.5, 0, 0)),
         strip.background = element_blank(),
         strip.text = element_text(size = 18),
         strip.placement = 'outside',
         legend.title.align = 0.5,
         legend.text = element_text(size = 16),
         legend.position = c(0.75,0.80),
         legend.spacing.y = unit(0.25, 'cm') )

png( filename = paste0("Figures/3_Waves_RegionSub_DOW.png"), 
     res = 300, width = 1.5 * 1928, height = 1.5 * 1514, unit = "px" )
temp
dev.off()




###########################
###
### Forecasting: sRMSLE
###
###########################

nCov_fit_1 <- readRDS( "3_Waves_DOW.RDS" )
nCov_fit_2 <- readRDS( "3_Waves_No_DOW.RDS" )
full_data_set <- readRDS("Data_File.RDS" )

days_ahead_to_predict <- 28
forecast_dates <- seq( from = nCov_fit_1$data$time[length(nCov_fit_1$data$time)] + 1, 
                       length.out = days_ahead_to_predict, by = 1 )
sRMSLE_at_day_t_1 <- vector(length = days_ahead_to_predict)
sRMSLE_at_day_t_2 <- vector(length = days_ahead_to_predict)

Regions = c( "Ontario", "Belgium", "Italy", "Japan", "Texas", "Peru" )

for (ii in 1:days_ahead_to_predict) {
  
  numerator_1 <- vector( length = length(Regions) )
  denominator_1 <- vector( length = length(Regions) )
  
  numerator_2 <- vector( length = length(Regions) )
  denominator_2 <- vector( length = length(Regions) )
  
  for ( jj in 1:length(Regions) ) { 

    ### DOW model 
    predicted_1 <- mean( nCov_fit_1[[Regions[jj]]]$dead_rep_2[,ii] )
    region_data_set_1 <- full_data_set[full_data_set$reporting_unit == Regions[jj],]
    observed_1 <- region_data_set_1[region_data_set_1$time == forecast_dates[ii],]$dead
    
    numerator_1[jj] <- log( predicted_1 / observed_1 )^2
    denominator_1[jj] <- log( observed_1 )
    
    ### No DOW model 
    predicted_2 <- mean( nCov_fit_2[[Regions[jj]]]$dead_rep_2[,ii] )
    region_data_set_2 <- full_data_set[full_data_set$reporting_unit == Regions[jj],]
    observed_2 <- region_data_set_2[region_data_set_2$time == forecast_dates[ii],]$dead
    
    numerator_2[jj] <- log( predicted_2 / observed_2 )^2
    denominator_2[jj] <- log( observed_2 )
  
  }
  
  sRMSLE_at_day_t_1[ii] <- mean( numerator_1 / denominator_1 )
  sRMSLE_at_day_t_2[ii] <- mean( numerator_2 / denominator_2 )
  
}

data_to_plot = tibble::tibble( "Dates" = forecast_dates, 
                               "Forecasting Model" = sRMSLE_at_day_t_1, 
                               "Forecasting Model, No Day-of-the-Week Effect" = sRMSLE_at_day_t_2 )

temp <- ggplot( data = data_to_plot ) + 
  xlab("Date") + ylab("Standardized Root Mean Squared Log Error (sRMSLE)") + 
  geom_line( aes( x = Dates, y = sRMSLE_at_day_t_1, color = "Forecasting Model" ) ) + 
  geom_line( aes( x = Dates, y = sRMSLE_at_day_t_2, color = "Forecasting Model, No Day-of-the-Week Effect" ) ) + 
  guides( color = guide_legend(byrow = TRUE, ncol = 1) ) + 

  scale_color_manual( values = c( "Forecasting Model" = "blue3", 
                                  "Forecasting Model, No Day-of-the-Week Effect" = "grey50" ) ) + 
  
  theme_classic() + 
  theme( axis.text = element_text(size = 16, color = "black"),
         axis.title = element_text(size = 18),
         axis.title.x = element_text(margin = margin(15, 0, 0, 0)),
         axis.title.y = element_text(margin = margin(0, 12.5, 0, 0)),
         legend.title.align = 0.5,
         legend.title = element_blank(),
         legend.text = element_text(size = 16),
         legend.position = "top", 
         legend.spacing.x = unit(0.25, 'cm'),
         legend.spacing.y = unit(0.25, 'cm') )

png( filename = paste0("Figures/sRMSLE_Selected_", days_ahead_to_predict, "_Days.png"), 
     res = 300, width = 1.5 * 1928, height = 1.5 * 1514, unit = "px" )
temp
dev.off()