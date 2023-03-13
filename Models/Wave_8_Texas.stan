
functions { 
  
  // Writing skew normal density in terms of normal_lcdf
  // Emperically observed to have better numerical properties
  // than the skew_normal density implementation in Stan
  real my_skew_normal_lpdf( real time_index, 
                            real location, 
                            real scale, 
                            real shape ) {
    
    return log(2) - log(scale) + 
           std_normal_lpdf( ( time_index - location ) / scale ) + 
           normal_lcdf( shape * ( time_index - location ) / scale | 0, 1 );
           
  }
  
}


data {
  int<lower=0> N;               // number of observations
  int timeNumeric[N];           // time (days) 
  
  int dead[N];                  // daily observated mortality counts 
  int dowInt[N];                // day-of-the-week effect (by day)
  real <lower=0> Expected;      // expected deaths in region J
  int Nwaves;                   // number of skew normal curves in sum (fixed)
}

parameters {
  // Skew-normal parameters
  real A_gen[Nwaves];                 // transformed time / location parameter
  real<lower=0> B[Nwaves];            // scale parameter
  real<lower=0> C[Nwaves];            // severity parameter
  real K[Nwaves];                     // shape parameter
  
  // Other parameters
  real<lower=0> eta_1;                // spark term (first period)
  real<lower=0> eta_2;                // spark term (second period)
  real<lower=0> inv_sqrt_phi;         // Neg-Bin: transformed overdispersion parameter
  real<lower=0> betaDay_1[6];         // day-of-the-week effect (excluding baseline day, first period)
  real<lower=0> betaDay_2[6];         // day-of-the-week effect (excluding baseline day, second period)
}

transformed parameters {
  
  real <lower=0> phi;                  // Neg-Bin: overdispersion parameter
  real waves[N,Nwaves];                // each of the N waves (one term in sum)
  real <lower=0> lambda[N];            // intensity: with day-of-the-week effect
  real <lower=0> lambda_smooth[N];     // intensity: without day-of-the-week effect
  real <lower=0> betaDayTrans_1[7];    // day-of-the-week effect (including baseline day, first period)
  real <lower=0> betaDayTrans_2[7];    // day-of-the-week effect (including baseline day, second period)
  real A[Nwaves];
  
  betaDayTrans_1[1] = betaDay_1[1]; betaDayTrans_2[1] = betaDay_2[1];
  betaDayTrans_1[2] = betaDay_1[2]; betaDayTrans_2[2] = betaDay_2[2];
  betaDayTrans_1[3] = betaDay_1[3]; betaDayTrans_2[3] = betaDay_2[3];
  betaDayTrans_1[4] = 1.0;          betaDayTrans_2[4] = 1.0;
  betaDayTrans_1[5] = betaDay_1[4]; betaDayTrans_2[5] = betaDay_2[4];
  betaDayTrans_1[6] = betaDay_1[5]; betaDayTrans_2[6] = betaDay_2[5];
  betaDayTrans_1[7] = betaDay_1[6]; betaDayTrans_2[7] = betaDay_2[6];
  
  A[1] = 8 * A_gen[1] + 18353;       // as.numeric( as.Date("2020/04/01") ) 
  A[2] = 8 * A_gen[2] + 18475;       // as.numeric( as.Date("2020/08/01") ) 
  A[3] = 8 * A_gen[3] + 18597;       // as.numeric( as.Date("2020/12/01") )
  A[4] = 8 * A_gen[4] + 18718;       // as.numeric( as.Date("2021/04/01") )
  A[5] = 8 * A_gen[5] + 18840;       // as.numeric( as.Date("2021/08/01") )
  A[6] = 8 * A_gen[6] + 18962;       // as.numeric( as.Date("2021/12/01") )
  A[7] = 8 * A_gen[7] + 19052;       // as.numeric( as.Date("2022/03/01") )
  A[8] = 8 * A_gen[8] + 19144;       // as.numeric( as.Date("2022/06/01") )
  
  phi = square(inv(inv_sqrt_phi));
  
  for (Dday in 1:N) {
    
    waves[Dday,1] = exp( log( Expected ) + log( C[1] ) + my_skew_normal_lpdf(timeNumeric[Dday] | A[1], B[1], K[1]) );
    waves[Dday,2] = exp( log( Expected ) + log( C[2] ) + my_skew_normal_lpdf(timeNumeric[Dday] | A[2], B[2], K[2]) );
    waves[Dday,3] = exp( log( Expected ) + log( C[3] ) + my_skew_normal_lpdf(timeNumeric[Dday] | A[3], B[3], K[3]) );
    waves[Dday,4] = exp( log( Expected ) + log( C[4] ) + my_skew_normal_lpdf(timeNumeric[Dday] | A[4], B[4], K[4]) );
    waves[Dday,5] = exp( log( Expected ) + log( C[5] ) + my_skew_normal_lpdf(timeNumeric[Dday] | A[5], B[5], K[5]) );
    waves[Dday,6] = exp( log( Expected ) + log( C[6] ) + my_skew_normal_lpdf(timeNumeric[Dday] | A[6], B[6], K[6]) );
    waves[Dday,7] = exp( log( Expected ) + log( C[7] ) + my_skew_normal_lpdf(timeNumeric[Dday] | A[7], B[7], K[7]) );
    waves[Dday,8] = exp( log( Expected ) + log( C[8] ) + my_skew_normal_lpdf(timeNumeric[Dday] | A[8], B[8], K[8]) );
  }
  
  // 'which( nCov_Merged[nCov_Merged$reporting_unit == "Texas",]$time == as.Date("2021-07-01") )' outputs 488
  // (after running up to line 437 in 8_Waves.R and fully processing the data set for input into Stan)
  for (Dday in 1:487) {
    
    lambda[Dday] = exp( log( Expected ) + log( eta_1 ) )  + 
      exp( log( Expected ) + log( C[1] ) + my_skew_normal_lpdf(timeNumeric[Dday] | A[1], B[1], K[1]) ) + 
      exp( log( Expected ) + log( C[2] ) + my_skew_normal_lpdf(timeNumeric[Dday] | A[2], B[2], K[2]) ) + 
      exp( log( Expected ) + log( C[3] ) + my_skew_normal_lpdf(timeNumeric[Dday] | A[3], B[3], K[3]) ) + 
      exp( log( Expected ) + log( C[4] ) + my_skew_normal_lpdf(timeNumeric[Dday] | A[4], B[4], K[4]) ) + 
      exp( log( Expected ) + log( C[5] ) + my_skew_normal_lpdf(timeNumeric[Dday] | A[5], B[5], K[5]) ) + 
      exp( log( Expected ) + log( C[6] ) + my_skew_normal_lpdf(timeNumeric[Dday] | A[6], B[6], K[6]) ) + 
      exp( log( Expected ) + log( C[7] ) + my_skew_normal_lpdf(timeNumeric[Dday] | A[7], B[7], K[7]) ) + 
      exp( log( Expected ) + log( C[8] ) + my_skew_normal_lpdf(timeNumeric[Dday] | A[8], B[8], K[8]) ); 
    
    lambda_smooth[Dday] = lambda[Dday];
    lambda[Dday] = lambda[Dday] * betaDayTrans_1[dowInt[Dday]];
  }
  
  for (Dday in 488:N) {
    
    lambda[Dday] = exp( log( Expected ) + log( eta_2 ) ) + 
      exp( log( Expected ) + log( C[1] ) + my_skew_normal_lpdf(timeNumeric[Dday] | A[1], B[1], K[1]) ) + 
      exp( log( Expected ) + log( C[2] ) + my_skew_normal_lpdf(timeNumeric[Dday] | A[2], B[2], K[2]) ) + 
      exp( log( Expected ) + log( C[3] ) + my_skew_normal_lpdf(timeNumeric[Dday] | A[3], B[3], K[3]) ) + 
      exp( log( Expected ) + log( C[4] ) + my_skew_normal_lpdf(timeNumeric[Dday] | A[4], B[4], K[4]) ) + 
      exp( log( Expected ) + log( C[5] ) + my_skew_normal_lpdf(timeNumeric[Dday] | A[5], B[5], K[5]) ) + 
      exp( log( Expected ) + log( C[6] ) + my_skew_normal_lpdf(timeNumeric[Dday] | A[6], B[6], K[6]) ) + 
      exp( log( Expected ) + log( C[7] ) + my_skew_normal_lpdf(timeNumeric[Dday] | A[7], B[7], K[7]) ) + 
      exp( log( Expected ) + log( C[8] ) + my_skew_normal_lpdf(timeNumeric[Dday] | A[8], B[8], K[8]) ); 
    
    
    lambda_smooth[Dday] = lambda[Dday];
    lambda[Dday] = lambda[Dday] * betaDayTrans_2[dowInt[Dday]];
  }
  
}

model {
  // Priors 
  A_gen ~ normal(0,1);               // transformed time / location parameter   
  B ~ gamma(7,0.5);                  // scale parameter
  C ~ gamma(0.4,0.25);               // severity parameter
  K ~ normal(0,2);                   // shape parameter
  
  betaDay_1 ~ gamma(1,1);            // day-of-the-week effect (first period)
  betaDay_2 ~ gamma(1,1);            // day-of-the-week effect (second period)
  inv_sqrt_phi ~ exponential(10);    // Neg-Bin: overdispersion parameter 
  eta_1 ~ exponential(23000);        // spark term (first period)
  eta_2 ~ exponential(23000);        // spark term (second period)
  
  // Negative binomial ikelihood
  for (Dday in 1:N) { 
    dead[Dday] ~ neg_binomial_2(lambda[Dday], phi); 
  }
}

generated quantities { 
  
  int timeNumeric_rep_1[63]; int timeNumeric_rep_2[32];
  int dowInt_rep_1[63]; int dowInt_rep_2[32]; 
  
  real waves_rep_1[63,Nwaves]; real waves_rep_2[32,Nwaves];
  real lambda_rep_1[63]; real lambda_rep_2[32];
  real lambda_smooth_rep_1[63]; real lambda_smooth_rep_2[32];
  int dead_rep_1[63]; int dead_rep_2[32];
  
  timeNumeric_rep_1 = { 
    18290, 18291, 18292, 
    18293, 18294, 18295, 18296, 
    18297, 18298, 18299, 18300, 
    18301, 18302, 18303, 18304, 
    18305, 18306, 18307, 18308, 
    18309, 18310, 18311, 18312, 
    18313, 18314, 18315, 18316, 
    18317, 18318, 18319, 18320, 
    18321, 18322, 18323, 18324, 
    18325, 18326, 18327, 18328, 
    18329, 18330, 18331, 18332, 
    18333, 18334, 18335, 18336,
    18337, 18338, 18339, 18340,
    18341, 18342, 18343, 18344,
    18345, 18346, 18347, 18348,
    18349, 18350, 18351, 18352 };
  
  timeNumeric_rep_2 = { 
    19144, 19145, 19146, 19147,
    19148, 19149, 19150, 19151, 
    19152, 19153, 19154, 19155, 
    19156, 19157, 19158, 19159, 
    19160, 19161, 19162, 19163, 
    19164, 19165, 19166, 19167, 
    19168, 19169, 19170, 19171, 
    19172, 19173, 19174, 19175 };
  
  dowInt_rep_1 = { 
    3, 4, 5,
    6, 7, 1, 2, 
    3, 4, 5, 6, 
    7, 1, 2, 3, 
    4, 5, 6, 7, 
    1, 2, 3, 4, 
    5, 6, 7, 1, 
    2, 3, 4, 5, 
    6, 7, 1, 2, 
    3, 4, 5, 6, 
    7, 1, 2, 3, 
    4, 5, 6, 7, 
    1, 2, 3, 4, 
    5, 6, 7, 1, 
    2, 3, 4, 5, 
    6, 7, 1, 2 };
  
  
  dowInt_rep_2 = { 
    3, 4, 5, 6, 7,
    1, 2, 3, 4, 5, 6, 7, 
    1, 2, 3, 4, 5, 6, 7, 
    1, 2, 3, 4, 5, 6, 7, 
    1, 2, 3, 4, 5, 6 };
  
  for ( Dday in 1:63 ) {
    
    waves_rep_1[Dday,1] = exp( log( Expected ) + log( C[1] ) + my_skew_normal_lpdf(timeNumeric_rep_1[Dday] | A[1], B[1], K[1]) );
    waves_rep_1[Dday,2] = exp( log( Expected ) + log( C[2] ) + my_skew_normal_lpdf(timeNumeric_rep_1[Dday] | A[2], B[2], K[2]) );
    waves_rep_1[Dday,3] = exp( log( Expected ) + log( C[3] ) + my_skew_normal_lpdf(timeNumeric_rep_1[Dday] | A[3], B[3], K[3]) );
    waves_rep_1[Dday,4] = exp( log( Expected ) + log( C[4] ) + my_skew_normal_lpdf(timeNumeric_rep_1[Dday] | A[4], B[4], K[4]) );
    waves_rep_1[Dday,5] = exp( log( Expected ) + log( C[5] ) + my_skew_normal_lpdf(timeNumeric_rep_1[Dday] | A[5], B[5], K[5]) );
    waves_rep_1[Dday,6] = exp( log( Expected ) + log( C[6] ) + my_skew_normal_lpdf(timeNumeric_rep_1[Dday] | A[6], B[6], K[6]) );
    waves_rep_1[Dday,7] = exp( log( Expected ) + log( C[7] ) + my_skew_normal_lpdf(timeNumeric_rep_1[Dday] | A[7], B[7], K[7]) );
    waves_rep_1[Dday,8] = exp( log( Expected ) + log( C[8] ) + my_skew_normal_lpdf(timeNumeric_rep_1[Dday] | A[8], B[8], K[8]) );
    
    
    lambda_rep_1[Dday] = exp( log(Expected) + log(eta_1) ) + 
      exp( log( Expected ) + log( C[1] ) + my_skew_normal_lpdf(timeNumeric_rep_1[Dday] | A[1], B[1], K[1]) ) + 
      exp( log( Expected ) + log( C[2] ) + my_skew_normal_lpdf(timeNumeric_rep_1[Dday] | A[2], B[2], K[2]) ) + 
      exp( log( Expected ) + log( C[3] ) + my_skew_normal_lpdf(timeNumeric_rep_1[Dday] | A[3], B[3], K[3]) ) + 
      exp( log( Expected ) + log( C[4] ) + my_skew_normal_lpdf(timeNumeric_rep_1[Dday] | A[4], B[4], K[4]) ) + 
      exp( log( Expected ) + log( C[5] ) + my_skew_normal_lpdf(timeNumeric_rep_1[Dday] | A[5], B[5], K[5]) ) + 
      exp( log( Expected ) + log( C[6] ) + my_skew_normal_lpdf(timeNumeric_rep_1[Dday] | A[6], B[6], K[6]) ) + 
      exp( log( Expected ) + log( C[7] ) + my_skew_normal_lpdf(timeNumeric_rep_1[Dday] | A[7], B[7], K[7]) ) + 
      exp( log( Expected ) + log( C[8] ) + my_skew_normal_lpdf(timeNumeric_rep_1[Dday] | A[8], B[8], K[8]) );
    
    
    lambda_smooth_rep_1[Dday] = lambda_rep_1[Dday];
    lambda_rep_1[Dday] = lambda_rep_1[Dday] * betaDayTrans_1[dowInt_rep_1[Dday]];
    
    dead_rep_1[Dday] = neg_binomial_2_rng(lambda_rep_1[Dday], phi); 
  }
  
  
  for ( Dday in 1:32 ) {
    
    waves_rep_2[Dday,1] = exp( log( Expected ) + log( C[1] ) + my_skew_normal_lpdf(timeNumeric_rep_2[Dday] | A[1], B[1], K[1]) );
    waves_rep_2[Dday,2] = exp( log( Expected ) + log( C[2] ) + my_skew_normal_lpdf(timeNumeric_rep_2[Dday] | A[2], B[2], K[2]) );
    waves_rep_2[Dday,3] = exp( log( Expected ) + log( C[3] ) + my_skew_normal_lpdf(timeNumeric_rep_2[Dday] | A[3], B[3], K[3]) );
    waves_rep_2[Dday,4] = exp( log( Expected ) + log( C[4] ) + my_skew_normal_lpdf(timeNumeric_rep_2[Dday] | A[4], B[4], K[4]) );
    waves_rep_2[Dday,5] = exp( log( Expected ) + log( C[5] ) + my_skew_normal_lpdf(timeNumeric_rep_2[Dday] | A[5], B[5], K[5]) );
    waves_rep_2[Dday,6] = exp( log( Expected ) + log( C[6] ) + my_skew_normal_lpdf(timeNumeric_rep_2[Dday] | A[6], B[6], K[6]) );
    waves_rep_2[Dday,7] = exp( log( Expected ) + log( C[7] ) + my_skew_normal_lpdf(timeNumeric_rep_2[Dday] | A[7], B[7], K[7]) );
    waves_rep_2[Dday,8] = exp( log( Expected ) + log( C[8] ) + my_skew_normal_lpdf(timeNumeric_rep_2[Dday] | A[8], B[8], K[8]) );
    
    lambda_rep_2[Dday] = exp( log( Expected ) + log( eta_2 ) ) + 
      exp( log( Expected ) + log( C[1] ) + my_skew_normal_lpdf(timeNumeric_rep_2[Dday] | A[1], B[1], K[1]) ) + 
      exp( log( Expected ) + log( C[2] ) + my_skew_normal_lpdf(timeNumeric_rep_2[Dday] | A[2], B[2], K[2]) ) + 
      exp( log( Expected ) + log( C[3] ) + my_skew_normal_lpdf(timeNumeric_rep_2[Dday] | A[3], B[3], K[3]) ) + 
      exp( log( Expected ) + log( C[4] ) + my_skew_normal_lpdf(timeNumeric_rep_2[Dday] | A[4], B[4], K[4]) ) + 
      exp( log( Expected ) + log( C[5] ) + my_skew_normal_lpdf(timeNumeric_rep_2[Dday] | A[5], B[5], K[5]) ) + 
      exp( log( Expected ) + log( C[6] ) + my_skew_normal_lpdf(timeNumeric_rep_2[Dday] | A[6], B[6], K[6]) ) + 
      exp( log( Expected ) + log( C[7] ) + my_skew_normal_lpdf(timeNumeric_rep_2[Dday] | A[7], B[7], K[7]) ) + 
      exp( log( Expected ) + log( C[8] ) + my_skew_normal_lpdf(timeNumeric_rep_2[Dday] | A[8], B[8], K[8]) ); 
    
    lambda_smooth_rep_2[Dday] = lambda_rep_2[Dday];
    lambda_rep_2[Dday] = lambda_rep_2[Dday] * betaDayTrans_2[dowInt_rep_2[Dday]];
    
    dead_rep_2[Dday] = neg_binomial_2_rng(lambda_rep_2[Dday], phi); 
  }
}
