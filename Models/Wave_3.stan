
functions { 

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
  int<lower=0> N;                // number of observations
  int timeNumeric[N];            // time (days)

  int dead[N];                    // daily observated mortality counts 
  int dowInt[N];                  // day-of-the-week effect (by day)
  real <lower=0> Expected;        // expected deaths in region J 
  int Nwaves;                     // number of skew normal curves in sum (fixed)
}


parameters {
  // Skew-normal parameters
  real A_gen[Nwaves];                 // transformed time / location parameter
  real<lower=0> B[Nwaves];            // scale parameter
  real<lower=0> C[Nwaves];            // severity parameter
  real K[Nwaves];                     // shape parameter

  // Other parameters
  real<lower=0> eta;                  // spark term 
  real<lower=0> inv_sqrt_phi;         // Neg-Bin: transformed overdispersion parameter
  real<lower=0> betaDay[6];           // day-of-the-week effect (excluding baseline day)
}


transformed parameters {

  real <lower=0> phi;                 // Neg-Bin: Overdispersion parameter
  real waves[N,Nwaves];               // each of the N waves (one term in sum)
  real <lower=0> lambda[N];           // intensity: with day-of-the-week effect
  real <lower=0> lambda_smooth[N];    // intensity: without day-of-the-week effect 
  real <lower=0> betaDayTrans[7];     // day-of-the-week effect (including baseline day)
  real A[Nwaves];                     // time / location parameter

  betaDayTrans[1] = betaDay[1];
  betaDayTrans[2] = betaDay[2];
  betaDayTrans[3] = betaDay[3];
  betaDayTrans[4] = 1.0;
  betaDayTrans[5] = betaDay[4];
  betaDayTrans[6] = betaDay[5];
  betaDayTrans[7] = betaDay[6];
  
  A[1] = 8 * A_gen[1] + 18353;       // as.numeric( as.Date("2020/04/01") ) 
  A[2] = 8 * A_gen[2] + 18475;       // as.numeric( as.Date("2020/08/01") ) 
  A[3] = 8 * A_gen[3] + 18597;       // as.numeric( as.Date("2020/12/01") )

  phi = square(inv(inv_sqrt_phi));

  for (Dday in 1:N) {

    waves[Dday,1] = exp( log( Expected ) + log( C[1] ) + my_skew_normal_lpdf(timeNumeric[Dday] | A[1], B[1], K[1]) );
    waves[Dday,2] = exp( log( Expected ) + log( C[2] ) + my_skew_normal_lpdf(timeNumeric[Dday] | A[2], B[2], K[2]) );
    waves[Dday,3] = exp( log( Expected ) + log( C[3] ) + my_skew_normal_lpdf(timeNumeric[Dday] | A[3], B[3], K[3]) );

    lambda[Dday] = exp( log(Expected) + log(eta) ) + 
      exp( log( Expected ) + log( C[1] ) + my_skew_normal_lpdf(timeNumeric[Dday] | A[1], B[1], K[1]) ) + 
      exp( log( Expected ) + log( C[2] ) + my_skew_normal_lpdf(timeNumeric[Dday] | A[2], B[2], K[2]) ) + 
      exp( log( Expected ) + log( C[3] ) + my_skew_normal_lpdf(timeNumeric[Dday] | A[3], B[3], K[3]) );

    lambda_smooth[Dday] = lambda[Dday];
    lambda[Dday] = lambda[Dday] * betaDayTrans[dowInt[Dday]];
    
  }
}


model {
  // Priors 
  A_gen ~ normal(0,1);               // transformed time / location parameter
  B ~ gamma(7,0.5);                  // scale parameter
  C ~ gamma(0.4, 0.25);              // severity parameter
  K ~ normal(0,2);                   // shape parameter

  betaDay ~ gamma(1,1);              // day-of-the-week effect
  inv_sqrt_phi ~ exponential(10);    // Neg-Bin: transformed overdispersion parameter
  eta ~ exponential(23000);          // spark term

  // Negative binomial likelihood
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
    18579, 18580, 18581, 
    18582, 18583, 18584, 18585, 
    18586, 18587, 18588, 18589, 
    18590, 18591, 18592, 18593, 
    18594, 18595, 18596, 18597, 
    18598, 18599, 18600, 18601, 
    18602, 18603, 18604, 18605, 
    18606, 18607, 18608, 18609,
    18610 };
  
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
    5, 6, 7, 
    1, 2, 3, 4, 5, 6, 7, 
    1, 2, 3, 4, 5, 6, 7, 
    1, 2, 3, 4, 5, 6, 7, 
    1, 2, 3, 4, 5, 6, 7,
    1 };
    
    for ( Dday in 1:63 ) {

      waves_rep_1[Dday,1] = exp( log( Expected ) + log( C[1] ) + my_skew_normal_lpdf(timeNumeric_rep_1[Dday] | A[1], B[1], K[1]) );
      waves_rep_1[Dday,2] = exp( log( Expected ) + log( C[2] ) + my_skew_normal_lpdf(timeNumeric_rep_1[Dday] | A[2], B[2], K[2]) );
      waves_rep_1[Dday,3] = exp( log( Expected ) + log( C[3] ) + my_skew_normal_lpdf(timeNumeric_rep_1[Dday] | A[3], B[3], K[3]) );

    lambda_rep_1[Dday] = exp( log(Expected) + log(eta) ) + 
      exp( log( Expected ) + log( C[1] ) + my_skew_normal_lpdf(timeNumeric_rep_1[Dday] | A[1], B[1], K[1]) ) + 
      exp( log( Expected ) + log( C[2] ) + my_skew_normal_lpdf(timeNumeric_rep_1[Dday] | A[2], B[2], K[2]) ) + 
      exp( log( Expected ) + log( C[3] ) + my_skew_normal_lpdf(timeNumeric_rep_1[Dday] | A[3], B[3], K[3]) );
      
    lambda_smooth_rep_1[Dday] = lambda_rep_1[Dday];
    lambda_rep_1[Dday] = lambda_rep_1[Dday] * betaDayTrans[dowInt_rep_1[Dday]];
    
    dead_rep_1[Dday] = neg_binomial_2_rng(lambda_rep_1[Dday], phi); 
  }

                 
  for ( Dday in 1:32 ) {

    waves_rep_2[Dday,1] = exp( log( Expected ) + log( C[1] ) + my_skew_normal_lpdf(timeNumeric_rep_2[Dday] | A[1], B[1], K[1]) );
    waves_rep_2[Dday,2] = exp( log( Expected ) + log( C[2] ) + my_skew_normal_lpdf(timeNumeric_rep_2[Dday] | A[2], B[2], K[2]) );
    waves_rep_2[Dday,3] = exp( log( Expected ) + log( C[3] ) + my_skew_normal_lpdf(timeNumeric_rep_2[Dday] | A[3], B[3], K[3]) );

    lambda_rep_2[Dday] = exp( log(Expected) + log(eta) ) + 
      exp( log( Expected ) + log( C[1] ) + my_skew_normal_lpdf(timeNumeric_rep_2[Dday] | A[1], B[1], K[1]) ) + 
      exp( log( Expected ) + log( C[2] ) + my_skew_normal_lpdf(timeNumeric_rep_2[Dday] | A[2], B[2], K[2]) ) + 
      exp( log( Expected ) + log( C[3] ) + my_skew_normal_lpdf(timeNumeric_rep_2[Dday] | A[3], B[3], K[3]) );
      
    lambda_smooth_rep_2[Dday] = lambda_rep_2[Dday];
    lambda_rep_2[Dday] = lambda_rep_2[Dday] * betaDayTrans[dowInt_rep_2[Dday]];
    
    dead_rep_2[Dday] = neg_binomial_2_rng(lambda_rep_2[Dday], phi); 
  }
  
}
