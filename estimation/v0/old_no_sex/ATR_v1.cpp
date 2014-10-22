// Normal linear mixed model specified through sparse design matrices.
#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(Age1);         // Observations
  DATA_VECTOR(Age2);         // Observations
  DATA_VECTOR(Length1);         // Observations
  DATA_VECTOR(Length2);         // Observations

  PARAMETER(ln_kmean); // Fixed effects vector
  PARAMETER(ln_Linf); // Fixed effects vector
  PARAMETER(ln_L0); // Fixed effects vector
  PARAMETER_VECTOR(ln_kdev);    // Random effects vector
  PARAMETER(ln_sd_kdev);      // Random effect standard deviations
  PARAMETER(ln_sd_obs);      // Measurement standard deviation

  // Initialize stuff
  int Nindiv = Age1.size();
  Type ans = 0;
  Type mean = 0;
  Type Linf = exp(ln_Linf);
  Type L0 = exp(ln_L0);
  Type kmean = exp(ln_kmean);
  Type sd_kdev = exp(ln_sd_kdev);
  Type sd_obs = exp(ln_sd_obs);
  
  // Distribution of obs given random effects (x|u):
  vector<Type> Length1_hat(Nindiv);
  vector<Type> Length2_hat(Nindiv); 
  for(int i=0;i<Nindiv; i++){ 
    // Random effect probability
    ans -= dnorm(ln_kdev(i), mean, sd_kdev, 1);
    // Probability of first length measurement
    Length1_hat(i) = Linf - (Linf-L0) * exp( -kmean*Age1(i)*exp(ln_kdev(i)) );        //  
    ans -= dnorm(Length1(i), Length1_hat(i), sd_obs*Length1_hat(i), 1);
    // Probability of second length measurement
    Length2_hat(i) = Linf - (Linf-L0) * exp( -kmean*Age2(i)*exp(ln_kdev(i)) );        //  
    ans -= dnorm(Length2(i), Length2_hat(i), sd_obs*Length2_hat(i), 1);
  }
  
  ADREPORT( Linf );
  ADREPORT( L0 );
  ADREPORT( kmean );
  ADREPORT( sd_kdev );
  ADREPORT( sd_obs );
  return ans;
}

