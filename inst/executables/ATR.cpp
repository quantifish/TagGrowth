// Tag recapture model
#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  // Options
  DATA_FACTOR(Options);
  
  // Observations
  DATA_FACTOR(iAge1);    // Age of individual at time 1 (with units days, weeks, months, or years, rounded to nearest integer)
  DATA_VECTOR(iLiberty); // Time at liberty of individual (with units days, weeks, months, or years)
  DATA_VECTOR(Length1);  // Length of individual at time 1
  DATA_VECTOR(Length2);  // Length of individual at time 2
  DATA_FACTOR(Sex);      // Sex of individual
  DATA_FACTOR(Time0);    // Time at which individual was born (time 0, with units days, weeks, months, or years)
  DATA_FACTOR(Time1);    // Time at which individual was caught/tagged (time 1, with units days, weeks, months, or years)
  //DATA_FACTOR(Time2);    // Time at which individual was recaptured (time 2, with units days, weeks, months, or years)
  DATA_FACTOR(Year0);    // Year in which individual was born (time 0)
  DATA_FACTOR(Year1);    // Year in which individual was caught/tagged (time 1)
  //DATA_FACTOR(Year2);    // Year in which individual was recaptured (time 2)
  DATA_FACTOR(Area1);    // Area that individual was in at time 1 

  // Parameters
  PARAMETER_VECTOR(ln_gamma);   // Fixed effect vector (2)
  PARAMETER(logit_psi);         // Fixed effect (1)
  PARAMETER_VECTOR(L0);         // Fixed effect vector (2, sex-specific)
  PARAMETER_VECTOR(ln_bmean);   // Random effects vector (2, sex-specific)
  PARAMETER_VECTOR(ln_bdev);    // Random effects vector (315, individual)
  PARAMETER_VECTOR(ln_sd_bdev); // Random effect standard deviation (2, sex-specific)
  PARAMETER(ln_sd_obs);         // Measurement standard deviation (1)
  PARAMETER_VECTOR(z1);         // Random effects vector (N, individual)
  PARAMETER_VECTOR(z2);         // Random effects vector (N, individual)
  PARAMETER(ln_sd_z);           // Random effects standard deviation (1)
  PARAMETER_VECTOR(ln_ydev);    // Random effects vector (40, year)
  PARAMETER(ln_sd_ydev);        // Random effects standard deviation (1)
  PARAMETER_VECTOR(ln_xdev);    // Random effects vector (40, area)
  PARAMETER(ln_sd_xdev);        // Random effects standard deviation (1)

  // Initialize dimension variables:
  int Nindiv = iAge1.size();    // Number of individuals
  int Nsex = 2;                 // Number of sexes

  // Initialize population-level parameters:
  Type psi = 1 / (1 + exp(-logit_psi));
  Type sd_obs = exp(ln_sd_obs);
  Type sd_z = exp(ln_sd_z);
  Type sd_ydev = exp(ln_sd_ydev);

  // Initialize sex-specific parameters:
  vector<Type> gamma(Nsex);
  vector<Type> bmean(Nsex);
  vector<Type> sd_bdev(Nsex);
  vector<Type> amean(Nsex);
  vector<Type> Linf(Nsex);
  bmean   = exp(ln_bmean);
  sd_bdev = exp(ln_sd_bdev);
  gamma   = exp(ln_gamma);
  amean   = gamma * pow(bmean, psi);
  Linf = (gamma * pow(bmean, psi)) / bmean;
  
  // Initialize some computational variables:
  int sex;
  Type ans  = 0.;
  Type sumj = 0.;
  
  // Distribution of observations x given random effects u (x|u):
  vector<Type> a_indiv(Nindiv);
  vector<Type> b_indiv(Nindiv);
  vector<Type> Length1_hat(Nindiv);
  vector<Type> Length2_hat(Nindiv);

  // Prior penalty for Linf for females and males:
  //Type Linf_cv = 0.102;
  //Type pLinfF  = 180.20;
  //Type pLinfM  = 169.07;
  //vector<Type> pLinf_sd(Nsex);
  //pLinf_sd(0) = Linf_cv * pLinfF; // sd=cv*mu
  //pLinf_sd(1) = Linf_cv * pLinfM; // sd=cv*mu
  //ans -= dnorm( Linf(0), pLinfF, pLinf_sd(0), 1 );
  //ans -= dnorm( Linf(1), pLinfM, pLinf_sd(1), 1 );

  // Prior penalty for L0 for females and males
  //ans -= dnorm( L0(0), Type(-0.34), Type(1), 1 );
  //ans -= dnorm( L0(1), Type(3.98), Type(1), 1 );

  // Prior penalty for sd_ydev
  //ans -= dgamma( sd_ydev, Type(1.), Type(1/500.), 1 );

  // Time varying individual stuff
  vector<Type> sd_z1(Nindiv);
  vector<Type> sd_z2(Nindiv);

  // Year effects
  int Nyears = ln_ydev.size(); // Number of years
  int time_step = 52;
  int time, year;
  // Random effect probability of each year
  if (Options(0) == 1)
  {
    for (int y = 0; y < Nyears; y++) { ans -= dnorm( ln_ydev(y), Type(0.), sd_ydev, true ); }
  }
  vector<Type> isYearUsedTF(Nyears);
  isYearUsedTF.setZero();
  
  // Area effects
  int Narea = ln_xdev.size();
  int area;
  Type sd_xdev = exp(ln_sd_xdev);
  // Random effect probability of each area
  //if (Options(1) == 1)
  //{
  //  for (int a = 0; a < Narea; a++) { ans -= dnorm( ln_xdev(a), Type(0.), sd_xdev, true ); }
  //}
  
  // Loop over each individual in the data set
  for (int i = 0; i < Nindiv; i++)
  {
    sex = Sex(i) - 1;                          // Sex of the individual
    year = Year0(i);                           // The (index) year that the individual was born
    area = Area1(i) - 1;                       // Location of the individual at time 1
    b_indiv(i) = bmean(sex) * exp(ln_bdev(i)); // Value for b_indiv
    a_indiv(i) = gamma(sex) * pow(b_indiv(i), psi); // Derived value for a_indiv
    // Random effect probability of bdevs
    if (Options(2) == 1)
    {
      ans -= dnorm( ln_bdev(i), Type(0.), sd_bdev(sex), true );
    }
    sumj = Type(0.);
    for (int j = 0; j < (iAge1(i)-1); j++)
    {
      // This piece of code gives us the year (from 0 to 39, ref to the years
      // 1973/74 to 2012/13) that the fish is in at each time-step.  This is
      // required because if a time-step other than annual is used then we need
      // to be referencing the correct year-effect parameter.
      time = Time0(i) + j;
      // THE ERROR IS IN HERE
      //if ( fmod (time, time_step) == 0. ) { year += 1; }
      // THE ERROR IS IN HERE, I've commented out year and area effects below too.
      sumj += gamma(sex) * exp(-b_indiv(i) * j);// * exp(ln_ydev(year)) * exp(ln_xdev(area));
      isYearUsedTF( year ) = 1;
    }
    Length1_hat(i) = ( L0(sex) * exp(-b_indiv(i) * iAge1(i)) ) + ( pow(b_indiv(i), psi-1) * (1-exp(-b_indiv(i))) * sumj );
    Length1_hat(i) += z1(i);
    // Time-variation probability from birth to first capture
    sumj = Type(0.0001);
    for (int j = 0; j < (iAge1(i)-1); j++) { sumj += exp(Type(2.0) * -b_indiv(i) * j); }
    sd_z1(i) = sd_z * (pow(b_indiv(i), psi-1) * (1-exp(-b_indiv(i)))) * pow(sumj,0.5);
    if (Options(3) == 1)
    {
      ans -= dnorm( z1(i), Type(0.), sd_z1(i), true );
    }
    ans -= dnorm( Length1(i), Length1_hat(i), sd_obs * Length1_hat(i), true );
    // Probability of second length measurement
    year = Year1(i); // The (index) year that the individual was first captured
    sumj = Type(0.0);
    for (int j = 0; j < (iLiberty(i)-1); j++)
    {
      time = Time1(i) + j;
      if ( fmod (time, time_step) == 0. ) { year += 1; }
      sumj += gamma(sex) * exp(-b_indiv(i) * j) * exp(ln_ydev(year)) * exp(ln_xdev(area));
      isYearUsedTF( year ) = 1;
    }
    Length2_hat(i) = ( Length1_hat(i) * exp(-b_indiv(i) * iLiberty(i)) ) + ( pow(b_indiv(i), psi-1) * (1 - exp(-b_indiv(i))) * sumj );
    Length2_hat(i) += z2(i);
    // Time-variation probability from first capture to second capture
    sumj = Type(0.0001);
    for (int j = 0; j < (iLiberty(i)-1); j++) { sumj += exp(Type(2.0) * -b_indiv(i) * j); }
    sd_z2(i) = sd_z * (pow(b_indiv(i), psi-1) * (1-exp(-b_indiv(i)))) * pow(sumj,0.5);
    if (Options(3) == 1)
    {
      ans -= dnorm( z2(i), Type(0.0), sd_z2(i), true );
    }
    ans -= dnorm( Length2(i), Length2_hat(i), sd_obs * Length2_hat(i), true );
  }

  // REPORT DIAGNOSTICS
  REPORT( b_indiv );
  REPORT( a_indiv );  
  REPORT( Linf );
  
  // Append outputs to report
  ADREPORT( gamma );
  ADREPORT( psi );
  ADREPORT( L0 );
  ADREPORT( amean );
  ADREPORT( bmean );
  ADREPORT( sd_bdev );
  ADREPORT( sd_obs );
  ADREPORT( sd_z );
  //ADREPORT( sd_ydev );
  //ADREPORT( sd_xdev );
  ADREPORT( Length1_hat );
  ADREPORT( Length2_hat );
  ADREPORT( a_indiv );
  ADREPORT( b_indiv );
  ADREPORT( ln_bdev );
  ADREPORT( sd_z1 );
  ADREPORT( sd_z2 );
  //ADREPORT( ln_ydev );
  //ADREPORT( ln_xdev );

  return ans;
}
