functions
{
  real get_y_predicted(real r1, real r2, real p1, real p2, real a, real ar, real ap, real b, real c)
  {
    real numerator;
    real denominator;
    
    numerator = r1^ar + c*p2^ap ;
    denominator = r2^ar + c*p1^ap;
    
    if(numerator <= 0 || denominator <= 0)
    {
      return not_a_number();
    }
    
    return log(numerator/denominator) + log(b);
  }
}
data 
{
  int<lower=1> N; //Number of subjects
  int<lower=1> unique_subject_IDs[N];
  int<lower=1> num_points; //Total number of data points across all subjects (not necessarily evenly divided between subjects)
  int<lower=1> subject_IDs[num_points];
  real<lower=0> b1[num_points];
  real<lower=0> b2[num_points];
  real<lower=0> r1[num_points];
  real<lower=0> r2[num_points];
  real<lower=0> p1[num_points];
  real<lower=0> p2[num_points];
}
transformed data
{
  int<lower = 0, upper = N> subject_indices[num_points]; #map between points and subject indices 
  real y_actual[num_points];
  
  for(p in 1:num_points)
  {
    y_actual[p] = log(b1[p]/b2[p]);
    
    for(k in 1:N)
    {
      if(subject_IDs[p] == unique_subject_IDs[k])
      {
        subject_indices[p] = k;
      }
    }
  }
}
parameters 
{ 
  real<lower=0, upper=10> sigma[N];

  real ar_mean;
  real<lower=0> ar_sd;
  real<lower=0> ar[N];
  
  real ap_mean;
  real<lower=0> ap_sd;
  real<lower=0> ap[N];
  
  real b_mean;
  real<lower=0> b_sd;
  real<lower=0> b[N];
  
  real c_mean;
  real<lower=0> c_sd;
  real<lower=0> c[N];
}
model 
{
  ar_mean ~ normal(.8, 10);
  ar_sd ~ cauchy(0, 10);
  
  ap_mean ~ normal(.8, 10);
  ap_sd ~ cauchy(0, 10);
  
  b_mean ~ normal(1, 10);
  b_sd ~ cauchy(0, 10);
  
  c_mean ~ normal(1.5, 10);
  c_sd ~ cauchy(0, 10);
  
  for(s in 1:N)
  {
    ar[s] ~ normal(ar_mean, ar_sd);
    ap[s] ~ normal(ap_mean, ap_sd);
    b[s] ~ normal(b_mean, b_sd);
    c[s] ~ normal(c_mean, c_sd);
  }
  
  for(p in 1:num_points)
  {
    real y_predicted;
    y_predicted = get_y_predicted(r1[p], 
                                  r2[p], 
                                  p1[p], 
                                  p2[p], 
                                  0, 
                                  ar[subject_indices[p]], 
                                  ap[subject_indices[p]], 
                                  b[subject_indices[p]], 
                                  c[subject_indices[p]]);
    if(is_nan(y_predicted))
    {
      target += -10000;
    }
    else
    {
      y_actual[p] ~ normal(y_predicted, sigma[subject_indices[p]]);
    }
  }
}
generated quantities
{
  vector[num_points] log_lik;
  real total_log_lik;
  
  total_log_lik = 0;
  for(p in 1:num_points)
  {
    real y_predicted;
    y_predicted = get_y_predicted(r1[p], 
                                  r2[p], 
                                  p1[p], 
                                  p2[p], 
                                  0, 
                                  ar[subject_indices[p]], 
                                  ap[subject_indices[p]], 
                                  b[subject_indices[p]], 
                                  c[subject_indices[p]]);
    if(is_nan(y_predicted))
    {
      log_lik[p] = -10000;
    }
    else
    {
      log_lik[p] = normal_lpdf(y_actual[p] | y_predicted, sigma[subject_indices[p]]);
    }
    
    total_log_lik = total_log_lik + log_lik[p];
  }
}

