functions
{
  real get_y_predicted(real r1, real r2, real p1, real p2, real a, real ar, real ap, real b, real c)
  {
    real numerator;
    real denominator;
    
    numerator = r1 + p2;
    denominator = r2 + p1;
    
    if(numerator <= 0 || denominator <= 0)
    {
      return not_a_number();
    }
    
    return a*log(numerator/denominator) + log(b);
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

  real a_mean;
  real<lower=0> a_sd;
  real<lower=0> a[N];
  
  real b_mean;
  real<lower=0> b_sd;
  real<lower=0> b[N];
}
model 
{
  a_mean ~ normal(.8, 10);
  a_sd ~ cauchy(0, 10);
  
  b_mean ~ normal(1, 10);
  b_sd ~ cauchy(0, 10);
  
  for(s in 1:N)
  {
    a[s] ~ normal(a_mean, a_sd);
    b[s] ~ normal(b_mean, b_sd);
  }
  
  for(p in 1:num_points)
  {
    real y_predicted;
    y_predicted = get_y_predicted(r1[p], 
                                  r2[p], 
                                  p1[p], 
                                  p2[p], 
                                  a[subject_indices[p]], 
                                  0, 
                                  0, 
                                  b[subject_indices[p]], 
                                  0);
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
                                  a[subject_indices[p]], 
                                  0, 
                                  0, 
                                  b[subject_indices[p]], 
                                  0);
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

