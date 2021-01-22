Comp_CI_diff_prop = function(e,n,alpha)
{
  ## input
  ## e: a bivariate vector of event counts
  ## n: a bivariate vector a sample sizes corresponding to counts in e
  ## alpha: probability to obtain a nominal coverage of (1-alpha)
  z = qnorm(p=1-alpha/2)
  p = e/n
  phi = (2*e+z^2)/(2*(n+z^2))
  psi = (e^2)/(n^2+n*z^2)
  l = phi - sqrt(phi^2-psi)
  u = phi + sqrt(phi^2-psi)
  delta = sqrt( (p[1] - l[1])^2 + (u[2]-p[2])^2 )
  eps =
    sqrt( (u[1] - p[1])^2 + (p[2]-l[2])^2 )
  ARR = p[1]-p[2] # Absolute Risk Reduction
  LL_ARR = ARR - delta
  UL_ARR = ARR + eps
  return(list(LL_ARR=LL_ARR,
              ARR=ARR,
              UL_ARR=UL_ARR))
}

