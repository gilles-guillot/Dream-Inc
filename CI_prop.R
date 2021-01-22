## en.wikipedia.org/wiki/Binomial_proportion_confidence_interval
## functions to compute CI for a proportion
CI_prop = function(x ,
                   alpha = 0.05, # 1-(nominal coverage)
                   method=c("normal","Wilson","Wilson_cc","CP")
)
{
  CI_normal = CI_Wilson = CI_Wilson_cc = CI_CP = NULL
  p.hat = x/size
  if("normal" %in% method)
  {
    z = qnorm(p=1-alpha/2)
    delta = z* sqrt(p.hat*(1-p.hat)/size)
    CI_normal = cbind(p.hat-delta,p.hat+delta)
  }
  if("Wilson" %in% method)
  {
    z = qnorm(p=1-alpha/2)
    delta = z* sqrt(p.hat*(1-p.hat)/size + z^2/(4*size^2))
    CI_Wilson = cbind((p.hat + z^2/(2*size) - delta)/(1+z^2/size),
                      (p.hat + z^2/(2*size) + delta)/(1+z^2/size))
  }
  if("Wilson_cc" %in% method)
  {
    z = qnorm(p=1-alpha/2)
    delta = z* sqrt(p.hat*(1-p.hat)/size + z^2/(4*size^2))
    CI_Wilson_cc = cbind((p.hat + z^2/(2*size) - delta)/(1+z^2/size),
                         ((p.hat + z^2/(2*size) + delta)/(1+z^2/size)))
  }
  if("CP" %in% method)
  {
    lb = qbeta(p=alpha/2,shape1=x , shape2 = size-x+1)
    ub = qbeta(p = 1-alpha/2, shape1 = x+1 , shape2 = size-x)
    CI_CP = cbind(lb,ub)
  }
  return(list(CI_normal=CI_normal,
              CI_Wilson=CI_Wilson,
              CI_Wilson_cc=CI_Wilson_cc,
              CI_CP=CI_CP))
}