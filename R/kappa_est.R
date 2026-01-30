kappa_bFF <- function(b1, FDR, FOR, tol = 1e-12)
{
  ## basic validity
  if (!is.finite(b1) || !is.finite(FDR) || !is.finite(FOR))
    return(-1)

  if (b1 <= tol || b1 >= 1 - tol)
    return(-1)

  if (FDR < 0 || FDR > 1 || FOR < 0 || FOR > 1)
    return(-1)

  ## numerator and denominator
  n <- 2 * b1 * (1 - b1) * (1 - FDR - FOR)

  d <- b1 +
    (1 - 2 * b1) * (b1 * (1 - FDR) + (1 - b1) * FOR)

  if (!is.finite(d) || abs(d) < tol)
    return(-1)

  kappa <- n / d

  if (!is.finite(kappa))
    return(-1)

  ## numerical safety
  max(min(kappa, 1), -1)
}
FOR_from_b1_FDR_kappa <- function(b1, FDR, kappa, tol = 1e-12)
{
  ## basic validity
  if (!is.finite(b1) || !is.finite(FDR) || !is.finite(kappa))
    return(-1)

  if (b1 <= tol || b1 >= 1 - tol)
    return(-1)

  if (FDR < 0 || FDR > 1)
    return(-1)

  if (kappa < -1 || kappa > 1)
    return(-1)

  ## denominator
  den <- (1 - b1) * (2 * b1 + kappa * (1 - 2 * b1))
  if (!is.finite(den) || abs(den) < tol)
    return(-1)

  ## numerator
  num <- b1 * (
    2 * (1 - b1) * (1 - FDR) -
      kappa * (1 + (1 - 2 * b1) * (1 - FDR))
  )

  FOR <- num / den

  if (!is.finite(FOR))
    return(-1)

  ## must be a probability
  max(min(FOR, 1), 0)
}

ps_est_proportion<-function(n,N=0,allowed_errors=0,alpha=0.05)
{
  if(alpha<0.001|alpha>0.1)
  {
    return(NA)
  }
  a = allowed_errors
  if(a>=n)
  {
    return(NA)
  }
  if(N<=0)
  {
    N = 1000000
  }
  if(n<1|n>=N)
  {
    return(NA)
  }
  left = 1
  right = N
  x=0
  while(right-left>1)
  {
    K = ceiling((left+right)/2)
    p = 1-stats::phyper(n-1-a,K,N-K,n)
    x = K/N
    if(p<alpha)
    {
      left = K
    }else
    {
      right=K
    }

  }
  return(x)
}
ps_estimates<-function(tp,fp,fn,tn,b1,N,alpha = 0.05)
{
  n = tp+fp+fn+tn
  if(n<1)
  {
    return(NULL)
  }

  p1 =ps_est_proportion(tp+fp,N=N,allowed_errors=fp,alpha=alpha/2)
  FDR = 1-p1
  p2 = ps_est_proportion(tn+fn,N=N,allowed_errors=fn,alpha=alpha/2)
  FOR = 1-p2
  kappa = kappa_bFF(b1,FDR,FOR)
  return(list(FDR=FDR,FOR=FOR,kappa=kappa))
}
ps_size_proportion<-function(p,N,alpha=0.025)
{
  if(p>=1)return(N)
  if(p<alpha)return(1)
  ceiling(log(alpha)/log(p))
}
ps_size_bk<-function(b1,kappa,n,alpha=0.05)
{
  np = ceiling(b1*n)
  nn = n-np
  min_n = n
  min_n1 = np
  min_n2 = nn
  for(i in c(0:np))
  {
    FDR = i/np
    p1 = 1-FDR
    FOR = FOR_from_b1_FDR_kappa(b1,FDR,kappa)
    p2 = 1-FOR
    n1 = ps_size_proportion(p1,np,alpha/2)
    n2 = ps_size_proportion(p2,nn,alpha/2)
    if((n1+n2)<min_n)
    {
      min_n = n1+n2
      min_n1 = n1
      min_n2 = n2
      #cat("\nmin_n=",min_n)
    }
  }
  list(n1 = min_n1,n2=min_n2)
}
kapp_est_test<-function()
{
tp = 10
fp = 1
fn = 2
tn = 100
n = tp+fp+fn+tn
tp = tp/n
fp = fp/n
fn = fn/n
tn = tn/n
b1 = tp+fp
b2 = tp+fn
pe = b1*b2+(1-b1)*(1-b2)
kappa = (tp+tn-pe)/(1-pe)
FDR = fp/(b1)
FOR = fn/(1-b1)
b1
FDR
kappa
FOR_from_b1_FDR_kappa(b1,FDR,kappa)
FOR

N = 2000


alpha = 0.05
ps_estimates(tp,fp,fn,tn,0.1,N)
b1 = 0.01
kappa = 0.8
N = 200000


ps_size_bk(b1,kappa,N,alpha=alpha)

perfect_sample_size(b1,kappa,alpha = alpha)
}
