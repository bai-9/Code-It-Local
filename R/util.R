# For given b1 and kappa,
# b2 is valid only in a range.
max_min_b2<-function(b1,k)
{
  max = (2-k)*b1/(2*b1*(1-k)+k)
  min = k*b1/(2-k-2*b1+2*k*b1)
  return(list(max=max,min=min))
}
# For given b1 and kappa,
# the maximum possible accuracy
# depends on the value of b1
# when b1<0.5
# the maximum possible accuracy occurs
# when b2 is of the minimum
# otherwise
# when b2 is of the maximum
max_accuracy<-function(b1,k)
{
  mbb = max_min_b2(b1,k)
  b2 = mbb$min
  if(b1>0.5)
  {
    b2 = mbb$max
  }
  pe = b1*b2+(1-b1)*(1-b2)
  accuracy = pe+k*(1-pe)
  return(accuracy)
}
# For given b1 and kappa
# perfect sample size is computed
# based on maximum possible accuracy
perfect_sample_size<-function(b1,k,alpha=0.05)
{
  a_max = max_accuracy(b1,k)
  n = ceiling(log(alpha)/log(a_max))
  return(n)
}