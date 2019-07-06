##Submitted by:SHEENAM,34
##SUBMITTED TO:MR. RAJAN GUPTA
##
#=============================DATA=========================================
x<-c(4,3,5,1,2,7,9,8,433,2232,11,2,331,1,121,133,123,2,132,13,43,1,98,11,20)

options('max.print'=99999999)
#==============================MODULE 1======================================
#%%%%%%%%%%%%%%%%%%%%%%%%  DESCRIPTIVE ANALYSIS   %%%%%%%%%%%%%%%%%%%%%%%%%%%
DESCRIPTIVE_ANALYSIS<-function()
{
  cat("\nDescriptive Analysis")
  cat("\na. MEAN \nb. MEDIAN \nc. MODE \nd. VARIANCE \ne. STANDARD DEVIATION \nf. MEAN ABSOLUTE DEVIATION \ng. RANGE \nh. QUARTILES \ni. INTER QUAURTILE RANGE \nj. MAXIMUM \nk. MINIMUM \nl.SKEWNESS \nm. KURTOSIS \nn. MOMENTS \no.PREVIOUS MENUE\np.EXIT")
  #print("Enter your choice : ")
  read<-function(){
  optn<-readline(prompt = "Enter your choice : ")
  optn<-as.character(optn)
  return(optn)
  }
  optn<- read() 
  if(optn=='a')
  {mean1(x)
  }else if(optn=='b')
  {median1(x)
  }else if(optn=='c')
  {mode1(x)
  }else if(optn=='d')
  {variance1(x)
  }else if(optn == 'e')
  {stddev1(x)
  }else if(optn=='f')
  {mad1(x)
  }else if(optn=='g')
  {range1(x)
  }else if(optn=='h')
  {quartile1(x)
  }else if (optn=='i')
  {IQR1(quartile)
  }else if (optn=='j')
  {max1(x)
  }else if (optn=='k')
  {min1(x)
  }else if(optn=='l')
  {skewness1(x)
  }else if(optn=='m')
  {kurtosis1(x)
  }else if(optn=='n')
  {moment1(x)
  }else if(optn=='o')
  {calculator()
  }else if(optn=='p'){cat("Goodbye")}
  }

#-------------------------------MEAN-----------------------------------------
mean1<-function(x)
{
  n<-length(x)
  meanx<-sum1(x)/n
  cat("\nMean = ",meanx,"\n")
  #return(meanx)
}

#---------------------------------MEDIAN-------------------------------------
median1<-function(x)
{
  y<-sort1(x)
  n <- length(y)
  if(n%%2 == 0)
  {
    h1 = n/2
    h2 = h1+1
    mid_1 = y[h1]
    mid_2 = y[h2]
    medianx = (y[h1] + y[h2])/2
    #return(medianx)
  }else if(n%%2 == 1)
  {
    h = (n+1)/2
    medianx = y[h]
    #return(medianx)
    
  }
  cat("\nMedian = ",medianx)
}
#-----------------------------------MODE-----------------------------------------
#x<-c(22,22,22,22,1,1,1,1,3,4)
mode1<-function(x)
{
  table(x)
  
  # Only gives the value for highest frequency
  # Doesn't tell which number that is
  max(table(x))
  
  # Gives a logical vector
  table(x) == max(table(x))
  
  #which(table(x) == max(table(x)))
  modex<-as.numeric(names(which(table(x) == max(table(x)))))
  return(modex)
  cat("\nMode = ",modex)
  
}

#--------------------------------VARIANCE---------------------------------------
variance1<-function(x)
{
  n<-length(x)
  m<- mean1(x)
  d<-x-m
  sq<-power1(d,2)
  s<-sum1(sq)
  v<-s/n
  #return(v)
  cat("\nVariance = ",v)
}


#--------------------------------STANDARD DEVIATION-----------------------------
stddev1<-function(x)
{
  v<-variance1(x)
  std<-sqrt1(v)
  #return(std)
  cat("Standard Deviation = ",std)
}
#----------------------------MEAN ABSOLUTE DEVIATION----------------------------
mad1<-function(x)
{
  n<-length(x)
  m<-mean1(x)
  d<-x-m
  s<-sum1(d)
  madx<-s/n
  #return(madx)
  cat("\nMean Absolute Deviation = ",madx)
}

#-----------------------------------RANGE--------------------------------------
range1<-function(x)
{
  m<-00
  n<-00
  maximum<-max1(x)
  minimum<-min1(x)
  r<-maximum-minimum
  #return(r)
  cat("\nRange = ",r)
}
#-------------------------------QUARTILE----------------------------------------
quartile1<-function(x)
{
  z<-sort1(x)
  n<-length(z)
  mid<-n/2
  q0<-z[1]
  q2<-median1(z)
  q4<-y[n]
  if(n%%2 == 0)
  {
    l<-c(y[1]:z[mid])
    u<-c(z[mid+1]:z[n])
    q1<-median1(l)
    q3<-median1(u)
  }
  else if(n%%2 == 1)
  {
    l<-c(z[1]:z[mid-1])
    u<-c(z[mid+1]:z[n])
    q1<-median1(l)
    q3<-median(u)
  }
  q<-c(q0,q1,q2,q3,q4)
  #return(q)
  cat("\nQuartiles = ",q)
}

#-------------------------INTER QUARTILE RANGE(IQR)-----------------------------
IQR1<-function(quartile1)
{
  q<-quartile1(x)
  a<-q[2]
  b<-q[4]
  iqr<- (b-a)/2
  #return(iqr)
  cat("\nInter Quartile Range = ",iqr)
}

#--------------------------------MAXIMUM----------------------------------------
max1<-function(x)
{
  maximum = 0
  for(i in x)
  {
    if(maximum < i){
      maximum<-i
    }
  }
  cat("\n Maximum = ",maximum)
  #return(maximum)
}
#----------------------------------MIMIMUM-------------------------------------
min1<-function(x)
{
  minimum = 100
  for(j in x)
  {
    if(minimum > j)
    {
      minimum<-j
    }
  }
  cat("\nMaximum",minimum)
  #return(minimum)
}
#-------------------------------SKEWNESS----------------------------------------
#..........PEARSON'S COEFFICIENT OF SKEWNESS USING MEAN AND MEDIAN..............
skewness1<-function(x)
{
  m<-mean1(x)
  md<-median1(x)
  s<-stddev1(x)
  sk<-3*(m-md)/s
  return(sk)
  cat("\nSkewness = ",sk)
}
#skewness1(x)

#-------------------------------KURTOSIS----------------------------------------
kurtosis1<-function(x)
{
  m1<-mean1(x)
  d<-x-m1
  a1<-power1(d,4)
  b1<-power1(d,2)
  a<-sum1(a1)
  b<-sum1(b1)
  c<-power1(b,2)
  k1<-(n*(n+1)*(n-1)*a)
  k2<-((n-2)*(n-3)*c)
  k<-k1/k2
  cat("\nKurtosis = ",k)
}

#-------------------------------MOMENTS-----------------------------------------
moment1<-function(x,a,r)
{
  m<-mean1(x)
  n<-length(x)
  d1<-x-a                             #1. moment around arbitrary 'a'
  d2<-x-m                             #2. moment around mean 
  pwr1<-c(power1(d1,2),power1(d1,3),power1(d1,4))
  s1<-c(sum1(pwr1[1]),sum1(pwr1[2]),sum1(pwr1[3]),sum1(pwr1[4]))
  mnt1<-(s1[r])/n
  print("2.Central Moment")
  pwr2<-c(power1(d2,2),power1(d2,3),power1(d2,4))
  s2<-c(sum1(pwr2[1]),sum1(pwr2[2]),sum1(pwr2[3]),sum1(pwr2[4]))
  mnt2<-(s2[r]/n)
  mnts<-c(mnt1,mnt2)
  #return(mnts)
  prompt <- "Enter your choice: 1.Arbitrary Moment 2.Central Moment \n "
  choice<- as.character(strsplit(readline(prompt)," ")[[1]])
  switch(choice,
         "1" = return(mnt1),
         "2" = return(mnt2)
  )
  
}
#------------------------------END OF MODULE 1---------------------------------

#==============================MODULE 2======================================
#%%%%%%%%%%%%%%%%%%%%%%%% PREDICTIVE  ANALYSIS   %%%%%%%%%%%%%%%%%%%%%%%%%%%%
PREDICTIVE_ANALYSIS<-function()
{
  cat("\nPredictive Analysis")
  cat("\n1. CORRELATION ")
  read<-function(){
    optn<-readline(prompt = "Enter your choice : ")
    optn<-as.numeric(optn)
    return(optn)
  }
  if(optn==1)
    {correlation2(x,y)}
}  
#-----------------------------CORRELATION--------------------------------------
  correlation2<-function(x,y)
  {
    c<-covar2(x,y)
    sdx<-stddev1(x)
    sdy<-stddev1(y)
    corlp<-c/(sdx*sdy)
    
    mx<-mean1(x)
    my<-mean1(y)
    dx<-x-mx
    dy<-y-my
    d<-dx*dy
    s<-sum1(d)
    sqx<-power1(dx,2)
    sx<-sum1(sqx)
    sqtx<-sqrt1(sx)
    sqy<-power1(dy,2)
    sy<-sum1(sqy)
    sqty<-sqrt1(sy)
    sqt<-sqtx*sqty
    corls<-s/sqt
    cat("\n Correlation coefficient = ",corls)
    #prompt <- "Enter your choice: 1.Population correlation 2.sample correaltion \n "
    #choice<- as.character(strsplit(readline(prompt)," ")[[1]])
    choice<-2
    switch(choice,
           "1" = return(corlp),
           "2" = return(corls)
    )
    
  }
  
#------------------------MULTIPLE LINEAR REGRESSION----------------------------
  
#-------------------------------END OF MODULE 2--------------------------------

#==============================MODULE 3======================================
#%%%%%%%%%%%%%%%%%%%%%% PROBABILITY ANALYSIS   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
PROBABILITY_ANALYSIS<-function()
{
  cat("\nProbability Analysis")
  cat("\n1. PERMUTATION \n2. COMBNINATION \n3. BASIC PROBABILITY \n4. CONDITIONAL \n5. BAYS THEOREM \n6. MENUE\n7.EXIT")
  read<-function(){
    optn<-readline(prompt = "Enter your choice : ")
    optn<-as.numeric(optn)
    return(optn)
  }
  if (optn==1)
  {
    Permutation(n,r)
  }else if(optn == 2)
  {
    ncr(n,r)
  }else if(optn==3)
  {
    Probability(x)
  }else if(optn==4)
  {
    ConditionalP(A,B)
  }else if(optn==5)
  {
    bayes(b,r)
  }else if(optn==6)
  {
    calculator()
  }else if(optn == 7)
  {
    stop("Goodbye")
  }
}
#------------------------------PERMUTATIONS-----------------------------------
  Permutation <-function(n,r)
  {
    if(n>=r)
    {
      if(r==0)
      {
        value = 1
      }
      else
      {
        value <- fac(n)/(fac(n-r))
      }
      cat("\nPermutation = ",value)
    }
    else
    {
      cat('passed argument voileted principle of permutation')
    }
  }
#------------------------------COMBINATIONS-----------------------------------
  ncr<-function(n=0,r=0)
  {
    if(n==0 && r==0)
    {
    n<-as.numeric(readline(prompt = "Enter favourable no of outcomes: "))
    r<-as.numeric(readline(prompt = "Enter total number of outcomes: "))
    }
    m<-fact(r)*fact(n-r)
    ncr<-fact(n)/m
    cat("\nCombination = ",ncr)
    return(ncr)
  }
#------------------------- BASIC PROBABILITY--------------------------------
  Probability<-function(x=0,n=0)
  {
    if(x==0 && n==0)
    {
      x<-as.numeric(readline(prompt = "Enter favourable no of outcomes: "))
      n<-as.numeric(readline(prompt = "Enter total number of outcomes: "))
      prob = 0
    }
    
    prob<-x/n
    cat("\nProbability = ",prob)
    return(prob)
  }
  
#------------------------- CONDITIONAL PROBABILITY--------------------------
  ConditionalP <- function(A=0,B=0)
  { if(B==0 && A==0)
  {
    B <- as.integer(readline(prompt = "ENTER THE SAMPLE EVENT B   ="))
    A <- as.integer(readline(prompt = "ENTER THE NO OF EVENT A DEPEND ON EVENT B   ="))
  } 
    temp<-(round((A/B),6))
    cat("\nConditional Probability = ",temp)
    return(temp)
  }
#------------------------------BAYS THEOREM--------------------------------
  b<-c(0.5,0.5,0.5,0.5)
  bayes<-function(b,r)
  {
    var1<-Probability(b[r])
    var2<-conditional(a,b[r])
    var3<-var1*var2
    for(i in 1:size)
    {
      var4<-ConditionalP(a,b[i])
    }
    for(i in 1:size)
    {
      var5<-probability(b[i])*var4[i]
    }
    var6<-sum1(var5)
    bys<-var3/var6
    cat("Result = ",bys)
  }
  
  
#--------------------------END OF MODULE 3---------------------------------


#==============================MODULE 4======================================
#%%%%%%%%%%%%%%%%%  DISCRETE DISTRIBUTION FUNCTIONS   %%%%%%%%%%%%%%%%%%%%%%%%

DISCRETE_DISTRIBUTION<-function()
{
  cat("\nDiscrete Distribution")
  cat("\na. UNIFORM \nb. BERNOULLI \nc. BINOMIAL \nd. GEOMETRIC \ne. HYPER GEOMETRIC \nf. NEGATIVE \ng. POISSON \nh. MULTINOMIAL \ni. MULTIVARIATE HYPERGEOMETRIC \nj. PREVIOUS MENUE\nk.EXIT")
  read<-function(){
    optn<-readline(prompt = "Enter your choice : ")
    optn<-as.character(optn)
    return(optn)
  }uniform(x)
  }else if(optn=='b')
  {
    bernoulli(x)
  }else if(optn=='c')
  {
    binomial4(k,n,p)
  }else if(optn=='d')
  {
    geometric(k,p)
  }else if(optn=='e')
  {
    hyper(x,n,N,M)
  }else if(optn=='f')
  {
    nbinomial(x,k,p)
  }else if(optn=='g')
  {
    poisson4(x,n,p)
  }else if(optn =='h')
  {
    multinomial(x,n,p)
  }else if(optn=='i')
  {
    multi_hyper(x,M)
  }else if(optn =='j')
  {
    calculator()
  }else if(optn=='k')
  {
    stop("Goodbye")
  }

}
#-------------------------------UNIFORM-----------------------------------------
  uniform<-function(x)
  {
    a<-min1(x)
    b<-max1(x)
    if(a<x && x<b)
    {
      u = 1/x
    }
    cat("\nf(x) = ",u)
  }
  
  #----------------------------BERNOULLI----------------------------------------
  bernoulli<-function(x)
  {
    theta<-as.numeric(readline(prompt =" enter value of theta : "))
    i=0
    n<-length(x)
    bern<-c()
    for(i in 1:n)
    {
      if(x[i]==1)
      {
        bern[i]<-theta
      }
      else if(x[i]==0)
      {
        bern[i]<-(1-theta)
      }
      else
      {
        bern[i]<-0
      }
      cat("\nf(x;theta) = ",bern[i])
    }
  }
  
  #---------------------------BINOMIAL---------------------------------------------
  binomial4<-function(k,n,p)
  {
    #n<-as.numeric(readline(prompt ="Number of trials : "))
    #k<-as.numeric(readline(prompt ="Number of successes : "))
    #p<-as.numeric(readline(prompt ="Probability of a success : "))
    q<-1-p
    k1<-n-k
    c<-ncr(n,k)
    pk<-power1(p,k)
    pk1<-power1(q,k1)
    p<-pk*pk1
    bio<-c*p
    cat("\nb(x;n,p) = ",bio)
  }
  #----------------------------GEOMETRIC------------------------------------------
  #......................Probability that kth trial is first success..............
  geometric<-function(k,p)
  {
    q<-1-p
    k1<-k-1
    pwr<-power1(q,k1)
    g<-p*pwr
    cat("\ng(x;theta) = ",g)
  }
  #-----------------------------HYPER GEOMETRIC----------------------------------
  hyper<-function(x,n,N,M)
  {
    #N & n size of population and sample respectively
    #M & x number of successes in population and sample respectively
    successes<-ncr(M,x)         #choosing x of M successes
    M1<-N-M
    x1<-n-x
    failures<-ncr(M1,x1)      #choosing (n-x) faliures of the (N-M) faliures
    ch<-ncr(N,n)                #choosing n of N elements 
    h<-(successes*failures)/ch
    cat("\nh(x;n,N,M) = ",h)
  }
  #----------------------------NEGATIVE BINOMIAL---------------------------------
  #............probalility that kth success occurs on xth trial...................
  nbinomial<-function(x,k,p)
  {
    q<-1-p                            #p is the probability of success on xth trial
    x1<-x-1
    k1<-k-1
    d<-x-k                           #difference x-k
    c<-ncr(x1,k1)
    pwr1<-power1(p,k)
    pwr2<-power1(q,d)
    pwrs<-pwr1*pwr2
    nbio<-c*pwrs
    cat("\nb*(x=",x,";k=",k,"p=",p,")  = ",nbio)
  }
  #-----------------------------POISSON-------------------------------------------
  poisson4<-function(x,n,p)
  {
    lambda<-n*p
    p1<-power1(lambda,x)
    e<-exp(-lambda)
    f<-fact(x)
    m<-p1*e
    poison<-m/f
    cat("\np(x;lambda) = ", poison)
  }
  #----------------------------MULTINOMIAL---------------------------------------
  x<-c(5,2,1)
  p<-c(0.5,0.3,0.2)
  n=8
  multinomial<-function(x,n,p)
  {
    probab<-sum1(p)
    num<-sum1(x)
    #  while(n==x && probab==1)
    # {
    size<-length(x)
    m<-1
    pwr<-1
    for(i in 1:size)
    {
      m1[1:size]<c(0)
      m1[i]<-c(fact(x[i]))
      m<-m1[i]*m
      #print(m)
    }
    fct<-fact(n)
    c<-fct/m
    for(i in 1:size)
    {
      pwr1<-p^x
      pwr<-pwr1[i]*pwr
      #print(pwr)
    }
    multi<-c*pwr
    cat("\nf(x1,x2....,x",size,";n,theta1,theta2,....,theta",size,") = ",multi)
  }
  #--------------------------------MULTIVARIATE HYPERGEOMETRIC--------------------
  x<-c(4,1,5,2)
  M<-c(6,3,7,4)
  multi_hyper<-function(x,M)
  {
    size<-length(M)
    sizex<-length(x)
    if(size == sizex)
    {
      N<-sum1(M)
      n<-sum1(x)
      var1<-ncr(N,n)
      for(i in 1:size)
      {
        var2[i]<-ncr(M[i],x[i])
      }
      var3 =1
      for(j in 1:size) 
      {
        var3<-var3*var2[j]
      }
      multivar_hyper<-var3/var1
      cat("\nf(x1,x2,....,x",size,";n,M1,M2,....,M",size,") = ",multivar_hyper)
    }else
    {
      cat("Please enter valid data")
    }
  }
  #-------------------------------END OF MODULE 4---------------------------------

#==============================MODULE 5======================================
#%%%%%%%%%%%%%%%% CONTINUOUS DISTRIBUTION FUNCTIONS   %%%%%%%%%%%%%%%%%%%%%%%
CONTINUOUS_DISTRIBUTION<-function()
{
  cat("\nContinious Distribution")
  cat("\n1. UNIFORM CONTINIOUS \n2. NORMAL \n3. BIVARIATE \n4. GAMMA \n5. EXPONENTIAL \n6. PREVIOUS MENUE\n7.EXIT")
  read<-function(){
    optn<-readline(prompt = "Enter your choice : ")
    optn<-as.numeric(optn)
    return(optn)
  }
  if(optn==1)
  {
    uniform_cont(x)
  }else if(optn==2)
  {
    normal(x)
  }else if(optn==3)
  {
    bivariate(x,y)
  }else if(optn==4)
  {
    gamma_dst(x,alpha,beta)
  }else if(optn==5)
  {
    exponential(x)
  }else if(optn==6)
  {
    calculator()
  }else if(optn==7)
  {
    stop("Goodbye")
  }
}
  
#-----------------------------UNIFORM------------------------------------
  uniform_cont<-function(x)
  {
    alpha<-as.numeric(readline(prompt =" enter alpha : "))
    beta<-as.numeric(readline(prompt= "enter beta : "))
    d<-beta-alpha
    if(beta<alpha)
    {
      cat("\nPlease enter correct values.")
    }
    if(alpha<x && x<beta)
    {
      u<-1/d
      cat("u(x;alpha,beta)= ",u)
    }else
    {
      return(0)
    }
    s<-alpha+beta
    m<-s/2
    cat("\nMean of Uniform Distribution: ",m)
    sqr<-power1(d,2)
    v<-sqr/12
    cat("\nVariance of Uniform Distribution: ",v)
  }
  #------------------------------NORMAL-----------------------------------------
  normal<-function(x)
  {
    #m<-as.numeric(readline(prompt = 'Enter mean : '))
    #sd<-as.numeric(readline(prompt = 'Enter standard deviation: '))
    m<-mean1(x)
    sd<-stddev1(x)
    var1<-1/(v*2.5071321)
    var2<-(x-m)/sd
    var3<-power1(var2,2)
    var4<-0.5*var3
    var5<-exp(var4)
    nrml<-var1*var5
    cat("\nn(x;mean,std_dev)= ",nrml)
  }
  #----------------------------BIVARIATE NORMAL----------------------------------
  bivariate<-function(x,y)
  {
    m1<-mean1(x)
    m2<-mean1(y)
    sd1<-stddev1(x)
    sd2<-stddev1(y)
    rho<-correlation2(x,y)
    rho2<-power1(rho,2)
    rho3<-sqrt1((1-rho2))
    var1<-(x-m1)/sd1
    var2<-(y-m2)/sd2
    var3<-power1(var1,2)
    var4<-power1(var2,2)
    var5<-2*rho*var1*var2
    var6<-(0.5*(1-rho2))(var3-var5+var4)
    var7<-6.2857142857*sd1*sd2*rho3
    cat("\nFor Y on given X=x")
    myx<-m2+rho*(sd1/sd2)*(x-m1)
    cat("\nMean is : ",myx)
    var_yx<-sd2*sd2(1-rho2)
    cat("\nAnd Variance is : ",var_yx)
    cat("\nFor X on given Y=y")
    mxy<-m1+rho*(sd2/sd1)*(y-m2)
    cat("\nMean is : ",mxy)
    var_xy<-sd1*sd1(1-rho2)
    cat("\nAnd Variance is : ",var_xy)
  }
  #------------------------------GAMMA-------------------------------------------
  gamma_dst<-function(x,alpha,beta)
  {
    if(alpha>0 && beta>0)
    {
      if(x>0)
      {
        y<-x/beta
        g<-gamma_func(y,alpha)
        var1<-1/(power1(beta,alpha)*g)
        var2<-power1(x,(alpha-1))*exp(-y)
        gma<-var1*var2
      }else
      {
        gma = 0
      }
    }
    cat("\ng(x;alpha,beta) = ",gma)
    cat("\nMean of Gamma Distribution : ",alpha*beta)
    cat("\nVariance of Gamma Distribution : ",alpha*beta*beta)
    return(gma)
  }
  #-----------------------------GAMMA FUNCTION-----------------------------------
  gamma_func<-function(y,alpha)
  {
    if(alpha==1)
    {
      return(gamma=1)
    }else
    {
      g<-function(y,alpha)
      {
        g<-integrate(power1(y,(alpha-1))*exp(-y))       #integration
        return(g)                                       #value of gamma function
      }
    }
  }
  #------------------------------EXPONENTIAL DISTRIBUTION-------------------------
  exponential<-function(x)
  {
    if(x>0)
    {
      theta<-as.numeric(readline(prompt = "Enter theta : "))
      var1<-1/theta
      var2<-exp(-x/theta)
      ge<-var1*var2
      
    }else
    {
      ge=0
      
    }
    cat("\ng(x;theta) = ",ge)
    cat("\nMean of Exponential Distribution : ",theta)
    cat("\nVariance of Exponential Distribution : ",theta*theta)
    return(ge)
  }
#------------------------------------END OF MODULE 5----------------------------

  #==============================MODULE 7======================================
  #%%%%%%%%%%%%%%%% INTERVAL ESTIMATIONS   %%%%%%%%%%%%%%%%%%%%%%%
  internal_estimation<-function()
  {
    #----------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    
    #Estimation of mean
    
    
    #----------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    eom <- function()
    {
      n <-as.numeric(readline(prompt = "Enter the number of sample population : "))
      sdeviation <-as.numeric(readline(prompt = "Enter the standard deviation for sample : "))
      mean <- as.numeric(readline(prompt = "Enter the mean for sample : "))
      
      if(sdeviation == "")
      {
        cat("\n you have not entered standard deviation for sample")
        psdeviation <-as.numeric(readline(prompt = "Enter the standard deviation of population"))
        if(psdeviation == "")
        {
          print("\n Wrong value has been inserted ")
        }
        
      }
      if(n < 30)
      { 
        lvalue <- (mean - zval*psdeviation)/number^(1/2)
        hvalue <- (mean + zval*psdeviation)/number^(1/2)
      }
      
      if(n > 30)
      { 
        tval<-qt(alpha,number)
        lvalue <- (mean - tval*psdeviation)/number^(1/2)
        hvalue <- (mean + tval*psdeviation)/number^(1/2)
      }
      
      cat("\nThe confidence interval lies between : ",lvalue," to ",hvalue)
    }
    #----------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    #----------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    
    #Estimation of proportions
    
    
    #----------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    
    
    eop <- function()
    {
      number<-as.numeric(readline(prompt ="\nEnter the number in sample (n): ")) 
      select <- as.numeric(readline(prompt ="\nEnter the number of selection (x) : "))
      if(number == "" || select == "")
      {
        cat("\n wrong input entered ")
        return()
      }
      theta <- select/number
      
      lvalue <- (theta - zval)*(((theta * (1-theta))^1/2)/((number)^1/2))
      hvalue <- (theta + zval*(theta * (1-theta))^1/2)/(number)^1/2
      
      cat("\n The confidence interval is : ",lvalue ," to ",hvalue)
    }
    #----------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    
    #Estimation of Variance  
    
    
    #----------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    eov <- function()
    { 
      number <- readline(prompt = "\nEnter the number of sample : ")
      variance <- readline(prompt = "\n Enter the variance of sample : ")
      chival<-qchisq(alpha,number)
      if(number == "" || variance == "")
      {
        cat ("\nwrong input parameters.. ")
        return()
      }
      lvalue <- (number-1)*variance/chival(alpha/2,number-1)
      hvalue <- (number -1)*variance/chival((1-alpha)/2,number-1)
      
      cat("\n the interval is",lavalue," to ",hvalue)
    }  
    #----------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    
    #Estimation of diffrence of mean
    
    
    #----------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    eodm <- function()
    {
      number1 <- as.double(readline(prompt = " Enter the size of popualtion 1 : "))
      mean1 <- as.double(readline(prompt = " Enter the mean of population 1 : "))
      variance1 <- as.double(readline(prompt = " Enter the variance of population 1 :"))
      
      number2 <- as.double(readline(prompt = " Enter the size of population 2 : "))
      mean2 <- as.double(readline(prompt = " Enter the mean of population 2 : "))
      variance2 <- as.double(readline(prompt = " Enter the variance of population 2 : "))
      
      if(is.na(number1) || is.na(number2) || number1 == 0 || number2 == 0)
      {
        cat("\n Oops.. you entered wrong values")
        return()
      }
      if(is.na(variance2) || is.na(variance1))
      {
        cat("\n You have not entered variance of either of sample \n please enter variances for both population")
        pvariance1 <- as.double(readline(prompt = " Enter the variance of population 1 :"))
        pvariance2 <- as.double(readline(prompt = " Enter the variance of population 2 :"))
        
        if(is.na(pvariance2) || is.na(pvariance1))
        {
          return()
        }
        if(number1 > 30 && number2 > 30)
        {
          value <- (number2 * pvariance1) + (number1 * pvariance2)
          value <- value/(number2*number1)
          value <- (value)^0.5
          value <- value * abs(qnorm(alpha))
          lvalue <- mean1 - mean2 - value
          hvalue <- mean1 - mean2 + value
        }
        else
        {
          sdpopulation <- ((number1 - 1)*pvariance1) + ((number2 - 1)*pvariance2)
          sdpopulation <- sdpopulation/(number1+number2-2)
          sdpopulation <- (sdpopulation)^0.5
          value <- number1 + number2
          value <- value / (number2 * number1)
          value <- (value)^0.5
          value <- value * sdpopulation
          value <- value * qt(alpha,number2+number1-2)
          lvalue <- mean1 - mean2 - value
          hvalue <- mean1 - mean2 + value
        }
      }
      else
      {
        value <- (variance1 * number2) + (variance2 * number1)
        value <- value/(number1 * number2)
        value <- (value)^0.5
        value <- value * abs(qnorm(alpha))
        lvalue <- mean1 - mean2 - value
        hvalue <- mean1 - mean2 + value
      }
      
      cat("\n The range is from ",lvalue ," to ",hvalue)
    }
    
    
    
    
    #----------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    
    #Estimation of diffrence of propotion
    
    
    #----------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    
    eodp <- function()
    {
      number1 <- as.double(readline(prompt = " Enter the length of sample 1 : "))
      select1 <- as.double(readline(prompt = " Enter the selection from sample 1 : "))
      number2 <- as.double(readline(prompt = " Enter the length of sample 2 : "))
      select2 <- as.double(readline(prompt = " Enter the selection from sample 2 : "))
      theta1 <- select1/number1
      theta2 <- select2/number2
      value <- (theta1 * (1 - theta1))/number1
      value <- value + ((theta2 * (1 - theta2))/number2)
      value <- (value)^0.5
      value <- value * abs(qnorm(alpha))
      lvalue <- theta1 - theta2 - value
      hvalue <- theta1 - theta2 + value
      
      cat("\n The confidence interval lies between ",lvalue," to ",hvalue)
      
    }
    
    
    #----------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    
    #Estimation for diffrence of variances
    
    
    #----------------------------------------------------------------------------------------------------------------------------------------------------------------------
    
    
    eodv <- function()
    {
      number1 <- as.double(readline(prompt = " Enter the size of population 1 :"))
      variance1 <- as.double(readline(prompt = " Enter the variance of population 1 : "))
      number2 <- as.double(readline(prompt = " Enter the size of population 2 :"))
      variance2 <- as.double(readline(prompt = " Enter the variance of population 2 : "))
      lvalue <- variance1/(variance2 * qf(alpha,number1-1,number2-1))
      hvalue <- variance1 * qf(alpha,number2-1,number1-1)
      cat("\n The interval is between ",lvalue," to ",hvalue)
    }
    
    repeat{
      cat('\n1.Estimation of Means\n')
      cat('2.Estimation of Differences in Means\n')
      cat('3.Estimation of Proportions\n')
      cat('4.Estimation of Differences in Proportions\n')
      cat('5.Estimation of Variances\n')
      cat('6.Estimation ofRatio of Two Variances\n')
      cat('7.BAck\n')
      choice <- as.integer(readline(prompt = "ENTER your choice   ="))
      if (choice<7 && choice>0)
      {cinterval <- (readline(prompt = "Enter confidence interval(in percentage) : "))
      
      alpha <- (100 - as.numeric(cinterval))/100
      cat("alpha is : ",alpha)
      zval<-abs(qnorm(alpha/2))
      }
      if(choice == 1)
      {
        eom()
      }
      else if( choice==2 )
      {
        
        eodm()
      }
      else if( choice==3 )
      {
        
        eop()
      }
      
      else if(choice==4)
      {
        eodp()
      }
      else if(choice==5)
      {
        eov()
      }
      else if(choice==6)
      {
        eodv()
      }
      else if(choice==7)
      {
        break
      }
      else
      {
        cat('wrong choice please re-enter the value')  
      }
      
    }
  }
  
#------------------------------- END OF MODULE 7-----------------  
  
#==============================MODULE 9======================================
#%%%%%%%%%%%%%%%%%%%%%%%%%%%% VISUALIZATIONS   %%%%%%%%%%%%%%%%%%%%%%%
VISUALISATION<-function()
{
  cat("\nVisualisation")
  cat("\na. HISTOGRAM \nb. LINE GRAPH \nc. BAR GRAPH \nd. PIE CHART \ne. SCATTER PLOT \nf. BOX PLOT \ng. q q PLOT \nh. STEM \ni. PREVIOUS MENUE\nj.EXIT")
  read<-function(){
    optn<-readline(prompt = "Enter your choice : ")
    optn<-as.character(optn)
    return(optn)
  }
  if(optn=='a')
  {
    
  }else if(optn == 'b')
  {
    
  }else if(optn == 'c')
  {
    
  }else if(optn == 'd')
  {
    
  }else if(optn == 'e')
  {
    
  }else if(optn == 'f')
  {
    
  }else if(optn == 'g')
  {
    
  }else if(optn == 'h')
  {
    
  }else if(optn == 'i')
  {
    
  }else if(optn =='j')
  {
    
  }
}

#----------------------------- HISTOGRAM ---------------------------
  {
    
    data<<-c(20,115,30,40,23,76)
    data1<<-c(10,130,25,70,45,76)
    data2<<-c(44,66,77,55,88,99)
    histogram <- function() {
      hist( data , xlab = "x",col = "blue",border = "red",main = "Histogram of Your Data")
    }
    
    #------------------------ LINE GRAPH -----------------------------
    lineGraph <- function(data1 = 0 , data2 = 0) {
      plot(data,type = "^", col = "pink", xlab = "x", ylab = "y",main = "Line Graph")
      lines(data1, type = "o", col = "blue")
      lines(data2, type = "o", col = "green")
    }
    
    #--------------------------- BAR GRAPH ---------------------------
    barPlot <- function() {
      barplot(data, col = "red", xlab = "x", ylab = "y",main = "Bar Graph")
    }
    
    #---------------------------- PIE CHART----------------------------
    pie <- function() {
      pie(data,  main = "Pie chart")
    }
    
    #----------------------------- SCATTER PLOT------------------------
    scatter <- function(d) {
      plot(x = data,y = data1, xlab = "x", ylab = "y", main = "Scatter Diagram")
    }
    
    #---------------------------- BOX PLOT------------------------
    boxPlot <- function() {
      boxplot(data1~data2, xlab = "x" , ylab = "y", main = "Box Plot")
    }
    
    #----------------------------- q q PLOT --------------------------
    qPlot <- function(data1,data2) {
      qqplot(x= data1,y=data2,xlab = "xBeer", ylab = "y")
      qqnorm(data2)
      qqline(data2, col = "green")
    }
    
    #---------------------------- STEM -------------------------------
    Stem <- function(data) {
      stem(data)
    }
    
    repeat
    {
      cat('\n1.HISTOGRAM')
      cat('\n2.LINE GRAPH')
      cat('\n3.BAR PLOT')
      cat('\n4.PIE PLOT')
      cat('\n5.SCATTER PLOT')
      cat('\n6.BOX PLOT')
      cat('\n7.QPLOT')
      cat('\nBack\n')
      choice <- as.integer(readline(prompt = "ENTER your choice   ="))
      if(choice == 1)
      {
        histogram()
      }
      else if( choice==2 )
      {
        
        lineGraph()
      }
      else if( choice==3 )
      {
        
        barPlot()
      }
      
      else if(choice==4)
      {
        pie()
      }
      else if(choice==5)
      {
        scatter()
      }
      else if(choice==6)
      {
        boxPlot()
      }
      else if(choice==7)
      {
        qPlot()
      }
      else if(choice==7)
      {
        break
      }
      
      else
      {
        cat('wrong choice please re-enter the value')  
      }
    }
  }
  #-------------------------------- END OF MODULE 9 --------------------------
}

#================================== ADDITIONAL FUNCTIONS ======================

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  SUM  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#---------------------------------SUM------------------------------------------
sum1<-function(x)
{
  sumx <- 0
  for (i in x)
  {
    sumx <- sumx + i 
  }
  
  return (sumx)
}

#--------------------------------------SORTING------------------------------------
#---------------------------------using Bubble Sort------------------------------
larger = function(pair) {
  if(pair[1] > pair[2]) return(TRUE) else return(FALSE)
}
swap_if_larger = function(pair) {
  if(larger(pair)) {
    return(rev(pair)) 
  } else {
    return(pair)
  }
}
swap_pass = function(x) { 
  for(i in seq(1, length(x)-1)) {
    x[i:(i+1)] = swap_if_larger(x[i:(i+1)])
  }
  return(x)
}
sort1 = function(x) {
  new_vec = swap_pass(x)
  if(isTRUE(all.equal(x, new_vec))) { 
    return(new_vec) 
  } else {
    return(sort1(new_vec))
  }
  return(x)
}
#-----------------------------------POWER--------------------------------------
power1<-function(x,y)   #x to the power y
{
  r=1
  p<-1:y 
  for(i in p)
  {
    t = x
    r = r*t
  }
  return(r)
}
#--------------------------------SQUARE ROOT-----------------------------------
sqrt1<-function(num)
{
  x1 = (num * 1.0) / 2
  x2= (x1 + (num / x1)) / 2
  while(abs(x1 - x2) >= 0.0000001) {
    x1 = x2
    x2 = (x1 + (num / x1)) / 2
  }
  return(x2)
}
#-----------------------------COVARIANCE--------------------------------------
covar2<-function(x,y)
{
  nx<-length(x)
  ny<-length(y)
  mx<- mean1(x)
  my<-mean1(y)
  dx<-x-mx
  dy<-y-my
  if(nx == ny)
  {
    p<-dx*dy
    s<-sum1(p)
    cov<-s/(n-1)
    return(cov)
  }else
  {
    print("incompatible dimentions")
    return
  }
}
#-----------------------------FACTORIAL-----------------------------------------
fact<-function(num)
{
  f<-1
  if(num>0)
  {
    for(i in 1:num)
    {
      f<-f*i
    }
    return(f)
  }else if (num==0)
  {
    return(1)
  }else print("Please enter a positive number")
}
#=============================== CALLING ===================================
calculator<-function()
{
  repeat{
    cat('\n1.DESCRIPTIVE ANALYSIS\n')
    cat('2. PREDECTIVE ANALYSIS\n')
    cat('3.PROBABILITY ANALYSIS\n')
    cat('4.DISCRETE DISTRIBUTION FUNCTION\n')
    cat('5.INTERVAL ESTIMATION\n')
    cat('6.VISUALIZATIONS\n')
    cat('7.BAck\n')
    choice <- as.integer(readline(prompt = "Enter your choice : "))
    if(choice == 1)
    {
      DESCRIPTIVE_ANALYSIS()
    }
    else if( choice==2 )
    {
      
      PREDICTIVE_ANALYSIS()
    }
    else if( choice==3 )
    {
      
      PROBABILITY_ANALYSIS()
    }
    
    else if(choice==4)
    {
      DISCRETE_DISTRIBUTION()
    }
    else if(choice==5)
    {
      internal_estimation()
    }
    
    else if(choice==6)
    {
      VISUALISATION()
    }
    else if(choice==7)
    {
      stop("Goodbye")
    }
    else
    {
      cat('wrong choice please re-enter the value')  
    }
  }
}
calculator()
