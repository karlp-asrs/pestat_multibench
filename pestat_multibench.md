---
title: "PE Stats with multiple benchmarks"
author: "Karl POlen"
date: "November 20, 2018"
output: 
  html_document:
    keep_md: TRUE
---



## Working with multiple benchmarks

Occasionally a circumstance may arise where you want to create a composite of private equity style investments that may have more than one benchmark.  For example, a large sponsor may provide both a general private equity fund and energy focused fund.  In that case you likely will want to benchmark the generalist fund with a broad market benchmark and the energy fund with a sector benchmark.  If you then want to calculate a composite for the overall performance of this manager you need a way to calculate a blended benchmark for comparison to the composite.  This post shows how to do that.

First let's prepare some data and calculate the performance for the individual funds.



```r
dates1=as.Date("2017/1/3")+c(0,280,400)
cf1=zoo(c(-100,20,100),dates1)
ind1=zoo(c(100,110,120),dates1)
dates2=as.Date("2017/2/12")+c(0,150,380)
cf2=zoo(c(-500,520,50),dates2)
ind2=zoo(c(100,110,130),dates2)
fund1=pestats(cf1,ind1)
fund2=pestats(cf2,ind2)
```

Fund 1 stats are below


```r
fund1[c("pme","irr","ind.irr")]
```

```
## $pme
## [1] 1.015152
## 
## $irr
## [1] 0.1916779
## 
## $ind.irr
## [1] 0.1745045
```

Fund 2 stats are below


```r
fund2[c("pme","irr","ind.irr")]
```

```
## $pme
## [1] 1.022378
## 
## $irr
## [1] 0.327971
## 
## $ind.irr
## [1] 0.2652845
```

So we have two investments with different performance, overlapping time frames and different scale.  The following illustrates a way to calculate composite performance.

IRR is easy.  Just add the two cash flows and calculate the IRR.


```r
irr.c=irr.z(mergesum.z(cf1,cf2),gips=TRUE)
irr.c
```

```
## [1] 0.2844453
```

Calculating the index irr and pme is a little trickier.  In order to do this, we calculate the future value of the cash flow using the respective benchmark for each fund and then use the future value data to calculate the pme and index irr.


```r
fv1=coredata(lastinvec(ind1))/ind1
fund1.fv=cf1*fv1
fv2=coredata(lastinvec(ind2))/ind2
fund2.fv=cf2*fv2
c.fv=mergesum.z(fund1.fv,fund2.fv)
pme.c=-sum(c.fv[c.fv>0])/sum(c.fv[c.fv<0])
alpha.c=log(1+irr.z(c.fv, gips=TRUE))
ind.irr=-1+exp(log(1+irr.c)-alpha.c)
pme.c
```

```
## [1] 1.021251
```

```r
alpha.c
```

```
## [1] 0.0384426
```

```r
ind.irr
```

```
## [1] 0.2360049
```

## A function to do this


```r
pestat_multibench=function(cfl,indl) {
  #cfl is a list of cash flows
  #indl is a list of total return indices
  if(length(cfl)!=length(indl)) stop("length of cash flow and index lists not the same")
  n=length(cfl)
  fv1=cfl[[1]]*0
  for(i in 1:n) {
    fv2=coredata(lastinvec(indl[[i]]))/indl[[i]]
    fv3=cfl[[i]]*fv2
    fv1=mergesum.z(fv1,fv3)
  }
  pme.c=-sum(fv1[fv1>0])/sum(fv1[fv1<0])
  alpha.c=log(1+irr.z(fv1,gips=TRUE))
  irr=irr.z(do.call(mergesum.z,cfl))
  ind.irr=-1+exp(log(1+irr)-alpha.c)
  pme=-sum(fv1[fv1>0])/sum(fv1[fv1<0])
  ans=list()
  ans$irr=irr
  ans$ind.irr=ind.irr
  ans$pme=pme
  ans$tvpi=tvpi(do.call(mergesum.z,cfl))
  return(ans)
}
```

Test the function


```r
cfl=list(cf1,cf2)
indl=list(ind1,ind2)
pestat_multibench(cfl,indl)
```

```
## $irr
## [1] 0.2844453
## 
## $ind.irr
## [1] 0.2360049
## 
## $pme
## [1] 1.021251
## 
## $tvpi
## [1] 1.15
```
