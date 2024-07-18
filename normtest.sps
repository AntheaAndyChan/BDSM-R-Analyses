* Encoding: UTF-8.
preserve.
set printback=none.
*****************************************************************************
* Univariate and multivariate tests of skew and kurtosis, a list of the
* 5 cases with the largest Mahalanobis distances, a plot of the
* squared distances, critical values for a single multivariate outlier.
*
* from: DeCarlo, L. T. (1997). On the meaning and use of kurtosis.
*         Psychological Methods, 2, 292-307.
*
* To use the macro, one needs two lines, one to include the macro
* in the program, and the other to execute it. Open the data file, then
* type the commands in a syntax window as follows:
*
* include 'c:\spsswin\normtest.sps'.
* normtest vars=x1,x2,x3,x4 /.
*
* The first line includes the macro, which in this case is named
* normtest.sps and is located in the spsswin directory, and the
* second line invokes the macro for variables x1 to x4, for example.
* (variable names can be separated by spaces or commas)
*
* Updated 2002: the plot command of SPSS is replaced by graph
*
* Updated 11/97:
* This version uses a corrected two-pass algorithm to compute
* the variance, from Chan, T. F., Golub, G. H., & LeVeque, R. J.
* (1983). Algorithms for computing the sample variance: Analysis
* and recommendations. American Statistician, 37, 242-247.
* Fisher's g statistics are given.
* Mardia's p-value fixed (multiplied by 2), and the statistic is
* computed using the biased variance estimator, as in SAS & EQS
*****************************************************************************.
define normtest (vars=!charend('/')).
matrix.
get x /variables=!vars /names=varnames /missing=omit.
compute n=nrow(x).
compute p=ncol(x).
compute s1=csum(x).
compute xbar=s1/n.
compute j=make(n,1,1).
compute xdev=x-j*xbar.
release x.
compute dev=csum(xdev).
compute devsq=(dev&*dev)/n.
compute ss=csum(xdev&*xdev).
* corrected two-pass algorithm.
compute m2=(ss-devsq)/n.
compute sdev=sqrt(m2).
compute m3=csum(xdev&**3)/n.
compute m4=csum(xdev&**4)/n.
compute sqrtb1=t(m3/(m2&*sdev)).
compute b2=t(m4/(m2&**2)).
compute g1=((sqrt(n*(n-1)))*sqrtb1)/(n-2).
compute g2=(b2-((3*(n-1))/(n+1)))*((n**2-1)/((n-2)*(n-3))).
******** quantities needed for multivariate statistics ********
compute s=sscp(xdev)/(n-1).
compute sb=s*(n-1)/n.
compute sinv=inv(s).
compute d=diag(s).
compute dmat=make(p,p,0).
call setdiag(dmat,d).
compute sqrtdinv=inv(sqrt(dmat)).
compute corr=sqrtdinv*s*sqrtdinv.
*** principal components for Srivastava's tests ***.
call svd(s,u,q,v).
compute pc=xdev*v.
call svd(sb,aa,bb,cc).
compute pcb=(xdev*cc).
release xdev.
*** Mahalanobis distances ***.
compute sqrtqinv=inv(sqrt(q)).
compute stdpc=pc*sqrtqinv.
compute dsq=rssq(stdpc).
release stdpc.
compute sqrtbbi=inv(sqrt(bb)).
compute stdpcb=pcb*sqrtbbi.
compute dsqb=rssq(stdpcb).
release stdpcb.
**************** univariate skew and kurtosis *****************
*** approximate Johnson's SU transformation for skew ***.
compute y=sqrtb1*sqrt((n+1)*(n+3)/(6*(n-2))).
compute beta2=3*(n**2+27*n-70)*(n+1)*(n+3)/((n-2)*(n+5)*(n+7)*
                (n+9)).
compute w=sqrt(-1+sqrt(2*(beta2-1))).
compute delta=1/sqrt(ln(w)).
compute alpha=sqrt(2/(w*w-1)).
compute sub1=delta*ln(y/alpha+sqrt((y/alpha)&**2+1)).
compute psub1=2*(1-cdfnorm(abs(sub1))).
print {n}/title"Number of observations:" /format=f5.
print {p}/title"Number of variables:" /format=f5.
print {g1,sqrtb1,sub1,psub1}
 /title"Measures and tests of skew:"
 /clabels="g1","sqrt(b1)","z(b1)","p-value"
 /rnames=varnames /format=f10.4.
*** Anscombe & Glynn's transformation for kurtosis.
compute eb2=3*(n-1)/(n+1).
compute vb2=24*n*(n-2)*(n-3)/(((n+1)**2)*(n+3)*(n+5)).
compute stm3b2=(b2-eb2)/sqrt(vb2).
compute beta1=6*(n*n-5*n+2)/((n+7)*(n+9))*sqrt(6*(n+3)*(n+5)/
                (n*(n-2)*(n-3))).
compute a=6+(8/beta1)*(2/beta1+sqrt(1+4/(beta1**2))).
compute zb2=(1-2/(9*a)-((1-2/a)/(1+stm3b2*sqrt(2/(a-4))))
            &**(1/3))/sqrt(2/(9*a)).
compute pzb2=2*(1-cdfnorm(abs(zb2))).
compute b2minus3=b2-3.
print {g2,b2minus3,zb2,pzb2}
 /title"Measures and tests of kurtosis:"
 /clabels="g2","b2-3","z(b2)","p-value"
 /rnames=varnames /format=f10.4.
compute ksq=sub1&**2+zb2&**2.
compute pksq=1-chicdf(ksq,2).
compute lm=n*((sqrtb1&**2/6)+(b2minus3&**2/24)).
compute plm=1-chicdf(lm,2).
print
 /title"Omnibus tests of normality (both chisq, 2 df):". 
print {ksq,pksq,lm,plm}
 /title"  D'Agostino & Pearson K sq    Jarque & Bera LM test"
 /clabels="K sq","p-value","LM","p-value"
 /rnames=varnames /format=f10.4.
do if p>1
print
 /title"*************** Multivariate Statistics ***************".
*** Small's multivariate tests ***.
compute uinv=inv(corr&**3).
compute uinv2=inv(corr&**4).
compute q1=t(sub1)*uinv*sub1.
* note: the variant of Small's kurtosis uses Anscombe & Glynn's
* transformation in lieu of SU (A & G is simpler to program).
compute q2=t(zb2)*uinv2*zb2.
compute pq1=1-chicdf(q1,p).
compute pq2=1-chicdf(q2,p).
print /title"Tests of multivariate skew:".
print {q1,p,pq1}/title"  Small's test (chisq)"
 /clabels="Q1","df","p-value"/format=f10.4.
*** Srivastava's multivariate tests ***.
compute pcs1=csum(pc).
compute pcs2=csum(pc&**2).
compute pcs3=csum(pc&**3).
compute pcs4=csum(pc&**4).
release pc.
compute mpc2=(pcs2-(pcs1&**2/n))/n.
compute mpc3=(pcs3-(3/n*pcs1&*pcs2)+(2/(n**2)*(pcs1&**3)))/n.
compute mpc4=(pcs4-(4/n*pcs1&*pcs3)+(6/(n**2)*(pcs2&*(pcs1&**2)))
            -(3/(n**3)*(pcs1&**4)))/n.
compute pcb1=mpc3/(mpc2&**1.5).
compute pcb2=mpc4/(mpc2&**2).
compute sqb1p=rsum(pcb1&**2)/p.
compute b2p=rsum(pcb2)/p.
compute chib1=sqb1p*n*p/6.
compute normb2=(b2p-3)*sqrt(n*p/24).
compute pchib1=1-chicdf(chib1,p).
compute pnormb2=2*(1-cdfnorm(abs(normb2))).
print {chib1,p,pchib1}
 /title"  Srivastava's test"
 /clabels="chi(b1p)","df","p-value"/format=f10.4.
print /title"Tests of multivariate kurtosis:".
print {q2,p,pq2}
 /title"  A variant of Small's test (chisq)"
 /clabels="VQ2","df","p-value"/format=f10.4.
print {b2p,normb2,pnormb2}
 /title"  Srivastava's test"
 /clabels="b2p","N(b2p)","p-value"/format=f10.4.
*** Mardia's multivariate kurtosis ***.
compute b2pm=csum(dsqb&**2)/n.
compute nb2pm=(b2pm-p*(p+2))/sqrt(8*p*(p+2)/n).
compute pnb2pm=2*(1-cdfnorm(abs(nb2pm))).
print {b2pm,nb2pm,pnb2pm}
 /title"  Mardia's test"
 /clabels="b2p","N(b2p)","p-value"/format=f10.4.
compute q3=q1+q2.
compute q3df=2*p.
compute pq3=1-chicdf(q3,q3df).
print /title"Omnibus test of multivariate normality:".
print {q3,q3df,pq3}
 /title"  (based on Small's test, chisq)"
 /clabels="VQ3","df","p-value"/format=f10.4.
end if.
compute cse={1:n}.
compute case=t(cse).
compute rnk=rnkorder(dsq).
compute top=(n+1)-rnk.
compute pvar=make(n,1,p).
compute ddf=make(n,1,(n-p-1)).
compute ncase=make(n,1,n).
compute a01=make(n,1,(1-.01/n)).
compute a05=make(n,1,(1-.05/n)).
compute mahal={case,rnk,top,dsq,pvar,ddf,ncase,a01,a05}.
save mahal /outfile=*
 /variables=case,rnk,top,dsq,pvar,ddf,ncase,a01,a05.
end matrix.
dataset name temp.
dataset activate temp.
sort cases by top (a).
do if case=1.
compute f01=idf.f(a01,pvar,ddf).
compute f05=idf.f(a05,pvar,ddf).
compute fc01=(f01*pvar*(ncase-1)**2)/(ncase*(ddf+pvar*f01)).
compute fc05=(f05*pvar*(ncase-1)**2)/(ncase*(ddf+pvar*f05)).
print space.
print
 /'Critical values (Bonferroni) for a single multivar. outlier:'.
print space.
print
 /'  critical F(.05/n) ='fc05 (f5.2)'  df ='pvar (f3)','ddf (f4).
print
 /'  critical F(.01/n) ='fc01 (f5.2)'  df ='pvar (f3)','ddf (f4).
print space.
print /'5 observations with largest Mahalanobis distances:'.
end if.
execute.
do if top < 6.
print
 /'  rank ='top (f2)'  case# ='case (f4)'  Mahal D sq ='dsq (f10.2).
end if.
execute.
compute chisq=idf.chisq((rnk-.5)/ncase,pvar).
graph.
 /TITLE='Plot of ordered squared distances'.
 /SCATTERPLOT (OVERLAY)=dsq with chisq.
execute.
!enddefine.
restore.