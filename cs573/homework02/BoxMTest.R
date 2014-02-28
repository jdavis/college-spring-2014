# Box's M-test for testing homogeneity of covariance matrices
#
# Written by Andy Liaw (2004) converted from Matlab
# Andy's note indicates that he has left the original Matlab comments intact
#
#
# Slight clean-up and fix with corrected documentation provided by Ranjan Maitra (2012)
#


BoxMTest <- function(X, cl, alpha=0.05) { 
  ## Multivariate Statistical Testing for the Homogeneity of Covariance 
  ## Matrices by the Box's M. 
  ## 
  ## Syntax: function [MBox] = BoxMTest(X,alpha) 
  ## 
  ## Inputs: 
  ## X - data matrix (Size of matrix must be n-by-p;  # RM changed
  ## variables=column 1:p). 
  ## alpha - significance level (default = 0.05). 
  ## Output: 
  ## MBox - the Box's M statistic. 
  ## Chi-sqr. or F - the approximation statistic test. 
  ## df's - degrees' of freedom of the approximation statistic test. 
  ## P - observed significance level. 
  ## 
  ## If the groups sample-size is at least 20 (sufficiently large), 
  ## Box's M test takes a Chi-square approximation; otherwise it takes 
  ## an F approximation. 
  ## 
  ## References: 
  ## 
  ## Stevens, J. (1992), Applied Multivariate Statistics for Social Sciences. 
  ## 2nd. ed., New-Jersey:Lawrance Erlbaum Associates Publishers. pp. 260-269. 
  
  if (alpha <= 0 || alpha >= 1) 
    stop('significance level must be between 0 and 1') 
  cl <- as.factor(cl)
  g = nlevels(cl) ## Number of groups. 
  n = table(cl) ## Vector of groups-size. 
  X <- as.matrix(X)
  N = nrow(X) 
  p = ncol(X) 
  bandera = 2 
  if (any(n >= 20))
    bandera = 1 
  ## Partition of the group covariance matrices.   
  
  covList <- tapply(as.matrix(X), rep(cl, ncol(X)), function(x, nc) cov(matrix(x, nc = nc)),
                    ncol(X))
  deno = sum(n) - g 
  suma = array(0, dim=dim(covList[[1]])) 
  for (k in 1:g) 
    suma = suma + (n[k] - 1) * covList[[k]] 
  Sp = suma / deno ## Pooled covariance matrix. 
  Falta=0 
  for (k in 1:g) 
    Falta = Falta + ((n[k] - 1) * log(det(covList[[k]]))) 
  
  MB = (sum(n) - g) * log(det(Sp)) - Falta ## Box's M statistic. 
  suma1 = sum(1 / (n[1:g] - 1)) 
  suma2 = sum(1 / ((n[1:g] - 1)^2)) 
  C = (((2 * p^2) + (3 * p) - 1) / (6 * (p + 1) * (g - 1))) * 
    (suma1 - (1 / deno)) ## Computing of correction factor. 
  if (bandera == 1)
  { 
    X2 = MB * (1 - C) ## Chi-square approximation. 
    v = as.integer((p * (p + 1) * (g - 1)) / 2) ## Degrees of freedom. 
    ## Significance value associated to the observed Chi-square statistic. 
    P = pchisq(X2, v, lower=FALSE)  #RM: corrected to be the upper tail 
    cat('------------------------------------------------\n'); 
    cat('   MBox       Chi-sqr.         df        P\n') 
    cat('------------------------------------------------\n') 
    cat(sprintf("%10.4f%11.4f%12.i%13.4f\n", MB, X2, v, P)) 
    cat('------------------------------------------------\n') 
    if (P >= alpha) { 
      cat('Covariance matrices are not significantly different.\n') 
    } else { 
      cat('Covariance matrices are significantly different.\n') 
    } 
    #return(list(MBox=MB, ChiSq=X2, df=v, pValue=P)) 
  }
  else
  { 
    ## To obtain the F approximation we first define Co, which combined to 
    ## the before C value are used to estimate the denominator degrees of 
    ## freedom (v2); resulting two possible cases. 
    Co = (((p-1) * (p+2)) / (6 * (g-1))) * (suma2 - (1 / (deno^2))) 
    if (Co - (C^2) >= 0) { 
      v1 = as.integer((p * (p + 1) * (g - 1)) / 2) ## Numerator DF. 
      v21 = as.integer(trunc((v1 + 2) / (Co - (C^2)))) ## Denominator DF. 
      F1 = MB * ((1 - C - (v1 / v21)) / v1) ## F approximation. 
      ## Significance value associated to the observed F statistic. 
      P1 = pf(F1, v1, v21, lower=FALSE) 
      cat('\n------------------------------------------------------------\n') 
      cat('   MBox           F           df1         df2          P\n') 
      cat('------------------------------------------------------------\n') 
      cat(sprintf("%10.4f%11.4f%11.i%14.i%13.4f\n", MB, F1, v1, v21, P1)) 
      cat('------------------------------------------------------------\n') 
      if (P1 >= alpha) { 
        cat('Covariance matrices are not significantly different.\n') 
      } else { 
        cat('Covariance matrices are significantly different.\n') 
      } 
      #return(list(MBox=MB, F=F1, df1=v1, df2=v21, pValue=P1)) 
    } else { 
      v1 = as.integer((p * (p + 1) * (g - 1)) / 2) ## Numerator df. 
      v22 = as.integer(trunc((v1 + 2) / ((C^2) - Co))) ## Denominator df. 
      b = v22 / (1 - C - (2 / v22)) 
      F2 = (v22 * MB) / (v1 * (b - MB)) ## F approximation. 
      ## Significance value associated to the observed F statistic. 
      P2 = pf(F2, v1, v22, lower=FALSE) 
      
      cat('\n------------------------------------------------------------\n') 
      cat(' MBox F df1 df2 P\n') 
      cat('------------------------------------------------------------\n') 
      cat(sprintf('%10.4f%11.4f%11.i%14.i%13.4f\n', MB, F2, v1, v22, P2)) 
      cat('------------------------------------------------------------\n') 
      
      if (P2 >= alpha) { 
        cat('Covariance matrices are not significantly different.\n') 
      } else { 
        cat('Covariance matrices are significantly different.\n') 
      } 
      #return(list(MBox=MB, F=F2, df1=v1, df2=v22, pValue=P2)) 
    } 
  }
}
