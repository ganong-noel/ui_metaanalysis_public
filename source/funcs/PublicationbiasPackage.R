MetastudyMoments <- function(betap,cutoffs,symmetric,X,sigma) {
  #Calculate GMM Moments for metastudy applications
  n=length(X);
  betap=as.matrix(betap);
  #betap=t(betap)
  betap=rbind(betap,1)

  #regressors for step function p
  T=X/sigma;

  Tpowers=matrix(0,n,length(cutoffs)+1)
  if (symmetric==1) {
    Tpowers[,1]=ifelse(abs(T)<cutoffs[1],1,0)
    if (length(cutoffs)>1) {
      for (m in c(2:length(cutoffs))) {
        Tpowers[,m]=(ifelse(abs(T)<cutoffs[m],1,0))*(ifelse(abs(T)>=cutoffs[m-1]));
      }
    }
    Tpowers[,length(cutoffs)+1]=ifelse(abs(T)>=cutoffs[length(cutoffs)],1,0);
  } else {
    Tpowers[,1]=ifelse(T<cutoffs[1],1,0);
    if (length(cutoffs)>1) {
      for (m in c(2:length(cutoffs))) {
        Tpowers[,m]=(ifelse(T<cutoffs[m],1,0))*(ifelse(T>=cutoffs[m-1],1,0));
      }
    }
    Tpowers[,length(cutoffs)+1]=ifelse(T>=cutoffs[length(cutoffs)],1,0)
  }

  phat=Tpowers%*%betap;

  Xmat1=matrix(X,length(X),length(X));
  Xmat2=t(Xmat1);
  sigmamat1=matrix(sigma,length(X),length(X));
  sigmamat2=t(sigmamat1);
  indicator=ifelse(sigmamat1>=sigmamat2,1,0);
  sigmadiff=sqrt(indicator*(sigmamat1^2-sigmamat2^2))+sqrt((1-indicator)*(sigmamat2^2-sigmamat1^2))
  sigmadiff=Re(sigmadiff)
  pmat1=matrix(phat,length(X),length(X))
  pmat2=t(pmat1)

  moment_mean=matrix(0,1,length(cutoffs))
  rhat=matrix(0,n,length(cutoffs))

  if (symmetric==1) {
    for (k in c(1:length(cutoffs))) {
      c=cutoffs[k]
      base_moments=(pmat2^-1)*(pmat1^-1)*indicator*((pnorm((c*sigmamat1-Xmat2)/sigmadiff)-pnorm((-c*sigmamat1-Xmat2)/sigmadiff))-(((ifelse(Xmat1<=c*sigmamat1,1,0))-(ifelse(Xmat1<=-c*sigmamat1,1,0)))))+(pmat2^-1)*(pmat1^-1)*(1-indicator)*((pnorm((c*sigmamat2-Xmat1)/sigmadiff)-pnorm((-c*sigmamat2-Xmat1)/sigmadiff))-(((ifelse(Xmat2<=c*sigmamat2,1,0))-(ifelse(Xmat2<=-c*sigmamat2,1,0)))))

      base_moments=base_moments-diag(diag(base_moments))
      base_moments[is.nan(base_moments)]=0
      moment_mean[1,k]=((choose(n,2)^-1)/2)*sum(base_moments)
      #Built normalization to variance into rhat to ensure correct answers with culstered variance estimators
      rhat[,k] = 2*((n-1)^-1)*as.matrix(rowSums(base_moments,na.rm=FALSE))
    }
  } else {
    for (k in c(1:length(cutoffs))) {
      c=cutoffs[k]
      base_moments = (pmat2^-1)*(pmat1^-1)*indicator*(pnorm((c*sigmamat1-Xmat2)/sigmadiff)-(ifelse(Xmat1<=c*sigmamat1,1,0)))+(pmat2^-1)*(pmat1^-1)*(1-indicator)*(pnorm((c*sigmamat2-Xmat1)/sigmadiff)-(ifelse(Xmat2<=c*sigmamat2,1,0)))
      base_moments=base_moments-diag(diag(base_moments))
      base_moments[is.nan(base_moments)]=0
      moment_mean[1,k]=((choose(n,2)^-1)/2)*sum(base_moments)
      #Built normalization to variance into rhat to ensure correct answers with culstered variance estimators
      rhat[,k] = 2*((n-1)^-1)*as.matrix(rowSums(base_moments,na.rm=FALSE))
    }
  }
  moment_var=cov(rhat)
  raw_moments=rhat
  return(list("moment_mean"= moment_mean, "moment_var" = moment_var, "raw_moments" = raw_moments))
}

ComputingMetastudyMoments <- function(betap,cutoffs,symmetric,X,sigma) {
  #Calculate GMM Moments for metastudy applications
  n=length(X);
  gamma_1=as.matrix(betap[1])
  gamma_2=as.matrix(betap[2])
  betap=as.matrix(betap[3:length(betap)]);

  #regressors for step function p
  T=X/sigma;

  Tpowers=matrix(0,n,length(cutoffs)+1)
  if (symmetric==1) {
    Tpowers[,1]=ifelse(abs(T)<cutoffs[1],1,0)
    if (length(cutoffs)>1) {
      for (m in c(2:length(cutoffs))) {
        Tpowers[,m]=(ifelse(abs(T)<cutoffs[m],1,0))*(ifelse(abs(T)>=cutoffs[m-1]));
      }
    }
    Tpowers[,length(cutoffs)+1]=ifelse(abs(T)>=cutoffs[length(cutoffs)],1,0);
  } else {
    Tpowers[,1]=ifelse(T<cutoffs[1],1,0);
    if (length(cutoffs)>1) {
      for (m in c(2:length(cutoffs))) {
        Tpowers[,m]=(ifelse(T<cutoffs[m],1,0))*(ifelse(T>=cutoffs[m-1],1,0));
      }
    }
    Tpowers[,length(cutoffs)+1]=ifelse(T>=cutoffs[length(cutoffs)],1,0)
  }

  phat=Tpowers%*%betap;

  Xmat1=matrix(X,length(X),length(X));
  Xmat2=t(Xmat1);
  sigmamat1=matrix(sigma,length(X),length(X));
  sigmamat2=t(sigmamat1);
  indicator=ifelse(sigmamat1>=sigmamat2,1,0);
  sigmadiff=sqrt(indicator*(sigmamat1^2-sigmamat2^2))+sqrt((1-indicator)*(sigmamat2^2-sigmamat1^2))
  sigmadiff=Re(sigmadiff)
  pmat1=matrix(phat,length(X),length(X))
  pmat2=t(pmat1)

  moment_mean=matrix(0,1,length(cutoffs))
  rhat=matrix(0,n,length(cutoffs))

  if (symmetric==1) {
    for (k in c(1:length(cutoffs))) {
      c=cutoffs[k]
      base_moments=(pmat2^-1)*(pmat1^-1)*indicator*((pnorm((c*sigmamat1-Xmat2)/sigmadiff)-pnorm((-c*sigmamat1-Xmat2)/sigmadiff))-(((ifelse(Xmat1<=c*sigmamat1,1,0))-(ifelse(Xmat1<=-c*sigmamat1,1,0)))))+(pmat2^-1)*(pmat1^-1)*(1-indicator)*((pnorm((c*sigmamat2-Xmat1)/sigmadiff)-pnorm((-c*sigmamat2-Xmat1)/sigmadiff))-(((ifelse(Xmat2<=c*sigmamat2,1,0))-(ifelse(Xmat2<=-c*sigmamat2,1,0)))))

      base_moments=base_moments-diag(diag(base_moments))
      base_moments[is.nan(base_moments)]=0
      moment_mean[1,k]=((choose(n,2)^-1)/2)*sum(base_moments)
      #Built normalization to variance into rhat to ensure correct answers with culstered variance estimators
      rhat[,k] = 2*((n-1)^-1)*as.matrix(rowSums(base_moments,na.rm=FALSE))
    }
  } else {
    for (k in c(1:length(cutoffs))) {
      c=cutoffs[k]
      base_moments = (pmat2^-1)*(pmat1^-1)*indicator*(pnorm((c*sigmamat1-Xmat2)/sigmadiff)-(ifelse(Xmat1<=c*sigmamat1,1,0)))+(pmat2^-1)*(pmat1^-1)*(1-indicator)*(pnorm((c*sigmamat2-Xmat1)/sigmadiff)-(ifelse(Xmat2<=c*sigmamat2,1,0)))
      base_moments=base_moments-diag(diag(base_moments))
      base_moments[is.nan(base_moments)]=0
      moment_mean[1,k]=((choose(n,2)^-1)/2)*sum(base_moments)
      #Built normalization to variance into rhat to ensure correct answers with culstered variance estimators
      rhat[,k] = 2*((n-1)^-1)*as.matrix(rowSums(base_moments,na.rm=FALSE))
    }
  }
  base_mean_moment=(phat^-1)*(X-as.vector(gamma_1))
  base_sd_moment=(phat^-1)*((X-as.vector(gamma_1))^2-sigma^2-as.vector(gamma_2)^2)
  mean_moment=(1/n)*sum(base_mean_moment,na.rm=FALSE)
  sd_moment=(1/n)*sum(base_sd_moment,na.rm=FALSE)
  moment_mean=cbind(mean_moment, sd_moment, moment_mean)
  rhat=cbind(base_mean_moment,base_sd_moment,rhat)
  moment_var=cov(rhat)
  raw_moments=rhat
  return(list("moment_mean"= moment_mean, "moment_var" = moment_var, "raw_moments" = raw_moments))
}

MetastudyGMMObjective <- function(betap,cutoffs,symmetric,X,sigma,cluster_ID) {
  #Calculate continuously updating GMM objective
  mom=MetastudyMoments(betap,cutoffs,symmetric,X,sigma);
  moments_mean=mom$moment_mean;
  rhat=mom$raw_moments;
  Sigma_hat=Clustered_covariance_estimate(rhat,cluster_ID);
  objective=length(X)*moments_mean%*%solve(Sigma_hat)%*%t(moments_mean);
  return(objective)
}

ComputingMetastudyGMMObjective <- function(beta_theta,betap,cutoffs,symmetric,X,sigma,cluster_ID) {
  betap=c(beta_theta,betap)
  betap=as.matrix(betap)
  betap=t(betap)
  betap=cbind(betap,1)
  #Calculate continuously updating GMM objective
  mom=ComputingMetastudyMoments(betap,cutoffs,symmetric,X,sigma);
  moments_mean=mom$moment_mean;
  rhat=mom$raw_moments;
  Sigma_hat=Clustered_covariance_estimate(rhat,cluster_ID);
  objective=length(X)*moments_mean%*%solve(Sigma_hat)%*%t(moments_mean);
  return(objective)
}

Clustered_covariance_estimate <- function(g,cluster_index) {
  sorted=sort(cluster_index,decreasing=FALSE,index.return=TRUE)
  cluster_index=as.matrix(sorted$x)
  I=as.matrix(sorted$ix)
  g=as.matrix(g[I,])
  g=g-matrix(rep(colMeans(g),dim(g)[1]),ncol=dim(g)[2],byrow=TRUE)
  gsum=matrix(cumsum(g),dim(g)[1],dim(g)[2])
  index_diff=as.matrix(ifelse(cluster_index[2:length(cluster_index),1]!=cluster_index[1:(length(cluster_index)-1),1],1,0))
  index_diff=rbind(index_diff,1)
  gsum=subset(gsum,index_diff==1)
  gsum=rbind(gsum[1,],diff(gsum))
  Sigma=(1/(dim(g)[1]-1))*(t(gsum)%*%gsum)
  return(Sigma)
}

EstimatingSelection <- function(X,sigma,symmetric,cluster_ID,cutoffs,Studynames, var_name) {
  GMM_obj <- function(Psi){
    MetastudyGMMObjective(Psi, cutoffs, symmetric, X, sigma, cluster_ID)+max(-min(Psi),0)*10^5
  }

  mini<-optim(par=Psihat0,fn=GMM_obj,method="BFGS",control = list(abstol=10^-8,maxit=10^5));

  #For more accurate optimization, use the following:
  #Psihat1=mini$par;
  #Objval=mini$value;

  #mini<-optim(par=Psihat1,fn=GMM_obj);

  Psihat=mini$par;
  Objval=mini$value;

  GMM_obj_new<-function(Psi){
    #all_in_one=cbind(Psi,Psihat)
    ComputingMetastudyGMMObjective(Psi,Psihat,cutoffs,symmetric,X,sigma,cluster_ID)
  }

  mini<-optim(par=Psihat0_theta,fn=GMM_obj_new,method="BFGS",control = list(abstol=10^-8,maxit=10^5));

  #For more accurate optimization, use the following:
  #Psihat1_theta=mini$par;
  #Objval_theta=mini$value;

  #mini<-optim(par=Psihat1_theta,fn=GMM_obj_new);

  Psihat_theta=mini$par;
  Objval_theta=mini$value;

  Psihat_theta[2]=abs(Psihat_theta[2])

  Psihat_all = cbind(Psihat_theta,Psihat);

  mom=ComputingMetastudyMoments(cbind(Psihat_all,1),cutoffs,symmetric,X,sigma)
  moments=mom$moment_mean;
  Sigma_temp=mom$moment_var;
  rhat=mom$raw_moments;

  Sigma_hat=Clustered_covariance_estimate(rhat,cluster_ID);

  stepsize=10^-3;
  G=matrix(0,dim(moments)[2],length(Psihat_all));
  for (n1 in c(1:length(Psihat_all))) {
    beta_plus=Psihat_all;
    beta_plus[n1]=beta_plus[n1]+stepsize
    mom=ComputingMetastudyMoments(c(beta_plus,1),cutoffs,symmetric,X,sigma);
    moments_plus=mom$moment_mean;
    beta_minus=Psihat_all;
    beta_minus[n1]=beta_minus[n1]-stepsize
    mom=ComputingMetastudyMoments(cbind(beta_minus,1),cutoffs,symmetric,X,sigma);
    moments_minus=mom$moment_mean;
    G[,n1]=t(moments_plus-moments_minus)/(2*stepsize);
  }

  #In any case you get an error on an almost numerically singular matrix,
  #try using ``MASS::ginv'' instead of ``solve''. This command will run a generalized
  #inverse matrix and usually deals with this error.

  Varhat_all=solve(t(G)%*%solve(Sigma_hat)%*%G)/length(X);
  se_robust=sqrt(diag(Varhat_all));
  dof=dim(moments)[2];
  Varhat=Varhat_all[3:dim(Varhat_all)[1],3:dim(Varhat_all)[2]];

  if (length(cutoffs)==1) {
    Psi_grid=c(seq(0.001,5,0.001),10,10^5)
    S_store=matrix(0,length(Psi_grid),1)
    for (m in c(1:length(Psi_grid))) {
      S_store[m,1]=GMM_obj(Psi_grid[m])
    }

    CS_LB=min(Psi_grid[S_store<qchisq(0.95,1)]);
    CS_UB=max(Psi_grid[S_store<qchisq(0.95,1)]);
  } else if (length(cutoffs)==2) {
    gridsteps=200;
    Psi_grid1=seq(0.01,0.25,(0.25-0.01)/(gridsteps-1));
    Psi_grid2=seq(0.05,5,(5-0.05)/(gridsteps-1));
    S_store=matrix(0,length(Psi_grid1),length(Psi_grid2));
    for (m1 in c(1:length(Psi_grid1))) {
      for (m2 in c(1:length(Psi_grid2))) {
        Psi_temp=c(Psi_grid1[m1],Psi_grid2[m2]);
        S_store[m1,m2]=GMM_obj(Psi_temp);
      }

    }
    Psi_grid=cbind(t(Psi_grid1),t(Psi_grid2));
  } else {
    Psi_grid=c(seq(0.001,5,0.001),10,10^5)
    S_store=matrix(0,length(Psi_grid),1)
  }

  Psi_grid=as.matrix(Psi_grid)
  
  # Define column names
  columns_names <- c("Theta", "Sigma")
  if (length(Psihat) == 1) {
    columns_names <- c(columns_names, "beta_{p}")
  } else {
    for (i in 1:length(Psihat)) {
      columns_names <- c(columns_names, paste0("beta_p,", i))
    }
  }
  
  # Create a matrix of results
  results <- matrix(rbind(format(Psihat_all, digits = 2), format(se_robust, digits = 2)), nrow = 2, dimnames = list(c(1:2), columns_names))
  results[2, ] <- paste0("(", results[2, ], ")")
  results <- as.matrix(results)
  
  # Convert results to a data frame
  df_results <- as.data.frame(results)
  colnames(df_results) <- gsub("beta_\\{p\\}", "beta_.p.", colnames(df_results))
  
  # store df_results as a global variable:
  assign(var_name, df_results, envir = .GlobalEnv)
  
}

PublicationbiasGMM <- function(X,sigma,cluster_ID,symmetric,cutoffs,Studynames, var_name) {
  #PublicationbiasGMM
  #Arguments the user has to define:
  #X: studies estimates
  #sigma: studies standard deviations
  #symmetric: dummy, whether p is symmetric around 0
  #cluster_ID: stuides for clustered standard errors
  #cutoffs: a COLUMN vector of cutoffs to use in step function,
  #should be given in an increasing order
  #Studynames: the names of the studies the estimates and standard errors are drawn from
  #Note that it is possible to just plug in numbers. However, the values must be
  #of mode character. For defult you may use
  #Studynames <- vector(mode="character", length=length(X)). This will create an empty vector
  #of size n with blank characters

  #Starting values for optimization (can be modified by the researcher)
  Psihat0 <<- matrix(1,1,length(cutoffs)); #betap
  Psihat0_theta <<- matrix(0:1,1,2); #[theta, sd(theta)]

  #Primitives
  n=length(X);
  C=matrix(1,n,1);
  name='GMMResults';

  includeinfigure <<- array(matrix(1,n,1));
  includeinestimation <<- array(matrix(1,n,1));

  #If you want to get the step function p values at the points of discontinuity
  doestimates <<- 1;

  # Estimating the model ----------------------------------------------------
  if (doestimates==1) {
    Estimates=EstimatingSelection(X,sigma,symmetric,cluster_ID,cutoffs,Studynames, var_name)
    Psihat=Estimates$Psihat
    Varhat=Estimates$Varhat
  }

}
