## 1.

df <- with(mtcars, data.frame(y=mpg, x1=disp, x2=hp, x3=wt))

## 2.

nll_lm <- function(par, data){
  sigma <- par[5]
  betas <- matrix(data = par[-5], ncol=1)
  
  # y 
  y_i <- data[,1]
  
  row_len <- nrow(data)
  col_len <- ncol(data)
  
  # x 
  x_i <- matrix(nrow=row_len, ncol=col_len)
  
  # 1st column of 1's
  x_i[,1] <- rep(1,row_len)
  
  # remaining columns with x_i
  for(i in 2:col_len) {
    x_i[,i] <- data[,i]
  }
  
  epsilon <- y_i - x_i %*% betas
  
  log_like <- sum(dnorm(epsilon, mean=0, sd=sigma, log=TRUE))
  return (-log_like)
  
}

# test with par
par <- c(1,2,3,4,0.5)
nll_lm(par,df)
```

## 3. 

my <- mean(df$y)
opt_lm <- optim(c(my, 5, 5, 5, 5), nll_lm, data=df)
paste0("beta_0_hat = ", opt_lm$par[1])
paste0("beta_1_hat = ", opt_lm$par[2])
paste0("beta_2_hat = ", opt_lm$par[3])
paste0("beta_3_hat = ", opt_lm$par[4])
paste0("sigma_hat = ", opt_lm$par[5])


## 4.

# The negative log likelihood function was necessary, as optim() by default minimises the given function. In order to find $\hat\beta$ and $\hat\sigma$ using the log likelihood, optim() would need to maximise the function.  


## 5.

# y
y_mat <- matrix(df[,1], ncol=1)

# x 
x_mat <- matrix(nrow=nrow(df), ncol=ncol(df))

# 1st column of 1's
x_mat[,1] <- rep(1,nrow(df))

# remaining columns with x_i
for(i in 2:ncol(df)) {
  x_mat[,i] <- df[,i]
}

# beta_hat = (X'X)^-1X'Y
beta_hat <- solve(t(x_mat) %*% x_mat) %*% (t(x_mat) %*% y_mat)
beta_hat
```

# The values for beta0 and beta3 are quite different from those given from the optim() function. The values for beta1 and beta2 are closer.    

## 6. 

sigma_sqr <- (sum(y_mat-mean(y_mat))^2) / nrow(df)-2
sqrt(sigma_sqr)

# The estimate for $\hat\sigma$ was so small it produced an NaN.  

## Exercise 4

mod <- lm(y ~ x1+x2+x3, data = df)
paste0("beta0 = ", mod$coefficients[1])
paste0("beta1 = ", mod$coefficients[2])
paste0("beta2 = ", mod$coefficients[3])
paste0("beta3 = ", mod$coefficients[4])
paste0("sigma = ", sigma(mod))

