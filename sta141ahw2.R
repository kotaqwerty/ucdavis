#### 
# sta 141A homework2 Kota Natsume 914530226

#Honor Code: "The codes and results derived by using these codes constitute my own work.
#I have consulted the following resources regarding this assignment:
#" (http://stackoverflow.com/questions/6827299/r-apply-function-with-multiple-parameters)


########################################## 1 ##############################################

# define function hidden marcov model
hmc = function(x0=1, pi00=0.5, pi10=0.5, myu0=0, myu1=0, sig0=1, sig1=1, n=10 ,z0=0){ 
  pi01 = 1-pi00 
  pi11 = 1-pi10 
  x = c(x0, numeric(n))
  for( i in 2:(n+1)){
    if(x[i-1] ==0 ){
      x[i] = sample( c(0,1) , 1 , prob = c(pi00,pi01))
    }else{
      x[i] = sample( c(0,1),  1, prob = c(pi10,pi11) )
    }
  }
  
  z = c(z0, numeric(n))
  for( i in 2:(n+1)){
    if(x[i] ==0 ){
      z[i] = rnorm(1,mean=myu0, sd=sig0)
    }else{
      z[i] = rnorm(1,mean=myu1, sd=sig1)
    }
  }
  ret <- c(list(x[-1]),list(z[-1])) ## have to add myu1  toka
  return(ret)
  print(pi10)
}

# generating vector for question(b), question(c)
b <- hmc( pi00 = 0.8,  pi10 = 0.2, myu1 = 2,n=200)
c <- hmc( x0 = 1,  pi00 = 0.8,  pi10 = 0.2, myu0 = 0, myu1 = 0, sig0 = 1, sig1 = 1, n = 200, z0 = 0)

head(b)
head(c)

### d
sum_b <- sum(b[[2]])
var_b <- sum((b[[2]])^2) - (length(b[[2]])*mean(b[[2]]))

sum_c <- sum(c[[2]])
var_c <- sum((c[[2]])^2) - (length(c[[2]])*mean(c[[2]]))

### e

sum_vec <- c()
var_vec <- c() 
n_vec <- c(1:200)
# generating new data
z_vec   <- b[[1]]

# get sum and var for each i's up to 200
for(i in c(1:200)){
  sum_b   <- sum(b[[2]][1:i])
  var_b  <- sum((b[[2]][1:i])^2) - (length(b[[2]][1:i])*mean(b[[2]][1:i]))
  sum_vec <- c(sum_vec,sum_b)
  var_vec <- c(var_vec,var_b)
}

# plot i against sum and var
par(mfrow=c(1,2))
plot(n_vec,sum_vec)
plot(n_vec,var_vec)

# plot i against zi
plot(n_vec,z_vec)





###################################### 2 ########################################

## function for getting index
find_ind <- function(var1,var2){
  which(var2 %in% var1)
}


## function for getting mean,sd,unique values
q2 <- function(x,z){
  unique_z <- unique(z)
  cat("unique values are: ", unique_z)
  ind      <- sapply(unique_z,find_ind,z)
  z0_mean  <- mean(x[ind[[2]]]) 
  cat(" z0mean: ", z0_mean)
  z1_mean  <- mean(x[ind[[1]]])  
  cat(" z1mean: ", z1_mean)
  z0_sd    <- (var(x[ind[[2]]]))^(0.5)
  cat(" z0sd: ", z0_sd)
  z1_sd    <- (var(x[ind[[1]]]))^(0.5)
  cat(" z1sd: ", z1_sd)
  return(c(ind,z0_mean,z1_mean,z0_sd,z1_sd))
}

# get the result of b

ret_b <- q2(b[[2]],b[[1]])


# t test for myu
var_x0x1 <- (ret_b[[5]]/length(ret_b[[1]])) + (ret_b[[6]]/length(ret_b[[2]])) 
t_stat   <-  (ret_b[[3]] - ret_b[[4]])/var_x0x1


## c

# set number 0 first
num_success <- 0

# when suceed +1 to numsucess repeat 100 times
for(i in c(1:100)){
  b <- hmc( pi00 = 0.8,  pi10 = 0.2, myu1 = 2,n=200)
  ret_b <- q2(b[[2]],b[[1]])
  var_x0x1 <- ((ret_b[[5]])^2/length(ret_b[[1]])) + ((ret_b[[6]])^2/length(ret_b[[2]])) 
  t_stat   <-  (ret_b[[3]] - ret_b[[4]])/var_x0x1
  # 95% critical value is about 1.97
  if (t_stat > 1.971896){
    num_success = num_success + 1
  }
}

# calculate the rate
success_rate = num_success/100
# 0.77






unique_z <- unique(b[[1]])
cat("unique values are: ", unique_z)
ind      <- sapply(unique_z,find_ind,b[[1]])
cat(" index: ", paste(ind[1]))
z0_mean  <- mean(b[[2]][ind[,1]]) 
cat(" z0mean: ", z0_mean)
z1_mean  <- mean(x[ind[[2]]])  
cat(" z1mean: ", z1_mean)
z0_sd    <- (var(x[ind[[1]]]))^(0.5)
cat(" z0sd: ", z0_sd)
z1_sd    <- (var(x[ind[[2]]]))^(0.5)
cat(" z1sd: ", z1_sd)










########## test
x = c(4,3,4,6,7,5,4)
z = c(0,0,1,1,1,0,1)
unique_z = unique(z)
ind      <- sapply(unique_z,myfxn,z)

find_ind <- function(var1,var2){
  which(var2 %in% var1)
}

ind_x    <- sapply(ind,mean)
x[,2]
sapply(1:3, function(x) mean(m[,x]))
which(ind %in% x)  
  
mean(x[ind[[1]]])


sapply(,mean) 
sapply(,mean) 
mean <- sapply(ind,mean) 

j <- list(a = 1:10, beta = exp(-3:3), logic = c(T,F,F,T))
sapply(j,mean)



