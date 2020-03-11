num_samples <- 100000
x1 <- rnorm(num_samples,0,1)
x2 <- rnorm(num_samples,0,1)
y1 <- rexp(num_samples,100)
y2 <- rexp(num_samples,100)


how_many_times <- 0

for (i in 1:num_samples){
  if (y1[i] < x1[i] & y1[i] < x2[i] & y1[i] < y2[i]){
    how_many_times <- how_many_times+1
  }
}

print (how_many_times/num_samples)
