
library(MASS)

# generate stimuli
N = 250;
Stimulus_generation <- function(mu_x, mu_y, sigma_x, sigma_y, cov_xy, num){
  # generate stimulus through GRT technique
  mu = c(mu_x, mu_y)
  sigma = matrix(0, nrow = 2,ncol = 2)
  
  sigma[1,1] = sigma_x
  sigma[1,2] = cov_xy
  sigma[2,1] = cov_xy
  sigma[2,2] = sigma_y 
  
  stimuli <- MASS::mvrnorm(n=num, mu = mu, Sigma = sigma)
  return(stimuli)
}

stimuli_a = Stimulus_generation(40, 60, 167.59, 167.59, 151.26, N)
stimuli_b = Stimulus_generation(60, 40, 167.59, 167.59, 151.26, N)

# scalling input stimuli
InputScaling <- function(input){
  stimuli = round(input/4)
  stimuli[stimuli > 25] = 25
  stimuli[stimuli < 1] = 1
  return(stimuli)
}

stimuli_a <- InputScaling(stimuli_a)
stimuli_b <- InputScaling(stimuli_b)

# scatter (input)
plot(stimuli_a[,1, drop=FALSE],
     stimuli_a[,2, drop=FALSE],
     pch = 19,
     xlim = c(1,25),ylim = c(1,25),
     main = 'Category Structure',
     xlab = 'Dimension X',
     ylab = 'Dimension Y')
points(stimuli_b[,1, drop=FALSE],
       stimuli_b[,2, drop=FALSE],pch = 5)
legend('topleft',pch = c(19,5),c('A','B'))

# combine stimuli matrix and label vector, and randomise stimuli presentation order
alabel <- matrix(1,nrow = N)
blabel <- matrix(2,nrow = N)

input = matrix(0,nrow = 500, ncol = 3)

input[1:250,1:2] = stimuli_a
input[1:250,3] = alabel
input[251:500,1:2] = stimuli_b
input[251:500,3] = blabel

input <- input[sample(1:nrow(input)),]

# initialise random weights
random_weights <- function(nrow,ncol,mu,sigma){
  random_vector = mu + sigma*runif(nrow*ncol,0,1)
  temp_weights <- matrix(random_vector, nrow = nrow, ncol = ncol)
  return(temp_weights)
}

weights_a = random_weights(25,25,0.0025,0.001);
weights_b = random_weights(25,25,0.0025,0.001);

# parameter settings
sc = 0.6
phi = 6

lr_att = 0
lr_w = 0.03

param = c(sc, lr_att, lr_w, phi)

# run the model
ALCOVE <- function(weights_a,weights_b, input, param){
  
  # position in hidden layer
  m = matrix(0, nrow = 25, ncol = 25)
  x = col(m)
  y = row(m)
  
  att = c(0.5, 0.5)
  ps = matrix(0,nrow = nrow(input), ncol = 2)
  result = matrix(0,nrow = nrow(input), ncol = 2)
  
  for (i in 1:nrow(input)){
    dx = (input[i,1] - x)**2
    dy = (input[i,2] - y)**2
    
    H = exp(-param[1]*sqrt(att[1]*dx + att[2]*dy))
    
    ca = sum(weights_a*H)
    cb = sum(weights_b*H)
    
    ca[ca < -1] = -1; ca[ca > 1] = 1
    cb[cb < -1] = -1; cb[cb > 1] = 1
    
    output = c(ca,cb)
    
    ps = exp(param[4]*output)/sum(exp(param[4]*output))
    
    if (input[i,3] == 1){
      targets = c(1,-1)
    }else{
      targets = c(-1,1)
    }
    
    # Error
    Err = sum((targets - c(ca,cb))**2)/2
    result[i,] = c(ps[input[i,3]], Err)
    
    signal = c(0,0)
    # updates
    signal[1] = targets[1] - ca
    signal[2] = targets[2] - cb
    
    weights_a = weights_a + param[3]*signal[1]*H
    weights_b = weights_b + param[3]*signal[2]*H
    
    deriv_atta = signal[1]*weights_a
    deriv_attb = signal[2]*weights_b
    deriv_att = deriv_atta + deriv_attb
    
    deriv_x = sum(param[1]*deriv_att*H*sqrt(dx))
    deriv_y = sum(param[1]*deriv_att*H*sqrt(dy))
    
    att = att - param[2]*c(deriv_x, deriv_y)
    att[att < 0] = 0
  }
  return(list(H,result))
}

result <- ALCOVE(weights_a,weights_b, input, param)

# result analysis
result1 = result[[1]]
result2 = result[[2]]

p = colMeans(matrix(result2[,1,drop = FALSE],
                    nrow = 50, ncol = 10))
Err = result2[,2,drop = FALSE]

# plot results
plot(c(1:500), Err, 
     xlab = 'Number of Trials', ylab = 'Error',
     main = 'Gradient Descent', pch = 18)

persp(c(1:25),c(1:25),result1)
