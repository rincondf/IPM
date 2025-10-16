
sink(file = "pl4RES10.csv")

########

GR = 0.15
resT = 10
Sur = 0.1

j <- combn(seq(1, 100, 2), 4)


#####

# 70 - 0.13


str_pl70013D <- function(x) {
  
  ires = resT
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires) | 
       i >= x[3] & i <= (x[3] + ires) | 
       i >= x[4] & i <= (x[4] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      if(i >= x[4] & i <= (x[4] + ires)) apl <- pop[x[4]]
      
      pop[i+1] <- Sur * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus70013(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
}

resp70013D <- rep(NA, length(j[1, ]))

for(i in 1: length(j[1, ])) {
  resp70013D[i] <- str_pl70013D(j[, i])
}


resp70013D[which.min(resp70013D)]


#####

# 70 - 0.26


str_pl70026D <- function(x) {
  
  ires = resT
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires) | 
       i >= x[3] & i <= (x[3] + ires) | 
       i >= x[4] & i <= (x[4] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      if(i >= x[4] & i <= (x[4] + ires)) apl <- pop[x[4]]
      
      pop[i+1] <- Sur * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus70026(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
}

resp70026D <- rep(NA, length(j[1, ]))

for(i in 1: length(j[1, ])) {
  resp70026D[i] <- str_pl70026D(j[, i])
}

resp70026D[which.min(resp70026D)]


#####

# 70 - 0.46


str_pl70046D <- function(x) {
  
  ires = resT
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires) | 
       i >= x[3] & i <= (x[3] + ires) | 
       i >= x[4] & i <= (x[4] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      if(i >= x[4] & i <= (x[4] + ires)) apl <- pop[x[4]]
      
      pop[i+1] <- Sur * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus70046(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
}

resp70046D <- rep(NA, length(j[1, ]))

for(i in 1: length(j[1, ])) {
  resp70046D[i] <- str_pl70046D(j[, i])
}



resp70046D[which.min(resp70046D)]


#######

# 70 - 1


str_pl701D <- function(x) {
  
  ires = resT
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires) | 
       i >= x[3] & i <= (x[3] + ires) | 
       i >= x[4] & i <= (x[4] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      if(i >= x[4] & i <= (x[4] + ires)) apl <- pop[x[4]]
      
      pop[i+1] <- Sur * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus701(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
}

resp701D <- rep(NA, length(j[1, ]))

for(i in 1: length(j[1, ])) {
  resp701D[i] <- str_pl701D(j[, i])
}


resp701D[which.min(resp701D)]


#####

# 80 - 0.2

sus8002 <- function(t) {
  sl = 0.2
  mid = 20
  1 / (1 + exp(-sl * (t - mid)))
}


str_pl8002D <- function(x) {
  
  ires = resT
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires) | 
       i >= x[3] & i <= (x[3] + ires) | 
       i >= x[4] & i <= (x[4] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      if(i >= x[4] & i <= (x[4] + ires)) apl <- pop[x[4]]
      
      pop[i+1] <- Sur * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus8002(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
}

resp8002D <- rep(NA, length(j[1, ]))

for(i in 1: length(j[1, ])) {
  resp8002D[i] <- str_pl8002D(j[, i])
}



resp8002D[which.min(resp8002D)]


#####

# 80 - 0.46

str_pl80046D <- function(x) {
  
  ires = resT
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires) | 
       i >= x[3] & i <= (x[3] + ires) | 
       i >= x[4] & i <= (x[4] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      if(i >= x[4] & i <= (x[4] + ires)) apl <- pop[x[4]]
      
      pop[i+1] <- Sur * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus80046(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
}

resp80046D <- rep(NA, length(j[1, ]))

for(i in 1: length(j[1, ])) {
  resp80046D[i] <- str_pl80046D(j[, i])
}


resp80046D[which.min(resp80046D)]

#####

# 80 - 1


str_pl801D <- function(x) {
  
  ires = resT
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires) | 
       i >= x[3] & i <= (x[3] + ires) | 
       i >= x[4] & i <= (x[4] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      if(i >= x[4] & i <= (x[4] + ires)) apl <- pop[x[4]]
      
      pop[i+1] <- Sur * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus801(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
}

resp801D <- rep(NA, length(j[1, ]))

for(i in 1: length(j[1, ])) {
  resp801D[i] <- str_pl801D(j[, i])
}




resp801D[which.min(resp801D)]

#####

# 90 - 0.4


str_pl9004D <- function(x) {
  
  ires = resT
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires) | 
       i >= x[3] & i <= (x[3] + ires) | 
       i >= x[4] & i <= (x[4] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      if(i >= x[4] & i <= (x[4] + ires)) apl <- pop[x[4]]
      
      pop[i+1] <- Sur * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus9004(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
}

resp9004D <- rep(NA, length(j[1, ]))

for(i in 1: length(j[1, ])) {
  resp9004D[i] <- str_pl9004D(j[, i])
}




resp9004D[which.min(resp9004D)]


#####

# 90 - 1


str_pl901D <- function(x) {
  
  ires = resT
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires) | 
       i >= x[3] & i <= (x[3] + ires) | 
       i >= x[4] & i <= (x[4] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      if(i >= x[4] & i <= (x[4] + ires)) apl <- pop[x[4]]
      
      pop[i+1] <- Sur * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001) * sus901(i)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
}

resp901D <- rep(NA, length(j[1, ]))

for(i in 1: length(j[1, ])) {
  resp901D[i] <- str_pl901D(j[, i])
}


resp901D[which.min(resp901D)]


#####

# 100 - 1



str_pl100D <- function(x) {
  
  ires = resT
  
  t <- seq(0, 100)
  
  res <- rep(NA, length(x))
  
  pop <- rep(NA, length(t))
  pop[1] <- 5
  
  dama <- rep(NA, length(t))
  dama[1] <- 0
  
  dama1 <- rep(NA, length(t))
  dama1[1] <- 0
  
  for(i in 1: length(t)) {
    pop[i+1] <- pop[i] * (1 + GR * (1 - (pop[i] / 150)))
    if(i >= x[1] & i <= (x[1] + ires) | 
       i >= x[2] & i <= (x[2] + ires) | 
       i >= x[3] & i <= (x[3] + ires) | 
       i >= x[4] & i <= (x[4] + ires)) {
      if(i >= x[1] & i <= (x[1] + ires)) apl <- pop[x[1]]
      if(i >= x[2] & i <= (x[2] + ires)) apl <- pop[x[2]]
      if(i >= x[3] & i <= (x[3] + ires)) apl <- pop[x[3]]
      if(i >= x[4] & i <= (x[4] + ires)) apl <- pop[x[4]]
      
      pop[i+1] <- Sur * apl
    }
    
    
    dama[i+1] <- (pop[i] * 0.001)
    dama1[i+1] <- dama1[i] + (dama[i+1] * (1 - (dama1[i] / 0.8)))
  }
  
  dama1[length(t)]
}


resp100D <- rep(NA, length(j[1, ]))

for(i in 1: length(j[1, ])) {
  resp100D[i] <- str_pl100D(j[, i])
}



resp100D[which.min(resp100D)]


##############

sink()
