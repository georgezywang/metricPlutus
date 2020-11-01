Covid_daily = read.csv("Covid - State - Daily.csv")
income = read.csv("income_distribution_state.csv")
population = read.csv("population_state.csv")
mobility = read.csv("Mobility_State.csv")
employment = read.csv("Employment Combined - State - Daily.csv")
business_revenue = read.csv("Womply Revenue - State - Daily.csv")
consumer_spending = read.csv("Affinity - State - Daily.csv")
X = array(dim = c(56, 300, 11))

#the first column is consumer spending for income workers
for (i in 3061:length(consumer_spending$spend_all)){
  X[consumer_spending$statefips, (i-3060)/51+1, 1] = as.numeric(consumer_spending$spend_all[i])
}

#the second column is business revenue
for (i in 2602:length(business_revenue$revenue_all)){
  X[business_revenue$statefips, (i-2601)/51+1, 2] = as.numeric(business_revenue$revenue_all[i])
}

#the third column is daily increase in Covid
for (i in 2041:length(Covid_daily$new_case_count)){
  if (Covid_daily$new_case_count[i] != "."){
    X[Covid_daily$statefips, (i-2040)/51+1, 3] = as.numeric(Covid_daily$new_case_count[i]) 
  }
  else{
    X[Covid_daily$statefips, (i-2040)/51+1, 3] = NA
  }
}

#the fourth column is cumulative increase in Covid
for (i in 2041:length(Covid_daily$case_count)){
  if (Covid_daily$new_case_count[i] != "."){
    X[Covid_daily$statefips, (i-2040)/51+1, 4] = as.numeric(Covid_daily$case_count[i]) 
  }
  else{
    X[Covid_daily$statefips, (i-2040)/51+1, 3] = NA
  }
}

#the fifth column is income
for (i in 1:300){
  for (j in 1:51){
    num = income$GEO_ID[j+1]
    X[as.numeric(substr(num, nchar(num[1])-1, nchar(num[1]))), i, 5] = as.numeric(income$S1901_C01_012E[j+1])
  }
}

#the sixth column is employment for low-income workers
for (i in 2398:length(employment$emp_combined_inclow)){
  X[employment$statefips, (i-2397)/51+1, 6] = as.numeric(employment$emp_combined_inclow[i])
}

#the seventh column is employment for medium income workers
for (i in 2398:length(employment$emp_combined_incmiddle)){
  X[employment$statefips, (i-2397)/51+1, 7] = as.numeric(employment$emp_combined_incmiddle[i])
}

#the eighth column is employment for high income workers
for (i in 2398:length(employment$emp_combined_inchigh)){
  if (employment$emp_combined_inchigh[i] != "."){
    X[employment$statefips, (i-2397)/51+1, 8] = as.numeric(employment$emp_combined_inchigh[i])
  }
  else{
    X[employment$statefips, (i-2397)/51+1, 8] = NA
  }
}

#the ninth column is population
for (i in 1:300){
  for (j in 1:51){
    num = population$GEO_ID[j+1]
    X[as.numeric(substr(num, nchar(num[1])-1, nchar(num[1]))), i, 9] = as.numeric(population$S0101_C01_001E[j+1])
  }
}

#the tenth column is mobility
for (i in 307:length(mobility$gps_residential)){
  X[mobility$statefips, (i-306)/51+1, 10] = as.numeric(mobility$gps_away_from_home[i])
}

#the eleventh column is statefip
for (i in 1:300){
  for (j in 1:52){
    X[j, i, 11] = j
  }
}

for (i in 1:56){
  if (!is.na(X[i, 1, 1])){
    m = rep(0, 11)
    for (j in 1:11){
      cnt = 0
      s = 0
      for (k in 1:300){
        if (!is.na(X[i, k, j])){
          cnt = cnt+1
          s = s+X[i, k, j]
        }
      }
      m[j] = s/cnt
    }
    for (j in 1:300){
      for (k in 1:11){
        if (is.na(X[i, j, k])){
          X[i, j, k] = m[k]
        }
      }
    }
  }
}

Y = data.frame()
for (i in 1:56){
  if (!is.na(X[i, 1, 1])){
    Y = rbind(Y, X[i, , ])
  }
}
write.csv(Y, file = "data.csv")