# Standardization Transformation

data <- Churn

for (k in 2:8) { # we used 2:8 since those are the numeric columns
  data[,k] <- (data[,k]-mean(data[,k]))/sd(data[,k])
}

data
