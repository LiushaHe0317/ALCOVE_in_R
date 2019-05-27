
# set working directory
setwd('F:/Advanced_R_Programming/data_preparation_practice')
# etwd('F:\\Advanced_R_Programming\\data_preparation_practice')
getwd()
fin <- read.csv('P3-Future-500-The-Dataset.csv')

# first 10 lines
head(fin,10)
# last 10 lines
tail(fin)
# data set summary
summary(fin)
# data structure
str(fin)

# changing from non-factor to factor/ using factor()
fin$ID = factor(fin$ID)
# then see structure
str(fin)

# factor variable trap (FVT) --> changing from factor to non-factor
a = c('11','11','12','15','18')
b = as.numeric(a)
c = as.integer(a)

# converting factors into numeric
z = factor(a)
str(z)
x = as.numeric(z)
y = as.numeric(as.character(z))
str(x)
str(y)

