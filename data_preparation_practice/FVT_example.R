# FVT example
read.csv('P3-Future-500-The-Dataset.csv') -> fin

factor(fin$Profit) -> fin$Profit
str(fin)
as.numeric(fin$Profit) -> fin$Profit

str(fin)
head(fin)