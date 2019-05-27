# importing data
read.csv('P3-Future-500-The-Dataset.csv') -> fin

head(fin)

fin$Expenses <- gsub(' Dollars','',fin$Expenses)
fin$Expenses <- gsub(',','',fin$Expenses)
# recap the dollar sign
fin$Revenue <- gsub('\\$','',fin$Revenue)
fin$Revenue <- gsub(',','',fin$Revenue)
fin$Growth <- gsub('%','',fin$Growth)

head(fin)
str(fin)

fin$Expenses <- as.numeric(fin$Expenses)
fin$Revenue <- as.numeric(fin$Revenue)
fin$Growth <- as.numeric(fin$Growth)

str(fin)
summary(fin)
