#Junchen Yi  Date: 03/12/2023 Class:ALY6010 70330

#Part1
install.packages("MASS")
library(MASS)

data(cats)

male <- subset(cats, subset=(cats$Sex=="M"))
female <- subset(cats, subset=(cats$Sex=="F"))

t.test(male$Bwt, female$Bwt, var.equal = FALSE)

#Part2
before <- c(4.6, 7.8, 9.1, 5.6, 6.9, 8.5, 5.3, 7.1, 3.2, 4.4)
after <- c(6.6, 7.7, 9.0, 6.2, 7.8, 8.3, 5.9, 6.5, 5.8, 4.9)
# Paired t-test
t.test(before, after, paired = TRUE)
