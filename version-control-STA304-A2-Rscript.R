# Question 3
# input data
# Name Brand I stratum 1:
N1 = 120
lastyrSale1 = 24500
popMu1 = lastyrSale1/N1
# the sample values are:
x1 <- c(204, 143, 82, 256, 275, 198)
y1 <- c(210, 160, 75, 280, 300, 190)
# number of samples:
n1 = length(y1)


# Name Brand II stratum 2:
N2 = 180
lastyrSale2 = 21200
popMu2 = lastyrSale2/N2
# the sample values are:
x2 <- c(137, 189, 119, 63, 103, 107, 159, 63, 87)
y2 <- c(150, 200, 125, 60, 110, 100, 180, 75, 90)
# number of samples:
n2 = length(y2)


# total population size
N = N1+N2
# weight
W1 = N1/N
W2 = N2/N
# fractions
f1 = n1/N1
f2 = n2/N2
# sample mean for brand I
y1_bar = mean(y1)
x1_bar = mean(x1)
# sample mean for Brand II
y2_bar = mean(y2)
x2_bar = mean(x2)
# sample variance for brand I
y1_s2 = var(y1)
x1_s2 = var(x1)
# sample variance for Brand II
y2_s2 = var(y2)
x2_s2 = var(x2)
# coefficients between x and y (brand I)
corr1 = cor(y1, x1)
# coefficients between x and y (brand II)
corr2 = cor(y2, x2)



















# Question 4
# load data
mydata<-read.csv("StudentsMarks.csv")
# use x to record term test 1 marks,
# after removing students who missed the test.
x<-na.omit(mydata$Test.1)
N<-length(x)
N