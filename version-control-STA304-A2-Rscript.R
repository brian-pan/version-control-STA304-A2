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



# Question 4
# load data
mydata<-read.csv("StudentsMarks.csv")
# use x to record term test 1 marks,
# after removing students who missed the test.
x<-na.omit(mydata$Test.1)
N<-length(x)
N