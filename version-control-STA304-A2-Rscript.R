# load data
mydata<-read.csv("StudentsMarks.csv")
# use x to record term test 1 marks,
# after removing students who missed the test.
x<-na.omit(mydata$Test.1)
N<-length(x)
N