library(party)
data <- read.csv(file.choose(),header=T)
start_time <- Sys.time()
str(data)
set.seed (1234)
dt = sort(sample(nrow(data), nrow(data)*.9))
train<-data[dt,]
validate<-data[-dt,]
tree<-ctree(stroke ~.,data = train)
plot(tree)
predict(tree,validate,type="prob")
pred <- predict(tree,validate)
pred1 <- as.integer(pred)
count=0

for (x in 1:length(validate$stroke)) {
  if(pred1[x]==validate$stroke[x] )
  {
    count=count+1
  }
}
accuracy = count/length(validate$stroke)
accuracy
end_time <- Sys.time()
end_time-start_time