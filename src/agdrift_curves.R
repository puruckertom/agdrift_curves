#updated list of installed packages
print("list of loaded packages: ")
print((.packages()))

#set some default directories based on machine location
if(Sys.info()[4]=="stp-air"){
  dir.root <- "~/git/agdrift_curves/"
}
print(paste("Root directory location: ", dir.root, sep=""))

dir.csv.in <- paste(dir.root, "data_in/", sep="")
dir.graphics <- paste(dir.root, "graphics/", sep="")

#check to see if directories are accessible
print(paste("check to see if R can access files OK: ", file.exists(dir.csv.in), sep = ""))

agdrift <- read.csv(file = paste(dir.csv.in, "opp_spray_drift_values.csv", sep = ""), header = TRUE)
summary(agdrift)

#http://math.stackexchange.com/questions/839385/how-to-perform-a-monotonic-function-fitting-of-data-points
a_start<-0.4
b_start<--0.05
y <- agdrift$aerial_vf2f
x <- agdrift$distance_ft
x[1] <- 0.0001
plot(x,y,type='l')
vf2f <- nls(y~a*exp(b*x), start=list(a=a_start,b=b_start))
summary(vf2f)
a <- coef(vf2f)[1]
b <- coef(vf2f)[2]
y2 = a*exp(b*x)
lines(x,y2,type='l',col='red')
