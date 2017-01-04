#updated list of installed packages
print("list of loaded packages: ")
print((.packages()))

#set some default directories based on machine location
if(Sys.info()[4]=="stp-air"){
  dir.root <- "~/git/agdrift_curves/"
}
if(Sys.info()[4]=="DZ2626UTPURUCKE"){
  dir.root <- "k:/git/agdrift_curves/"
}
if(Sys.info()[4]=="DZ2626USSINNATH"){
  dir.root <- "c:/git/agdrift_curves/"
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
#5th order
a_start = -0.1
b_start = 0.1
c_start = -0.1
d_start = 0.1
e_start = -0.1
f_start = 0.45
vf2f <- nls(y~a*x^5 + b*x^4 + c*x^3 + d *x^2 + e*x + f, 
            start=list(a=a_start,b=b_start,c=c_start,d=d_start,e=e_start,f=f_start))
a = coefficients(vf2f)[1]
b = coefficients(vf2f)[2]
c = coefficients(vf2f)[3]
d = coefficients(vf2f)[4]
e = coefficients(vf2f)[5]
f = coefficients(vf2f)[6]
y3 <- a*x^5 + b*x^4 + c*x^3 + d *x^2 + e*x + f
lines(x,y3,type='l',col='blue')
#ground_lvf
a_start<-0.5
b_start<-1.25
c_start<-1.0
y <- agdrift$ground_low_vf
x <- agdrift$distance_ft
x[1] <- 0.0001
plot(x,y,type='l')
glvf <- nls(y~c/(1+a*x)^b, start=list(a=a_start,b=b_start,c=c_start))
summary(glvf)
a <- coef(glvf)[1]
b <- coef(glvf)[2]
c <- coef(glvf)[3]
y4 = c/(1+a*x)^b
lines(x,y4,type='l',col='red')
#ground_lfmc
a_start<-2.3
b_start<-1.10
c_start<-1.0
y <- agdrift$ground_low_fmc
x <- agdrift$distance_ft
x[1] <- 0.0001
plot(x,y,type='l')
glfmc <- nls(y~c/(1+a*x)^b, start=list(a=a_start,b=b_start,c=c_start))
summary(glfmc)
a <- coef(glfmc)[1]
b <- coef(glfmc)[2]
c <- coef(glfmc)[3]
y5 = c/(1+a*x)^b
lines(x,y5,type='l',col='red')
#ground_hvf
a_start<-1.0
b_start<-1.0
c_start<-1.0
y <- agdrift$ground_high_vf
x <- agdrift$distance_ft
x[1] <- 0.0001
plot(x,y,type='l')
ghvf <- nls(y~c/(1+a*x)^b, start=list(a=a_start,b=b_start,c=c_start))
summary(ghvf)
a <- coef(ghvf)[1]
b <- coef(ghvf)[2]
c <- coef(ghvf)[3]
y6 = c/(1+a*x)^b
lines(x,y6,type='l',col='red')
#ground_hfmc
a_start<-1.5
b_start<-1.0
c_start<-1.0
y <- agdrift$ground_high_fmc
x <- agdrift$distance_ft
x[1] <- 0.0001
plot(x,y,type='l')
ghfmc <- nls(y~c/(1+a*x)^b, start=list(a=a_start,b=b_start,c=c_start))
summary(ghfmc)
a <- coef(ghfmc)[1]
b <- coef(ghfmc)[2]
c <- coef(ghfmc)[3]
y7 = c/(1+a*x)^b
lines(x,y7,type='l',col='red')
#airblastnormal
a_start<-0.925
b_start<-0.556
c_start<-0.010
y <- agdrift$airblast_normal
x <- agdrift$distance_ft
x[1] <- 0.0001
plot(x,y,type='l')
ab_n <- nls(y~c/(1+a*x)^b, start=list(a=a_start,b=b_start,c=c_start))
summary(ab_n)
a <- coef(ab_n)[1]
b <- coef(ab_n)[2]
c <- coef(ab_n)[3]
y8 = c/(1+a*x)^b
lines(x,y8,type='l',col='red')
#airblastdense
a_start<-0.048
b_start<-1.59
c_start<-0.115
y <- agdrift$airblast_dense
x <- agdrift$distance_ft
x[1] <- 0.0001
plot(x,y,type='l')
ab_d <- nls(y~c/(1+a*x)^b, start=list(a=a_start,b=b_start,c=c_start))
summary(ab_d)
a <- coef(ab_d)[1]
b <- coef(ab_d)[2]
c <- coef(ab_d)[3]
y9 = c/(1+a*x)^b
lines(x,y9,type='l',col='red')
#airblastsparse
a_start<-0.03
b_start<-2.7
c_start<-0.47
y <- agdrift$airblast_sparse
x <- agdrift$distance_ft
x[1] <- 0.0001
plot(x,y,type='l')
ab_sp <- nls(y~c/(1+a*x)^b, start=list(a=a_start,b=b_start,c=c_start))
summary(ab_sp)
a <- coef(ab_sp)[1]
b <- coef(ab_sp)[2]
c <- coef(ab_sp)[3]
y10 = c/(1+a*x)^b
lines(x,y10,type='l',col='red')
#airblastvineyard
a_start<-0.4198
b_start<-0.7563
c_start<-0.0415
y <- agdrift$airblast_vineyard
x <- agdrift$distance_ft
x[1] <- 0.0001
plot(x,y,type='l')
ab_vine <- nls(y~c/(1+a*x)^b, start=list(a=a_start,b=b_start,c=c_start))
summary(ab_vine)
a <- coef(ab_vine)[1]
b <- coef(ab_vine)[2]
c <- coef(ab_vine)[3]
y11 = c/(1+a*x)^b
lines(x,y11,type='l',col='red')
#airblastorchard
a_start<-0.04
b_start<-2.02
c_start<-0.22
y <- agdrift$airblast_orchard
x <- agdrift$distance_ft
x[1] <- 0.0001
plot(x,y,type='l')
ab_ord <- nls(y~c/(1+a*x)^b, start=list(a=a_start,b=b_start,c=c_start))
summary(ab_ord)
a <- coef(ab_ord)[1]
b <- coef(ab_ord)[2]
c <- coef(ab_ord)[3]
y12 = c/(1+a*x)^b
lines(x,y12,type='l',col='red')
