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
#6th order
#aerial_vf2f
a_start = 0.001
b_start = -0.001
c_start = 0.001
d_start = -0.001
e_start = 0.001
f_start = -0.001
g_start = 0.45
vf2f <- nls(y~a*x^6 + b*x^5 + c*x^4 + d *x^3 + e*x^2 + f*x+ g, 
            start=list(a=a_start,b=b_start,c=c_start,d=d_start,e=e_start,f=f_start,g=g_start))
a = coefficients(vf2f)[1]
b = coefficients(vf2f)[2]
c = coefficients(vf2f)[3]
d = coefficients(vf2f)[4]
e = coefficients(vf2f)[5]
f = coefficients(vf2f)[6]
g = coefficients(vf2f)[7]
y36 <- a*x^6 + b*x^5 + c*x^4 + d *x^3 + e*x^2 + f*x+ g
lines(x,y36,type='l',col='blue')
#7th order
#aerial_vf2f
a_start = 0.001
b_start = -0.001
c_start = 0.001
d_start = -0.001
e_start = 0.001
f_start = -0.001
g_start = 0.001
h_start = 0.45
vf2f <- nls(y~a*x^7 + b*x^6 + c*x^5 + d *x^4 + e*x^3 + f*x^2+ g*x+ h, 
            start=list(a=a_start,b=b_start,c=c_start,d=d_start,e=e_start,f=f_start,g=g_start,h=h_start))
a = coefficients(vf2f)[1]
b = coefficients(vf2f)[2]
c = coefficients(vf2f)[3]
d = coefficients(vf2f)[4]
e = coefficients(vf2f)[5]
f = coefficients(vf2f)[6]
g = coefficients(vf2f)[7]
h = coefficients(vf2f)[8]
y37 <- a*x^7 + b*x^6 + c*x^5 + d *x^4 + e*x^3 + f*x^2+ g*x+ h
lines(x,y37,type='l',col='red')
#8th order
#aerial_vf2f
a_start = 0.001
b_start = -0.001
c_start = 0.001
d_start = -0.001
e_start = 0.001
f_start = -0.001
g_start = 0.001
h_start = -0.001
i_start = 0.45
vf2f <- nls(y~a*x^8 + b*x^7 + c*x^6 + d *x^5 + e*x^4 + f*x^3+ g*x^2+ h*x+ i, 
            start=list(a=a_start,b=b_start,c=c_start,d=d_start,e=e_start,f=f_start,g=g_start,h=h_start,i=i_start))
a = coefficients(vf2f)[1]
b = coefficients(vf2f)[2]
c = coefficients(vf2f)[3]
d = coefficients(vf2f)[4]
e = coefficients(vf2f)[5]
f = coefficients(vf2f)[6]
g = coefficients(vf2f)[7]
h = coefficients(vf2f)[8]
i = coefficients(vf2f)[9]
y38 <- a*x^8 + b*x^7 + c*x^6 + d *x^5 + e*x^4 + f*x^3+ g*x^2+ h*x+ i
lines(x,y38,type='l',col='green')
#9th order
#aerial_vf2f
a_start = 0.001
b_start = -0.001
c_start = 0.001
d_start = -0.001
e_start = 0.001
f_start = -0.001
g_start = 0.001
h_start = -0.001
i_start = 0.001
j_start = 0.45
vf2f <- nls(y~a*x^9 + b*x^8 + c*x^7 + d *x^6 + e*x^5 + f*x^4+ g*x^3+ h*x^2+ i*x+ j, 
            start=list(a=a_start,b=b_start,c=c_start,d=d_start,e=e_start,f=f_start,g=g_start,h=h_start,i=i_start,j=j_start))
a = coefficients(vf2f)[1]
b = coefficients(vf2f)[2]
c = coefficients(vf2f)[3]
d = coefficients(vf2f)[4]
e = coefficients(vf2f)[5]
f = coefficients(vf2f)[6]
g = coefficients(vf2f)[7]
h = coefficients(vf2f)[8]
i = coefficients(vf2f)[9]
j = coefficients(vf2f)[10]
y39 <- a*x^9 + b*x^8 + c*x^7 + d *x^6 + e*x^5 + f*x^4+ g*x^3+ h*x^2+ i*x+ j
lines(x,y39,type='l',col='orange')

#aerial_f2m
a_start = 0.001
b_start = -0.001
c_start = 0.001
d_start = -0.001
e_start = 0.001
f_start = -0.001
g_start = 0.40
y <- agdrift$aerial_f2m
x <- agdrift$distance_ft
x[1] <- 0.0001
plot(x,y,type='l')
af2m <- nls(y~a*x^6 + b*x^5 + c*x^4 + d *x^3 + e*x^2 + f*x+ g, 
            start=list(a=a_start,b=b_start,c=c_start,d=d_start,e=e_start,f=f_start,g=g_start))
a = coefficients(af2m)[1]
b = coefficients(af2m)[2]
c = coefficients(af2m)[3]
d = coefficients(af2m)[4]
e = coefficients(af2m)[5]
f = coefficients(af2m)[6]
g = coefficients(af2m)[7]
y46 <- a*x^6 + b*x^5 + c*x^4 + d *x^3 + e*x^2 + f*x+ g
lines(x,y46,type='l',col='slategray')

#7th order
a_start = 0.001
b_start = -0.001
c_start = 0.001
d_start = -0.001
e_start = 0.001
f_start = -0.001
g_start = 0.001
h_start = 0.40
af2m <- nls(y~a*x^7 + b*x^6 + c*x^5 + d *x^4 + e*x^3 + f*x^2+ g*x+ h, 
            start=list(a=a_start,b=b_start,c=c_start,d=d_start,e=e_start,f=f_start,g=g_start,h=h_start))
a = coefficients(af2m)[1]
b = coefficients(af2m)[2]
c = coefficients(af2m)[3]
d = coefficients(af2m)[4]
e = coefficients(af2m)[5]
f = coefficients(af2m)[6]
g = coefficients(af2m)[7]
h = coefficients(af2m)[8]
y47 <- a*x^7 + b*x^6 + c*x^5 + d *x^4 + e*x^3 + f*x^2+ g*x+ h
lines(x,y47,type='l',col='red')
#8th order
a_start = 0.001
b_start = -0.001
c_start = 0.001
d_start = -0.001
e_start = 0.001
f_start = -0.001
g_start = 0.001
h_start = -0.001
i_start = 0.40
af2m <- nls(y~a*x^8 + b*x^7 + c*x^6 + d *x^5 + e*x^4 + f*x^3+ g*x^2+ h*x+ i, 
            start=list(a=a_start,b=b_start,c=c_start,d=d_start,e=e_start,f=f_start,g=g_start,h=h_start,i=i_start))
a = coefficients(af2m)[1]
b = coefficients(af2m)[2]
c = coefficients(af2m)[3]
d = coefficients(af2m)[4]
e = coefficients(af2m)[5]
f = coefficients(af2m)[6]
g = coefficients(af2m)[7]
h = coefficients(af2m)[8]
i = coefficients(af2m)[9]
y48 <- a*x^8 + b*x^7 + c*x^6 + d *x^5 + e*x^4 + f*x^3+ g*x^2+ h*x+ i
lines(x,y48,type='l',col='green')
#9th order
a_start = 0.001
b_start = -0.001
c_start = 0.001
d_start = -0.001
e_start = 0.001
f_start = -0.001
g_start = 0.001
h_start = -0.001
i_start = 0.001
j_start = 0.35
af2m <- nls(y~a*x^9 + b*x^8 + c*x^7 + d *x^6 + e*x^5 + f*x^4+ g*x^3+ h*x^2+ i*x+ j, 
            start=list(a=a_start,b=b_start,c=c_start,d=d_start,e=e_start,f=f_start,g=g_start,h=h_start,i=i_start,j=j_start))
a = coefficients(af2m)[1]
b = coefficients(af2m)[2]
c = coefficients(af2m)[3]
d = coefficients(af2m)[4]
e = coefficients(af2m)[5]
f = coefficients(af2m)[6]
g = coefficients(af2m)[7]
h = coefficients(af2m)[8]
i = coefficients(af2m)[9]
j = coefficients(af2m)[10]
y49 <- a*x^9 + b*x^8 + c*x^7 + d *x^6 + e*x^5 + f*x^4+ g*x^3+ h*x^2+ i*x+ j
lines(x,y49,type='l',col='orange')


#aerial_m2c
a_start = 0.001
b_start = -0.001
c_start = 0.001
d_start = -0.001
e_start = 0.001
f_start = -0.001
g_start = 0.33
y <- agdrift$aerial_m2c
x <- agdrift$distance_ft
x[1] <- 0.0001
plot(x,y,type='l')
am2c <- nls(y~a*x^6 + b*x^5 + c*x^4 + d *x^3 + e*x^2 + f*x+ g, 
            start=list(a=a_start,b=b_start,c=c_start,d=d_start,e=e_start,f=f_start,g=g_start))
a = coefficients(am2c)[1]
b = coefficients(am2c)[2]
c = coefficients(am2c)[3]
d = coefficients(am2c)[4]
e = coefficients(am2c)[5]
f = coefficients(am2c)[6]
g = coefficients(am2c)[7]
y56 <- a*x^6 + b*x^5 + c*x^4 + d *x^3 + e*x^2 + f*x+ g
lines(x,y56,type='l',col='seagreen')
#7th order
a_start = 0.001
b_start = -0.001
c_start = 0.001
d_start = -0.001
e_start = 0.001
f_start = -0.001
g_start = 0.001
h_start = 0.33
am2c <- nls(y~a*x^7 + b*x^6 + c*x^5 + d *x^4 + e*x^3 + f*x^2+ g*x+ h, 
            start=list(a=a_start,b=b_start,c=c_start,d=d_start,e=e_start,f=f_start,g=g_start,h=h_start))
a = coefficients(am2c)[1]
b = coefficients(am2c)[2]
c = coefficients(am2c)[3]
d = coefficients(am2c)[4]
e = coefficients(am2c)[5]
f = coefficients(am2c)[6]
g = coefficients(am2c)[7]
h = coefficients(am2c)[8]
y57 <- a*x^7 + b*x^6 + c*x^5 + d *x^4 + e*x^3 + f*x^2+ g*x+ h
lines(x,y57,type='l',col='red')
#8th order
a_start = 0.001
b_start = -0.001
c_start = 0.001
d_start = -0.001
e_start = 0.001
f_start = -0.001
g_start = 0.001
h_start = -0.001
i_start = 0.33
am2c <- nls(y~a*x^8 + b*x^7 + c*x^6 + d *x^5 + e*x^4 + f*x^3+ g*x^2+ h*x+ i, 
            start=list(a=a_start,b=b_start,c=c_start,d=d_start,e=e_start,f=f_start,g=g_start,h=h_start,i=i_start))
a = coefficients(am2c)[1]
b = coefficients(am2c)[2]
c = coefficients(am2c)[3]
d = coefficients(am2c)[4]
e = coefficients(am2c)[5]
f = coefficients(am2c)[6]
g = coefficients(am2c)[7]
h = coefficients(am2c)[8]
i = coefficients(am2c)[9]
y58 <- a*x^8 + b*x^7 + c*x^6 + d *x^5 + e*x^4 + f*x^3+ g*x^2+ h*x+ i
lines(x,y58,type='l',col='green')
#9th order
a_start = 0.001
b_start = -0.001
c_start = 0.001
d_start = -0.001
e_start = 0.001
f_start = -0.001
g_start = 0.001
h_start = -0.001
i_start = 0.001
j_start = 0.33
am2c <- nls(y~a*x^9 + b*x^8 + c*x^7 + d *x^6 + e*x^5 + f*x^4+ g*x^3+ h*x^2+ i*x+ j, 
            start=list(a=a_start,b=b_start,c=c_start,d=d_start,e=e_start,f=f_start,g=g_start,h=h_start,i=i_start,j=j_start))
a = coefficients(am2c)[1]
b = coefficients(am2c)[2]
c = coefficients(am2c)[3]
d = coefficients(am2c)[4]
e = coefficients(am2c)[5]
f = coefficients(am2c)[6]
g = coefficients(am2c)[7]
h = coefficients(am2c)[8]
i = coefficients(am2c)[9]
j = coefficients(am2c)[10]
y59 <- a*x^9 + b*x^8 + c*x^7 + d *x^6 + e*x^5 + f*x^4+ g*x^3+ h*x^2+ i*x+ j
lines(x,y59,type='l',col='orange')




#aerial_c2vc
a_start = 0.001
b_start = -0.001
c_start = 0.001
d_start = -0.001
e_start = 0.001
f_start = -0.001
g_start = 0.30
y <- agdrift$aerial_c2vc
x <- agdrift$distance_ft
x[1] <- 0.0001
plot(x,y,type='l')
ac2vc <- nls(y~a*x^6 + b*x^5 + c*x^4 + d *x^3 + e*x^2 + f*x+ g, 
            start=list(a=a_start,b=b_start,c=c_start,d=d_start,e=e_start,f=f_start,g=g_start))
a = coefficients(am2c)[1]
b = coefficients(am2c)[2]
c = coefficients(am2c)[3]
d = coefficients(am2c)[4]
e = coefficients(am2c)[5]
f = coefficients(am2c)[6]
g = coefficients(am2c)[7]
y6 <- a*x^6 + b*x^5 + c*x^4 + d *x^3 + e*x^2 + f*x+ g
lines(x,y6,type='l',col='orange')
#7th order
a_start = 0.001
b_start = -0.001
c_start = 0.001
d_start = -0.001
e_start = 0.001
f_start = -0.001
g_start = 0.001
h_start = 0.30
ac2vc <- nls(y~a*x^7 + b*x^6 + c*x^5 + d *x^4 + e*x^3 + f*x^2+ g*x+ h, 
            start=list(a=a_start,b=b_start,c=c_start,d=d_start,e=e_start,f=f_start,g=g_start,h=h_start))
a = coefficients(ac2vc)[1]
b = coefficients(ac2vc)[2]
c = coefficients(ac2vc)[3]
d = coefficients(ac2vc)[4]
e = coefficients(ac2vc)[5]
f = coefficients(ac2vc)[6]
g = coefficients(ac2vc)[7]
h = coefficients(ac2vc)[8]
y67 <- a*x^7 + b*x^6 + c*x^5 + d *x^4 + e*x^3 + f*x^2+ g*x+ h
lines(x,y67,type='l',col='red')
#8th order
a_start = 0.001
b_start = -0.001
c_start = 0.001
d_start = -0.001
e_start = 0.001
f_start = -0.001
g_start = 0.001
h_start = -0.001
i_start = 0.30
ac2vc <- nls(y~a*x^8 + b*x^7 + c*x^6 + d *x^5 + e*x^4 + f*x^3+ g*x^2+ h*x+ i, 
            start=list(a=a_start,b=b_start,c=c_start,d=d_start,e=e_start,f=f_start,g=g_start,h=h_start,i=i_start))
a = coefficients(ac2vc)[1]
b = coefficients(ac2vc)[2]
c = coefficients(ac2vc)[3]
d = coefficients(ac2vc)[4]
e = coefficients(ac2vc)[5]
f = coefficients(ac2vc)[6]
g = coefficients(ac2vc)[7]
h = coefficients(ac2vc)[8]
i = coefficients(ac2vc)[9]
y68 <- a*x^8 + b*x^7 + c*x^6 + d *x^5 + e*x^4 + f*x^3+ g*x^2+ h*x+ i
lines(x,y68,type='l',col='green')
#9th order
a_start = 0.001
b_start = -0.001
c_start = 0.001
d_start = -0.001
e_start = 0.001
f_start = -0.001
g_start = 0.001
h_start = -0.001
i_start = 0.001
j_start = 0.30
ac2vc <- nls(y~a*x^9 + b*x^8 + c*x^7 + d *x^6 + e*x^5 + f*x^4+ g*x^3+ h*x^2+ i*x+ j, 
            start=list(a=a_start,b=b_start,c=c_start,d=d_start,e=e_start,f=f_start,g=g_start,h=h_start,i=i_start,j=j_start))
a = coefficients(ac2vc)[1]
b = coefficients(ac2vc)[2]
c = coefficients(ac2vc)[3]
d = coefficients(ac2vc)[4]
e = coefficients(ac2vc)[5]
f = coefficients(ac2vc)[6]
g = coefficients(ac2vc)[7]
h = coefficients(ac2vc)[8]
i = coefficients(ac2vc)[9]
j = coefficients(ac2vc)[10]
y69 <- a*x^9 + b*x^8 + c*x^7 + d *x^6 + e*x^5 + f*x^4+ g*x^3+ h*x^2+ i*x+ j
lines(x,y69,type='l',col='blue')


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
y7 = c/(1+a*x)^b
lines(x,y7,type='l',col='red')
#9th order
a_start = 0.001
b_start = -0.001
c_start = 0.001
d_start = -0.001
e_start = 0.001
f_start = -0.001
g_start = 0.001
h_start = -0.001
i_start = 0.001
j_start = 0.35
glvf <- nls(y~a*x^9 + b*x^8 + c*x^7 + d *x^6 + e*x^5 + f*x^4+ g*x^3+ h*x^2+ i*x+ j, 
            start=list(a=a_start,b=b_start,c=c_start,d=d_start,e=e_start,f=f_start,g=g_start,h=h_start,i=i_start,j=j_start))
a = coefficients(glvf)[1]
b = coefficients(glvf)[2]
c = coefficients(glvf)[3]
d = coefficients(glvf)[4]
e = coefficients(glvf)[5]
f = coefficients(glvf)[6]
g = coefficients(glvf)[7]
h = coefficients(glvf)[8]
i = coefficients(glvf)[9]
j = coefficients(glvf)[10]
y79 <- a*x^9 + b*x^8 + c*x^7 + d *x^6 + e*x^5 + f*x^4+ g*x^3+ h*x^2+ i*x+ j
lines(x,y79,type='l',col='orange')

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
