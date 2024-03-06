library(chisq.posthoc.test)
# plotting
rev.values <- c(13,25,8,21,25,18)
space.rev <- matrix(rev.values,nrow=3,byrow = T)
colnames(space.rev) <- c("MAC","PC")
rownames(space.rev) <- c("None","Static","Dynamic")
space.rev
color.names = c("grey15","grey45", "grey85")
barplot(space.rev, beside = T, xlab= "System",ylab= "N. of users", col=color.names)
legend("topleft",rownames(space.rev), cex=1.0, fill = color.names, bty = "n")

# import data
wallpaper=matrix(c(13,8,25,
                   25,21,18),
              byrow = TRUE,
              ncol=3,
              dimnames=list(c("mac","pc"),
                            c("none","static","dynamic")));
# compute total
row = nrow(wallpaper)
column = ncol(wallpaper)
rowsum = apply(wallpaper,1,sum)
colsum = apply(wallpaper,2,sum)
totalsum = sum(wallpaper)

wallTot=matrix(c(13,8,25,rowsum[1],
                   25,21,18,rowsum[2],
                   colsum[1],colsum[2],colsum[3],totalsum),
                 byrow = TRUE,
                 ncol=4,
                 dimnames=list(c("mac","pc","tot"),
                               c("none","static","dynamic","tot")));
wallTot

value_expect=(rowsum%*%t(colsum))/totalsum
wallpaper_eDisplay=matrix(round(value_expect,2),
                       ncol=3,
                       byrow=FALSE,
                       dimnames=list(c("MAC","PC"),
                                     c("None","Static","Dynamic"))
)
wallpaper_eDisplay

#chi-squared test
z=chisq.test(wallpaper, correct=F); 
round(z$exp,2)
z$observed
#individual contribution
summaryChi=(z$observed-z$expected)^2/z$expected
summaryChi
# compute total
rowChi = nrow(summaryChi)
columnChi = ncol(summaryChi)
rowsumChi = apply(summaryChi,1,sum)
colsumChi = apply(summaryChi,2,sum)
totalsumChi = sum(summaryChi)
rowsumChi
colsumChi
totalsumChi

#X2
x2 = sum((wallpaper-value_expect)^2/value_expect); 
x2

#pvalue: P(X2 >= chi-squared)
1-pchisq(x2,(row-1)*(column-1))

#chi-squared test
chisq.test(wallpaper, correct=F)

#post-hoc analysis
library(fifer)
library(chisq.posthoc.test)
# import data
wallpaperReverse=matrix(c(13,25,
                   8,21,
                   25,18),
                 byrow = TRUE,
                 ncol=2,
                 dimnames=list(c("none","static","dynamic"),
                               c("mac","pc")));


chisq.post.hoc(wallpaperReverse, test='chisq.test')
#chisq.posthoc.test(wallpaper,method = "none")      # not needed
#computing chisquare for each pair
qchisq(0.7540, df=2, lower.tail=FALSE)  #p=0.7540
qchisq(0.0535, df=2, lower.tail=FALSE)  #p=0.0535
qchisq(0.0208, df=2, lower.tail=FALSE)  #p=0.0208



