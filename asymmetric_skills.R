# value <- matrix(
#           c(44,64,10,39,21,10, #participant1  web
#            40,28,10,13,10,7,  #              mobile
#            47,39,9,35,22,9,   #participant2  web
#            39,25,10,15,9,10,   #              mobile
#            42,48,11,40,18,10, #participant3  web
#            43,29,10,11,10,9,  #              mobile
#            46,42,11,37,19,10, #participant4  web
#            41,26,10,10,10,7,  #              mobile
#            45,58,10,41,22,8,  #participant5  web
#            37,24,9,16,10,8,  #              mobile
#            38,21,10,16,6,4,    #participant6  web
#            42,29,10,27,11,9,  #              mobile
#            39,29,10,17,9,4,    #participant7  web
#            40,30,9,20,10,10, #              mobile
#            37,22,9,16,6,5,     #participant8  web
#            39,28,11,30,9,10,  #              mobile
#            36,25,10,14,8,5,    #participant9  web
#            44,31,10,22,9,8,   #              mobile
#            37,23,9,16,6,4,     #participant10 web
#            41,29,10,31,10,9   #              mobile
# ),byrow=T,nrow=20
# )
# value

#=========================Task1=================================
#p1 to p5 started with web, and then mobile
#p6 to p10 started with mobile, and then web
MatrixT1 <- matrix(c(44,47,42,46,45,
                     42,40,39,44,41,                     
                     40,39,43,41,37,
                     38,39,37,36,37), 
                   nrow=5, ncol=4,
                   dimnames=list(c("Participant 1 or 6",
                                   "Participant 2 or 7",
                                   "Participant 3 or 8",
                                   "Participant 4 or 9",
                                   "Participant 5 or 10"),
                                 c("Group1mod1",
                                   "Group2mod2",
                                   "Group1mod2",
                                   "Group2mod1")))

MatrixT1

#average participant 1 to 5 for web-based
avgMatrixT1 <- colMeans(MatrixT1)
avgMatrixT1

#modelling skill trasfer:
#group1 starts with web and group2 ends with web1
#group2 starts with mobile and group1 ends with mobile
avgMatrixT1plot <- matrix(c(avgMatrixT1[1],avgMatrixT1[2],
                            avgMatrixT1[4],avgMatrixT1[3]),
                          nrow=2,ncol=2,
                          dimnames=list(c("web-based",
                                          "mobile"),
                                        c("First(Trials 1-5)",
                                          "Second (Trials 6-10)")))
avgMatrixT1plot

#plotting skill transfer
matplot(t(avgMatrixT1plot),
        type="b",
        lwd=1,
        lty=1,
        col=c(2,9),
        pch=c(19,15),
        ylim =c(30,45),
        xlim= c(0.8,2.2),
        ylab="avg task 1 time (seconds)",
        xlab = "Testing Half",
        main="Skill Transfer Task 1",
        xaxt='n',
        axes=F) + geom_point() 
axis(side=1,at=1:2,labels = rownames(t(avgMatrixT1plot)),las=1)
axis(2)
legend("bottom",inset=0.02, horiz=T,legend=c("web-based","mobile"), col=c(2,9),pch=c(19,15))

#====================Task2==================================
#p1 to p5 started with web, and then mobile
#p6 to p10 started with mobile, and then web
MatrixT2 <- matrix(c(64,39,48,42,58,
                     29,30,28,31,29,                     
                     28,25,29,26,24,
                     21,29,22,25,23), 
                   nrow=5, ncol=4,
                   dimnames=list(c("Participant 1 or 6",
                                   "Participant 2 or 7",
                                   "Participant 3 or 8",
                                   "Participant 4 or 9",
                                   "Participant 5 or 10"),
                                 c("Group1mod1",
                                   "Group2mod2",
                                   "Group1mod2",
                                   "Group2mod1")))

MatrixT2

#average participant 1 to 5 for web-based
avgMatrixT2 <- colMeans(MatrixT2)
avgMatrixT2

#modelling skill trasfer:
#group1 starts with web and group2 ends with web1
#group2 starts with mobile and group1 ends with mobile
avgMatrixT2plot <- matrix(c(avgMatrixT2[1],avgMatrixT2[2],
                            avgMatrixT2[4],avgMatrixT2[3]),
                          nrow=2,ncol=2,
                          dimnames=list(c("web-based",
                                          "mobile"),
                                        c("First(Trials 1-5)",
                                          "Second (Trials 6-10)")))
avgMatrixT2plot

#plotting skill transfer
matplot(t(avgMatrixT2plot),
        type="b",
        lwd=1,
        lty=1,
        col=c(2,9),
        pch=c(19,15),
        ylim =c(0,65),
        xlim= c(0.8,2.2),
        ylab="avg task 2 time (seconds)",
        xlab = "Testing Half",
        main="Skill Transfer Task 2",
        xaxt='n',
        axes=F) + geom_point() 
axis(side=1,at=1:2,labels = rownames(t(avgMatrixT2plot)),las=1)
axis(2)
legend("bottom",inset=0.02, horiz=T,legend=c("web-based","mobile"), col=c(2,9),pch=c(19,15))
#====================Task3==================================
#p1 to p5 started with web, and then mobile
#p6 to p10 started with mobile, and then web
MatrixT3 <- matrix(c(10,9,11,11,10,
                     10,9,11,10,10,                     
                     10,10,10,10,9,                     
                     10,10,9,10,9), 
                   nrow=5, ncol=4,
                   dimnames=list(c("Participant 1 or 6",
                                   "Participant 2 or 7",
                                   "Participant 3 or 8",
                                   "Participant 4 or 9",
                                   "Participant 5 or 10"),
                                 c("Group1mod1",
                                   "Group2mod2",
                                   "Group1mod2",
                                   "Group2mod1")))

MatrixT3

#average participant 1 to 5 for web-based
avgMatrixT3 <- colMeans(MatrixT3)
avgMatrixT3

#modelling skill trasfer:
#group1 starts with web and group2 ends with web1
#group2 starts with mobile and group1 ends with mobile
avgMatrixT3plot <- matrix(c(avgMatrixT3[1],avgMatrixT3[2],
                            avgMatrixT3[4],avgMatrixT3[3]),
                          nrow=2,ncol=2,
                          dimnames=list(c("web-based",
                                          "mobile"),
                                        c("First(Trials 1-5)",
                                          "Second (Trials 6-10)")))
avgMatrixT3plot

#plotting skill transfer
matplot(t(avgMatrixT3plot),
        type="b",
        lwd=1,
        lty=1,
        col=c(2,9),
        pch=c(19,15),
        ylim =c(9,11),
        xlim= c(0.8,2.2),
        ylab="avg task 3 time (seconds)",
        xlab = "Testing Half",
        main="Skill Transfer Task 3",
        xaxt='n',
        axes=F) + geom_point() 
axis(side=1,at=1:2,labels = rownames(t(avgMatrixT3plot)),las=1)
axis(2)
legend("bottom",inset=0.02, horiz=T,legend=c("web-based","mobile"), col=c(2,9),pch=c(19,15))
#====================Task4==================================
#p1 to p5 started with web, and then mobile
#p6 to p10 started with mobile, and then web
MatrixT4 <- matrix(c(39,35,40,37,41,
                     27,20,30,22,31,                     
                     13,15,11,10,16,
                     16,17,16,14,16), 
                   nrow=5, ncol=4,
                   dimnames=list(c("Participant 1 or 6",
                                   "Participant 2 or 7",
                                   "Participant 3 or 8",
                                   "Participant 4 or 9",
                                   "Participant 5 or 10"),
                                 c("Group1mod1",
                                   "Group2mod2",
                                   "Group1mod2",
                                   "Group2mod1")))

MatrixT4

#average participant 1 to 5 for web-based
avgMatrixT4 <- colMeans(MatrixT4)
avgMatrixT4

#modelling skill trasfer:
#group1 starts with web and group2 ends with web1
#group2 starts with mobile and group1 ends with mobile
avgMatrixT4plot <- matrix(c(avgMatrixT4[1],avgMatrixT4[2],
                            avgMatrixT4[4],avgMatrixT4[3]),
                          nrow=2,ncol=2,
                          dimnames=list(c("web-based",
                                          "mobile"),
                                        c("First(Trials 1-5)",
                                          "Second (Trials 6-10)")))
avgMatrixT4plot

#plotting skill transfer
matplot(t(avgMatrixT4plot),
        type="b",
        lwd=1,
        lty=1,
        col=c(2,9),
        pch=c(19,15),
        ylim =c(0,40),
        xlim= c(0.8,2.2),
        ylab="avg task 4 time (seconds)",
        xlab = "Testing Half",
        main="Skill Transfer Task 4",
        xaxt='n',
        axes=F) + geom_point() 
axis(side=1,at=1:2,labels = rownames(t(avgMatrixT4plot)),las=1)
axis(2)
legend("bottom",inset=0.02, horiz=T,legend=c("web-based","mobile"), col=c(2,9),pch=c(19,15))
#====================Task5==================================
#p1 to p5 started with web, and then mobile
#p6 to p10 started with mobile, and then web
MatrixT5 <- matrix(c(21,22,18,19,22,
                     11,10,9,9,10,                     
                     10,9,10,10,10,
                     6,9,6,8,6), 
                   nrow=5, ncol=4,
                   dimnames=list(c("Participant 1 or 6",
                                   "Participant 2 or 7",
                                   "Participant 3 or 8",
                                   "Participant 4 or 9",
                                   "Participant 5 or 10"),
                                 c("Group1mod1",
                                   "Group2mod2",
                                   "Group1mod2",
                                   "Group2mod1")))

MatrixT5

#average participant 1 to 5 for web-based
avgMatrixT5 <- colMeans(MatrixT5)
avgMatrixT5

#modelling skill trasfer:
#group1 starts with web and group2 ends with web1
#group2 starts with mobile and group1 ends with mobile
avgMatrixT5plot <- matrix(c(avgMatrixT5[1],avgMatrixT5[2],
                            avgMatrixT5[4],avgMatrixT5[3]),
                          nrow=2,ncol=2,
                          dimnames=list(c("web-based",
                                          "mobile"),
                                        c("First(Trials 1-5)",
                                          "Second (Trials 6-10)")))
avgMatrixT5plot

#plotting skill transfer
matplot(t(avgMatrixT5plot),
        type="b",
        lwd=1,
        lty=1,
        col=c(2,9),
        pch=c(19,15),
        ylim =c(0,20),
        xlim= c(0.8,2.2),
        ylab="avg task 5 time (seconds)",
        xlab = "Testing Half",
        main="Skill Transfer Task 5",
        xaxt='n',
        axes=F) + geom_point() 
axis(side=1,at=1:2,labels = rownames(t(avgMatrixT5plot)),las=1)
axis(2)
legend("bottom",inset=0.02, horiz=T,legend=c("web-based","mobile"), col=c(2,9),pch=c(19,15))
#====================Task6==================================
#p1 to p5 started with web, and then mobile
#p6 to p10 started with mobile, and then web
MatrixT6 <- matrix(c(10,9,10,10,8,
                     9,10,10,8,9,                     
                     7,10,9,7,8,
                     4,4,5,5,4), 
                   nrow=5, ncol=4,
                   dimnames=list(c("Participant 1 or 6",
                                   "Participant 2 or 7",
                                   "Participant 3 or 8",
                                   "Participant 4 or 9",
                                   "Participant 5 or 10"),
                                 c("Group1mod1",
                                   "Group2mod2",
                                   "Group1mod2",
                                   "Group2mod1")))

MatrixT6

#average participant 1 to 5 for web-based
avgMatrixT6 <- colMeans(MatrixT6)
avgMatrixT6

#modelling skill trasfer:
#group1 starts with web and group2 ends with web1
#group2 starts with mobile and group1 ends with mobile
avgMatrixT6plot <- matrix(c(avgMatrixT6[1],avgMatrixT6[2],
                            avgMatrixT6[4],avgMatrixT6[3]),
                          nrow=2,ncol=2,
                          dimnames=list(c("web-based",
                                          "mobile"),
                                        c("First(Trials 1-5)",
                                          "Second (Trials 6-10)")))
avgMatrixT6plot

#plotting skill transfer
matplot(t(avgMatrixT6plot),
        type="b",
        lwd=1,
        lty=1,
        col=c(2,9),
        pch=c(19,15),
        ylim =c(0,10),
        xlim= c(0.8,2.2),
        ylab="avg task 6 time (seconds)",
        xlab = "Testing Half",
        main="Skill Transfer Task 6",
        xaxt='n',
        axes=F) + geom_point() 
axis(side=1,at=1:2,labels = rownames(t(avgMatrixT6plot)),las=1)
axis(2)
legend("bottom",inset=0.02, horiz=T,legend=c("web-based","mobile"), col=c(2,9),pch=c(19,15))
#====================TaskTotal==================================
#p1 to p5 started with web, and then mobile
#p6 to p10 started with mobile, and then web
MatrixTotal <- matrix(c(188,161,169,165,184,
                     128,119,127,124,130,                     
                     108,108,112,104,104,
                     95,108,95,98,95), 
                   nrow=5, ncol=4,
                   dimnames=list(c("Participant 1 or 6",
                                   "Participant 2 or 7",
                                   "Participant 3 or 8",
                                   "Participant 4 or 9",
                                   "Participant 5 or 10"),
                                 c("Group1mod1",
                                   "Group2mod2",
                                   "Group1mod2",
                                   "Group2mod1")))

MatrixTotal

#average participant 1 to 5 for web-based
avgMatrixTotal <- colMeans(MatrixTotal)
avgMatrixTotal

#modelling skill trasfer:
#group1 starts with web and group2 ends with web1
#group2 starts with mobile and group1 ends with mobile
avgMatrixTotalplot <- matrix(c(avgMatrixTotal[1],avgMatrixTotal[2],
                            avgMatrixTotal[4],avgMatrixTotal[3]),
                          nrow=2,ncol=2,
                          dimnames=list(c("web-based",
                                          "mobile"),
                                        c("First(Trials 1-5)",
                                          "Second (Trials 6-10)")))
avgMatrixTotalplot

#plotting skill transfer
matplot(t(avgMatrixTotalplot),
        type="b",
        lwd=1,
        lty=1,
        col=c(2,9),
        pch=c(19,15),
        ylim =c(0,200),
        xlim= c(0.8,2.2),
        ylab="avg tasks time (seconds)",
        xlab = "Testing Half",
        main="Skill Transfer All tasks",
        xaxt='n',
        axes=F) + geom_point() 
axis(side=1,at=1:2,labels = rownames(t(avgMatrixTotalplot)),las=1)
axis(2)
legend("bottom",inset=0.02, horiz=T,legend=c("web-based","mobile"), col=c(2,9),pch=c(19,15))

