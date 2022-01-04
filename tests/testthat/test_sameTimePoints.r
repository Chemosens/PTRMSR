 time=c(1,2,7,7.1)
 duration=c(1,5,0.1,0.1)
 intensity=c(1,10,4,8)

 df=data.frame(time=time,duration=duration,intensity=intensity)

 test_that("sameTimePoints1",
           expect_true(sameTimePoints(df,npoints=2)$new[,"intensity"]==weighted.mean(df[,"intensity"],df[,"duration"]))
 )
 sameTimePoints(df,npoints=3)

 test_that("sameTimePoints2",
           expect_true( abs(sameTimePoints(df,npoints=5)$new[1,"intensity"]-weighted.mean(c(1,10),c(1,0.55)))<1e-8)
 )

 sameTimePoints(df,npoints=100)$new


