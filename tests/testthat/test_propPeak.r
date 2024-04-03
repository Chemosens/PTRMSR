time=1:100
intensity=1+dnorm(seq(-3,3,length.out=100))
#intensity=2+dnorm(seq(-0.8,0.8,length.out=100))
#plot(intensity)
timeForPropPeak(time,intensity,proportion=0.7,timing="first")==37
timeForPropPeak(time,intensity,proportion=0.7,timing="last")==64

time=1:200
intensity1=1+dnorm(seq(-3,3,length.out=100))
intensity2=1.02+dnorm(seq(-3,3,length.out=100))
intensity=c(intensity1,intensity2)
plot(intensity)
test_that("test1",
         expect_true(timeForPropPeak(time,intensity,proportion=0.7,timing="last")==164)
)

test_that("test2",
         expect_true(
timeForPropPeak(time,intensity,proportion=0.7,timing="first")==38)
)
# timeForPropPeak(time,intensity,proportion=0.7,timing="beforeClose")==38
# timeForPropPeak(time,intensity,proportion=0.7,timing="afterClose")==38