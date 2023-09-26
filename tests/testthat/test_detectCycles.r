data(ptrv)
ptrv2=ptrv
ions=colnames(ptrv2)[-c(1:3)]
res=reshape(ptrv2,direction="long",varying=list(ions),times=ions,v.names="intensity")
colnames(res)=c("abs","time","cycle","ion","intensity","id")
referenceBreath="m69.06906..69.06906...Conc."
referenceBreath="m69.06989..isoprene...Conc."
res1=res[res[,"ion"]==as.character(referenceBreath),]
cyclesDefault=detectCycle(df=res1)
cyclesDefault$gg$p2 # to check the smoothing impact

cyclesDefault$param
cyclesDefault$finalPeakTable
cyclesDefault$peakTable[cyclesDefault$peakTable[,"toRemove"]==TRUE,]
strangePeaks=cyclesDefault$peakTable[cyclesDefault$peakTable[,"toRemove"]==TRUE,]
nbStrangePeaks=dim(strangePeaks)[1]
test_that("11 strange peaks here",expect_true(nbStrangePeaks==11))



cyclesDefault25=detectCycle(df=res1,minExpi=25)
strangePeaks25=cyclesDefault25$peakTable[cyclesDefault25$peakTable[,"toRemove"]==TRUE,]
dim(strangePeaks25)[1]
nbStrangePeaks25=dim(strangePeaks25)[1]
test_that("9 strange peaks here, when minExpi is lower",expect_true(nbStrangePeaks25==9))



test_that("Peaks not removed in the cycle25 had maxIntensities >25",
          expect_true(all(strangePeaks[!strangePeaks[,"start"]%in%strangePeaks25[,"start"],"maxIntensity"]>25))
          )


# on a un cycle qui est trop petit
#cycles=detectCycle(df=res1,maxExpi =5,minInspi=15,smoothMethod="MovingAverage",method="MAD",halfWindowSize=12,maximum=NULL,SNR=0,minimalDuration=1)
cycles=detectCycle(df=res1,minExpi =15,maxInspi=5,smoothMethod="MovingAverage",method="MAD",halfWindowSize=6,maximum=NULL,SNR=0,minimalDuration=1)
dim(cycles$peakTable[cycles$peakTable[,"toRemove"]==TRUE,])[1]==6

cycles$finalPeakTable
cycles$peakTable[cycles$peakTable[,"toRemove"]==TRUE,]
cycles$gg$p3
cycles$gg$p2


#cycles=detectCycle(df=res1,maxPeaks=5,smoothMethod="MovingAverage",method="MAD",halfWindowSize=12,maximum=NULL,SNR=0,minimalDuration=1)
cycles=detectCycle(df=res1,minExpi=5,smoothMethod="MovingAverage",method="MAD",halfWindowSize=7,maximum=NULL,SNR=0,minimalDuration=1)
test_that("cycles avec duration minimale 1",expect_true(sum(diff(cycles$cycles)<1)==0))


cycles=detectCycle(df=res1,smoothMethod="MovingAverage",method="MAD",halfWindowSize=5,maximum=NULL,SNR=0,minimalDuration=2)
test_that("cycles avec duration minimale",expect_true(sum(diff(cycles$cycles)<2)==0))

