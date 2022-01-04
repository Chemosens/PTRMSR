data(ptrv)
ptrv2=ptrv
  ions=colnames(ptrv2)[-c(1:3)]
res=reshape(ptrv2,direction="long",varying=list(ions),times=ions,v.names="intensity")
colnames(res)=c("abs","time","cycle","ion","intensity","id")
referenceBreath=substr("m69.06906..69.06906...Conc.",1,7)
res1=res[res[,"ion"]==as.character(referenceBreath),]
cycles=detectCycle(res1=res1,maxPeaks=NULL,smoothMethod="MovingAverage",method="MAD",halfWindowSize=12,maximum=NULL,SNR=0,minimalDuration=1)


test_that("cycles avec duration minimale 1",expect_true(sum(diff(cycles$cycles)<1)==0))


cycles=detectCycle(res1=res1,maxPeaks=NULL,smoothMethod="MovingAverage",method="MAD",halfWindowSize=5,maximum=NULL,SNR=0,minimalDuration=2)
test_that("cycles avec duration minimale",expect_true(sum(diff(cycles$cycles)<2)==0))

