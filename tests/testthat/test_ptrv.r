library(ggplot2)
library(plotly)
library(reshape2)
library(gridExtra)
data("ptrv")
breath="m69.06989..isoprene...Conc."


# Visualiser le jdd brut ()
p_resp_raw=ggplot(ptrv,aes(x=RelTime,y=m69.06989..isoprene...Conc.))+geom_line()+ggtitle("all")+theme_bw()+xlim(0,120)
ggplotly(p_resp_raw)


test_that("plot ions",
          expect_true(!is.null(p_resp_raw))
)
