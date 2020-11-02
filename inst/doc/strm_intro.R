## ----setup, include=FALSE, warning=FALSE, message=FALSE-----------------------
knitr::opts_chunk$set(echo = TRUE, fig.height = 5, fig.width = 8, message = TRUE, warning = FALSE)

## ---- warning=FALSE, message=FALSE--------------------------------------------
#load libraries
#load strm
library(strm)
library(rmarkdown)
#load package dependencies for the vignette
library(tidyr)
library(dplyr)
library(rgdal)
library(spdep)
library(rgeos)
library(sf)
library(splm)
library(knitr)
library(Ecdat)

## -----------------------------------------------------------------------------
#load data example
data("Produc")
data("usaww")

#explore the data structures
str(Produc)
str(usaww)


## -----------------------------------------------------------------------------
#create list of spatial weights
usalw <- mat2listw(usaww)
class(usalw)

## -----------------------------------------------------------------------------
formula1 <-as.formula(log(gsp)  ~ log(pcap) + log(pc) + log(emp) + unemp)
out <- strm(formula1, id="state", data=Produc, listw = usalw, time=2,wide=FALSE,
            filter_options="year==1970 | year==1971")

summary(out)


## -----------------------------------------------------------------------------
formula2 <-as.formula(log(gsp)  ~ log(pcap) + log(pc) + log(emp) + unemp)
out <- strm(formula2, id="state", data=Produc, listw= usalw, time=3,
            wide=FALSE,filter_options="year==1970 | year==1971 | year ==1972")


## -----------------------------------------------------------------------------
summary(out)


## -----------------------------------------------------------------------------
data(sptdmg3)
class(sptdmg3)
names(sptdmg3)

## -----------------------------------------------------------------------------
formula2 <- as.formula(LNP1000 ~ LNP0090 + POLD00 + POLD90)
m2 <- lm(formula2, data = sptdmg3)
summary(m2)


## -----------------------------------------------------------------------------
#convert to simple features
sptdmg3_sf <- sf::st_as_sf(sptdmg3)
class(sptdmg3_sf)

## -----------------------------------------------------------------------------
#knn4
coords <-st_coordinates(st_centroid(sptdmg3_sf)) #extract centroid of each  minor civil division
col.knn <- knearneigh(coords, k=4)
nbknn <- knn2nb(col.knn, row.names = sptdmg3$STFID)
listwk <- nb2listw(nbknn, style="W")
plot(sptdmg3, main="Wisconsin: 4 Nearest-Neighbors"); plot(nbknn, coords,col="forestgreen", add=TRUE)

## -----------------------------------------------------------------------------
#since this data is in wide format, include temporal lag for both explanatory and response variable manually
res <- strm(formula2, id="STFID", data=sptdmg3_sf, listw = listwk, time=2,wide=TRUE)
summary(res)

## ----test, echo=FALSE, include=FALSE, eval=FALSE------------------------------
#  moran.test(sptdmg3$LNP1000, listwk, alternative = "two.sided")
#  moran.test(sptdmg3$LNP0090, listwk, alternative = "two.sided")
#  moran.test(sptdmg3$POLD00, listwk, alternative = "two.sided")
#  moran.test(sptdmg3$PUNMP00, listwk, alternative = "two.sided")
#  moran.test(sptdmg3$ACSAIR, listwk, alternative = "two.sided")
#  moran.test(sptdmg3$PFRST, listwk, alternative = "two.sided")
#  moran.test(sptdmg3$DEVPB, listwk, alternative = "two.sided")
#  
#  formulatest <- as.formula(LNP1000 ~ LNP0090 + POLD00 + PUNMP00 + ACSAIR + PFRST + DEVPB )
#  m2 <- lm(formulatest, data = sptdmg3)
#  summary(m2)
#  
#  moran.test(m2$residuals, listwk, alternative = "two.sided")

