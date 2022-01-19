## ----setup, include=FALSE, warning=FALSE, message=FALSE-----------------------
knitr::opts_chunk$set(echo = TRUE, fig.height = 5, fig.width = 8, message = TRUE, warning = FALSE)

## ---- message = TRUE, warning = FALSE-----------------------------------------
#load libraries:
#load strm
library(strm)
library(rmarkdown)
#load package dependencies for the vignette
library(tidyr)
library(gt)
library(dplyr)
library(ggplot2)
library(patchwork)
library(purrr)
library(rgdal)
library(spdep)


## -----------------------------------------------------------------------------
data(wi_raw)


## -----------------------------------------------------------------------------
class(wi_raw)
names(wi_raw)
str(wi_raw)
head(wi_raw)

## -----------------------------------------------------------------------------
wi <- wi_raw %>%
    dplyr::select(-moe) %>%
    spread(key = variable, value=estimate) %>%
    mutate(pov_prct = B17020_002/B17020_001,
           feemp_prct = B23022_026/B23022_001,
           year=id,
           logpov = log(pov_prct)) 

wi <- st_transform(wi, 4326)
class(wi)
str(wi)

## -----------------------------------------------------------------------------
summary(wi$pov_prct)
tapply(wi$pov_prct, wi$year, summary)

## -----------------------------------------------------------------------------
summary(wi$feemp_prct)
tapply(wi$feemp_prct, wi$year, summary)

## -----------------------------------------------------------------------------
cor(wi$pov_prct,wi$feemp_prct)

## -----------------------------------------------------------------------------
ggplot(data=wi, aes(x=pov_prct)) +
    facet_grid(.~ year) +
    geom_histogram(binwidth=0.01,color="black", aes(fill=factor(year))) +
    geom_density(alpha=.2, aes(fill=factor(year))) +
    theme_minimal() +
    xlab("Percent Poverty") +
    labs(title="County-Level Percent Poverty in Wisconsin Across Time", fill="Year") +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none") +
    scale_fill_manual(values=c("chartreuse4", "deeppink4", "cornflowerblue","orange2","tomato3"))

breaks <- quantile(wi$logpov,seq(0,1,by=0.1))
ggplot(data=wi, aes(x=logpov)) +
    geom_histogram(aes(y =..density.., fill=factor(year)),color="black", breaks=breaks) +
    facet_grid(.~ year) +
    geom_density(alpha=.2, aes(fill=factor(year))) +
    theme_minimal() +
    xlab("(Log) Percent Poverty") +
    labs(title="County-Level (Log) Percent Poverty in Wisconsin Across Time", fill="Year") +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none") +
    scale_fill_manual(values=c("chartreuse4", "deeppink4", "cornflowerblue","orange2","tomato3"))
    

## -----------------------------------------------------------------------------
ggplot(wi) +
    geom_sf(aes(fill = pov_prct)) +
    scale_fill_viridis_c() +
    coord_sf(datum = NA) +
    facet_grid(. ~ year) +
    theme_minimal()+
    theme(plot.title=element_text(hjust = 0.5)) +
    labs(title = "County-Level Percent Poverty in Wisconsin Across Time", fill="%Poverty")


ggplot(wi) +
    geom_sf(aes(fill = logpov)) +
    scale_fill_viridis_c() +
    coord_sf(datum = NA) +
    facet_grid(. ~ year) +
    theme_minimal()+
    theme(plot.title=element_text(hjust = 0.5)) +
    labs(title = "County-Level (Log) Percent Poverty in Wisconsin Across Time", fill="(Log) %Poverty")

## -----------------------------------------------------------------------------
ggplot(data=wi, aes(x=feemp_prct)) +
    facet_grid(.~ year) +
    geom_histogram(binwidth=0.01,color="black", aes(fill=factor(year))) +
    geom_density(alpha=.2, aes(fill=factor(year))) +
    theme_minimal() +
    xlab("Percent Female Employment") +
    labs(title="County-Level Percent Female Employment in Wisconsin Across Time", fill="Year") +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none") +
    scale_fill_manual(values=c("chartreuse4", "deeppink4", "cornflowerblue","orange2","tomato3"))



## -----------------------------------------------------------------------------
ggplot(wi) +
    geom_sf(aes(fill = feemp_prct)) +
    scale_fill_viridis_c() +
    coord_sf(datum = NA) +
    facet_grid(. ~ year) +
    theme_minimal()+
    theme(plot.title=element_text(hjust = 0.5)) +
    labs(title = "County-Level Percent Female Employment in Wisconsin Across Time", fill="%Feemp")



## -----------------------------------------------------------------------------

wi_uniq <- wi %>%
    dplyr::filter(year==2018) %>%
    dplyr::select(GEOID, NAME)  
nrow(wi_uniq)
class(wi_uniq)

## -----------------------------------------------------------------------------
row.names(wi_uniq) <- wi_uniq$NAME
head(row.names(wi_uniq))
wi_nb <- poly2nb(wi_uniq, row.names=row.names(wi_uniq))
str(head(wi_nb))


## -----------------------------------------------------------------------------
listw_w <- nb2listw(wi_nb, style = "W")
class(listw_w)
str(head(listw_w$weights))

## ---- message=FALSE, warning=FALSE--------------------------------------------
county_geoms <- st_geometry(wi_uniq)
cntrd <- st_centroid(county_geoms)
coords <- st_coordinates(cntrd)
head(coords)

## -----------------------------------------------------------------------------
plot(county_geoms)
plot(listw_w, coords, col="blue", add=TRUE)

## -----------------------------------------------------------------------------
#create model formula
formula <-as.formula(logpov  ~ feemp_prct)
#use strm
out <- strm(formula, id="NAME", data=wi, listw = listw_w, time=2,wide=FALSE, returndf = TRUE)
#explore model summary output
summary(out$res)
#explore modified dataframe used in strm computation
summary(out$modframe)

## ---- warning=FALSE, message=FALSE--------------------------------------------
out_df <- cbind.data.frame(resids = out$res$residuals,
                           stdresids = out$res$residuals/sd(out$res$residuals),
                           fit = out$res$fitted.values)

## -----------------------------------------------------------------------------

#QQ Plot
p1 <- ggplot(out_df, aes(sample = stdresids)) +
    geom_qq(size = 1) +
    stat_qq_line() +
    theme_minimal() +
    xlab("Theoretical Quantiles") +
    ylab("Standardized Residuals") +
    ggtitle("QQ Plot") + 
    theme(plot.title=element_text(hjust = 0.5))

#Residuals vs. Fitted Plot
p2 <- ggplot(out_df, aes(x=fit, y=stdresids)) +
    geom_point(size = 0.5) +
    geom_hline(yintercept = 0, col="red", linetype="dashed") +
    theme_minimal() +
    xlab("Fitted Values") +
    ylab("Standardized Residuals") +
    ggtitle("Residuals vs. Fitted Plot") + 
    theme(plot.title=element_text(hjust = 0.5))

p1 + p2


## -----------------------------------------------------------------------------
wi_w <- as.data.frame(wi_raw) %>%
    dplyr::select(-moe) %>%
    spread(key = variable, value=estimate) %>%
    mutate(pov_prct = B17020_002/B17020_001,
           feemp_prct = B23022_026/B23022_001,
           year=id,
           logpov = log(pov_prct),
           logfeemp = log(pov_prct)) %>%
    dplyr::select(-id, -(B17020_001:B23022_026), - geometry) %>%
    relocate(year, .after=GEOID) %>%
    gather(variable, value, -(GEOID:NAME)) %>%
    unite(temp, variable, year) %>%
    spread(temp, value)

str(wi_w)
head(wi_w)
nrow(wi_w)
    

## -----------------------------------------------------------------------------
wi_2018geom <- wi %>%
    filter(year==2018) %>%
    dplyr::select(GEOID, geometry)
str(wi_2018geom)


## -----------------------------------------------------------------------------
#merge 2018 geometries back in
wi_w.sf <- merge(wi_2018geom, wi_w, by="GEOID")
    
nrow(wi_w.sf)
class(wi_w.sf)

## -----------------------------------------------------------------------------
p18 <- ggplot(wi_w.sf) +
    geom_sf(aes(fill = logpov_2018)) +
    scale_fill_viridis_c(limits = c(-3.2, -1)) +
    coord_sf(datum = NA) +
    theme_minimal() +
    theme(plot.title=element_text(hjust = 0.5)) +
    labs(title = "2018", fill="(Log) %Poverty")

p17 <- ggplot(wi_w.sf) +
    geom_sf(aes(fill = logpov_2017)) +
    scale_fill_viridis_c(limits = c(-3.2, -1)) +
    coord_sf(datum = NA) +
    theme_minimal() +
    theme(plot.title=element_text(hjust = 0.5)) +
    labs(title = "2017", fill="(Log) %Poverty")


#patchwork the plots together
patch1 <- p17 + p18
patch1 + 
    plot_annotation(title="County-Level (Log) Percent Poverty in Wisconsin Across Time") +
    plot_layout(guides = "collect")


## -----------------------------------------------------------------------------
row.names(wi_w.sf) <- wi_w.sf$NAME
head(row.names(wi_w.sf))
wi_nb_w <- poly2nb(wi_w.sf, row.names=row.names(wi_w.sf))
str(head(wi_nb_w))


## -----------------------------------------------------------------------------
listw_w <- nb2listw(wi_nb_w, style = "W")
class(listw_w)
str(head(listw_w$weights))

## ---- message=FALSE, warning=FALSE--------------------------------------------
county_geoms <- st_geometry(wi_w.sf)
cntrd <- st_centroid(county_geoms)
coords <- st_coordinates(cntrd)
head(coords)

## -----------------------------------------------------------------------------
plot(county_geoms)
plot(listw_w, coords, col="blue", add=TRUE)


## -----------------------------------------------------------------------------

formula2 <-as.formula(logpov_2018  ~feemp_prct_2017 + feemp_prct_2018 + logpov_2017)
out2 <- strm(formula2, id="NAME", data=wi_w.sf, listw= listw_w, time=2, wide=TRUE)
summary(out2)


## -----------------------------------------------------------------------------
out_coefs <- c(as.vector(out$res$coefficients), out$res$lambda)
out_ses <- c(as.vector(out$res$rest.se),out$res$lambda.se)
out_names <- c(attributes(out$res$coefficients)[[1]], "lambda")

out2_coefs <- c(as.vector(out2$coefficients), out2$lambda)
out2_ses <- c(as.vector(out2$rest.se), out2$lambda.se)
out2_names <- c(attributes(out2$coefficients)[[1]], "lambda")

out_table <- rbind.data.frame(cbind.data.frame(Term=out_names, Estimate=out_coefs, SE=out_ses, format=rep("Long format",length(out_names))),
                              cbind.data.frame(Term=out2_names, Estimate=out2_coefs, SE=out2_ses,format=rep("Wide format",length(out2_names))))

out_table %>%
  gt(groupname_col = c("format")) %>%
  tab_header(title="Comparison of Long and Wide format Output") %>%
  fmt_number(columns = c(Estimate, SE),  decimals = 2) %>%
  cols_label(Term = md("**Term**"), Estimate=md("**Estimate**"), SE=md("**SE**"))



## ---- eval=FALSE--------------------------------------------------------------
#  #load tidycensus
#  library(tidycensus)
#  options(tigris_use_cache = TRUE)

## ---- echo=FALSE, eval = FALSE, results=FALSE---------------------------------
#  census_api_key(Sys.getenv("CENSUS_API_KEY"), install=TRUE, overwrite = TRUE)
#  readRenviron("~/.Renviron")

## ----eval=FALSE---------------------------------------------------------------
#  census_api_key("YOUR API KEY HERE", install=TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  vars <- load_variables(2018, "acs5", cache=TRUE)
#  years <- lst(2017,2018)
#  multi_year <- map(years,~ get_acs(geography = "county",
#                                    variables = c("B17020_001","B17020_002","B23022_026","B23022_001"),
#                                    state = "WI",
#                                    year = .x,
#                                    geometry = TRUE)) %>%
#                map2(years, ~ mutate(.x, id = .y))
#  
#  wi_raw <- reduce(multi_year, rbind)
#  

