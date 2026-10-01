###############################################################################
######      Real Data Analysis: PE-HDMM vs HDMM                          ######
######    Using Low Birthweight Infants (LBW) as the outcome variable    ######
###############################################################################

###############################################################################
## Load Data ## 
rm(list=ls())
library(POEMED)
load("data/data_GDPR_GHED_LBW.RData")
dim(data_gdpr) # 73 by 22
dim(data_lbw)  # 73 by 22
dim(data_ghed) # 1533 by 62
## columns: country, code, region, income, year, 57 indicators 
## rows: 73 countries * 21 years 

## sanity check 
sum(data_gdpr$Country.Code != sort(data_gdpr$Country.Code))
sum(data_lbw$Country.Code != sort(data_lbw$Country.Code))
sum(data_ghed$code != sort(data_ghed$code))
###############################################################################

###############################################################################
## ----------------------------------------- ##
## Model Fitting 1: A Global Mediation Model ## 
## ----------------------------------------- ## 

## transforming the format of data_gdpr 
code_column <- c()
gdp_column <- c()
year_column <- c()
for(i in 1:nrow(data_gdpr))
{
  code_temp <- rep(data_gdpr[i,]$Country.Code,21)
  gdp_temp <- unlist(data_gdpr[i,2:ncol(data_gdpr)])
  code_column <- c(code_column, code_temp)
  gdp_column <- c(gdp_column, gdp_temp)
  year_column <- c(year_column, 2000:2020)
}
data_gdpr_mat <- data.frame(code = code_column,
                            year = year_column, 
                            gdp = unname(gdp_column))
dim(data_gdpr_mat) # 1533 by 3

## transforming the format of data_lbw
code_column <- c()
lbw_column <- c()
year_column <- c()
for(i in 1:nrow(data_lbw))
{
  code_temp <- rep(data_lbw[i,]$Country.Code,21)
  lbw_temp <- unlist(data_lbw[i,2:ncol(data_lbw)])
  code_column <- c(code_column, code_temp)
  lbw_column <- c(lbw_column, lbw_temp)
  year_column <- c(year_column, 2000:2020)
}
data_lbw_mat <- data.frame(code = code_column,
                           year = year_column, 
                           lbw = unname(lbw_column))
dim(data_lbw_mat)  # 1533 by 3

## preprocessing data_ghed
data_ghed$country <- as.factor(data_ghed$country)
data_ghed$code <- as.factor(data_ghed$code)
data_ghed$region <- as.factor(data_ghed$region)
data_ghed$income <- as.factor(data_ghed$income)
data_gdpr_mat$code <- as.factor(data_gdpr_mat$code)
data_lbw_mat$code <- as.factor(data_lbw_mat$code)

## model fitting 
X <- matrix(data_gdpr_mat$gdp,ncol=1)
Y <- data_lbw_mat$lbw
M <- data_ghed[,6:ncol(data_ghed)]
S <- data_ghed[,c("region", "income", "year")]
## S contains categorical variables --> one-hot encoding
S <- model.matrix(lm(Y~., data=data.frame(Y, S)))[,-1]

ngrid = 100 ## set up HBIC for different 
lamb_grid = seq(0.1,2,length.out = ngrid) 
lamb_grid0 = seq(0.1,2,length.out = ngrid) 

# HDMM & PE-HDMM
output_PE <- pe_mediation_linear(scale(X), Y-mean(Y), scale(M), S,
                                 scale=FALSE, lambda_grid = lamb_grid, 
                                 lambda_grid_reduced =lamb_grid0)
output_PE$value[output_PE$term == "pval_hdmm"] # 0.0087
output_PE$value[output_PE$term == "pval_pe"]   # 0 
colnames(M)[attr(output_PE, "active_mediators")] # "shi_che" "gge_gdp"

###############################################################################

###############################################################################
## ------------------------------------------ ##
## Model Fitting 2: Regional Mediation Models ## 
## ------------------------------------------ ##
unique(data_ghed$region)
country_list_AFR  <- unique(data_ghed[data_ghed$region == 'AFR', 'code'])
country_list_AMR  <- unique(data_ghed[data_ghed$region == 'AMR', 'code'])
country_list_EMR  <- unique(data_ghed[data_ghed$region == 'EMR', 'code'])
country_list_EUR  <- unique(data_ghed[data_ghed$region == 'EUR', 'code'])
country_list_SEAR <- unique(data_ghed[data_ghed$region == 'SEAR', 'code'])
country_list_WPR  <- unique(data_ghed[data_ghed$region == 'WPR', 'code'])
length(country_list_AFR)  # 24
length(country_list_AMR)  # 22
length(country_list_EMR)  #  6
length(country_list_EUR)  #  9
length(country_list_SEAR) #  5
length(country_list_WPR)  #  7

data_gdpr_mat_AFR  <- data_gdpr_mat[data_gdpr_mat$code %in% country_list_AFR, ]
data_gdpr_mat_AMR  <- data_gdpr_mat[data_gdpr_mat$code %in% country_list_AMR, ]
data_gdpr_mat_EMR  <- data_gdpr_mat[data_gdpr_mat$code %in% country_list_EMR, ]
data_gdpr_mat_EUR  <- data_gdpr_mat[data_gdpr_mat$code %in% country_list_EUR, ]
data_gdpr_mat_SEAR <- data_gdpr_mat[data_gdpr_mat$code %in% country_list_SEAR, ]
data_gdpr_mat_WPR  <- data_gdpr_mat[data_gdpr_mat$code %in% country_list_WPR, ]

data_lbw_mat_AFR  <- data_lbw_mat[data_lbw_mat$code %in% country_list_AFR,]
data_lbw_mat_AMR  <- data_lbw_mat[data_lbw_mat$code %in% country_list_AMR,]
data_lbw_mat_EMR  <- data_lbw_mat[data_lbw_mat$code %in% country_list_EMR,]
data_lbw_mat_EUR  <- data_lbw_mat[data_lbw_mat$code %in% country_list_EUR,]
data_lbw_mat_SEAR <- data_lbw_mat[data_lbw_mat$code %in% country_list_SEAR,]
data_lbw_mat_WPR  <- data_lbw_mat[data_lbw_mat$code %in% country_list_WPR,]

data_ghed_AFR  <- data_ghed[data_ghed$code %in% country_list_AFR,]
data_ghed_AMR  <- data_ghed[data_ghed$code %in% country_list_AMR,]
data_ghed_EMR  <- data_ghed[data_ghed$code %in% country_list_EMR,]
data_ghed_EUR  <- data_ghed[data_ghed$code %in% country_list_EUR,]
data_ghed_SEAR <- data_ghed[data_ghed$code %in% country_list_SEAR,]
data_ghed_WPR  <- data_ghed[data_ghed$code %in% country_list_WPR,]

## region == 'AFR' (24 countries)
X <- matrix(data_gdpr_mat_AFR$gdp,ncol=1); dim(X) # 504
Y <- data_lbw_mat_AFR$lbw; length(Y)
M <- data_ghed_AFR[,6:ncol(data_ghed_AFR)]; dim(M)
S <- data_ghed_AFR[,c("income", "year")]; dim(S)
S <- model.matrix(lm(Y~., data=data.frame(Y, S)))[,-1]; dim(S)
ngrid = 100
lamb_grid = seq(0.1,2,length.out = ngrid) 
lamb_grid0 = seq(0.1,2,length.out = ngrid) 
output_region_AFR_POEM <- pe_mediation_linear(scale(X), Y-mean(Y), scale(M), S,
                                              scale = FALSE,
                                              lambda_grid = lamb_grid,
                                              lambda_grid_reduced = lamb_grid0)
output_region_AFR_POEM$value[output_region_AFR_POEM$term == "pval_hdmm"] # 0.1846
output_region_AFR_POEM$value[output_region_AFR_POEM$term == "pval_pe"]   # 0
colnames(M)[attr(output_region_AFR_POEM, "active_mediators")] # "gfa_che"

## region == 'AMR' (22 countries)
X <- matrix(data_gdpr_mat_AMR$gdp,ncol=1); dim(X) # 462
Y <- data_lbw_mat_AMR$lbw; length(Y)
M <- data_ghed_AMR[,6:ncol(data_ghed_AMR)]; dim(M)
S <- data_ghed_AMR[,c("income", "year")]; dim(S)
S <- model.matrix(lm(Y~., data=data.frame(Y, S)))[,-1]; dim(S)
ngrid = 100
lamb_grid = seq(0.1,2,length.out = ngrid) 
lamb_grid0 = seq(0.1,2,length.out = ngrid) 
output_region_AMR_POEM <- pe_mediation_linear(scale(X), Y-mean(Y), scale(M), S,
                                              scale = FALSE,
                                              lambda_grid = lamb_grid,
                                              lambda_grid_reduced = lamb_grid0)
output_region_AMR_POEM$value[output_region_AMR_POEM$term == "pval_hdmm"] # 0.4680
output_region_AMR_POEM$value[output_region_AMR_POEM$term == "pval_pe"]   # 0.4680
colnames(M)[attr(output_region_AMR_POEM, "active_mediators")] # null

## region == 'EMR' (6 countries)
X <- matrix(data_gdpr_mat_EMR$gdp,ncol=1); dim(X) # 126
Y <- data_lbw_mat_EMR$lbw; length(Y)
M <- data_ghed_EMR[,6:ncol(data_ghed_EMR)]; dim(M)
S <- data_ghed_EMR[,c("income", "year")]; dim(S)
S <- model.matrix(lm(Y~., data=data.frame(Y, S)))[,-1]; dim(S)
ngrid = 100
lamb_grid = seq(0.1,2,length.out = ngrid) 
lamb_grid0 = seq(0.1,2,length.out = ngrid) 
output_region_EMR_POEM <- pe_mediation_linear(scale(X), Y-mean(Y), scale(M), S,
                                              scale = FALSE,
                                              lambda_grid = lamb_grid,
                                              lambda_grid_reduced = lamb_grid0)
output_region_EMR_POEM$value[output_region_EMR_POEM$term == "pval_hdmm"] # 0.7743
output_region_EMR_POEM$value[output_region_EMR_POEM$term == "pval_pe"]   # 0.7743
colnames(M)[attr(output_region_EMR_POEM, "active_mediators")] # null

## region == 'EUR' (9 countries)
X <- matrix(data_gdpr_mat_EUR$gdp,ncol=1); dim(X) # 189
Y <- data_lbw_mat_EUR$lbw; length(Y)
M <- data_ghed_EUR[,6:ncol(data_ghed_EUR)]; dim(M)
S <- data_ghed_EUR[,c("income", "year")]; dim(S)
S <- model.matrix(lm(Y~., data=data.frame(Y, S)))[,-1]; dim(S)
ngrid = 100
lamb_grid = seq(0.1,2,length.out = ngrid) 
lamb_grid0 = seq(0.1,2,length.out = ngrid) 
output_region_EUR_POEM <- pe_mediation_linear(scale(X), Y-mean(Y), scale(M), S,
                                              scale = FALSE,
                                              lambda_grid = lamb_grid,
                                              lambda_grid_reduced = lamb_grid0)
output_region_EUR_POEM$value[output_region_EUR_POEM$term == "pval_hdmm"] # 0.0572
output_region_EUR_POEM$value[output_region_EUR_POEM$term == "pval_pe"]   # 0
colnames(M)[attr(output_region_EUR_POEM, "active_mediators")] # "shi_che" "gghed_usd2021_pc"

## region == 'SEAR' (5 countries)
X <- matrix(data_gdpr_mat_SEAR$gdp,ncol=1); dim(X) # 105
Y <- data_lbw_mat_SEAR$lbw; length(Y)
M <- data_ghed_SEAR[,6:ncol(data_ghed_SEAR)]; dim(M)
S <- data_ghed_SEAR[,c("income", "year")]; dim(S)
S <- model.matrix(lm(Y~., data=data.frame(Y, S)))[,-1]; dim(S)
ngrid = 100
lamb_grid = seq(0.1,2,length.out = ngrid) 
lamb_grid0 = seq(0.1,2,length.out = ngrid) 
output_region_SEAR_POEM <- pe_mediation_linear(scale(X), Y-mean(Y), scale(M), S,
                                               scale = FALSE,
                                               lambda_grid = lamb_grid,
                                               lambda_grid_reduced = lamb_grid0)
output_region_SEAR_POEM$value[output_region_SEAR_POEM$term == "pval_hdmm"] # 0.4265
output_region_SEAR_POEM$value[output_region_SEAR_POEM$term == "pval_pe"]   # 0.4265
colnames(M)[attr(output_region_SEAR_POEM, "active_mediators")] # null

## region == 'WPR' (7 countries)
X <- matrix(data_gdpr_mat_WPR$gdp,ncol=1); dim(X) # 147
Y <- data_lbw_mat_WPR$lbw; length(Y)
M <- data_ghed_WPR[,6:ncol(data_ghed_WPR)]; dim(M)
S <- data_ghed_WPR[,c("income", "year")]; dim(S)
S <- model.matrix(lm(Y~., data=data.frame(Y, S)))[,-1]; dim(S)
ngrid = 100
lamb_grid = seq(0.1,2,length.out = ngrid) 
lamb_grid0 = seq(0.1,2,length.out = ngrid) 
output_region_WPR_POEM <- pe_mediation_linear(scale(X), Y-mean(Y), scale(M), S,
                                              scale = FALSE,
                                              lambda_grid = lamb_grid,
                                              lambda_grid_reduced = lamb_grid0)
output_region_WPR_POEM$value[output_region_WPR_POEM$term == "pval_hdmm"] # 0.0083
output_region_WPR_POEM$value[output_region_WPR_POEM$term == "pval_pe"]   # 0
colnames(M)[attr(output_region_WPR_POEM, "active_mediators")] # "gfa_che"      "chi_pvt_che"  "pvtd_ncu2021"
###############################################################################

###############################################################################
## ------------------------------------------------ ##
## Model Fitting 3: Group Data by Income Categories ## 
## ------------------------------------------------ ##
unique(data_ghed$income)
country_list_High  <- unique(data_ghed[data_ghed$income == 'High', 'code'])
country_list_UM  <- unique(data_ghed[data_ghed$income == 'Upper-middle', 'code']) 
country_list_LM  <- unique(data_ghed[data_ghed$income == 'Lower-middle', 'code'])
country_list_Low  <- unique(data_ghed[data_ghed$income == 'Low', 'code'])
length(country_list_High) # 20
length(country_list_UM)   # 21
length(country_list_LM)   # 22
length(country_list_Low)  # 10 

data_gdpr_mat_High <- data_gdpr_mat[data_gdpr_mat$code %in% country_list_High, ]
data_gdpr_mat_UM   <- data_gdpr_mat[data_gdpr_mat$code %in% country_list_UM, ]
data_gdpr_mat_LM   <- data_gdpr_mat[data_gdpr_mat$code %in% country_list_LM, ]
data_gdpr_mat_Low  <- data_gdpr_mat[data_gdpr_mat$code %in% country_list_Low, ]

data_lbw_mat_High <-  data_lbw_mat[data_lbw_mat$code %in% country_list_High, ]
data_lbw_mat_UM   <-  data_lbw_mat[data_lbw_mat$code %in% country_list_UM, ]
data_lbw_mat_LM   <-  data_lbw_mat[data_lbw_mat$code %in% country_list_LM, ]
data_lbw_mat_Low  <-  data_lbw_mat[data_lbw_mat$code %in% country_list_Low, ]

data_ghed_High <- data_ghed[data_ghed$code %in% country_list_High,]
data_ghed_UM   <- data_ghed[data_ghed$code %in% country_list_UM,]
data_ghed_LM   <- data_ghed[data_ghed$code %in% country_list_LM,]
data_ghed_Low  <- data_ghed[data_ghed$code %in% country_list_Low,]

## income == 'Low' (10 countries)
X <- matrix(data_gdpr_mat_Low$gdp,ncol=1); dim(X) # 210
Y <- data_lbw_mat_Low$lbw; length(Y)
M <- data_ghed_Low[,6:ncol(data_ghed_Low)]; dim(M)
#  S <- data_ghed_Low[,c("region", "year")]; dim(S)
#  S <- model.matrix(lm(Y~., data=data.frame(Y, S)))[,-1]; dim(S)
## Error:  contrasts can be applied only to factors with 2 or more levels
## Soln: remove "region" from S
S <- matrix(data_ghed_Low[,c("year")], ncol=1); dim(S)
ngrid = 100
lamb_grid = seq(0.1,2,length.out = ngrid) 
lamb_grid0 = seq(0.1,2,length.out = ngrid) 
output_income_Low_POEM <- pe_mediation_linear(scale(X), Y-mean(Y), scale(M), S,
                                              scale = FALSE,
                                              lambda_grid = lamb_grid,
                                              lambda_grid_reduced = lamb_grid0)
output_income_Low_POEM$value[output_income_Low_POEM$term == "pval_hdmm"] # 0.1753
output_income_Low_POEM$value[output_income_Low_POEM$term == "pval_pe"]   # 0.1753
colnames(M)[attr(output_income_Low_POEM, "active_mediators")] # null

## income == 'Lower-middle' (22 countries)
X <- matrix(data_gdpr_mat_LM$gdp,ncol=1); dim(X) # 462
Y <- data_lbw_mat_LM$lbw; length(Y)
M <- data_ghed_LM[,6:ncol(data_ghed_LM)]; dim(M)
S <- data_ghed_LM[,c("region", "year")]; dim(S)
S <- model.matrix(lm(Y~., data=data.frame(Y, S)))[,-1]; dim(S)
ngrid = 100
lamb_grid = seq(0.1,2,length.out = ngrid) 
lamb_grid0 = seq(0.1,2,length.out = ngrid) 
output_income_LM_POEM <- pe_mediation_linear(scale(X), Y-mean(Y), scale(M), S,
                                             scale = FALSE,
                                             lambda_grid = lamb_grid,
                                             lambda_grid_reduced = lamb_grid0)
output_income_LM_POEM$value[output_income_LM_POEM$term == "pval_hdmm"] # 0.2509
output_income_LM_POEM$value[output_income_LM_POEM$term == "pval_pe"]   # 0.2509
colnames(M)[attr(output_income_LM_POEM, "active_mediators")] # null

## income == 'Upper-middle' (21 countries)
X <- matrix(data_gdpr_mat_UM$gdp,ncol=1); dim(X) # 441
Y <- data_lbw_mat_UM$lbw; length(Y)
M <- data_ghed_UM[,6:ncol(data_ghed_UM)]; dim(M)
S <- data_ghed_UM[,c("region", "year")]; dim(S)
S <- model.matrix(lm(Y~., data=data.frame(Y, S)))[,-1]; dim(S)
ngrid = 100
lamb_grid = seq(0.1,2,length.out = ngrid) 
lamb_grid0 = seq(0.1,2,length.out = ngrid) 
output_income_UM_POEM <- pe_mediation_linear(scale(X), Y-mean(Y), scale(M), S,
                                             scale = FALSE,
                                             lambda_grid = lamb_grid,
                                             lambda_grid_reduced = lamb_grid0)
output_income_UM_POEM$value[output_income_UM_POEM$term == "pval_hdmm"] # 0.9900
output_income_UM_POEM$value[output_income_UM_POEM$term == "pval_pe"]   # 0.9900
colnames(M)[attr(output_income_UM_POEM, "active_mediators")] # null

## income == 'High' (20 countries)
X <- matrix(data_gdpr_mat_High$gdp,ncol=1); dim(X) # 420
Y <- data_lbw_mat_High$lbw; length(Y)
M <- data_ghed_High[,6:ncol(data_ghed_High)]; dim(M)
S <- data_ghed_High[,c("region", "year")]; dim(S)
S <- model.matrix(lm(Y~., data=data.frame(Y, S)))[,-1]; dim(S)
## sum(is.na(scale(M))) is non-zero. 
## which(is.na(scale(M)[1,])) # 41 "ext_gdp"
M <- M[,-which(is.na(scale(M)[1,]))]
dim(M) # 420 by 56
ngrid = 100
lamb_grid = seq(0.1,2,length.out = ngrid) 
lamb_grid0 = seq(0.1,2,length.out = ngrid) 
output_income_High_POEM <- pe_mediation_linear(scale(X), Y-mean(Y), scale(M), S,
                                               scale = FALSE,
                                               lambda_grid = lamb_grid,
                                               lambda_grid_reduced = lamb_grid0)
output_income_High_POEM$value[output_income_High_POEM$term == "pval_hdmm"] # 0.7673
output_income_High_POEM$value[output_income_High_POEM$term == "pval_pe"]   # 0
colnames(M)[attr(output_income_High_POEM, "active_mediators")] # "shi_che"

###############################################################################

###############################################################################
## --------------------------------------------- ##
## Model Fitting 4: Group Data by Region*Income  ## 
## --------------------------------------------- ##

length(intersect(country_list_AFR, country_list_High)) #  0
length(intersect(country_list_AFR, country_list_UM))   #  4
length(intersect(country_list_AFR, country_list_LM))   # 10
length(intersect(country_list_AFR, country_list_Low))  # 10 

length(intersect(country_list_AMR, country_list_High)) #  7
length(intersect(country_list_AMR, country_list_UM))   # 12
length(intersect(country_list_AMR, country_list_LM))   #  3
length(intersect(country_list_AMR, country_list_Low))  #  0 

length(intersect(country_list_EMR, country_list_High)) #  2
length(intersect(country_list_EMR, country_list_UM))   #  1
length(intersect(country_list_EMR, country_list_LM))   #  3
length(intersect(country_list_EMR, country_list_Low))  #  0 

length(intersect(country_list_EUR, country_list_High)) #  8
length(intersect(country_list_EUR, country_list_UM))   #  0
length(intersect(country_list_EUR, country_list_LM))   #  1
length(intersect(country_list_EUR, country_list_Low))  #  0

length(intersect(country_list_SEAR, country_list_High)) # 0
length(intersect(country_list_SEAR, country_list_UM))   # 2
length(intersect(country_list_SEAR, country_list_LM))   # 3
length(intersect(country_list_SEAR, country_list_Low))  # 0

length(intersect(country_list_WPR, country_list_High)) #  3
length(intersect(country_list_WPR, country_list_UM))   #  2
length(intersect(country_list_WPR, country_list_LM))   #  2
length(intersect(country_list_WPR, country_list_Low))  #  0

data_gdpr_mat_AFR_High  <- data_gdpr_mat[data_gdpr_mat$code %in% intersect(country_list_AFR, country_list_High), ]
data_gdpr_mat_AFR_UM  <- data_gdpr_mat[data_gdpr_mat$code %in% intersect(country_list_AFR, country_list_UM), ]
data_gdpr_mat_AFR_LM  <- data_gdpr_mat[data_gdpr_mat$code %in% intersect(country_list_AFR, country_list_LM), ]
data_gdpr_mat_AFR_Low  <- data_gdpr_mat[data_gdpr_mat$code %in% intersect(country_list_AFR, country_list_Low), ]

data_gdpr_mat_AMR_High  <- data_gdpr_mat[data_gdpr_mat$code %in% intersect(country_list_AMR, country_list_High), ]
data_gdpr_mat_AMR_UM  <- data_gdpr_mat[data_gdpr_mat$code %in% intersect(country_list_AMR, country_list_UM), ]
data_gdpr_mat_AMR_LM  <- data_gdpr_mat[data_gdpr_mat$code %in% intersect(country_list_AMR, country_list_LM), ]
data_gdpr_mat_AMR_Low  <- data_gdpr_mat[data_gdpr_mat$code %in% intersect(country_list_AMR, country_list_Low), ]

data_gdpr_mat_EMR_High  <- data_gdpr_mat[data_gdpr_mat$code %in% intersect(country_list_EMR, country_list_High), ]
data_gdpr_mat_EMR_UM  <- data_gdpr_mat[data_gdpr_mat$code %in% intersect(country_list_EMR, country_list_UM), ]
data_gdpr_mat_EMR_LM  <- data_gdpr_mat[data_gdpr_mat$code %in% intersect(country_list_EMR, country_list_LM), ]
data_gdpr_mat_EMR_Low  <- data_gdpr_mat[data_gdpr_mat$code %in% intersect(country_list_EMR, country_list_Low), ]

data_gdpr_mat_EUR_High  <- data_gdpr_mat[data_gdpr_mat$code %in% intersect(country_list_EUR, country_list_High), ]
data_gdpr_mat_EUR_UM  <- data_gdpr_mat[data_gdpr_mat$code %in% intersect(country_list_EUR, country_list_UM), ]
data_gdpr_mat_EUR_LM  <- data_gdpr_mat[data_gdpr_mat$code %in% intersect(country_list_EUR, country_list_LM), ]
data_gdpr_mat_EUR_Low  <- data_gdpr_mat[data_gdpr_mat$code %in% intersect(country_list_EUR, country_list_Low), ]

data_gdpr_mat_SEAR_High  <- data_gdpr_mat[data_gdpr_mat$code %in% intersect(country_list_SEAR, country_list_High), ]
data_gdpr_mat_SEAR_UM  <- data_gdpr_mat[data_gdpr_mat$code %in% intersect(country_list_SEAR, country_list_UM), ]
data_gdpr_mat_SEAR_LM  <- data_gdpr_mat[data_gdpr_mat$code %in% intersect(country_list_SEAR, country_list_LM), ]
data_gdpr_mat_SEAR_Low  <- data_gdpr_mat[data_gdpr_mat$code %in% intersect(country_list_SEAR, country_list_Low), ]

data_gdpr_mat_WPR_High  <- data_gdpr_mat[data_gdpr_mat$code %in% intersect(country_list_WPR, country_list_High), ]
data_gdpr_mat_WPR_UM  <- data_gdpr_mat[data_gdpr_mat$code %in% intersect(country_list_WPR, country_list_UM), ]
data_gdpr_mat_WPR_LM  <- data_gdpr_mat[data_gdpr_mat$code %in% intersect(country_list_WPR, country_list_LM), ]
data_gdpr_mat_WPR_Low  <- data_gdpr_mat[data_gdpr_mat$code %in% intersect(country_list_WPR, country_list_Low), ]

data_lbw_mat_AFR_High  <- data_lbw_mat[data_lbw_mat$code %in% intersect(country_list_AFR, country_list_High), ]
data_lbw_mat_AFR_UM  <- data_lbw_mat[data_lbw_mat$code %in% intersect(country_list_AFR, country_list_UM), ]
data_lbw_mat_AFR_LM  <- data_lbw_mat[data_lbw_mat$code %in% intersect(country_list_AFR, country_list_LM), ]
data_lbw_mat_AFR_Low  <- data_lbw_mat[data_lbw_mat$code %in% intersect(country_list_AFR, country_list_Low), ]

data_lbw_mat_AMR_High  <- data_lbw_mat[data_lbw_mat$code %in% intersect(country_list_AMR, country_list_High), ]
data_lbw_mat_AMR_UM  <- data_lbw_mat[data_lbw_mat$code %in% intersect(country_list_AMR, country_list_UM), ]
data_lbw_mat_AMR_LM  <- data_lbw_mat[data_lbw_mat$code %in% intersect(country_list_AMR, country_list_LM), ]
data_lbw_mat_AMR_Low  <- data_lbw_mat[data_lbw_mat$code %in% intersect(country_list_AMR, country_list_Low), ]

data_lbw_mat_EMR_High  <- data_lbw_mat[data_lbw_mat$code %in% intersect(country_list_EMR, country_list_High), ]
data_lbw_mat_EMR_UM  <- data_lbw_mat[data_lbw_mat$code %in% intersect(country_list_EMR, country_list_UM), ]
data_lbw_mat_EMR_LM  <- data_lbw_mat[data_lbw_mat$code %in% intersect(country_list_EMR, country_list_LM), ]
data_lbw_mat_EMR_Low  <- data_lbw_mat[data_lbw_mat$code %in% intersect(country_list_EMR, country_list_Low), ]

data_lbw_mat_EUR_High  <- data_lbw_mat[data_lbw_mat$code %in% intersect(country_list_EUR, country_list_High), ]
data_lbw_mat_EUR_UM  <- data_lbw_mat[data_lbw_mat$code %in% intersect(country_list_EUR, country_list_UM), ]
data_lbw_mat_EUR_LM  <- data_lbw_mat[data_lbw_mat$code %in% intersect(country_list_EUR, country_list_LM), ]
data_lbw_mat_EUR_Low  <- data_lbw_mat[data_lbw_mat$code %in% intersect(country_list_EUR, country_list_Low), ]

data_lbw_mat_SEAR_High  <- data_lbw_mat[data_lbw_mat$code %in% intersect(country_list_SEAR, country_list_High), ]
data_lbw_mat_SEAR_UM  <- data_lbw_mat[data_lbw_mat$code %in% intersect(country_list_SEAR, country_list_UM), ]
data_lbw_mat_SEAR_LM  <- data_lbw_mat[data_lbw_mat$code %in% intersect(country_list_SEAR, country_list_LM), ]
data_lbw_mat_SEAR_Low  <- data_lbw_mat[data_lbw_mat$code %in% intersect(country_list_SEAR, country_list_Low), ]

data_lbw_mat_WPR_High  <- data_lbw_mat[data_lbw_mat$code %in% intersect(country_list_WPR, country_list_High), ]
data_lbw_mat_WPR_UM  <- data_lbw_mat[data_lbw_mat$code %in% intersect(country_list_WPR, country_list_UM), ]
data_lbw_mat_WPR_LM  <- data_lbw_mat[data_lbw_mat$code %in% intersect(country_list_WPR, country_list_LM), ]
data_lbw_mat_WPR_Low  <- data_lbw_mat[data_lbw_mat$code %in% intersect(country_list_WPR, country_list_Low), ]

data_ghed_AFR_High  <- data_ghed[data_ghed$code %in% intersect(country_list_AFR, country_list_High), ]
data_ghed_AFR_UM  <- data_ghed[data_ghed$code %in% intersect(country_list_AFR, country_list_UM), ]
data_ghed_AFR_LM  <- data_ghed[data_ghed$code %in% intersect(country_list_AFR, country_list_LM), ]
data_ghed_AFR_Low  <- data_ghed[data_ghed$code %in% intersect(country_list_AFR, country_list_Low), ]

data_ghed_AMR_High  <- data_ghed[data_ghed$code %in% intersect(country_list_AMR, country_list_High), ]
data_ghed_AMR_UM  <- data_ghed[data_ghed$code %in% intersect(country_list_AMR, country_list_UM), ]
data_ghed_AMR_LM  <- data_ghed[data_ghed$code %in% intersect(country_list_AMR, country_list_LM), ]
data_ghed_AMR_Low  <- data_ghed[data_ghed$code %in% intersect(country_list_AMR, country_list_Low), ]

data_ghed_EMR_High  <- data_ghed[data_ghed$code %in% intersect(country_list_EMR, country_list_High), ]
data_ghed_EMR_UM  <- data_ghed[data_ghed$code %in% intersect(country_list_EMR, country_list_UM), ]
data_ghed_EMR_LM  <- data_ghed[data_ghed$code %in% intersect(country_list_EMR, country_list_LM), ]
data_ghed_EMR_Low  <- data_ghed[data_ghed$code %in% intersect(country_list_EMR, country_list_Low), ]

data_ghed_EUR_High  <- data_ghed[data_ghed$code %in% intersect(country_list_EUR, country_list_High), ]
data_ghed_EUR_UM  <- data_ghed[data_ghed$code %in% intersect(country_list_EUR, country_list_UM), ]
data_ghed_EUR_LM  <- data_ghed[data_ghed$code %in% intersect(country_list_EUR, country_list_LM), ]
data_ghed_EUR_Low  <- data_ghed[data_ghed$code %in% intersect(country_list_EUR, country_list_Low), ]

data_ghed_SEAR_High  <- data_ghed[data_ghed$code %in% intersect(country_list_SEAR, country_list_High), ]
data_ghed_SEAR_UM  <- data_ghed[data_ghed$code %in% intersect(country_list_SEAR, country_list_UM), ]
data_ghed_SEAR_LM  <- data_ghed[data_ghed$code %in% intersect(country_list_SEAR, country_list_LM), ]
data_ghed_SEAR_Low  <- data_ghed[data_ghed$code %in% intersect(country_list_SEAR, country_list_Low), ]

data_ghed_WPR_High  <- data_ghed[data_ghed$code %in% intersect(country_list_WPR, country_list_High), ]
data_ghed_WPR_UM  <- data_ghed[data_ghed$code %in% intersect(country_list_WPR, country_list_UM), ]
data_ghed_WPR_LM  <- data_ghed[data_ghed$code %in% intersect(country_list_WPR, country_list_LM), ]
data_ghed_WPR_Low  <- data_ghed[data_ghed$code %in% intersect(country_list_WPR, country_list_Low), ]

## region=='AFR' & income=='Low'
X <- matrix(data_gdpr_mat_AFR_Low$gdp,ncol=1); dim(X) # 210
Y <- data_lbw_mat_AFR_Low$lbw; length(Y)
M <- data_ghed_AFR_Low[,6:ncol(data_ghed_AFR_Low)]; dim(M)
S <- matrix(data_ghed_AFR_Low[,c("year")], ncol=1); dim(S)
sum(is.na(scale(M))) == 0  # TRUE
ngrid = 100
lamb_grid = seq(0.1,2,length.out = ngrid) 
lamb_grid0 = seq(0.1,2,length.out = ngrid) 
output_AFR_Low_POEM <- pe_mediation_linear(scale(X), Y-mean(Y), scale(M), S,
                                           scale = FALSE,
                                           lambda_grid = lamb_grid,
                                           lambda_grid_reduced = lamb_grid0)
output_AFR_Low_POEM$value[output_AFR_Low_POEM$term == "pval_hdmm"] # 0.1753
output_AFR_Low_POEM$value[output_AFR_Low_POEM$term == "pval_pe"]   # 0.1753
colnames(M)[attr(output_AFR_Low_POEM, "active_mediators")] # null

## region=='AFR' & income=='Lower-middle'
X <- matrix(data_gdpr_mat_AFR_LM$gdp,ncol=1); dim(X) # 210
Y <- data_lbw_mat_AFR_LM$lbw; length(Y)
M <- data_ghed_AFR_LM[,6:ncol(data_ghed_AFR_LM)]; dim(M)
S <- matrix(data_ghed_AFR_LM[,c("year")], ncol=1); dim(S)
sum(is.na(scale(M))) == 0  # FALSE
## There are some constant columns in M, which becomes NA after scaling 
M <- M[,-which(is.na(scale(M)[1,]))]
dim(M) # 210 by 56
ngrid = 100
lamb_grid = seq(0.1,2,length.out = ngrid) 
lamb_grid0 = seq(0.1,2,length.out = ngrid) 
output_AFR_LM_POEM <- pe_mediation_linear(scale(X), Y-mean(Y), scale(M), S,
                                          scale = FALSE,
                                          lambda_grid = lamb_grid,
                                          lambda_grid_reduced = lamb_grid0)
output_AFR_LM_POEM$value[output_AFR_LM_POEM$term == "pval_hdmm"] # 0.0363
output_AFR_LM_POEM$value[output_AFR_LM_POEM$term == "pval_pe"]   # 0
colnames(M)[attr(output_AFR_LM_POEM, "active_mediators")] # "pvtd_usd2021_pc"

## region=='AFR' & income=='Upper-middle'
X <- matrix(data_gdpr_mat_AFR_UM$gdp,ncol=1); dim(X) # 84
Y <- data_lbw_mat_AFR_UM$lbw; length(Y) 
M <- data_ghed_AFR_UM[,6:ncol(data_ghed_AFR_UM)]; dim(M)
S <- matrix(data_ghed_AFR_UM[,c("year")], ncol=1); dim(S)
sum(is.na(scale(M))) == 0  # FALSE
## There are some constant columns in M, which becomes NA after scaling 
M <- M[,-which(is.na(scale(M)[1,]))]
dim(M) # 84 by 56
ngrid = 100
lamb_grid = seq(0.1,2,length.out = ngrid) 
lamb_grid0 = seq(0.1,2,length.out = ngrid) 
output_AFR_UM_POEM <- pe_mediation_linear(scale(X), Y-mean(Y), scale(M), S,
                                          scale = FALSE,
                                          lambda_grid = lamb_grid,
                                          lambda_grid_reduced = lamb_grid0)
output_AFR_UM_POEM$value[output_AFR_UM_POEM$term == "pval_hdmm"] # 0.0022
output_AFR_UM_POEM$value[output_AFR_UM_POEM$term == "pval_pe"]   # 0.0022
colnames(M)[attr(output_AFR_UM_POEM, "active_mediators")] # null

## region=='AFR' & income=='High' -- no samples
X <- matrix(data_gdpr_mat_AFR_High$gdp,ncol=1); dim(X) # 0
Y <- data_lbw_mat_AFR_High$lbw; length(Y) # 0

## region=='AMR' & income=='Low' -- no samples
X <- matrix(data_gdpr_mat_AMR_Low$gdp,ncol=1); dim(X) # 0
Y <- data_lbw_mat_AMR_Low$lbw; length(Y)  

## region=='AMR' & income=='Lower-middle'
X <- matrix(data_gdpr_mat_AMR_LM$gdp,ncol=1); dim(X) # 63
Y <- data_lbw_mat_AMR_LM$lbw; length(Y) 
M <- data_ghed_AMR_LM[,6:ncol(data_ghed_AMR_LM)]; dim(M)
S <- matrix(data_ghed_AMR_LM[,c("year")], ncol=1); dim(S)
sum(is.na(scale(M))) == 0  # TRUE
ngrid = 100
lamb_grid = seq(0.1,2,length.out = ngrid) 
lamb_grid0 = seq(0.1,2,length.out = ngrid) 
output_AMR_LM_POEM <- pe_mediation_linear(scale(X), Y-mean(Y), scale(M), S,
                                          scale = FALSE,
                                          lambda_grid = lamb_grid,
                                          lambda_grid_reduced = lamb_grid0)
output_AMR_LM_POEM$value[output_AMR_LM_POEM$term == "pval_hdmm"] # 0.8768
output_AMR_LM_POEM$value[output_AMR_LM_POEM$term == "pval_pe"]   # 0.8768
colnames(M)[attr(output_AMR_LM_POEM, "active_mediators")] # null

## region=='AMR' & income=='Upper-middle'
X <- matrix(data_gdpr_mat_AMR_UM$gdp,ncol=1); dim(X) # 252
Y <- data_lbw_mat_AMR_UM$lbw; length(Y) 
M <- data_ghed_AMR_UM[,6:ncol(data_ghed_AMR_UM)]; dim(M)
S <- matrix(data_ghed_AMR_UM[,c("year")], ncol=1); dim(S)
sum(is.na(scale(M))) == 0  # TRUE
ngrid = 100
lamb_grid = seq(0.1,2,length.out = ngrid) 
lamb_grid0 = seq(0.1,2,length.out = ngrid) 
output_AMR_UM_POEM <- pe_mediation_linear(scale(X), Y-mean(Y), scale(M), S,
                                          scale = FALSE,
                                          lambda_grid = lamb_grid,
                                          lambda_grid_reduced = lamb_grid0)
output_AMR_UM_POEM$value[output_AMR_UM_POEM$term == "pval_hdmm"] # 0.4277
output_AMR_UM_POEM$value[output_AMR_UM_POEM$term == "pval_pe"]   # 0.4277
colnames(M)[attr(output_AMR_UM_POEM, "active_mediators")] # null

## region=='AMR' & income=='High'
X <- matrix(data_gdpr_mat_AMR_High$gdp,ncol=1); dim(X) # 147
Y <- data_lbw_mat_AMR_High$lbw; length(Y) 
M <- data_ghed_AMR_High[,6:ncol(data_ghed_AMR_High)]; dim(M)
S <- matrix(data_ghed_AMR_High[,c("year")], ncol=1); dim(S)
sum(is.na(scale(M))) == 0  # FALSE
## There are some constant columns in M, which becomes NA after scaling 
M <- M[,-which(is.na(scale(M)[1,]))]
dim(M) # 147 by 56
ngrid = 100
lamb_grid = seq(0.1,2,length.out = ngrid) 
lamb_grid0 = seq(0.1,2,length.out = ngrid) 
output_AMR_High_POEM <- pe_mediation_linear(scale(X), Y-mean(Y), scale(M), S,
                                            scale = FALSE,
                                            lambda_grid = lamb_grid,
                                            lambda_grid_reduced = lamb_grid0)
output_AMR_High_POEM$value[output_AMR_High_POEM$term == "pval_hdmm"] # 0.9316
output_AMR_High_POEM$value[output_AMR_High_POEM$term == "pval_pe"]   # 0.9316
colnames(M)[attr(output_AMR_High_POEM, "active_mediators")] # null

## region=='EMR' & income=='Low' -- no samples
unique(data_ghed_EMR_Low$country) 
X <- matrix(data_gdpr_mat_EMR_Low$gdp,ncol=1); dim(X) # 0
Y <- data_lbw_mat_EMR_Low$lbw; length(Y) # 0

## region=='EMR' & income=='Lower-middle' 
unique(data_ghed_EMR_LM$country) 
X <- matrix(data_gdpr_mat_EMR_LM$gdp,ncol=1); dim(X) # 63
Y <- data_lbw_mat_EMR_LM$lbw; length(Y) 
M <- data_ghed_EMR_LM[,6:ncol(data_ghed_EMR_LM)]; dim(M)
S <- matrix(data_ghed_EMR_LM[,c("year")], ncol=1); dim(S)
sum(is.na(scale(M))) == 0  # TRUE
ngrid = 100
lamb_grid = seq(0.1,2,length.out = ngrid) 
lamb_grid0 = seq(0.1,2,length.out = ngrid) 
output_EMR_LM_POEM <- pe_mediation_linear(scale(X), Y-mean(Y), scale(M), S,
                                          scale = FALSE,
                                          lambda_grid = lamb_grid,
                                          lambda_grid_reduced = lamb_grid0)
output_EMR_LM_POEM$value[output_EMR_LM_POEM$term == "pval_hdmm"] # 0.1830
output_EMR_LM_POEM$value[output_EMR_LM_POEM$term == "pval_pe"]   # 0.1830
colnames(M)[attr(output_EMR_LM_POEM, "active_mediators")] # null

## region=='EMR' & income=='Upper-middle'
unique(data_ghed_EMR_UM$country)
X <- matrix(data_gdpr_mat_EMR_UM$gdp,ncol=1); dim(X) # 21
Y <- data_lbw_mat_EMR_UM$lbw; length(Y) # 21
M <- data_ghed_EMR_UM[,6:ncol(data_ghed_EMR_UM)]; dim(M)
S <- matrix(data_ghed_EMR_UM[,c("year")], ncol=1); dim(S)
sum(is.na(scale(M))) == 0  # FALSE
## There are some constant columns in M, which becomes NA after scaling 
M <- M[,-which(is.na(scale(M)[1,]))]
dim(M) # 21 by 56
ngrid = 100
lamb_grid = seq(0.1,2,length.out = ngrid) 
lamb_grid0 = seq(0.1,2,length.out = ngrid) 
output_EMR_UM_POEM <- pe_mediation_linear(scale(X), Y-mean(Y), scale(M), S,
                                          scale = FALSE,
                                          lambda_grid = lamb_grid,
                                          lambda_grid_reduced = lamb_grid0)
output_EMR_UM_POEM$value[output_EMR_UM_POEM$term == "pval_hdmm"] # 0.0055
output_EMR_UM_POEM$value[output_EMR_UM_POEM$term == "pval_pe"]   # 0.0055
colnames(M)[attr(output_EMR_UM_POEM, "active_mediators")] # null

## region=='EMR' & income=='High'
unique(data_ghed_EMR_High$country) 
X <- matrix(data_gdpr_mat_EMR_High$gdp,ncol=1); dim(X) # 42
Y <- data_lbw_mat_EMR_High$lbw; length(Y) # 42
M <- data_ghed_EMR_High[,6:ncol(data_ghed_EMR_High)]; dim(M)
S <- matrix(data_ghed_EMR_High[,c("year")], ncol=1); dim(S)
sum(is.na(scale(M))) == 0 ## FALSE
## There are some constant columns in M, which becomes NA after scaling 
M <- M[,-which(is.na(scale(M)[1,]))]
dim(M) # 42 by 44
ngrid = 100
lamb_grid = seq(0.1,2,length.out = ngrid) 
lamb_grid0 = seq(0.1,2,length.out = ngrid) 
output_EMR_High_POEM <- pe_mediation_linear(scale(X), Y-mean(Y), scale(M), S,
                                            scale = FALSE,
                                            lambda_grid = lamb_grid,
                                            lambda_grid_reduced = lamb_grid0)
output_EMR_High_POEM$value[output_EMR_High_POEM$term == "pval_hdmm"] # 0.0057
output_EMR_High_POEM$value[output_EMR_High_POEM$term == "pval_pe"]   # 0.0057
colnames(M)[attr(output_EMR_High_POEM, "active_mediators")] # null

## region=='EUR' & income=='Low' -- no samples
X <- matrix(data_gdpr_mat_EUR_Low$gdp,ncol=1); dim(X) # 0
Y <- data_lbw_mat_EUR_Low$lbw; length(Y)  

## region=='EUR' & income=='Lower-middle'
X <- matrix(data_gdpr_mat_EUR_LM$gdp,ncol=1); dim(X) # 21
Y <- data_lbw_mat_EUR_LM$lbw; length(Y)  
M <- data_ghed_EUR_LM[,6:ncol(data_ghed_EUR_LM)]; dim(M)
S <- matrix(data_ghed_EUR_LM[,c("year")], ncol=1); dim(S)
sum(is.na(scale(M))) == 0  # FALSE
## There are some constant columns in M, which becomes NA after scaling 
M <- M[,-which(is.na(scale(M)[1,]))]
dim(M) # 21 by 54
ngrid = 100
lamb_grid = seq(0.1,2,length.out = ngrid) 
lamb_grid0 = seq(0.1,2,length.out = ngrid) 
output_EUR_LM_POEM <- pe_mediation_linear(scale(X), Y-mean(Y), scale(M), S,
                                          scale = FALSE,
                                          lambda_grid = lamb_grid,
                                          lambda_grid_reduced = lamb_grid0)
output_EUR_LM_POEM$value[output_EUR_LM_POEM$term == "pval_hdmm"] # 0.0874
output_EUR_LM_POEM$value[output_EUR_LM_POEM$term == "pval_pe"]   # 0.0874
colnames(M)[attr(output_EUR_LM_POEM, "active_mediators")] # null

## region=='EUR' & income=='Upper-middle' -- no samples
X <- matrix(data_gdpr_mat_EUR_UM$gdp,ncol=1); dim(X) # 0
Y <- data_lbw_mat_EUR_UM$lbw; length(Y)  

## region=='EUR' & income=='High' 
X <- matrix(data_gdpr_mat_EUR_High$gdp,ncol=1); dim(X) # 168
Y <- data_lbw_mat_EUR_High$lbw; length(Y)  # 168
M <- data_ghed_EUR_High[,6:ncol(data_ghed_EUR_High)]; dim(M)
S <- matrix(data_ghed_EUR_High[,c("year")], ncol=1); dim(S)
sum(is.na(scale(M))) == 0  # FALSE
## There are some constant columns in M, which becomes NA after scaling 
M <- M[,-which(is.na(scale(M)[1,]))]
dim(M) # 168 by 56
ngrid = 100
lamb_grid = seq(0.1,2,length.out = ngrid) 
lamb_grid0 = seq(0.1,2,length.out = ngrid) 
output_EUR_High_POEM <- pe_mediation_linear(scale(X), Y-mean(Y), scale(M), S,
                                            scale = FALSE,
                                            lambda_grid = lamb_grid,
                                            lambda_grid_reduced = lamb_grid0)
output_EUR_High_POEM$value[output_EUR_High_POEM$term == "pval_hdmm"] # 0.0744
output_EUR_High_POEM$value[output_EUR_High_POEM$term == "pval_pe"]   # 0
colnames(M)[attr(output_EUR_High_POEM, "active_mediators")] # "shi_che" "gghed_usd2021_pc"

## region=='SEAR' & income=='Low' -- no samples
X <- matrix(data_gdpr_mat_SEAR_Low$gdp,ncol=1); dim(X) # 0
Y <- data_lbw_mat_SEAR_Low$lbw; length(Y)  # 0

## region=='SEAR' & income=='Lower-middle' 
X <- matrix(data_gdpr_mat_SEAR_LM$gdp,ncol=1); dim(X) # 63
Y <- data_lbw_mat_SEAR_LM$lbw; length(Y) 
M <- data_ghed_SEAR_LM[,6:ncol(data_ghed_SEAR_LM)]; dim(M)
S <- matrix(data_ghed_SEAR_LM[,c("year")], ncol=1); dim(S)
sum(is.na(scale(M))) == 0  # FALSE
## There are some constant columns in M, which becomes NA after scaling 
M <- M[,-which(is.na(scale(M)[1,]))]
dim(M) # 63 by 56
ngrid = 100
lamb_grid = seq(0.1,2,length.out = ngrid) 
lamb_grid0 = seq(0.1,2,length.out = ngrid) 
output_SEAR_LM_POEM <- pe_mediation_linear(scale(X), Y-mean(Y), scale(M), S,
                                           scale = FALSE,
                                           lambda_grid = lamb_grid,
                                           lambda_grid_reduced = lamb_grid0)
output_SEAR_LM_POEM$value[output_SEAR_LM_POEM$term == "pval_hdmm"] # 0.2666
output_SEAR_LM_POEM$value[output_SEAR_LM_POEM$term == "pval_pe"]   # 0
colnames(M)[attr(output_SEAR_LM_POEM, "active_mediators")] # "oops_che"  "gghed_gge"

## region=='SEAR' & income=='Upper-middle'
X <- matrix(data_gdpr_mat_SEAR_UM$gdp,ncol=1); dim(X) # 42
Y <- data_lbw_mat_SEAR_UM$lbw; length(Y) 
M <- data_ghed_SEAR_UM[,6:ncol(data_ghed_SEAR_UM)]; dim(M)
S <- matrix(data_ghed_SEAR_UM[,c("year")], ncol=1); dim(S)
sum(is.na(scale(M))) == 0  # TRUE
ngrid = 100
lamb_grid = seq(0.1,2,length.out = ngrid) 
lamb_grid0 = seq(0.1,2,length.out = ngrid) 
output_SEAR_UM_POEM <- pe_mediation_linear(scale(X), Y-mean(Y), scale(M), S,
                                           scale = FALSE,
                                           lambda_grid = lamb_grid,
                                           lambda_grid_reduced = lamb_grid0)
output_SEAR_UM_POEM$value[output_SEAR_UM_POEM$term == "pval_hdmm"] # 0.4435
output_SEAR_UM_POEM$value[output_SEAR_UM_POEM$term == "pval_pe"]   # 0.4435
colnames(M)[attr(output_SEAR_UM_POEM, "active_mediators")] # null

## region=='SEAR' & income=='High' -- no samples
X <- matrix(data_gdpr_mat_SEAR_High$gdp,ncol=1); dim(X) # 0
Y <- data_lbw_mat_SEAR_High$lbw; length(Y)  

## region=='WPR' & income=='Low' -- no samples
X <- matrix(data_gdpr_mat_WPR_Low$gdp,ncol=1); dim(X) # 0
Y <- data_lbw_mat_WPR_Low$lbw; length(Y)  # 0

## region=='WPR' & income=='Lower-middle'
X <- matrix(data_gdpr_mat_WPR_LM$gdp,ncol=1); dim(X) # 42
Y <- data_lbw_mat_WPR_LM$lbw; length(Y)
M <- data_ghed_WPR_LM[,6:ncol(data_ghed_WPR_LM)]; dim(M)
S <- matrix(data_ghed_WPR_LM[,c("year")], ncol=1); dim(S)
sum(is.na(scale(M))) == 0  # FALSE
## There are some constant columns in M, which becomes NA after scaling 
M <- M[,-which(is.na(scale(M)[1,]))]
dim(M) # 42 by 56
ngrid = 100
lamb_grid = seq(0.1,2,length.out = ngrid) 
lamb_grid0 = seq(0.1,2,length.out = ngrid) 
output_WPR_LM_POEM <- pe_mediation_linear(scale(X), Y-mean(Y), scale(M), S,
                                          scale = FALSE,
                                          lambda_grid = lamb_grid,
                                          lambda_grid_reduced = lamb_grid0)
output_WPR_LM_POEM$value[output_WPR_LM_POEM$term == "pval_hdmm"] # 0.0018
output_WPR_LM_POEM$value[output_WPR_LM_POEM$term == "pval_pe"]   # 0
colnames(M)[attr(output_WPR_LM_POEM, "active_mediators")] # "gfa_che"        "che_ncu2021_pc" "ext_ncu2021_pc"

## region=='WPR' & income=='Upper-middle'
X <- matrix(data_gdpr_mat_WPR_UM$gdp,ncol=1); dim(X) # 42
Y <- data_lbw_mat_WPR_UM$lbw; length(Y) 
M <- data_ghed_WPR_UM[,6:ncol(data_ghed_WPR_UM)]; dim(M)
S <- matrix(data_ghed_WPR_UM[,c("year")], ncol=1); dim(S)
sum(is.na(scale(M))) == 0  # FALSE
## There are some constant columns in M, which becomes NA after scaling 
M <- M[,-which(is.na(scale(M)[1,]))]
dim(M) # 42 by 56
ngrid = 100
lamb_grid = seq(0.1,2,length.out = ngrid) 
lamb_grid0 = seq(0.1,2,length.out = ngrid) 
output_WPR_UM_POEM <- pe_mediation_linear(scale(X), Y-mean(Y), scale(M), S,
                                          scale = FALSE,
                                          lambda_grid = lamb_grid,
                                          lambda_grid_reduced = lamb_grid0)
output_WPR_UM_POEM$value[output_WPR_UM_POEM$term == "pval_hdmm"] # 0.1373
output_WPR_UM_POEM$value[output_WPR_UM_POEM$term == "pval_pe"]   # 0.1373
colnames(M)[attr(output_WPR_UM_POEM, "active_mediators")] # null

## region=='WPR' & income=='High'
X <- matrix(data_gdpr_mat_WPR_High$gdp,ncol=1); dim(X) # 63
Y <- data_lbw_mat_WPR_High$lbw; length(Y) 
M <- data_ghed_WPR_High[,6:ncol(data_ghed_WPR_High)]; dim(M)
S <- matrix(data_ghed_WPR_High[,c("year")], ncol=1); dim(S)
sum(is.na(scale(M))) == 0  # FALSE
## There are some constant columns in M, which becomes NA after scaling 
M <- M[,-which(is.na(scale(M)[1,]))]
dim(M) # 63 by 45
# Error in solve.default(t(MS) %*% MS) : system is computationally singular
## Soln: "cfa_che" and "vfa_che" are collinear; we remove "vfa_che"
scale(M)[,c("cfa_che", "vfa_che")]
colnames(M)[20]
M <- M[,-20]
dim(M) # 63 by 44
ngrid = 100
lamb_grid = seq(0.1,2,length.out = ngrid) 
lamb_grid0 = seq(0.1,2,length.out = ngrid) 
output_WPR_High_POEM <- pe_mediation_linear(scale(X), Y-mean(Y), scale(M), S,
                                            scale = FALSE,
                                            lambda_grid = lamb_grid,
                                            lambda_grid_reduced = lamb_grid0)
output_WPR_High_POEM$value[output_WPR_High_POEM$term == "pval_hdmm"] # 5.4e-09
output_WPR_High_POEM$value[output_WPR_High_POEM$term == "pval_pe"]   # 0
colnames(M)[attr(output_WPR_High_POEM, "active_mediators")] # "pvtd"    "cfa_che"

###############################################################################
