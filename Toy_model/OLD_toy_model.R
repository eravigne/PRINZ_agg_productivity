

# Summary


True sectoral productivity growth

Primal
```{r}
primal_tfp_sector
```
Dual
```{r}
dual_tfp_sector
```

Framework approximation
```{r}
approx_aihat
```


## Aggregate productivity

```{r}
agg_c<-
  data.frame("True value"=agg_prod,
             "Primal"=agg_tfp_primal,
             "Dual"=agg_tfp_dual)
agg_c
```


```{r}
agg_zeta_c<-
  rbind(  
    # Framework
    c(method="Framework",zeta="zeta",hat_aggtfp=approximate_aihat(zeta)%*% matrix(DW_t0,ncol=1)),
    c(method="Framework",zeta="0",hat_aggtfp=approximate_aihat(c(0,0,0))%*% matrix(DW_t0,ncol=1)),
    c(method="Framework",zeta="0.129",hat_aggtfp=approximate_aihat(c(0.129,0.129,0.129))%*% matrix(DW_t0,ncol=1)),
    
    # CCC (non processed)
    c(method="CCC (non processed cost)",zeta="zeta",hat_aggtfp=approx_aihat_ccc_np_zeta),
    c(method="CCC (non processed cost)",zeta="0",hat_aggtfp=approx_aihat_ccc_np_0),
    c(method="CCC (non processed cost)",zeta="0.129",hat_aggtfp=approx_aihat_ccc_np_0129),
    
    # CCC (opt 1 processed)
    c(method="CCC (processed cost opt 1)",zeta="zeta",hat_aggtfp=approx_aihat_ccc_opt1_zeta),
    c(method="CCC (processed cost opt 1)",zeta="0",hat_aggtfp=approx_aihat_ccc_opt1_0),
    c(method="CCC (processed cost opt 1)",zeta="0.129",hat_aggtfp=approx_aihat_ccc_opt1_0129),
    
    # CCC (opt 2 processed)
    c(method="CCC (processed cost opt 2)",zeta="zeta",hat_aggtfp=approx_aihat_ccc_opt2_zeta),
    c(method="CCC (processed cost opt 2)",zeta="0",hat_aggtfp=approx_aihat_ccc_opt2_0),
    c(method="CCC (processed cost opt 2)",zeta="0.129",hat_aggtfp=approx_aihat_ccc_opt2_0129)
    
  )
data.frame(agg_zeta_c)%>%
  arrange(zeta)%>%
  mutate(hat_aggtfp=round(as.numeric(hat_aggtfp),3))
```




# OLD CCC framework


```{r}


approx_glog_price<-(totalcost_t1-totalcost_t0)/(sales_t0)

approximate_aihat(zeta_c=zeta,gprice_c =approx_glog_price )
approximate_aihat(zeta_c=c(0,0,0),gprice_c =approx_glog_price)
approximate_aihat(zeta_c=c(0.129,0.129,0.129),gprice_c =approx_glog_price)
```
Domar aggregation
```{r}
approx_aihat_ccc_np_zeta<-
  approximate_aihat(zeta_c=zeta,gprice_c =approx_glog_price )%*% matrix(DW_t0,ncol=1)
approx_aihat_ccc_np_0<-
  approximate_aihat(zeta_c=c(0,0,0),gprice_c =approx_glog_price)%*% matrix(DW_t0,ncol=1)
approx_aihat_ccc_np_0129<-
  approximate_aihat(zeta_c=c(0.129,0.129,0.129),gprice_c =approx_glog_price)%*% matrix(DW_t0,ncol=1)
```



```{r}
# transport extra elec cost are set to 0
transport_extracost$elec<-0
# transport extra fossil cost are set to 0
transport_extracost$fossil<-0

extra_cost_c<-c(sum(elec_extracost),sum(unlist(transport_extracost)),sum(fossil_extracost))

approx_glog_price<-extra_cost_c/(sales_t0)

approximate_aihat(zeta_c=zeta,gprice_c =approx_glog_price )
approximate_aihat(zeta_c=c(0,0,0),gprice_c =approx_glog_price)
approximate_aihat(zeta_c=c(0.129,0.129,0.129),gprice_c =approx_glog_price)
```
Domar aggregation
```{r}
approx_aihat_ccc_opt1_zeta<-
  approximate_aihat(zeta_c=zeta,gprice_c =approx_glog_price )%*% matrix(DW_t0,ncol=1)
approx_aihat_ccc_opt1_0<-
  approximate_aihat(zeta_c=c(0,0,0),gprice_c =approx_glog_price)%*% matrix(DW_t0,ncol=1)
approx_aihat_ccc_opt1_0129<-
  approximate_aihat(zeta_c=c(0.129,0.129,0.129),gprice_c =approx_glog_price)%*% matrix(DW_t0,ncol=1)
```


```{r}
# reinitialise extracost
read_scenario(scenario_name)
# transport extra elec cost are set to 0
transport_extracost$elec<-0
# fossil extra labour cost are set to 0
fossil_extracost$labour<-as.list(fossil_extracost)$labour+as.list(transport_extracost)$fossil

extra_cost_c<-c(sum(elec_extracost),sum(unlist(transport_extracost)),sum(unlist(fossil_extracost)))

approx_glog_price<-extra_cost_c/(sales_t0)

approximate_aihat(zeta_c=zeta,gprice_c =approx_glog_price )
approximate_aihat(zeta_c=c(0,0,0),gprice_c =approx_glog_price)
approximate_aihat(zeta_c=c(0.129,0.129,0.129),gprice_c =approx_glog_price)
```
Domar aggregation
```{r}
approx_aihat_ccc_opt2_zeta<-
  approximate_aihat(zeta_c=zeta,gprice_c =approx_glog_price )%*% matrix(DW_t0,ncol=1)
approx_aihat_ccc_opt2_0<-
  approximate_aihat(zeta_c=c(0,0,0),gprice_c =approx_glog_price)%*% matrix(DW_t0,ncol=1)
approx_aihat_ccc_opt2_0129<-
  approximate_aihat(zeta_c=c(0.129,0.129,0.129),gprice_c =approx_glog_price)%*% matrix(DW_t0,ncol=1)
```
Option 1 and 2 give the same result.



# OLD Checks

Is the price at t=0 equal to the unit cost at t=0
```{r}
price_t0[1:3]==unitcost_t0
price_t1[1:3]==unitcost_t1
```


Is the total production is equal to total demand (intermediate + final)
```{r}
final_demand_t1+intermediate_demand_t1==output_t1
final_demand_t0+intermediate_demand_t0==output_t0


```

Total cost = total sales (ensuring profit=0)
```{r}
totalcost_t0==sales_t0
totalcost_t1==sales_t1
```
Each row of the input output matrix (including final demand) sums to total output
```{r}
rowSums(cbind(IOM_t0,final_demand_t0))==output_t0
rowSums(cbind(IOM_t1,final_demand_t1))==output_t1
```

Each column in aij sums to 1 (the factor shares sum to 1)

```{r}
colSums(aij_t0)==1
colSums(aij_t1)==1

```

Check factor share sum to 1
```{r}
# per sector
colSums(factor_share_t0)==1
colSums(factor_share_t1)==1

# total
sum(factor_totshare_t0)==1
sum(factor_totshare_t1)==1

```

GDP consistency

```{r}
GDP_income_t0==GDP_VA_t0
GDP_income_t0==GDP_FD_t0
GDP_VA_t0==GDP_FD_t0

# t1
GDP_income_t1==GDP_VA_t1
GDP_income_t1==GDP_FD_t1
GDP_VA_t1==GDP_FD_t1

```




Constant factor shares across time

```{r}
aij_t0==aij_t1
```




# Functions

Total cost
```{r}
compute_totalcost<-function(sector,time){
  df<-get(paste0(sector,"_cost_t",time))
  total_cost<-sum(df)
  return(total_cost)
}
```


Compute unit cost
```{r}
compute_unit_cost<-function(cost,output){
  uc=cost/output
  return(uc)
}
```


intermediate demand
```{r}

compute_total_intermediate_demand<-function(t){
  c_tot<-c()
  
  for (i in cs){
    tot=0
    
    price<-get(paste0("price_t",t))[i]
    
    for (s in cs){
      cost<-get(paste0(s,"_cost_t",t))[i]
      tot=
        tot+ cost/price
    }
    c_tot<-c(c_tot,tot)
  }
  return(c_tot)
}
```


```{r}
compute_total_factor_demand<-function(t){
  c_tot<-c()
  for (i in fs){
    tot=0
    
    price<-get(paste0("price_t",t))[i]
    
    for (s in cs){
      cost<-get(paste0(s,"_cost_t",t))[i]
      tot=
        tot+ cost/price
    }
    c_tot<-c(c_tot,tot)
  }
  return(c_tot)
}
```


# Build the cost data frame

We have $C(t=0)$ cost, and the $\Delta C$ cost between 0 and 1


Computing $C(t=1)$
  
  ```{r}

# cost t=1
elec_cost_t1<-elec_extracost+elec_cost_t0
transport_cost_t1<-transport_extracost+transport_cost_t0
fossil_cost_t1<-fossil_extracost+fossil_cost_t0
```


Compute total cost for all sector and times
```{r}
# for( t in ct){
#   for (s in cs){
#     assign(paste0(s,"_totalcost_t",t),compute_totalcost(s,t))
#   }
# }

t=0
totalcost_t0<-c(compute_totalcost("elec",t),
                compute_totalcost("transport",t),
                compute_totalcost("fossil",t))
t=1
totalcost_t1<-c(compute_totalcost("elec",t),
                compute_totalcost("transport",t),
                compute_totalcost("fossil",t))
```


compute unit cost

```{r}
unitcost_t0<-
  totalcost_t0/output_t0

unitcost_t1<-
  totalcost_t1/output_t1
```

Price (= unit cost)
```{r}
price_t1=c(unitcost_t1,c("capex"=1,"labour"=1))
```



Total demand of each good
```{r}


intermediate_demand_t0<-compute_total_intermediate_demand(0)
intermediate_demand_t1<-compute_total_intermediate_demand(1)

```


Final demand is the residual of production - demand (in quantity)
```{r}
final_demand_t0<-
  output_t0-
  intermediate_demand_t0

final_demand_t1<-
  output_t1-
  intermediate_demand_t1
```


final_demand (monetary)
```{r}
final_demand_pQ_t0<-final_demand_t0*price_t0[1:3]
final_demand_pQ_t1<-final_demand_t1*price_t1[1:3]

```


Sales
```{r}
sales_t0<-
  output_t0*price_t0[1:3]
sales_t1<-
  output_t1*price_t1[1:3]

```

Input-output matrix
input requirement : incl K and L
each line is the quantity requirement of each sector
IOM : only intermediate inputs
I_ij=[quantity of input sold by i to j]
```{r,warning=F}
input_requirement_t0<-
  matrix(c((elec_cost_t0/price_t0),
           (transport_cost_t0/price_t0),
           (fossil_cost_t0/price_t0)),
         nrow=5,ncol=3)
IOM_t0<-input_requirement_t0[1:3,1:3]

input_requirement_t1<-
  matrix(c((elec_cost_t1/price_t1),
           (transport_cost_t1/price_t1),
           (fossil_cost_t1/price_t1)),
         nrow=5,ncol=3)
IOM_t1<-input_requirement_t1[1:3,1:3]

```


Xij matrix
transposition of the input requirement matrix
xij: quantity of j used in i

Mij : monetary flows between  sectors (including factors)


Aij matrix:
  aij is the share of i in j's total expenses

\[a_{ji}	=\frac{p_j \times X_{ij}}{X_i\times p_i}\]

```{r}
Xij_t0<-  t(input_requirement_t0)
Xij_t1<-  t(input_requirement_t1)
```



Mij : monetary flows between  sectors (including factors)
```{r}
# Multiply each column by the corresponding element in price
Mij_t0 <- sweep(Xij_t0, 2, matrix(price_t0,ncol=1), FUN = "*")
Mij_t1 <- sweep(Xij_t1, 2, price_t1, FUN = "*")
```


Aij matrix:
aij is the share of i in j's total expenses

\[a_{ji}	=\frac{p_j \times X_{ij}}{X_i\times p_i}\]

```{r}
# Each column of the transposed monetary matrix (each column j is now a sector) is divided by the total expenses of the sector j
aij_t0<-sweep(t(Mij_t0), 2, totalcost_t0, FUN = "/")
aij_t1<-sweep(t(Mij_t1), 2, totalcost_t1, FUN = "/")

# average between the two periods

aij_avg<-(aij_t0+aij_t1)/2
```

Share of final demand
```{r}
theta_t0<-final_demand_pQ_t0/sum(final_demand_pQ_t0)
theta_t1<-final_demand_pQ_t1/sum(final_demand_pQ_t1)
```

Total quantity of Labour and Capital
```{r}
KL_t0<-compute_total_factor_demand(0)
KL_t1<-compute_total_factor_demand(1)

```


Factor share
col: elec, transport, fossil
row: capital, labour
```{r}
factor_sum_t0<-colSums(t(Mij_t0[1:3,4:5]))
factor_share_t0<-
  sweep(t(Mij_t0[1:3,4:5]), 2, factor_sum_t0, FUN = "/")

factor_sum_t1<-colSums(t(Mij_t1[1:3,4:5]))
factor_share_t1<-
  sweep(t(Mij_t1[1:3,4:5]), 2, factor_sum_t1, FUN = "/")

factor_share_avg<-(factor_share_t0+factor_share_t1)/2

```

total factor share
```{r}
factor_totshare_t0<-KL_t0*price_t0[4:5]/sum(KL_t0*price_t0[4:5])
factor_totshare_t1<-KL_t1*price_t1[4:5]/sum(KL_t1*price_t1[4:5])

factor_totshare_avg<-(factor_totshare_t0+factor_totshare_t1)/2

```



GDP
Three approaches
Income
```{r}
GDP_income_t0<-sum(KL_t0*price_t0[4:5])
GDP_income_t1<-sum(KL_t1*price_t1[4:5])

```

Value added

```{r}
GDP_VA_t0<-sum(output_t0*price_t0[1:3]-colSums(Mij_t0)[1:3])
GDP_VA_t1<-sum(output_t1*price_t1[1:3]-colSums(Mij_t1)[1:3])

```

Final Consumption

```{r}
GDP_FD_t0<-sum(final_demand_pQ_t0)
GDP_FD_t1<-sum(final_demand_pQ_t1)

```


Domar weights
```{r}
DW_t0<-sales_t0/GDP_FD_t0
DW_t1<-sales_t1/GDP_FD_t1
DW_avg<-(DW_t1+DW_t0)/2

```


# Productivity

Primal


for elec
```{r}
glog_output<-log(output_t1)-log(output_t0)

glog_Xij<-log(Xij_t1)-log(Xij_t0)
glog_Xij[is.na(glog_Xij)] <- 0

glog_Xij[3,]

sum_diag_terms<-(t(aij_t0)%*%matrix(glog_Xij[3,],ncol=1))[3]

glog_output[3]-sum_diag_terms
```


```{r}
glog_output<-log(output_t1)-log(output_t0)

glog_Xij<-log(Xij_t1)-log(Xij_t0)
glog_Xij[is.na(glog_Xij)] <- 0

diag_terms<-diag( t(aij_t0)%*%t(glog_Xij)  )
# The terms we want are the diagonal terms


primal_tfp_sector<-glog_output-diag_terms[1:3]
primal_tfp_sector
```



```{r}
glog_price<-log(price_t1)-log(price_t0)
glog_price[is.na(glog_price)] <- 0

diag_terms<- t(aij_t0)%*%matrix(glog_price,ncol=1)
# The terms we want are the diagonal terms


dual_tfp_sector<- -(glog_price[1:3]-diag_terms[1:3])
dual_tfp_sector
```


Domar aggregation
```{r}
agg_tfp_primal<-primal_tfp_sector%*% matrix(DW_t0,ncol=1)
agg_tfp_primal
agg_tfp_dual<-dual_tfp_sector%*% matrix(DW_t0,ncol=1)
agg_tfp_dual
```
Aggregate productivity growth

\[\hat{A} \equiv \underbrace{\sum_{i=1}^N \frac{p_iC_i}{pY} \hat{C}_i}_{\text{growth of real GDP}} - \underbrace{\sum_{f=1}^F \frac{w_f L_f}{pY} \hat{L}_f.}_{\text{growth of volume of factors}}\]

```{r}
glog_final_demand<-log(final_demand_t1)-log(final_demand_t0)
glog_final_demand[is.na(glog_final_demand)] <- 0

term_1<-matrix(final_demand_pQ_t0/GDP_FD_t0,nrow=1) %*% matrix(glog_final_demand,ncol=1)

glog_factor<-log(KL_t1)-log(KL_t0)

term_2<-matrix(factor_totshare_t0,nrow=1) %*% matrix(glog_factor,ncol=1)


agg_prod<-term_1-term_2
```
# Productivity approximation

Introducing the notation
\[
  \hat{\Sigma}_{X_i} \equiv \sum_{j=1}^N  a_{ji} \hat{X}_{ij} + \sum_{f=1}^F  \tilde{l}_{fi}\hat{L}_{fi} 
  \]
for the growth of the index of inputs of $i$, and
\[
  \hat{\Sigma}_{p_i} \equiv \sum_{j=1}^N  w_j \hat{p}_j + \sum_{f=1}^F \tilde{l}_{fi} \hat{w}_f
  \]


```{r}
sigma_x=diag( t(aij_t0)%*%t(glog_Xij)  )

sigma_p=c(t(aij_t0)%*%matrix(glog_price,ncol=1))
```
Check that it 
\[
  \hat{A}_i = \hat{X}_i - \hat{\Sigma}_{X_i} = \hat{\Sigma}_{p_i} - \hat{p}_i\]
```{r}
glog_xi<-log(output_t1)-log(output_t0)
glog_xi[is.na(glog_xi)] <- 0

term1=glog_xi-sigma_x
term2=sigma_p-glog_price[1:3]

term1/term2-1<10^-12
# for whatever reason, it is not numerically equal due to computionnal issues
```



```{r}
glog_sales<-log(sales_t1)-log(sales_t0)

sum_xhati_phat_i<-glog_xi+glog_price[1:3]

sum_sigma<-sigma_x+sigma_p

glog_sales/sum_xhati_phat_i-1<10^-12
sum_sigma/sum_xhati_phat_i-1<10^-12
sum_sigma/glog_sales-1<10^-12

```
Compute actual zeta
\[\zeta = \frac{\hat{\Sigma}_{p_i}}{\hat{\Sigma}_{p_i} + \hat{\Sigma}_{X_i}}.\]
```{r}
zeta=sigma_p/(sigma_p+sigma_x)
zeta
```


```{r}
approximate_aihat<-function(zeta_c,gx_c=glog_xi,gprice_c=glog_price){
  
  aihat= zeta_c*gx_c+(zeta_c-1)*gprice_c[1:3]
  return(aihat)
}
```

Test with 3 values of zeta

- true value (computed on sigmas)
- 0
- 0.126 (derived from KLEMS data)

```{r}
approx_aihat<-rbind(
  data.frame(sector=cs,zeta=zeta,aihat=approximate_aihat(zeta),value_zeta="zeta"),
  data.frame(sector=cs,zeta=0,aihat=approximate_aihat(c(0,0,0)),value_zeta="0"),
  data.frame(sector=cs,zeta=0.129,aihat=approximate_aihat(c(0.129,0.129,0.129)),value_zeta="0.129"))

rownames(approx_aihat) <- NULL
```
Okayist approximation

NB: in the cost factor share scenario, we have quite a gap between the values for the transport sector, but that's also because the changes are quite big (hence zeta is $\approx$ -1)

Domar aggregation of the approximations

```{r}
approximate_aihat(zeta)%*% matrix(DW_t0,ncol=1)

approximate_aihat(c(0,0,0))%*% matrix(DW_t0,ncol=1)
approximate_aihat(c(0.129,0.129,0.129))%*% matrix(DW_t0,ncol=1)
```

## Old functions model


```{r}
# Functions

## Compute Total Cost
compute_totalcost <- function(sector, time) {
  df <- get(paste0(sector, "_cost_t", time))
  sum(df)
}
```


```{r}
## Compute Unit Cost
compute_unit_cost <- function(cost, output) {
  cost / output
}
```


```{r}
## Compute Total Intermediate or Factor Demand
compute_total_demand <- function(type, t) {
  sectors <- if (type == "intermediate") cs else fs
  sapply(sectors, function(i) {
    price <- get(paste0("price_t", t))[i]
    sum(sapply(cs, function(s) {
      cost <- get(paste0(s, "_cost_t", t))[i]
      cost / price
    }))
  })
}
```


We have $C(t=0)$ cost, and the $\Delta C$ cost between 0 and 1
Computing $C(t=1)$
```{r}
## Build Cost Data Frame
compute_cost_t1 <- function(base_cost, extra_cost) {
  base_cost + extra_cost
}

elec_cost_t1 <- compute_cost_t1(elec_cost_t0, elec_extracost)
transport_cost_t1 <- compute_cost_t1(transport_cost_t0, transport_extracost)
fossil_cost_t1 <- compute_cost_t1(fossil_cost_t0, fossil_extracost)
```


```{r}
## Compute Total Cost for All Sectors
compute_total_costs <- function(time) {
  sapply(c("elec", "transport", "fossil"), function(s) compute_totalcost(s, time))
}

totalcost_t0 <- compute_total_costs(0)
totalcost_t1 <- compute_total_costs(1)
```

Price (= unit cost)
```{r}
## Compute Unit Cost
unitcost_t0 <- compute_unit_cost(totalcost_t0, output_t0)
unitcost_t1 <- compute_unit_cost(totalcost_t1, output_t1)

## Compute Prices
price_t1 <- c(unitcost_t1, c("capex" = 1, "labour" = 1))
```


```{r}
## Compute Total Demand
intermediate_demand_t0 <- compute_total_demand("intermediate", 0)
intermediate_demand_t1 <- compute_total_demand("intermediate", 1)
```


```{r}
## Compute Final Demand
compute_final_demand <- function(output, intermediate_demand) {
  output - intermediate_demand
}

final_demand_t0 <- compute_final_demand(output_t0, intermediate_demand_t0)
final_demand_t1 <- compute_final_demand(output_t1, intermediate_demand_t1)
```

Final demand is the residual of production - demand (in quantity)
```{r}
## Compute Final Demand (Monetary)
final_demand_pQ_t0 <- final_demand_t0 * price_t0[1:3]
final_demand_pQ_t1 <- final_demand_t1 * price_t1[1:3]
```


```{r}
## Compute Sales
sales_t0 <- output_t0 * price_t0[1:3]
sales_t1 <- output_t1 * price_t1[1:3]
```


input requirement : incl K and L
each line is the quantity requirement of each sector
IOM : only intermediate inputs
I_ij=[quantity of input sold by i to j]
```{r}
# Input-Output Matrix
## Compute Input Requirement and IO Matrices
compute_input_matrix <- function(cost_t, price_t) {
  input_requirement <- matrix(c(
    (elec_cost_t / price_t),
    (transport_cost_t / price_t),
    (fossil_cost_t / price_t)
  ), nrow = 5, ncol = 3)
  list(
    input_requirement = input_requirement,
    IOM = input_requirement[1:3, 1:3]
  )
}

iom_t0 <- compute_input_matrix(c(elec_cost_t0, transport_cost_t0, fossil_cost_t0), price_t0)
iom_t1 <- compute_input_matrix(c(elec_cost_t1, transport_cost_t1, fossil_cost_t1), price_t1)
```




Xij matrix
transposition of the input requirement matrix
xij: quantity of j used in i



```{r}
# Xij and Mij Matrices
Xij_t0 <- t(iom_t0$input_requirement)
Xij_t1 <- t(iom_t1$input_requirement)
```

Mij : monetary flows between  sectors (including factors)


```{r}
Mij_t0 <- sweep(Xij_t0, 2, price_t0, FUN = "*")
Mij_t1 <- sweep(Xij_t1, 2, price_t1, FUN = "*")
```



Aij matrix:
aij is the share of i in j's total expenses

\[a_{ji}	=\frac{p_j \times X_{ij}}{X_i\times p_i}\]
```{r}
# Aij Matrix
compute_aij_matrix <- function(Mij, totalcost) {
  sweep(t(Mij), 2, totalcost, FUN = "/")
}

aij_t0 <- compute_aij_matrix(Mij_t0, totalcost_t0)
aij_t1 <- compute_aij_matrix(Mij_t1, totalcost_t1)
aij_avg <- (aij_t0 + aij_t1) / 2
```

Factor share
col: elec, transport, fossil
row: capital, labour

```{r}
# Factor Shares
compute_factor_shares <- function(Mij) {
  factor_sum <- colSums(t(Mij[1:3, 4:5]))
  sweep(t(Mij[1:3, 4:5]), 2, factor_sum, FUN = "/")
}

factor_share_t0 <- compute_factor_shares(Mij_t0)
factor_share_t1 <- compute_factor_shares(Mij_t1)
factor_share_avg <- (factor_share_t0 + factor_share_t1) / 2
```


```{r}
# GDP
## Income, Value Added, and Final Demand
GDP_income_t0 <- sum(KL_t0 * price_t0[4:5])
GDP_income_t1 <- sum(KL_t1 * price_t1[4:5])

GDP_VA_t0 <- sum(output_t0 * price_t0[1:3] - colSums(Mij_t0)[1:3])
GDP_VA_t1 <- sum(output_t1 * price_t1[1:3] - colSums(Mij_t1)[1:3])

GDP_FD_t0 <- sum(final_demand_pQ_t0)
GDP_FD_t1 <- sum(final_demand_pQ_t1)
```


```{r}
# Domar Weights
compute_DW <- function(sales, GDP_FD) {
  sales / GDP_FD
}

DW_t0 <- compute_DW(sales_t0, GDP_FD_t0)
DW_t1 <- compute_DW(sales_t1, GDP_FD_t1)
DW_avg <- (DW_t0 + DW_t1) / 2

```
