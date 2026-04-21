


# interpolate_trajectory --------------------------------------------------



interpolate_trajectory <- function(data,
                                   first_date_available=0,
                                   # could be 0 for q, but not for prices, or the interpolation will start at 0.
                                   first_available_data=0,
                                   var, 
                                   interpolation_type = "linear") {
  # Filter the data for the specified techno and scenario
  subset_data <- data %>% 
    mutate(Year=as.numeric(Year))%>%
    rename("var"=var)%>%
    ungroup()
  
  # Extract starting date and quantity
  
  
  # var <- subset_data%>%select(var)
  var<-subset_data$var
  
  # Create a sequence of years between the minimum and maximum years
  all_years <- data.frame("Year"=seq(min(subset_data$Year), max(subset_data$Year), by = 1))
  
  # Create a data frame with all_years and NA for var
  interpolated_data <- 
    all_years%>%
    left_join(subset_data%>%select(Year,var),by="Year")%>%
    mutate(var=ifelse(Year<first_date_available & first_date_available>0,first_available_data,var))
  
  
  
  non_missing_indices <- which(!is.na(interpolated_data$var))
  non_missing_year <- interpolated_data$Year[non_missing_indices]
  non_missing_values <- interpolated_data$var[non_missing_indices]
  missing_indices <- which(is.na(interpolated_data$var))
  
  n_non_missing=length(non_missing_indices)  
  
  # Linear interpolation
  if (interpolation_type == "linear" || (n_non_missing==2)) {
    interpolated_values <- approx(x = non_missing_indices, y = non_missing_values, xout = missing_indices)$y
    interpolated_data$var[missing_indices] <- interpolated_values
  }
  # Polynomial interpolation of degree 2
  if((interpolation_type == "polynomial" & n_non_missing>=3) || (interpolation_type %in% c("spline","automatic_poly")& n_non_missing==3)) {
    poly_model <- lm(non_missing_values ~ poly(non_missing_indices, 2))
    
    # Predict missing values using the polynomial model
    interpolated_values <- predict(poly_model, newdata = data.frame(non_missing_indices = missing_indices))
    
    interpolated_data$var[missing_indices] <- interpolated_values
    
  }
  # Polynomial interpolation of degree 2
  if((interpolation_type == "spline" & length(non_missing_indices)>3)) {
    poly_model <- lm(non_missing_values ~ poly(non_missing_indices, 3))
    
    # Predict missing values using the polynomial model
    interpolated_values <- predict(poly_model, newdata = data.frame(non_missing_indices = missing_indices))
    
    interpolated_data$var[missing_indices] <- interpolated_values
    
  }
  if((interpolation_type == "automatic_poly" & length(non_missing_indices)>3)) {
    interpolated_values <- signal::pchip(non_missing_year,non_missing_values,interpolated_data$Year)
    
    
    interpolated_data$var <- interpolated_values
    
  }
  
  
  return(interpolated_data)
}



# get_all_scenarios -------------------------------------------------------


get_all_scenarios<-function(df_price,df_quantity,mix_scenario_pq=T){
  
  scenario_p=unique(unique(df_price$Scenario))
  scenario_q=unique(unique(df_quantity$Scenario))
  
  price_scenario_tco=unique(unique(df_price$price_scenario_tco))
  carbon_price<-paste(paste(unique(df_price$Year),unique(df_price$carbon_price),sep="-"),collapse = ' / ')
  price_s=unique(unique(df_price$price_scenario))
  tco=unique(unique(df_price$carbon_price))
  interpolation_type_q=unique(unique(df_quantity$interpolation_type))
  interpolation_type_p=unique(unique(df_price$interpolation_type))
  
  if(mix_scenario_pq==T){
    combinations_df <- expand.grid(scenario_p,scenario_q, price_s,tco, interpolation_type_q,interpolation_type_p)
    colnames(combinations_df)<-c("scenario_p","scenario_q","price_s","tco","interpolation_type_q","interpolation_type_p")}
  if(mix_scenario_pq==F){
    combinations_df <- expand.grid(scenario_p, price_s,tco, interpolation_type_q,interpolation_type_p)
    colnames(combinations_df)<-c("scenario_p","price_s","tco","interpolation_type_q","interpolation_type_p")
    combinations_df$scenario_q=combinations_df$scenario_p
    combinations_df<-combinations_df%>%select(scenario_p,scenario_q,price_s,tco,interpolation_type_q,interpolation_type_p)}
  return(combinations_df)
}


# f_aggregate_techno ------------------------------------------------------


f_aggregate_techno<-function(df_price,df_quantity){
  df_price<-
    df_price%>%
    select(Year,technology,price)
  
  df_quantity<-
    df_quantity%>%
    select(Year,technology,quantity)
  
  df_agg<-
    df_quantity%>%
    left_join(df_price,by=c("Year","technology"))%>%
    group_by(Year)%>%
    summarise(p=weighted.mean(price,quantity),
              q=sum(quantity))
  return(df_agg)
}

# df<-
#   df_agg
# zeta=0.5

# compute_productivity ----------------------------------------------------


compute_productivity<-function(df,zeta){
  #3 columns: p,q,Year 
  
  
  df<-
    df%>%
    filter(!is.na(p) & !is.na(q))%>%
    arrange(Year)%>%
    mutate(phat=log(p)-log(dplyr::lag(p)),
           qhat=log(q)-log(dplyr::lag(q)))%>%
    mutate(zeta=zeta,
           Ahat=zeta*qhat+(zeta-1)*phat)
  
  return(df)
}


# f_interpolate_q ---------------------------------------------------------


f_interpolate_q<-function(quantity){
  
  df_quantity<-c()
  for (scenario in unique(quantity$Scenario)){
    print(scenario)
    df_scenario<-quantity%>%filter(Scenario==scenario)%>%
      filter(!is.na(quantity))
    for (techno in unique(df_scenario$technology)){
      print(techno)
      df_techno_scenario<-df_scenario%>%filter(technology==techno)
      for(interpolation_type in c("linear","polynomial","spline","automatic_poly")){
        
        data<-df_techno_scenario
        first_date_available<-min(0,as.numeric(unique(df_techno_scenario$first_date_available)))
        q_interpolate<-interpolate_trajectory(data=data,
                                              first_date_available,
                                              var="quantity",
                                              interpolation_type)
        q_interpolate$technology<-techno
        q_interpolate$Scenario<-scenario
        q_interpolate$interpolation_type<-interpolation_type
        q_interpolate$first_date_available<-first_date_available
        df_quantity<-rbind(df_quantity,q_interpolate)
      }
    }
  }
  df_quantity<-df_quantity%>%rename("quantity"="var")
  return(df_quantity)
}


# f_interpolate_p ---------------------------------------------------------


f_interpolate_p<-function(price){
  
  
  df_price<-c()
  for (scenario in unique(price$Scenario)){
    print(scenario)
    df_scenario<-price%>%filter(Scenario==scenario)
    for (techno in unique(df_scenario$technology)){
      print(techno)
      df_techno_scenario<-df_scenario%>%filter(technology==techno)
      for (price_s_tco in unique(df_techno_scenario$price_scenario_tco)){
        print(price_s_tco)
        df_techno_scenario_p<-df_techno_scenario%>%filter(price_scenario_tco==price_s_tco)
        # for (tco in unique(df_techno_scenario_p$carbon_price)){
        #   print(tco)
        # df_techno_scenario_p_tco<-df_techno_scenario_p%>%filter(carbon_price==tco)
        for(interpolation_type in c("linear","polynomial","spline","automatic_poly")){
          
          data<-df_techno_scenario_p
          p_interpolate<-interpolate_trajectory(data=data,
                                                var="price",
                                                interpolation_type=interpolation_type)
          p_interpolate$technology<-techno
          p_interpolate$Scenario<-scenario
          p_interpolate$price_scenario_tco<-price_s_tco
          p_interpolate$price_scenario<-unique(df_techno_scenario_p$price_scenario)
          p_interpolate$carbon_price<-paste(paste(unique(df_techno_scenario_p$Year),unique(df_techno_scenario_p$carbon_price),sep="-"),collapse = ' / ')
          p_interpolate$interpolation_type<-interpolation_type
          df_price<-rbind(df_price,p_interpolate)
          # }
        }}}
  }
  df_price<-df_price%>%rename("price"="var")
  return(df_price)  
}


# compute_Ahat ------------------------------------------------------------


compute_Ahat<-function(zeta_c,df_price_all,df_quantity_all,mix_scenario_pq=T){
  
  all_scenario<-get_all_scenarios(df_price_all,df_quantity_all,mix_scenario_pq)
  
  
  n=dim(all_scenario)[1]
  print(n)
  df_all<-c()
  
  for(zeta in zeta_c){
    print(zeta)
    for(i in 1:n){
      if (i %% 100 == 0) {print(i)}
      
      hyp<-all_scenario[i,]
      scenario_p=hyp$scenario_p
      scenario_q=hyp$scenario_q
      price_s=hyp$price_s
      tco=hyp$tco
      interpolation_type_q=hyp$interpolation_type_q
      interpolation_type_p=hyp$interpolation_type_p
      
      df_price<-
        df_price_all%>%
        filter(Scenario==scenario_p,
               price_scenario==price_s,
               carbon_price==tco,
               interpolation_type==interpolation_type_p)%>%
        ungroup()
      
      df_quantity<-
        df_quantity_all%>%
        filter(Scenario==scenario_q,
               interpolation_type==interpolation_type_q)%>%
        ungroup()
      
      df_agg<-f_aggregate_techno(df_price,df_quantity)
      df_Ahat<-compute_productivity(df_agg,zeta)
      if(dim(df_Ahat)[1]>0){
        df_Ahat<-cbind(df_Ahat,hyp)
        df_all<-rbind(df_all,df_Ahat)}
      
      
    }
  }
  
  return(df_all)
}
