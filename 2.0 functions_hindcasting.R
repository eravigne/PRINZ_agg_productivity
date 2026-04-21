compute_abs_mean_error<-function(zeta){
  mean_error<-(df%>%
                 mutate(predAihat=zeta*yihat+(zeta-1)*(pihat-hat_infl))%>%
                 ungroup()%>%
                 summarise(mean_error=abs(mean(predAihat-Aihat))))$mean_error
  # summarise(error=mean(pred_cumAhat-true_cumAhat))
  return(mean_error)
}
compute_mean_abs_error<-function(zeta){
  mean_error<-(df%>%
                 mutate(predAihat=zeta*yihat+(zeta-1)*(pihat-hat_infl))%>%
                 ungroup()%>%
                 summarise(mean_error=mean(abs(predAihat-Aihat))))$mean_error
  return(abs(mean_error))
}

compute_mean_error<-function(zeta){
  mean_error<-(df%>%
                 mutate(predAihat=zeta*yihat+(zeta-1)*(pihat-hat_infl))%>%
                 ungroup()%>%
                 summarise(mean_error=mean(predAihat-Aihat)))$mean_error
  return(abs(mean_error))
}

compute_median_error<-function(zeta){
  median_error<-(df%>%
                   mutate(predAihat=zeta*yihat+(zeta-1)*(pihat-hat_infl))%>%
                   ungroup()%>%
                   summarise(median_error=median(predAihat-Aihat)))$median_error
  return(abs(median_error))
}

compute_median_abs_error<-function(zeta){
  median_error<-(df%>%
                   mutate(predAihat=zeta*yihat+(zeta-1)*(pihat-hat_infl))%>%
                   ungroup()%>%
                   summarise(median_error=median(abs(predAihat-Aihat))))$median_error
  return(median_error)
}

compute_rmse<-function(zeta){
  rmse<-(df%>%
           mutate(predAihat=zeta*yihat+(zeta-1)*(pihat-hat_infl))%>%
           ungroup()%>%
           summarise(rmse=sqrt(mean((predAihat-Aihat)^2))))$rmse
  return(rmse)
}

compute_mean_error_sales_weighted<-function(zeta){
  mean_error_country<-(df%>%
                         mutate(predAihat=zeta*yihat+(zeta-1)*(pihat-hat_infl))
                       
                       %>%
                         ungroup()%>%
                         summarise(mean_error=weighted.mean(abs(predAihat-Aihat),si)))$mean_error
  # summarise(error=mean(pred_cumAhat-true_cumAhat))
  return(mean_error_country)
}


compute_mean_error_sales_share_weighted<-function(zeta){
  mean_error_country<-(df%>%
                         mutate(predAihat=zeta*yihat+(zeta-1)*(pihat-hat_infl))
                       
                       %>%
                         ungroup()%>%
                         summarise(mean_error=weighted.mean(abs(predAihat-Aihat),si/s)))$mean_error
  # summarise(error=mean(pred_cumAhat-true_cumAhat))
  return(mean_error_country)
}



optimise_all_specifications<-function(df,
                                      lower_lim=0,
                                      upper_lim=1){
  
  abs_mean_error<- optimize(compute_abs_mean_error, lower=lower_lim, upper = upper_lim)$minimum
  mean_abs_error<- optimize(compute_mean_abs_error, lower=lower_lim, upper = upper_lim)$minimum
  mean_error<- optimize(compute_mean_error, lower=lower_lim, upper = upper_lim)$minimum
  median_error<- optimize(compute_median_error, lower=lower_lim, upper = upper_lim)$minimum
  median_abs_error<- optimize(compute_median_abs_error, lower=lower_lim, upper = upper_lim)$minimum
  rms_error<- optimize(compute_rmse, lower=lower_lim, upper = upper_lim)$minimum
  mean_error_sales_weighted<- optimize(compute_mean_error_sales_weighted, lower=lower_lim, upper = upper_lim) $minimum
  mean_error_sales_share_weighted<- optimize(compute_mean_error_sales_share_weighted, lower=lower_lim, upper = upper_lim)$minimum
  
  tib<-data.frame(
    c("Absolute Mean error",
      "Mean Absolute error",
      "Mean error",
      "Median error",
      "Median Absolute error",
      "Root Mean Square error",
      "Mean absolute output weighted error",
      "Mean absolute output share weighted error"
    ),
    c(abs_mean_error,
      mean_abs_error,
      mean_error,
      median_error,
      median_abs_error,
      rms_error,
      mean_error_sales_weighted,
      mean_error_sales_share_weighted
    )
  )
  colnames(tib)<-c("Error","Zeta")
  return(tib)
}



