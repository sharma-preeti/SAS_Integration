Dashboard_dataset_ip_fn <- function(Retailer,File_type){
  if(File_type=="General"){
    #Output Model Estimates Filename
    model_est_dat_filename<<-paste("Model_Est_",Retailer,sep="")
    
    filtered_model_dataset_filename<<-paste("CrossBox_Model_EDLP_TPR_",Retailer,sep="")
    
    #Output Baselines Filename
    baselines_dat_filename<<-paste("Model_Baselines_",Retailer,sep="")
    
    #Output Cannibalisation Filename
    cannibal_dat_filename<<-paste("Model_Cannibal_",Retailer,sep="")
    
    # Model Estimates- Flat Format- Output File
    model_est_transformed_op_filename<<-paste0("Model_Coefficients_EDLP_TPR_Tranformed_",Retailer,".csv")
    
    #Model Est. for computing cannibalisation in Scenario Planning 
    scenarioplannining_cannibalisation_working_filename<<-paste0("ModelEstForCannibalization_",Retailer,".RData")
    
    #Competitior Product Names- WB and CB
    comp_prd_dat_filename<<-paste0("Competitor_Products_EDLP_TPR_Transformed_",Retailer,".csv")
    
    #Cannibalisation Flat Report
    cannibal_dat_op_filename<<-paste0("Cannibal_Data_",Retailer,".csv")
    cannibal_dat_op_filename1<<-paste0("Cannibal_Data_predicted_",Retailer,".csv")
    
    #Base Data with Predictions Flat Report
    base_transform_op_filename<<-paste0("Base_Data_Transformed_",Retailer,".csv")
    
    #Base Data with Predictions Flat Report Aggregated to Quarter
    base_transform_op_filename2<<-paste0("Base_Data_Transformed2_",Retailer,".csv")
    
    #Dashboard UI Input Range
    price_agg_op_filename<<-paste0("price_agg_",Retailer,".csv")
    
  }else if(File_type =="Optimizer"){
    #Output Model Estimates Filename
    model_est_dat_filename<<-paste("Model_Est_",Retailer,sep="")
    
    filtered_model_dataset_filename<<-paste("CrossBox_Model_EDLP_TPR_",Retailer,"_Optimizer",sep="")
    
    #Output Baselines Filename
    baselines_dat_filename<<-paste("Model_Baselines_",Retailer,"_Optimizer",sep="")
    
    #Output Cannibalisation Filename
    cannibal_dat_filename<<-paste("Model_Cannibal_",Retailer,"_Optimizer",sep="")
    
    # Model Estimates- Flat Format- Output File
    model_est_transformed_op_filename<<-paste0("Model_Coefficients_EDLP_TPR_Tranformed_",Retailer,"_Optimizer.csv")
    
    #Model Est. for computing cannibalisation in Scenario Planning 
    scenarioplannining_cannibalisation_working_filename<<-paste0("ModelEstForCannibalization_",Retailer,".RData")
    
    #Competitior Product Names- WB and CB
    comp_prd_dat_filename<<-paste0("Competitor_Products_EDLP_TPR_Transformed_",Retailer,"_Optimizer.csv")
    
    #Cannibalisation Flat Report
    cannibal_dat_op_filename<<-paste0("Cannibal_Data_",Retailer,"_Optimizer.csv")
    
    #Base Data with Predictions Flat Report
    base_transform_op_filename<<-paste0("Base_Data_Transformed_",Retailer,"_Optimizer.csv")
    
    #Base Data with Predictions Flat Report Aggregated to Quarter
    base_transform_op_filename2<<-paste0("Base_Data_Transformed2_",Retailer,"_Optimizer.csv")
    
    #Dashboard UI Input Range
    price_agg_op_filename<<-paste0("price_agg_",Retailer,"_Optimizer.csv")
    
  }
  
}

