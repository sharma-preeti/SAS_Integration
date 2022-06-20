Price_Elasticity_ip_fn <- function(Retailer){
  options("scipen"=4)
  # Setting ip model parameters ####
  # Model Parameters
  #Specify Model Regularization Parameter
  
  #R Square Threshold to account vol. contri into cannibalisation
  cannibalisation_cor_cutoff<<-0.5
  
  model_data_filename<<-paste("Model_Data_",Retailer,".csv",sep="")
  
  model_val_filename<<-paste("Model_Coeff_",Retailer,".csv",sep="")
  
  Material_PPG_mapping_filename<<-paste("Model_Mapping_",Retailer,".csv",sep="")
  
  product_mapping_filename<<-paste("Product_Mapping.csv",sep="")
  
  filtered_model_dataset_filename<<-paste("CrossBox_Model_EDLP_TPR_",Retailer,sep="")
  
  filtered_model_dataset_filename_opt<<-paste("CrossBox_Model_EDLP_TPR_",Retailer,"_Optimizer",sep="")
  
  #Output Model Estimates Filename
  model_est_dat_filename<<-paste("Model_Est_",Retailer,sep="")
  
  #Output Cannibalisation Filename
  cannibal_dat_filename<<-paste("Model_Cannibal_",Retailer,sep="")
  
  cannibal_dat_filename_opt<<-paste("Model_Cannibal_",Retailer,"_Optimizer",sep="")
  #Output Baselines Filename
  baselines_dat_filename<<-paste("Model_Baselines_",Retailer,sep="")
  
  baselines_dat_filename_opt<<-paste("Model_Baselines_",Retailer,"_Optimizer",sep="")
  
  upc_quarterly_filename<<-paste("upc_level_data_including_quarterly_factors_",Retailer,".csv",sep="")
  
  material_upc_mapping_filename<<-paste("UPC_Material_ID_Mapping.csv")
}
