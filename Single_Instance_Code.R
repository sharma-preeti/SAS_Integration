# clearing current environment & setting working directory
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

####################------------- User Inputs -------------####################
# checks
data_prep_check = F # If data need to be pulled from shilonext or not
model_building_check = T # model building is to be done or not

# data prepration folder
data_prep_filename = 'Data_Prep_v2'

# Retailer / Category
Retailer_list = c("Target")

# model building dates
Model_Start_Date = "2019-12-08"
Model_End_Date = "2021-11-29"

################------------ Packages & Directories -----------#################
options(stringsAsFactors = F)
options(buildtools.check = NULL)

packages_required <- c("brms", "readxl", "reshape2", "dplyr", "readr", "data.table",
                       "sqldf", "tidyr", "xlsx", "rstudioapi", "glmnet", "glinternet", 
                       "DMwR", "caret", "stringi", "stringr", "data.table", "zoo", "lubridate",
                       "openxlsx","RcppBDT","DT","foreach","iterators","doParallel","AzureKeyVault",
                       "AzureStor"
)

check <- lapply(packages_required, require, character.only = TRUE)
if (sum(check!=T)) {
  stop('Something went wrong with libraries')
}
rm(check,packages_required)

####################------------- Source Files -------------###################

source('Azure_Connection_Config_fn.R')
source('1a.Price_Elasticity_inputs_fn.R')
source('1b.Scoring_module_fn.R')
source('2a.Dashboard_dataset_inputs_fn.R')
source('2b.Dashboard dataset creation_fn.R')


# working directories
# change working directoring and support directory for independent model runs
setwd('../1. Data_Prep/')
base_dir <- getwd()
support_dir <- paste0(base_dir,"/../4. Supported Files/")

################---------- Data Preparation Part ------------################

if (data_prep_check) {
  
  ip_hive_check = read.csv(paste0(support_dir,'RetailerNameMapping.csv'))
  setwd(base_dir)
  if(dir.exists(paste0(base_dir,'/',data_prep_filename))==T){
    print("Data Prep OP folder is present")
  }else{dir.create(paste0(base_dir,'/',data_prep_filename))}
  
  ##Extracting the data from datalake
  Data = Data_Extraction()
  
  model_data = Data[[1]]
  model_val = Data[[2]]
  Material_UPC_mapping = Data[[3]]
  Material_PPG_mapping = Data[[4]]
  
  write.csv(Material_UPC_mapping,paste0(support_dir,"UPC_Material_ID_Mapping.csv"))
  
  Retailer_list = unique(model_data[,c("L5CustomerName","L5CustomerID")])
  
  for (Retailer_Name in Retailer_list$L5CustomerName) {
    # Retailer_Name <- Retailer_list$L5CustomerName[1]
    
    Retailer_ID = Retailer_list[Retailer_list$L5CustomerName == Retailer_Name,]$L5CustomerID
    Retailer = ip_hive_check[ip_hive_check$HiveDataName%in%Retailer_Name,]$ShilohDataName
    
    setwd(paste0(base_dir,'/',data_prep_filename))
    
    # Creating Retailer Folder
    if(dir.exists(paste0(getwd(),"/",Retailer))==T){
      print("Retailer folder is present")
    }else{dir.create(paste0(getwd(),"/",Retailer))}
    
    setwd(paste0(getwd(),"/",Retailer))
    Retailer_base_dir <- getwd()
    
    if(dir.exists(paste0(getwd(),"/Output Files"))==T){
      print("Output Files folder is present")
    }else{
      dir.create(paste0(getwd(),"/Output Files"))
    }
    
    model_data_ret = model_data[model_data$L5CustomerName==Retailer_Name,]
    model_val_ret = model_val[model_val$L5CustomerID==Retailer_ID,]
    Material_PPG_mapping_ret = Material_PPG_mapping[Material_PPG_mapping$L5CustomerID==Retailer_ID,]
    
    Price_Elasticity_ip_fn(Retailer)
      
    ##Saving the data at retailer level
    write.csv(model_data_ret,paste0(getwd(),"/",model_data_filename))
    write.csv(model_val_ret,paste0(getwd(),"/",model_val_filename))
    write.csv(Material_PPG_mapping_ret,paste0(getwd(),"/",Material_PPG_mapping_filename))
     
    }
  
  print(paste0("Data Preparation for ",Retailer," Completed"))
  
}


################---------- Model Building  Part ------------################
if (model_building_check) {
  
  for (Retailer in Retailer_list) {
    # Retailer <- Retailer_list[1]
    
    setwd(paste0(base_dir,"/",data_prep_filename))
    
    setwd(paste0(getwd(),"/",Retailer))
    Retailer_base_dir <- getwd()
    
    ##loading the file names
    # Price_Elasticity_ip_fn(Retailer)
    # 
    # ##Function call 1: Preprocessing module to align the SAS data with TPO framework
    # Preprocessed_data = Preprocessing_module(model_data_filename,model_val_filename,
    #                                          Model_Start_Date,Model_End_Date,Retailer_base_dir)
    # print("Preprocessed files are generated")
    # 
    # model_data = Preprocessed_data[[1]]
    # model_val = Preprocessed_data[[2]]
    # 
    # ##Function call 2: Updating the optimizer model data
    # ppgs = unique(model_data$PPG_Key)
    # model_data_2 = data.frame(NULL)
    # for(ppg in ppgs){
    #   model_data_set_2_df = model_data[model_data$PPG_Key==ppg,]
    #   if(nrow(model_data_set_2_df)>1){
    #     model_data_set_2_df = Optimiser_Module(model_data_set_2_df)
    #   }else{
    #     model_data_set_2_df[,flag := 0]
    #     model_data_set_2_df[PPG_Retailer == "ROM" | PPG_MFG == "Non-NPP",flag := 1]
    #   }
    #   model_data_2 = rbind(model_data_2,model_data_set_2_df)
    # }
    # 
    # ##Function call 3 : Scoring module 
    # scoring_module(Retailer,model_data, model_val, model_data_set_1,model_results_final,
    #                glbl_model_cannibal_data,glbl_model_cannibal_data_2,glbl_model_base_data,
    #                model_data_set_2,glbl_model_base_data_2,Quarterly_factor_fnl,product_mapping_filename,
    #                material_upc_mapping_filename,Material_PPG_mapping_filename,support_dir,
    #                Retailer_base_dir)
    # print("Scoring module is completed")
    # 
    # Function call 4 : Inputs for Dashboard dataset preparation ####
    File_type <- "General"
    Dashboard_dataset_ip_fn(Retailer,File_type)
    print("Input files are defined for Dashboard dataset preparation")
    
    # Function call 5 : Model Coeff transform ####
    model_coeff_transform_fn(model_est_dat_filename,
                             model_est_transformed_op_filename,
                             comp_prd_dat_filename,
                             scenarioplannining_cannibalisation_working_filename,
                             Retailer,Retailer_base_dir)
    print("Model coeff transform files created")
    
    # Function call 6 : Base transform fn #####
    base_transform_fn(base_dat_filename,
                      raw_dat_filename,
                      MappingFile_Description_path,
                      cannibal_dat_filename,
                      scenarioplannining_cannibalisation_working_filename,
                      cannibal_dat_op_filename,
                      Retailer,Retailer_base_dir,File_type)
    
    # Function call 7 : Optimizer Input for Dashboard dataset preparation ####
    
    File_type <- "Optimizer"
    Dashboard_dataset_ip_fn(Retailer,File_type)
    print("Optimizer Input files are defined for Dashboard dataset preparation")
    
    setwd(Retailer_base_dir)
    # Function call 8 : Optimizer Model Coeff transform ####
    model_coeff_transform_fn(model_est_dat_filename,
                             model_est_transformed_op_filename,
                             comp_prd_dat_filename,
                             scenarioplannining_cannibalisation_working_filename,
                             Retailer,Retailer_base_dir)
    print("Optimizer Model coeff transform files created")
    # Function call 9 : Optimizer Base transform fn #####
    setwd(Retailer_base_dir)
    base_transform_fn(base_dat_filename,
                      raw_dat_filename,
                      MappingFile_Description_path,
                      cannibal_dat_filename,
                      scenarioplannining_cannibalisation_working_filename,
                      cannibal_dat_op_filename,
                      Retailer,Retailer_base_dir,File_type)
    
    print("Optimizer Base transform files created")
  
   
  }
  
}

