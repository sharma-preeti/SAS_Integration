
# sink(file = ""F:\\salespoc_AO\\Sales_POC\\Tiger\\Workspace\\Anuj\\log.txt")
library(data.table)
library(lubridate)
library(dplyr)
require(stringi)
library(readxl)
options(stringsAsFactors = F)

op_dir <- "E:\\Tiger\\preeti\\SAS Integration\\3. Tool_Input/Tool_Input_v2"
base_dir <- "E:\\Tiger\\preeti\\SAS Integration\\1. Data_Prep\\Data_Prep_v2"
if(dir.exists(op_dir)){
  print("op dir is already present")
}else{dir.create(op_dir)}
setwd(base_dir)
Retailers_W_FnD = read_excel(paste0("E:\\Tiger\\preeti\\SAS Integration\\4. Supported Files/F&D Lumpsum_brand_ppg.xlsx"),sheet=2)$Retailer
FnD_Spend = read_excel(paste0("E:\\Tiger\\preeti\\SAS Integration\\4. Supported Files/F&D Lumpsum_brand_ppg.xlsx"),sheet=1)
Retailers_1 = c("Petsmart","Meijer","Pigggly Wiggly Midwest","ShopRite","Target")
category_format_fn <- function(df_col_name){
  df_col_name <- gsub("CAT_LITTER", "Litter", df_col_name)
  df_col_name <- gsub("SNACKS", "TREATS", df_col_name)
  df_col_name<- stri_trans_general(gsub("_", " ", df_col_name), id = "Title")
  return(df_col_name)
}

retailer_format_fn <- function(df_col_name){
  #df_col_name<- stri_trans_general(gsub("_", " ", df_col_name), id = "Title")
  df_col_name<- gsub("_", " ", df_col_name)
  return(df_col_name)
}
matchColClasses <- function(df1, df2) {
  
  sharedColNames <- names(df1)[names(df1) %in% names(df2)]
  sharedColTypes <- sapply(df1[,sharedColNames], class)
  
  for (n in sharedColNames) {
    class(df2[, n]) <- sharedColTypes[n]
  }
  
  return(df2)
}

data_write <- function(file,consolidated_Df){
  
  consolidated_Df$PPG_Cat <- category_format_fn(consolidated_Df$PPG_Cat)
  consolidated_Df$Retailer <- retailer_format_fn(consolidated_Df$Retailer)
  
  if(file==paste0(Retailer,"/Base_Data_Transformed_Hist.csv")){
    
    consolidated_Df <- consolidated_Df[!colnames(consolidated_Df)%like% "CB_Comp_"]
    consolidated_Df <- consolidated_Df[!colnames(consolidated_Df)%like%"WB_Comp_"]
  }
  
  
  if(file==paste0(Retailer,"/model_coefficient_optimizer.csv") ||
     file==paste0(Retailer,"/Model_Coefficients_EDLP_TPR_Tranformed.csv")){
    PPG_Description_fnl1a <- PPG_Description_fnl1
    PPG_Description_fnl1a$PPG_Cat <- category_format_fn(PPG_Description_fnl1a$PPG_Cat)
    PPG_Description_fnl1a$PPG_Item_No <- paste0(PPG_Description_fnl1a$PPG_Item_No,"_",PPG_Description_fnl1$Market)
    
    da <- merge(consolidated_Df,PPG_Description_fnl1a[c("PPG_Item_No","PPG_Cat","Retailer","Model_flag")],
                by.x=c("Modelled_PPG_Item_No","PPG_Cat","Retailer"),
                by.y=c("PPG_Item_No","PPG_Cat","Retailer"),all.x=T)
    consolidated_Df <- da
  }
  
  setwd(op_dir)
  #list.files()
  print(paste0("Saving ",file))
  write.csv(consolidated_Df,file,row.names = F)
}

# Appending Publix & DG files #####
for (Retailer in Retailers_1){
  # Initialising base dfs #####
  tryCatch({
    print(paste0(Retailer))
    #Setting WD based on Retailer #####
    setwd(paste0(base_dir,"/",Retailer,"/Output Files"))
   
    file_list = list.files() 
    #Non Optimizer file ####
    if(paste0("Base_Data_Transformed2_",Retailer,".csv") %in% file_list){
      Base_Data_Transformed_fnl = read.csv(paste0("Base_Data_Transformed2_",Retailer,".csv"))
    }else{
      stop(paste0("Base_Data_Transformed_2 not avl for ",Retailer))}
    
    if(paste0("Base_Data_Transformed_",Retailer,".csv") %in% file_list){
      Base_Data_Transformed_Hist_fnl = read.csv(paste0("Base_Data_Transformed_",Retailer,".csv"))
    }else{
      stop(paste0("Base_Data_Transformed_Hist not avl for ",Retailer))}
    
    if(paste0("Cannibal_Data_",Retailer,".csv") %in% file_list){
      Cannibal_Data_fnl = read.csv(paste0("Cannibal_Data_",Retailer,".csv"))
    }else{print(paste0("Cannibal data not avl",Retailer))}
    
    if(paste0("Model_Coefficients_EDLP_TPR_Tranformed_",Retailer,".csv") %in% file_list){
      
      Model_Coefficients_EDLP_TPR_Tranformed_fnl = read.csv(paste0("Model_Coefficients_EDLP_TPR_Tranformed_",Retailer,".csv"))
      # Model_Coefficients_EDLP_TPR_Tranformed$PPG_Cat <- Category
      # Model_Coefficients_EDLP_TPR_Tranformed$Retailer <- Retailer
    }else{print("Model_Coefficients_EDLP_TPR_Tranformed not avl")}
    
    if(paste0("ModelEstForCannibalization_",Retailer,".RData") %in% file_list){
      New_Model_Coeff_fnl = get(load(paste0("ModelEstForCannibalization_",Retailer,".RData")))
      #new_model_coeff$PPG_Cat <- Category
      #new_model_coeff$Retailer <- Retailer
      rm(new_model_coeff)
      #print("K")
    }else{print("ModelEstForCannibalization not avl")}
    
    if(paste0("price_agg_",Retailer,".csv") %in% file_list){
      price_agg_fnl = read.csv(paste0("price_agg_",Retailer,".csv"))
    }else{print("price_agg not avl")}
    
    
    ##Optimizer file append ####
    if(paste0("Base_Data_Transformed2_",Retailer,"_Optimizer.csv") %in% file_list){
      Base_Data_Transformed_2_fnl = read.csv(paste0("Base_Data_Transformed2_",Retailer,"_Optimizer.csv"))
    }else{
      stop(paste0("Base_Data_Transformed2_optimizer not avl for ",Retailer))}
    
    if(paste0("Base_Data_Transformed_",Retailer,"_Optimizer.csv") %in% file_list){
      BaseData_Optimizer_fnl = read.csv(paste0("Base_Data_Transformed_",Retailer,"_Optimizer.csv"))
      #   BaseData_Optimizer$PPG_Cat <- Category
      #   BaseData_Optimizer$Retailer <- Retailer
    }else{print("BaseData_Optimizer not avl")}
    
    if(paste0("ModelEstForCannibalization_",Retailer,".RData") %in% file_list){
      New_Model_Coeff_Optimizer_fnl = get(load(paste0("ModelEstForCannibalization_",Retailer,".RData")))
    }else{print("New_Model_Coeff_Optimizer not avl")}
    
    if(paste0("CrossBox_Model_EDLP_TPR_",Retailer,"_Optimizer.RData") %in% file_list){
      model_data_set_2_fnl = get(load(paste0("CrossBox_Model_EDLP_TPR_",Retailer,"_Optimizer.RData")))
      #   model_data_set_2$PPG_Cat <- Category
      #   model_data_set_2$Retailer <- Retailer
    }else{print("model_data_set_2_fnl not avl")}
    
    
    if(paste0("Model_Coefficients_EDLP_TPR_Tranformed_",Retailer,"_Optimizer.csv") %in% file_list){
      Model_Coefficients_EDLP_TPR_Tranformed_Optimizer_fnl <- read.csv(paste0("Model_Coefficients_EDLP_TPR_Tranformed_",Retailer,"_Optimizer.csv"))
      #   Model_Coefficients_EDLP_TPR_Tranformed_Optimizer$PPG_Cat <- Category
      #   Model_Coefficients_EDLP_TPR_Tranformed_Optimizer$Retailer <- Retailer
    }else{print("Model_Coefficients_EDLP_TPR_Tranformed not avl")}
    # adding co eff flags
    #price_agg_fnl1 <- price_agg_fnl 
    price_agg_fnl$EDLP_flag <- ifelse(price_agg_fnl$RP_Elastic==0,1,0)
    price_agg_fnl$TPR_flag <- ifelse(price_agg_fnl$TPR_Elastic==0,1,0)
    price_agg_fnl$ACV_flag <- ifelse(price_agg_fnl$ACV_Selling_Impact==0,1,0)
    
    
    # UPC W####
    #setwd("../")
    # setwd(base_dir)
    #file_list = list.files()
    
    if(paste0("upc_level_data_including_quarterly_factors_",Retailer,".csv") %in% file_list){
      upc_factors_final = read.csv(paste0("upc_level_data_including_quarterly_factors_",Retailer,".csv"), check.names = FALSE)
      upc_factors_final$PPG_Cat = category_format_fn(upc_factors_final$PPG_Cat)
    }
    
    
    #Binding RMS data file for PPG level brand
    # load(paste0("RMS_Data_PPG_",Retailer,".RData"))
    # PPG_Brand_file <- unique(RMS_Data_PPG_2[,c("PPGName","PPG_Item","PPG_Category","BRAND GROUP(C)")])
    # 
    rm(price_agg,new_model_coeff,Model_Coefficients_EDLP_TPR_Tranformed,Cannibal_Data,BaseData_Hist,BaseData,BaseData_2,upcdata)
  
    setnames(Base_Data_Transformed_fnl, c("PPG_Description"), c("PPG_Item"))
    setnames(Base_Data_Transformed_2_fnl, c("PPG_Description"), c("PPG_Item"))
    setnames(Model_Coefficients_EDLP_TPR_Tranformed_fnl, c("PPG_Description"), c("PPG_Item"))
    setnames(Cannibal_Data_fnl, c("PPG_Cat"), c("Category"))
    # Cannibal_Data_fnl$Category <- Cannibal_Data_fnl$PPG_Cat
    # Cannibal_Data_fnl$PPG_Cat <- NULL
    model_data_set_2_fnl <- model_data_set_2_fnl[,c("Date","PPG_Item_No","PPG_MFG", "PPG_Cat", "Final_baseprice", "Retailer","Model_flag")]
    
    #PPG Description_creation ####
    PPG_Description <- price_agg_fnl[,c("Retailer","PPG_Item_No","PPG_Item","PPG_Cat",
                                        "PPG_MFG","Market","Model_flag")]
    colnames(PPG_Description) <- c("Retailer","PPG_Item_No","PPG_Item","PPG_Cat","MFG","Market","Model_flag")
    PPG_Description$PPG_Item_No <- gsub("_ROM","",PPG_Description$PPG_Item_No)
    PPG_Description$PPG_Item_No <- gsub("_Retailer","",PPG_Description$PPG_Item_No)
    PPG_Description <- unique(PPG_Description)
    library(stringi)
    
    PPG_Description_fnl <- merge(unique(PPG_Description[,c("PPG_Item_No","PPG_Item", "PPG_Cat","MFG",
                                                           "Market","Retailer","Model_flag")]),
                                 unique(upc_factors_final[,c("PPG_Description", "PPG_Cat","BRAND GROUP(C)")]),
                                 by.x = c("PPG_Item_No","PPG_Cat"), by.y = c("PPG_Description", "PPG_Cat"))
    
    
    
    PPG_Description_fnl$`BRAND GROUP(C)`<- stri_trans_general(PPG_Description_fnl$`BRAND GROUP(C)`, id = "Title")
    
    colnames(PPG_Description_fnl) <- c("PPG_Item_No","PPG_Cat","PPG_Item","MFG","Market", "Retailer","Model_flag","PPG_Brand")
    PPG_Description_fnl1 <<- PPG_Description_fnl
    PPG_Description_fnl$PPG_Cat <- category_format_fn(PPG_Description_fnl$PPG_Cat)
    brand <- unique(PPG_Description_fnl[,c("PPG_Item", "PPG_Cat","Market","Retailer","PPG_Brand")])
    quarters<- unique(quarter(Base_Data_Transformed_Hist_fnl$Date, with_year = T))
    quarters<- gsub("[.]","Q",quarters, fixed=F)
    quarters1<- paste0(substring(quarters,5,6),"_",substring(quarters,1,4))
    #quarters<- quarters[length(quarters)-8: length(quarters)]
    cols1<-c("PPG_Item_no","PPG_Description","PPG_Cat","Item.Number","Product_Name","Retailer","Market",
             "BRAND GROUP(C)", quarters1)
    
    upc_factors_final<-setDT(upc_factors_final)[,..cols1]
    names(upc_factors_final)<-c("PPG_Item_no","PPG_Description","PPG_Cat","Item.Number","Product_Name",
                                "Retailer","Market","BRAND GROUP(C)",quarters)
    
    
    ##Saving consolidated files #### 
    
    setnames(brand, c("PPG_Cat", "PPG_Brand"), c("Category", "Brand"))
    if(Retailer%in%Retailers_W_FnD){
      brand = merge(brand,FnD_Spend,by="Brand",all.x=T)
      brand$Feature_Spend = ifelse(is.na(brand$Feature_Spend),0,brand$Feature_Spend)
      brand$Display_Spend = ifelse(is.na(brand$Display_Spend),0,brand$Display_Spend)
    }else{
      brand$Display_Spend = 0
      brand$Feature_Spend = 0
    }
    
    #getwd()
    
    #setwd(paste0("../../../Tool_input/"))
    setwd(op_dir)
    if(dir.exists(paste0(op_dir,"/",Retailer))==T){
      print("Retailer folder is present")
    }else{dir.create(paste0(Retailer))}
    
    #setwd(paste0(op_dir,"/",Retailer))
    data_write(paste0(Retailer,"/Base_Data_Transformed.csv"),Base_Data_Transformed_fnl)
    data_write(paste0(Retailer,"/Base_Data_Transformed_optimizer.csv"),Base_Data_Transformed_2_fnl)
    if(any(is.na(Base_Data_Transformed_Hist_fnl$Within))){
      Base_Data_Transformed_Hist_fnl[is.na(Base_Data_Transformed_Hist_fnl$Within),]$Within <- 0
    }
    if(any(is.na(Base_Data_Transformed_Hist_fnl$Cross))){
      Base_Data_Transformed_Hist_fnl[is.na(Base_Data_Transformed_Hist_fnl$Cross),]$Cross <- 0
    }
    data_write(paste0(Retailer,"/Base_Data_Transformed_Hist.csv"),Base_Data_Transformed_Hist_fnl)
    data_write(paste0(Retailer,"/BaseData_optimizer.csv"),BaseData_Optimizer_fnl)
    # data_write("Cannibal_data_all.csv",Cannibal_Data_fnl)
    Cannibal_Data_fnl$Category <- category_format_fn(Cannibal_Data_fnl$Category)
    write.csv(Cannibal_Data_fnl,file=paste0(Retailer,"/Cannibal_data_all.csv"))
    data_write(paste0(Retailer,"/Model_Coefficients_EDLP_TPR_Tranformed.csv"),Model_Coefficients_EDLP_TPR_Tranformed_fnl)
    data_write(paste0(Retailer,"/new_model_coeff.csv"),New_Model_Coeff_fnl)
    data_write(paste0(Retailer,"/new_model_coeff_optimizer.csv"),New_Model_Coeff_fnl)
    price_agg_fnl$ACV_TPR_Max = price_agg_fnl$ACV_TPR_Max/100
    data_write(paste0(Retailer,"/Retailer_price_agg.csv"),price_agg_fnl)
    data_write(paste0(Retailer,"/model_coefficient_optimizer.csv"),Model_Coefficients_EDLP_TPR_Tranformed_Optimizer_fnl)
    data_write(paste0(Retailer,"/model_data_set_2_optimizer.csv"),model_data_set_2_fnl)
    data_write(paste0(Retailer,"/upclist.csv"),upc_factors_final)
   
    write.csv(PPG_Description_fnl,paste0(Retailer,"/PPG_Description.csv"),row.names = F)
    #brand file creation
    write.csv(brand,paste0(Retailer,"/Brand.csv"),row.names = F)
    
    
  },error = function(e){
    print(paste0("Error in ",Retailer))
  })
}
