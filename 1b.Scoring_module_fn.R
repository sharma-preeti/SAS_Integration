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

Preprocessing_module <- function(model_data_filename,model_val_filename,
                                 Model_Start_Date,Model_End_Date,base_dir){
  model_data = read.csv(paste0(base_dir,"/",model_data_filename))
  model_val = read.csv(paste0(base_dir,"/",model_val_filename))
  ###Distribution in decimals
  model_data = model_data[!is.na(model_data$modellingPpg) & !model_data$modellingPpg == "",]
  # model_data$pctAcv = model_data$pctAcv/100
  # model_data$featAndDispPctAcv = model_data$featAndDispPctAcv/100
  # model_data$featWoDispPctAcv = model_data$featWoDispPctAcv/100
  # model_data$dispWoFeatPctAcv = model_data$dispWoFeatPctAcv/100
  # model_data$priceDecrOnlyPctAcv = model_data$priceDecrOnlyPctAcv/100
  # model_data$smoothPctACV = model_data$smoothPctACV/100
  # model_data$medianPctACV = model_data$medianPctACV/100
    
  model_data$Vendor = gsub(" ","",model_data$Vendor)
  model_val$Vendor = gsub(" ","",model_val$Vendor)
  
  model_data$DateID = as.Date(model_data$DateID,format = "%m/%d/%Y")
  model_data = model_data[model_data$DateID >= Model_Start_Date &
                            model_data$DateID < Model_End_Date ,]
  model_data$DateID = model_data$DateID-1
  
  
  model_val$PPG_Key = paste0(model_val$Segment,"_",model_val$Vendor,"_",model_val$MarketDescription,"_",model_val$modellingPpg)
  model_data$PPG_Key = paste0(model_data$segment,"_",model_data$Vendor,"_",model_data$MarketDescription,"_",model_data$modellingPpg)
  model_data$PPG_Market = paste0(model_data$MarketDescription,"-",model_data$Vendor,"-",model_data$modellingPpg)
  
  model_val = model_val[model_val$PPG_Key %in% model_data$PPG_Key,]
  
  model_data = merge(model_data,unique(model_val[,c("PPG_Key","yearFlag")]),by=c("PPG_Key"),all.x=T)
  setDT(model_data)
  setDT(model_val)
  
  model_val$model_coefficient_name = ifelse(model_val$model_coefficient_name=="pcttpr","tpr_discount_byppg",model_val$model_coefficient_name)
  model_val$model_coefficient_name = ifelse(model_val$model_coefficient_name=="price","wk_sold_median_base_price_byppg_log",model_val$model_coefficient_name)
  model_val$model_coefficient_name = ifelse(model_val$model_coefficient_name=="segTrend","category_trend",model_val$model_coefficient_name)
  model_val$model_coefficient_name = ifelse(model_val$model_coefficient_name=="qtrFlag2","flag_qtr2",model_val$model_coefficient_name)
  model_val$model_coefficient_name = ifelse(model_val$model_coefficient_name=="qtrFlag3","flag_qtr3",model_val$model_coefficient_name)
  model_val$model_coefficient_name = ifelse(model_val$model_coefficient_name=="qtrFlag4","flag_qtr4",model_val$model_coefficient_name)
  model_val$model_coefficient_name = ifelse(model_val$model_coefficient_name=="smoothPctACV","ACV_Selling",model_val$model_coefficient_name)
  model_val$model_coefficient_name = ifelse(model_val$model_coefficient_name=="featAndDispPctAcv","ACV_Feat_Disp",model_val$model_coefficient_name)
  model_val$model_coefficient_name = ifelse(model_val$model_coefficient_name=="featWoDispPctAcv","ACV_Feat_Only",model_val$model_coefficient_name)
  model_val$model_coefficient_name = ifelse(model_val$model_coefficient_name=="dispWoFeatPctAcv","ACV_Disp_Only",model_val$model_coefficient_name)
  model_val$model_coefficient_name = ifelse(model_val$model_coefficient_name=="runningMonthNum","monthno",model_val$model_coefficient_name)
  model_val$model_coefficient_name = ifelse(model_val$model_coefficient_name=="tprPercentLag1","tpr_discount_byppg_lag1",model_val$model_coefficient_name)
  model_val$model_coefficient_name = ifelse(model_val$model_coefficient_name=="tprPercentLag2","tpr_discount_byppg_lag2",model_val$model_coefficient_name)
  model_val$model_coefficient_name = ifelse(model_val$model_coefficient_name=="Intercept","(Intercept)",model_val$model_coefficient_name)
  
  setnames(model_val, c("Segment","modellingPpg","yearFlag","RSquare","mape"),
           c("PPG_Cat","PPG_Description","Model_flag","model_RSq","TrainMAPE"))
  
  model_val$PPG_MFG = ifelse(model_val$Vendor == "NPPC", "NPP","Non-NPP")
  model_val$Market = ifelse(model_val$MarketDescription =="CTA","Retailer","ROM")
  model_val$PPG_Item_No = paste0(model_val$PPG_Description,"_",model_val$Market)
  
  setnames(model_data, c("L5CustomerName","L5CustomerID","Vendor","modellingPpg","DateID","segment","salesUnits","salesDollars","baseUnits","baseDollars",
                               "avgPrice","newBasePrice","medianBasePrice","finalBasePrice","estimatedBasePrice","tprPercent",
                               "tprPercentLag1","tprPercentLag2","smoothPctACV","medianPctACV","featAndDispPctAcv",
                               "featWoDispPctAcv","dispWoFeatPctAcv","qtrFlag2","qtrFlag3",           
                               "qtrFlag4","segTrend","runningMonthNum","yearFlag","priceDecrOnlyPctAcv","logSalesUnits",
                               "logAvgPrice","logMedianBasePrice"),
           c("Retailer","RetailerID","VENDOR(C)","PPG_Description","Date","PPG_Cat","wk_sold_qty_byppg","wk_sold_doll_byppg","wk_sold_qty_base_byppg",
             "wk_sold_doll_base_byppg","wk_sold_avg_price_byppg","New_base_price","median_baseprice",       
             "Final_baseprice","Estimated_baseprice","tpr_discount_byppg","tpr_discount_byppg_lag1","tpr_discount_byppg_lag2","ACV_Selling","median_acv_selling",
             "ACV_Feat_Disp","ACV_Feat_Only","ACV_Disp_Only",
             "flag_qtr2","flag_qtr3","flag_qtr4","category_trend","monthno",
             "Model_flag","ACV_TPR_Only","wk_sold_qty_byppg_log","wk_sold_avg_price_byppg_log",        
             "wk_sold_median_base_price_byppg_log"))
  
  model_data$PPG_MFG = ifelse(model_data$`VENDOR(C)` == "NPPC", "NPP","Non-NPP")
  model_data$PPG_Retailer = ifelse(model_data$MarketDescription == "CTA","Retailer","ROM")
  model_data$PPG_Item_No = paste0(model_data$PPG_Description,"_",model_data$PPG_Retailer)
  model_data$Year_Qtr = paste0(year(model_data$Date),"_",quarter(model_data$Date))
  return(list(model_data,model_val))
  
}

cannibalisation_module<-function(cannibal_ppg,
                                 datast,model_data,modelCoeffs_temp1,model_intercept,ppg,
                                 model_coefficients_check_pantry){
  i = sapply(strsplit(as.character(ppg), "_"), "[[", 4)
  PPG_Cat = sapply(strsplit(as.character(ppg), "_"), "[[", 1)
  
  cols=modelCoeffs_temp1$model_coefficient_name
  datast_temp=datast[,cols]
  rownames(datast_temp) <- NULL
  colnames(datast_temp) <- NULL
  modelCoeffs_temp2=as.vector(modelCoeffs_temp1$model_coefficient_value)
  modelCoeffs_temp3 = t(replicate(nrow(datast_temp),modelCoeffs_temp2))
  
  pred_vol = as.matrix(modelCoeffs_temp3)*as.matrix(datast_temp)
  pred_vol = exp(rowSums(pred_vol) + model_intercept)
  
  ppg_model_cannibal_data<-data.frame()
  
  iter<-lapply(cannibal_ppg,function(x){
    ppg_x<-gsub("(edlp_CTA-NPPC-)|(tpr_CTA-NPPC-)","",x)
    datast_new<-datast[,c(cols,"Date")]
    
    if(x %like% "tpr"){
      #Computing Cannibalisation Qty. for TPR effects
      datast_new[x]<-0
      datast_new["Date"]<-NULL
      rownames(datast_new) <- NULL
      colnames(datast_new) <- NULL
      
      cannibal_vol = as.matrix(modelCoeffs_temp3)*as.matrix(datast_new)
      cannibal_vol = exp(rowSums(cannibal_vol) + model_intercept)
      ppg_measure<-"Promoted Discount"
    }else{
      #Computing Cannibalisation Qty. for EDLP effects
      datast_new<-merge(datast_new,
                          unique(model_data[which(model_data$PPG_Description==gsub("(edlp_CTA-NPPC-)|(tpr_CTA-NPPC-)","",x) & model_data$MarketDescription=="CTA"),
                                                  c("Date","Final_baseprice")]),
                          by="Date",all.x=TRUE)
      datast_new[,x]<-log(as.numeric(datast_new[,"Final_baseprice"]))
      datast_new["Final_baseprice"]<-NULL
      datast_new["Date"]<-NULL
      rownames(datast_new) <- NULL
      colnames(datast_new) <- NULL
      cannibal_vol = as.matrix(modelCoeffs_temp3)*as.matrix(datast_new)
      cannibal_vol = exp(rowSums(cannibal_vol) + model_intercept)
      ppg_measure<-"Regular Price"
    }
    
    cannibal_vol<-(cannibal_vol-pred_vol)*datast$wk_sold_avg_price_byppg
    cannibal_vol<-ifelse(cannibal_vol<0,0,cannibal_vol)
    cannibal_dat<-as.data.frame(cbind(ppg_x,i,PPG_Cat,ppg_measure,cannibal_vol))
    colnames(cannibal_dat)<-c("Cannibal_PPG","Cannibalised_PPG","PPG_Cat","Measure","Cannibal_Doll")
    if(nrow(cannibal_dat)>0){
      cannibal_dat$Date = datast$Date
    }
    
    ppg_model_cannibal_data<<-rbind(ppg_model_cannibal_data,cannibal_dat)
    return(TRUE)
  })
  
  #----------Canibalisation from Pantry Loading--------------
  if(length(model_coefficients_check_pantry)>0 & ! (sapply(strsplit(as.character(ppg), "_"), "[[", 3) %like% "RM")){
    datast_new<-data.frame(datast[,cols])
    #pred_vol<-exp(predict(model,data.matrix(box_xmat_new),s=model_lambda))
    if("tpr_discount_byppg_lag1" %in% colnames(datast_new))
    {
      datast_new$tpr_discount_byppg_lag1<-0
      rownames(datast_new) <- NULL
      colnames(datast_new) <- NULL
      
      cannibal_vol = as.matrix(modelCoeffs_temp3)*as.matrix(datast_new)
      cannibal_vol = exp(rowSums(cannibal_vol) + model_intercept)
      
      cannibal_vol<-(cannibal_vol-pred_vol)*datast$wk_sold_avg_price_byppg
      cannibal_vol<-ifelse(cannibal_vol<0,0,cannibal_vol)
      
    }else
    {cannibal_vol=rep(0,nrow(datast_new))}  
    ppg_measure<-"Pantry Loading 1"
    cannibal_vol<-c(cannibal_vol[2:length(cannibal_vol)],0)
    
    cannibal_dat<-as.data.frame(cbind(i,i,PPG_Cat,ppg_measure,cannibal_vol))
    colnames(cannibal_dat)<-c("Cannibal_PPG","Cannibalised_PPG","PPG_Cat","Measure","Cannibal_Doll")
    if(nrow(cannibal_dat)>0){
      cannibal_dat$Date = datast$Date
    }
    ppg_model_cannibal_data<-rbind(ppg_model_cannibal_data,cannibal_dat)
    
    datast_new<-data.frame(datast[,cols])
    if("tpr_discount_byppg_lag2" %in% colnames(datast_new))
    {
      datast_new$tpr_discount_byppg_lag2<-0
      rownames(datast_new) <- NULL
      colnames(datast_new) <- NULL
      
      cannibal_vol = as.matrix(modelCoeffs_temp3)*as.matrix(datast_new)
      cannibal_vol = exp(rowSums(cannibal_vol) + model_intercept)
      
      cannibal_vol<-(cannibal_vol-pred_vol)*datast$wk_sold_avg_price_byppg
      cannibal_vol<-ifelse(cannibal_vol<0,0,cannibal_vol)
      
    }else
    {cannibal_vol=rep(0,nrow(datast_new))} 
    ppg_measure<-"Pantry Loading 2"
    
    cannibal_vol<-c(cannibal_vol[3:length(cannibal_vol)],0,0)
    cannibal_dat<-as.data.frame(cbind(i,i,PPG_Cat,ppg_measure,cannibal_vol))
    colnames(cannibal_dat)<-c("Cannibal_PPG","Cannibalised_PPG","PPG_Cat","Measure","Cannibal_Doll")
    if(nrow(cannibal_dat)>0){
      cannibal_dat$Date = datast$Date
    }
    ppg_model_cannibal_data<-rbind(ppg_model_cannibal_data,cannibal_dat)
    
  }
  if(nrow(ppg_model_cannibal_data)>0){
    ppg_model_cannibal_data$Cannibalised_PPG <- ifelse(grepl("_CTA_",ppg),paste0(ppg_model_cannibal_data$Cannibalised_PPG,"_Retailer"),
                                                       paste0(ppg_model_cannibal_data$Cannibalised_PPG,"_ROM")) 
    
  }
  return(ppg_model_cannibal_data)
}

Optimiser_Module <- function(model_data_set_2){
  
  ###### changing base price and average price for optimizer data set ####
  #model_data_set_2 = model_data
  setDT(model_data_set_2)
  
  max_qtr = max(model_data_set_2$Year_Qtr)
  last_quarter_input_sales = model_data_set_2[model_data_set_2$Year_Qtr==max_qtr, c("PPG_Description", "MarketDescription","PPG_MFG", "Estimated_baseprice","pctACV")]
  
  last_quarter_input_sales_1 = unique(last_quarter_input_sales %>% group_by(PPG_Description, MarketDescription, PPG_MFG)%>% mutate(median_ebp= median(Estimated_baseprice),ACV_median=median(pctACV)) %>% select(c(PPG_Description, MarketDescription, PPG_MFG,median_ebp,ACV_median)))
  model_data_set_2$percent_diff_med_bp = (model_data_set_2$Estimated_baseprice- model_data_set_2$median_baseprice)/model_data_set_2$Estimated_baseprice
  model_data_set_2 = merge(model_data_set_2, last_quarter_input_sales_1, by=(c("PPG_Description", "MarketDescription","PPG_MFG")), all.x = TRUE)
  model_data_set_2$ACV_Selling <- model_data_set_2$ACV_median
  model_data_set_2$ACV_median <- NULL
  model_data_set_2$median_baseprice_1 = ifelse(model_data_set_2$percent_diff_med_bp<0, model_data_set_2$median_ebp, model_data_set_2$median_ebp*(1-model_data_set_2$percent_diff_med_bp))
  model_data_set_2_edlp  = model_data_set_2[model_data_set_2$tpr_discount_byppg==0,]
  
  model_data_set_2_edlp$percent_diff_avg_price = (model_data_set_2_edlp$Estimated_baseprice- model_data_set_2_edlp$wk_sold_avg_price_byppg)/model_data_set_2_edlp$Estimated_baseprice
  
  model_data_set_2_edlp$wk_sold_avg_price_byppg_1 = ifelse(model_data_set_2_edlp$percent_diff_avg_price<0,model_data_set_2_edlp$median_ebp, model_data_set_2_edlp$median_ebp*(1-model_data_set_2_edlp$percent_diff_avg_price))
  model_data_set_2 = merge(model_data_set_2, model_data_set_2_edlp[,c("PPG_Description","MarketDescription", "PPG_MFG", "Date", "wk_sold_avg_price_byppg_1")],  by = c("PPG_Description","MarketDescription", "PPG_MFG", "Date"), all.x = TRUE)
  
  model_data_set_2$wk_sold_avg_price_byppg = ifelse(model_data_set_2$tpr_discount_byppg==0, model_data_set_2$wk_sold_avg_price_byppg_1, model_data_set_2$wk_sold_avg_price_byppg)
  
  model_data_set_2$median_baseprice = model_data_set_2$median_baseprice_1
  model_data_set_2$Estimated_baseprice = model_data_set_2$median_ebp
  model_data_set_2$Final_baseprice = model_data_set_2$median_ebp
  model_data_set_2$wk_sold_avg_price_byppg_1 = NULL
  model_data_set_2_tpr = model_data_set_2[model_data_set_2$tpr_discount_byppg!=0,]
  model_data_set_2_tpr$wk_sold_avg_price_byppg_1 = model_data_set_2_tpr$Estimated_baseprice*(1-model_data_set_2_tpr$tpr_discount_byppg)
  model_data_set_2 = merge(model_data_set_2, model_data_set_2_tpr[,c("PPG_Description","MarketDescription", "PPG_MFG", "Date", "wk_sold_avg_price_byppg_1")],  by = c("PPG_Description","MarketDescription", "PPG_MFG", "Date"), all.x = TRUE)
  
  model_data_set_2$wk_sold_avg_price_byppg = ifelse(model_data_set_2$tpr_discount_byppg!=0, model_data_set_2$wk_sold_avg_price_byppg_1, model_data_set_2$wk_sold_avg_price_byppg)
  model_data_set_2[,c("wk_sold_avg_price_byppg_1","median_baseprice_1", "percent_diff_med_bp", "median_ebp" )] = NULL
  
  model_data_set_2[,flag := 0]
  model_data_set_2[MarketDescription == "RM" | PPG_MFG == "Non-NPP",flag := 1]
  
  model_data_set_2[flag == 1,wk_sold_avg_price_byppg := Final_baseprice ]
  model_data_set_2[flag == 1,median_baseprice := Final_baseprice]
  
  model_data_set_2[flag == 1,tpr_discount_byppg := 0]
  
  model_data_set_2[,`:=` (wk_sold_avg_price_byppg_log = log(wk_sold_avg_price_byppg),
                          wk_sold_median_base_price_byppg_log=log(median_baseprice))]
  
  model_data_set_2<-model_data_set_2[order(PPG_Description,Date),]
  model_data_set_2$monthno = model_data_set_2$monthno + 12
  
  return(model_data_set_2)
  
}

UPC_Quarterly_module <- function(Material_UPC_mapping,Material_PPG_mapping,Retailer,Retailer_ID){
  setDT(Material_UPC_mapping)
  Material_UPC_mapping$UPC = paste0(Material_UPC_mapping$Consumer.ManfID,Material_UPC_mapping$Consumer.UPC)
  setnames(Material_UPC_mapping,"Material..","Material_no")
  Material_UPC_mapping = Material_UPC_mapping[,c("Material_no","UPC")]
  
  Material_PPG_mapping = Material_PPG_mapping[Material_PPG_mapping$L5CustomerID==Retailer_ID,]
  
  Material_PPG_mapping1 = Material_PPG_mapping[grepl("/",Material_PPG_mapping$DateID),]
  Material_PPG_mapping2 = Material_PPG_mapping[!grepl("/",Material_PPG_mapping$DateID),]
  
  Material_PPG_mapping2$DateID = as.Date(as.numeric(Material_PPG_mapping2$DateID), origin = "1899-12-30")
  Material_PPG_mapping1$DateID = as.Date((Material_PPG_mapping1$DateID), format="%m/%d/%Y")
  
  Material_PPG_mapping3 = rbind(Material_PPG_mapping1,Material_PPG_mapping2)
  
  Final_Mapping = merge(Material_PPG_mapping3,Material_UPC_mapping,by.x = "forecastedMaterialNo",
                        by.y = "Material_no",all.x = T)
  Final_Mapping$yr_qtr = paste0("Q",as.character(quarter(Final_Mapping$DateID)),'_',as.character(year(Final_Mapping$DateID)))
  
  Final_Mapping = Final_Mapping[,c("modellingPpg","Vendor","MarketDescription","Segment","yr_qtr","UPC","salesUnits")]
  setDT(Final_Mapping)
  
  setnames(Final_Mapping,c("modellingPpg","Vendor","MarketDescription","Segment","UPC"),
           c("PPGName","PPG_MFG","PPG_Retailer","PPG_Category","Item Number"))
  
  Final_Mapping$PPG_MFG = ifelse(Final_Mapping$PPG_MFG=="NPPC","NPP","Non-NPP")
  Final_Mapping$PPG_Retailer = ifelse(Final_Mapping$PPG_Retailer=="CTA","Retailer","ROM")
  Final_Mapping$PPG_Item = paste0(Final_Mapping$PPGName,"_",Final_Mapping$PPG_Retailer)
  Final_Mapping = Final_Mapping %>% group_by(PPG_Item,PPGName,PPG_Category,`Item Number`,PPG_Retailer,PPG_MFG,yr_qtr) %>% summarise(upc_sales = sum(salesUnits))
  Final_Mapping <- Final_Mapping %>% group_by(PPG_Item,PPGName,PPG_Category,PPG_Retailer,PPG_MFG,yr_qtr) %>% 
    mutate(ppg_sales = sum(upc_sales,na.rm = T))
  
  Final_Mapping$Sales_Ratio <- Final_Mapping$upc_sales/Final_Mapping$ppg_sales
  Final_Mapping$upc_sales <- NULL
  Final_Mapping$ppg_sales <- NULL
  Final_Mapping<-reshape2::dcast(Final_Mapping, PPG_Item+PPGName+PPG_Category+`Item Number`+PPG_Retailer+PPG_MFG~yr_qtr, fun=mean,fill=0)	
  Final_Mapping = Final_Mapping[Final_Mapping$PPG_MFG=="NPP",]
  setnames(Final_Mapping,c("PPG_Item","PPGName","PPG_Category","Item Number",
                           "PPG_Retailer"),c("PPG_Item_no", "PPG_Description","PPG_Cat","Item.Number",	
                                                              "Market"))
  Final_Mapping$Product_Name = Final_Mapping$PPG_Description
  Final_Mapping$Retailer = Retailer
  return(Final_Mapping)
} 
  
scoring_module <- function(Retailer,model_data, model_val, model_data_set_1,model_results_final,
                           glbl_model_cannibal_data,glbl_model_cannibal_data_2,glbl_model_base_data,
                           model_data_set_2,glbl_model_base_data_2,Quarterly_factor_fnl,product_mapping_filename,
                           material_upc_mapping_filename,Material_PPG_mapping_filename,support_dir,
                           base_dir){
  
  Material_UPC_mapping = read.csv(paste0(support_dir,"/",material_upc_mapping_filename))
  Material_PPG_mapping = read.csv(paste0(base_dir,"/",Material_PPG_mapping_filename))
  
  glbl_model_cannibal_data<-data.frame()
  glbl_model_base_data<-data.frame()
  glbl_model_base_data_2<-data.frame()
  glbl_model_cannibal_data_2<-data.frame()
  
  for(ppg in unique(model_val$PPG_Key)){
    #ppg="CAT LITTER_NPPC_RM_TIDYCATSCLUMPNONLW20LB"
    print(ppg)
    datast <- model_data[model_data$PPG_Key==ppg,]
    datast_2 <- model_data_2[model_data_2$PPG_Key==ppg,]
    modelCoeffs_temp = model_val[model_val$PPG_Key==ppg,]
    modelCoeffs_temp1 = modelCoeffs_temp[,c("model_coefficient_name","model_coefficient_value")]
    modelCoeffs_temp1 = modelCoeffs_temp1[!modelCoeffs_temp1$model_coefficient_name=="(Intercept)",]
    
    model_base_data = model_data[model_data$PPG_Key==ppg,c("PPG_Key","PPG_Description","MarketDescription","Date","PPG_Cat","VENDOR(C)","wk_sold_qty_byppg","wk_sold_doll_byppg",
                                                           "wk_sold_avg_price_byppg","median_baseprice",       
                                                           "Final_baseprice","Estimated_baseprice","tpr_discount_byppg","tpr_discount_byppg_lag1","tpr_discount_byppg_lag2",
                                                           "ACV_Feat_Disp","ACV_Feat_Only","ACV_Disp_Only","ACV_Selling","flag_qtr2","flag_qtr3","flag_qtr4",
                                                           "category_trend","monthno","Model_flag")]
    
    
    model_base_data_2 = model_data_2[model_data_2$PPG_Key==ppg,c("PPG_Key","PPG_Description","MarketDescription","Date","PPG_Cat","VENDOR(C)","wk_sold_qty_byppg","wk_sold_doll_byppg",
                                                                 "wk_sold_avg_price_byppg","median_baseprice",       
                                                                 "Final_baseprice","Estimated_baseprice","tpr_discount_byppg","tpr_discount_byppg_lag1","tpr_discount_byppg_lag2",
                                                                 "ACV_Feat_Disp","ACV_Feat_Only","ACV_Disp_Only","ACV_Selling","flag_qtr2","flag_qtr3","flag_qtr4",
                                                                 "category_trend","monthno","Model_flag")]
    
    cols=modelCoeffs_temp1$model_coefficient_name
    
    
    ##Add prices and discount for acting and interaction items
    if(any(grepl("CTA|RM",cols))){
      acting_Inter_item = cols[grepl("CTA|RM",cols)]
      
      ##List of Interaction item
      RP_Interaction = acting_Inter_item[grepl("\\*edlp",acting_Inter_item)]
      TPR_Interaction = acting_Inter_item[grepl("\\*tpr",acting_Inter_item)]
      
      ##List of acting item
      acting_item_edlp = acting_Inter_item[grepl("edlp",acting_Inter_item)]
      acting_item_edlp = acting_item_edlp[!acting_item_edlp%in%RP_Interaction]
      acting_item_tpr = acting_Inter_item[grepl("tpr",acting_Inter_item)]
      acting_item_tpr = acting_item_tpr[!acting_item_tpr%in%TPR_Interaction]
      
      if(length(acting_item_edlp)>0){
        acting_item_edlp1 = sapply(strsplit(as.character(acting_item_edlp), "_"), "[[", 2)
        temporary_1 <- setDT(model_data[model_data$PPG_Market %in% acting_item_edlp1,c("PPG_Market","Date","wk_sold_median_base_price_byppg_log")])
        action_ppg = dcast(temporary_1, Date~PPG_Market, value.var = "wk_sold_median_base_price_byppg_log")
        colnames(action_ppg)[!colnames(action_ppg) %in% c("Date")]<-paste("edlp_",colnames(action_ppg)[!colnames(action_ppg) %in% c("Date")],sep="")
        datast = merge(datast, action_ppg, all.x = T, by="Date")
        
      }
      
      if(length(acting_item_tpr)>0){
        acting_item_tpr1 = sapply(strsplit(as.character(acting_item_tpr), "_"), "[[", 2)
        temporary_1 <- setDT(model_data[model_data$PPG_Market %in% acting_item_tpr1,c("PPG_Market","Date","tpr_discount_byppg")])
        action_ppg = dcast(temporary_1, Date~PPG_Market, value.var = "tpr_discount_byppg")
        colnames(action_ppg)[!colnames(action_ppg) %in% c("Date")]<-paste("tpr_",colnames(action_ppg)[!colnames(action_ppg) %in% c("Date")],sep="")
        datast = merge(datast, action_ppg, all.x = T, by="Date")
      }
      
      if(length(acting_item_edlp)>0){
        acting_item_edlp1 = sapply(strsplit(as.character(acting_item_edlp), "_"), "[[", 2)
        temporary_2 <- setDT(model_data_2[model_data_2$PPG_Market %in% acting_item_edlp1,c("PPG_Market","Date","wk_sold_median_base_price_byppg_log")])
        action_ppg_2 = dcast(temporary_2, Date~PPG_Market, value.var = "wk_sold_median_base_price_byppg_log")
        colnames(action_ppg_2)[!colnames(action_ppg_2) %in% c("Date")]<-paste("edlp_",colnames(action_ppg_2)[!colnames(action_ppg_2) %in% c("Date")],sep="")
        datast_2 = merge(datast_2, action_ppg_2, all.x = T, by="Date")
        
      }
      
      if(length(acting_item_tpr)>0){
        acting_item_tpr1 = sapply(strsplit(as.character(acting_item_tpr), "_"), "[[", 2)
        temporary_2 <- setDT(model_data_2[model_data_2$PPG_Market %in% acting_item_tpr1,c("PPG_Market","Date","tpr_discount_byppg")])
        action_ppg_2 = dcast(temporary_2, Date~PPG_Market, value.var = "tpr_discount_byppg")
        colnames(action_ppg_2)[!colnames(action_ppg_2) %in% c("Date")]<-paste("tpr_",colnames(action_ppg_2)[!colnames(action_ppg_2) %in% c("Date")],sep="")
        datast_2 = merge(datast_2, action_ppg_2, all.x = T, by="Date")
      }
      
      if(length(RP_Interaction)>0){
        for(r in 1:length(RP_Interaction)){
          r_ppg = RP_Interaction[r]
          RP_Interaction1 = sapply(strsplit(as.character(r_ppg), "_"), "[[", 2)
          temporary_1 <- setDT(model_data[model_data$PPG_Market %in% RP_Interaction1 | model_data$PPG_Key %in% ppg,c("PPG_Market","Date","wk_sold_median_base_price_byppg_log")])
          RP_Iter_data = dcast(temporary_1, Date~PPG_Market, value.var = "wk_sold_median_base_price_byppg_log")
          RP_Iter_data[is.na(RP_Iter_data)] =0
          colnames(RP_Iter_data) = gsub("-","_",colnames(RP_Iter_data))
          RP_List=colnames(RP_Iter_data)[-1]
          formulae=as.formula(paste("~(",paste(RP_List,collapse="+"),")^",2,sep=""))
          RP_Iter_data1=data.frame(model.matrix(formulae,RP_Iter_data))
          RP_Iter_data1$Date=RP_Iter_data$Date
          RP_Iter_data1=RP_Iter_data1[,4:5]
          colnames(RP_Iter_data1)[!colnames(RP_Iter_data1) %in% c("Date")]<-r_ppg
          datast = merge(datast, RP_Iter_data1, all.x = T, by="Date")
        }
        
      }
      
      if(length(TPR_Interaction)>0){
        for(r in 1:length(TPR_Interaction)){
          tpr_ppg = TPR_Interaction[r]
          TPR_Interaction1 = sapply(strsplit(as.character(tpr_ppg), "_"), "[[", 2)
          temporary_1 <- setDT(model_data[model_data$PPG_Market %in% TPR_Interaction1 | model_data$PPG_Key %in% ppg,c("PPG_Market","Date","tpr_discount_byppg")])
          TPR_Iter_data = dcast(temporary_1, Date~PPG_Market, value.var = "tpr_discount_byppg")
          TPR_Iter_data[is.na(TPR_Iter_data)] =0
          colnames(TPR_Iter_data) = gsub("-","_",colnames(TPR_Iter_data))
          TPR_List=colnames(TPR_Iter_data)[-1]
          formulae=as.formula(paste("~(",paste(TPR_List,collapse="+"),")^",2,sep=""))
          TPR_Iter_data1=data.frame(model.matrix(formulae,TPR_Iter_data))
          TPR_Iter_data1$Date=TPR_Iter_data$Date
          TPR_Iter_data1=TPR_Iter_data1[,4:5]
          colnames(TPR_Iter_data1)[!colnames(TPR_Iter_data1) %in% c("Date")]<-tpr_ppg
          datast = merge(datast, TPR_Iter_data1, all.x = T, by="Date")
        }
        
      }
      
      if(length(RP_Interaction)>0){
        for(r in 1:length(RP_Interaction)){
          r_ppg = RP_Interaction[r]
          RP_Interaction1 = sapply(strsplit(as.character(r_ppg), "_"), "[[", 2)
          temporary_2 <- setDT(model_data_2[model_data_2$PPG_Market %in% RP_Interaction1 | model_data_2$PPG_Key %in% ppg,c("PPG_Market","Date","wk_sold_median_base_price_byppg_log")])
          RP_Iter_data = dcast(temporary_2, Date~PPG_Market, value.var = "wk_sold_median_base_price_byppg_log")
          RP_Iter_data[is.na(RP_Iter_data)] =0
          colnames(RP_Iter_data) = gsub("-","_",colnames(RP_Iter_data))
          RP_List=colnames(RP_Iter_data)[-1]
          formulae=as.formula(paste("~(",paste(RP_List,collapse="+"),")^",2,sep=""))
          RP_Iter_data2=data.frame(model.matrix(formulae,RP_Iter_data))
          RP_Iter_data2$Date=RP_Iter_data$Date
          RP_Iter_data2=RP_Iter_data2[,4:5]
          colnames(RP_Iter_data2)[!colnames(RP_Iter_data2) %in% c("Date")]<-r_ppg
          datast_2 = merge(datast_2, RP_Iter_data2, all.x = T, by="Date")
        }
        
      }
      
      if(length(TPR_Interaction)>0){
        for(r in 1:length(TPR_Interaction)){
          tpr_ppg = TPR_Interaction[r]
          TPR_Interaction1 = sapply(strsplit(as.character(tpr_ppg), "_"), "[[", 2)
          temporary_2 <- setDT(model_data_2[model_data_2$PPG_Market %in% TPR_Interaction1 | model_data_2$PPG_Key %in% ppg,c("PPG_Market","Date","tpr_discount_byppg")])
          TPR_Iter_data = dcast(temporary_2, Date~PPG_Market, value.var = "tpr_discount_byppg")
          TPR_Iter_data[is.na(TPR_Iter_data)] =0
          colnames(TPR_Iter_data) = gsub("-","_",colnames(TPR_Iter_data))
          TPR_List=colnames(TPR_Iter_data)[-1]
          formulae=as.formula(paste("~(",paste(TPR_List,collapse="+"),")^",2,sep=""))
          TPR_Iter_data2=data.frame(model.matrix(formulae,TPR_Iter_data))
          TPR_Iter_data2$Date=TPR_Iter_data$Date
          TPR_Iter_data2=TPR_Iter_data2[,4:5]
          colnames(TPR_Iter_data2)[!colnames(TPR_Iter_data2) %in% c("Date")]<-tpr_ppg
          datast_2 = merge(datast_2, TPR_Iter_data2, all.x = T, by="Date")
        }
        
      }
      
    }
    datast=as.data.frame(datast)
    datast_temp=datast[,cols]
    datast_temp[is.na(datast_temp)] <- 0
    
    rownames(datast_temp) <- NULL
    colnames(datast_temp) <- NULL
    modelCoeffs_temp2=as.vector(modelCoeffs_temp1$model_coefficient_value)
    modelCoeffs_temp3 = t(replicate(nrow(datast_temp),modelCoeffs_temp2))
    model_intercept = modelCoeffs_temp$model_coefficient_value[modelCoeffs_temp$model_coefficient_name=="(Intercept)"]
    
    new_pred = as.matrix(modelCoeffs_temp3)*as.matrix(datast_temp)
    new_pred_vol = rowSums(new_pred) + model_intercept
    Rsq = R2(datast$wk_sold_qty_byppg_log,new_pred_vol)
    model_base_data$pred_vol = exp(new_pred_vol)
    model_base_data$base_vol = exp(new_pred_vol)
    model_base_data$R_Sq = Rsq
    
    datast_2=as.data.frame(datast_2)
    datast_temp_2=datast_2[,cols]
    datast_temp_2[is.na(datast_temp_2)] <- 0
    rownames(datast_temp_2) <- NULL
    colnames(datast_temp_2) <- NULL
    
    new_pred_2 = as.matrix(modelCoeffs_temp3)*as.matrix(datast_temp_2)
    new_pred_vol_2 = rowSums(new_pred_2) + model_intercept
    
    model_base_data_2$pred_vol = exp(new_pred_vol_2)
    model_base_data_2$base_vol = exp(new_pred_vol_2)
    
    #Compute Baseline Volume for Retailer PPGs
    if(!ppg %like% "_RM_"){
      datast_new<- datast
      
      if(sum(colnames(datast_new)%in% "wk_sold_median_base_price_byppg_log")>0){
        datast_new$wk_sold_median_base_price_byppg_log<-log(datast_new$Final_baseprice)
      }
      if(sum(colnames(datast_new)%in% "tpr_discount_byppg")>0){
        datast_new$tpr_discount_byppg<-0
      }
      if(sum(colnames(datast_new) %like% "\\*tpr")>0){
        datast_new[,colnames(datast_new) %like% "\\*tpr"] <- 0
      }
      reg_base_inter <- colnames(datast_new)[colnames(datast_new) %like% "\\*edlp"]
      if(length(reg_base_inter)>0){
        for(r in 1:length(reg_base_inter)){
          r_ppg <- reg_base_inter[r]
          r_ppg1 <- sapply(strsplit(as.character(r_ppg), "_"), "[[", 2)
          
          temp_df <- model_data[model_data$PPG_Market==r_ppg1,c("Date","wk_sold_median_base_price_byppg_log")]
          temp_df$wk_price_log <- temp_df$wk_sold_median_base_price_byppg_log
          datast_new<-merge(datast_new,temp_df[,c("Date","wk_price_log")],by="Date",all.x=TRUE)
          
          datast_new$Final_baseprice<-as.numeric(as.character(datast_new$Final_baseprice))
          datast_new$wk_price_log<-as.numeric(as.character(datast_new$wk_price_log))
          if(modelCoeffs_temp1[modelCoeffs_temp1$model_coefficient_name==r_ppg,]$model_coefficient_value<0){
            datast_new[r_ppg]<- -1*log(datast_new$Final_baseprice)*datast_new$wk_price_log
          }else{
            datast_new[r_ppg]<- log(datast_new$Final_baseprice)*datast_new$wk_price_log
          }
          datast_new$wk_price_log<-NULL
        }
      }
      datast_new[,colnames(datast_new) %in% c("ACV_Feat_Disp","ACV_Feat_Only","ACV_Disp_Only")] <- 0
      
      datast_new=datast_new[,cols]
      datast_new[is.na(datast_new)] <- 0
      rownames(datast_new) <- NULL
      colnames(datast_new) <- NULL
      
      
      base_vol = as.matrix(modelCoeffs_temp3)*as.matrix(datast_new)
      base_vol = rowSums(base_vol) + model_intercept
      model_base_data$base_vol<-exp(base_vol)
    }
    
    #Compute Baseline Volume for Retailer PPGs Optimizer
    if(!ppg %like% "_RM_"){
      datast_new_2<- datast_2
      
      if(sum(colnames(datast_new_2)%in% "wk_sold_median_base_price_byppg_log")>0){
        datast_new_2$wk_sold_median_base_price_byppg_log<-log(datast_new_2$Final_baseprice)
      }
      if(sum(colnames(datast_new_2)%in% "tpr_discount_byppg")>0){
        datast_new_2$tpr_discount_byppg<-0
      }
      if(sum(colnames(datast_new_2) %like% "\\*tpr")>0){
        datast_new_2[,colnames(datast_new_2) %like% "\\*tpr"] <- 0
      }
      reg_base_inter <- colnames(datast_new_2)[colnames(datast_new_2) %like% "\\*edlp"]
      if(length(reg_base_inter)>0){
        for(r in 1:length(reg_base_inter)){
          r_ppg <- reg_base_inter[r]
          r_ppg1 <- sapply(strsplit(as.character(r_ppg), "_"), "[[", 2)
          
          temp_df <- model_data_2[model_data_2$PPG_Market==r_ppg1,c("Date","wk_sold_median_base_price_byppg_log")]
          temp_df$wk_price_log <- temp_df$wk_sold_median_base_price_byppg_log
          datast_new_2<-merge(datast_new_2,temp_df[,c("Date","wk_price_log")],by="Date",all.x=TRUE)
          
          datast_new_2$Final_baseprice<-as.numeric(as.character(datast_new_2$Final_baseprice))
          datast_new_2$wk_price_log<-as.numeric(as.character(datast_new_2$wk_price_log))
          if(modelCoeffs_temp1[modelCoeffs_temp1$model_coefficient_name==r_ppg,]$model_coefficient_value<0){
            datast_new_2[r_ppg]<- -1*log(datast_new_2$Final_baseprice)*datast_new_2$wk_price_log
          }else{
            datast_new_2[r_ppg]<- log(datast_new_2$Final_baseprice)*datast_new_2$wk_price_log
          }
          datast_new_2$wk_price_log<-NULL
        }
      }
      datast_new_2[,colnames(datast_new_2) %in% c("ACV_Feat_Disp","ACV_Feat_Only","ACV_Disp_Only")] <- 0
      
      datast_new_2=datast_new_2[,cols]
      datast_new_2[is.na(datast_new_2)] <- 0
      rownames(datast_new_2) <- NULL
      colnames(datast_new_2) <- NULL
      
      base_vol_2 = as.matrix(modelCoeffs_temp3)*as.matrix(datast_new_2)
      base_vol_2 = rowSums(base_vol_2) + model_intercept
      model_base_data_2$base_vol<-exp(base_vol_2)
    }
    ##Cannibalisation module
    ppg_model_cannibal_data<-data.frame()
    ppg_model_cannibal_data_2 <- data.frame()
    model_RSq = unique(modelCoeffs_temp$model_RSq)
    if(model_RSq>cannibalisation_cor_cutoff){
      cannibal_ppg = cols[grepl("CTA|RM",cols)]
      
      cannibal_ppg<-cannibal_ppg[!grepl("\\_RM",cannibal_ppg)]
      cannibal_ppg<-cannibal_ppg[grepl("NPPC",cannibal_ppg) & !grepl("price",cannibal_ppg)]
      model_coefficients_check_pantry <- modelCoeffs_temp$model_coefficient_value[modelCoeffs_temp$model_coefficient_name %in% c("tpr_discount_byppg_lag1","tpr_discount_byppg_lag2") & as.numeric(modelCoeffs_temp$model_coefficient_value<0)]
      
      ppg_model_cannibal_data<-cannibalisation_module(cannibal_ppg,
                                                      datast,model_data,modelCoeffs_temp1,model_intercept,ppg,
                                                      model_coefficients_check_pantry)
      
      ppg_model_cannibal_data_2<-cannibalisation_module(cannibal_ppg,
                                                        datast_2,model_data_2,modelCoeffs_temp1,model_intercept,ppg,
                                                        model_coefficients_check_pantry)
      
    }
    ppg_model_cannibal_data<-ppg_model_cannibal_data[as.numeric(as.character(ppg_model_cannibal_data$Cannibal_Doll))>0,]
    
    if(nrow(ppg_model_cannibal_data)>0){
      if(!is.na(ppg_model_cannibal_data$Cannibal_PPG)){
        glbl_model_cannibal_data<-rbind(glbl_model_cannibal_data,ppg_model_cannibal_data)
      }
    }
    
    ppg_model_cannibal_data_2<-ppg_model_cannibal_data_2[as.numeric(as.character(ppg_model_cannibal_data_2$Cannibal_Doll))>0,]
    
    if(nrow(ppg_model_cannibal_data_2)>0){
      if(!is.na(ppg_model_cannibal_data_2$Cannibal_PPG)){
        glbl_model_cannibal_data_2<-rbind(glbl_model_cannibal_data_2,ppg_model_cannibal_data_2)
      }
    }
    glbl_model_base_data<-rbind(glbl_model_base_data,model_base_data)
    glbl_model_base_data_2<-rbind(glbl_model_base_data_2,model_base_data_2)
    
  }
  
  glbl_model_cannibal_data = merge(glbl_model_cannibal_data,unique(model_val[model_val$Vendor=="NPPC" & model_val$MarketDescription=="CTA",c("PPG_Description","PPG_Cat","Model_flag")]),
                                   by.x=c("Cannibal_PPG","PPG_Cat"),by.y=c("PPG_Description","PPG_Cat"),all.x=T)
  
  glbl_model_cannibal_data_2 = merge(glbl_model_cannibal_data_2,unique(model_val[model_val$Vendor=="NPPC" & model_val$MarketDescription=="CTA",c("PPG_Description","PPG_Cat","Model_flag")]),
                                     by.x=c("Cannibal_PPG","PPG_Cat"),by.y=c("PPG_Description","PPG_Cat"),all.x=T)
  
  if(nrow(glbl_model_cannibal_data) == 0){
    glbl_model_cannibal_data <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("Cannibal_PPG","Cannibalised_PPG","PPG_Cat","Measure","Date","Cannibal_Doll","Model_flag"))
  }
  
  
  if(nrow(glbl_model_cannibal_data_2) == 0){
    glbl_model_cannibal_data_2 <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("Cannibal_PPG","Cannibalised_PPG","PPG_Cat","Measure","Date","Cannibal_Doll","Model_flag"))
  }
  
  setDT(glbl_model_cannibal_data)
  setDT(glbl_model_cannibal_data_2)
  setDT(model_val)
  
  model_results_final = model_val
  model_results_final$PPG_Key = NULL
  model_results_final$MarketDescription = NULL
  
  glbl_model_base_data$PPG_MFG = ifelse(glbl_model_base_data$`VENDOR(C)`=="NPPC", "NPP","Non-NPP")
  glbl_model_base_data$PPG_Retailer = ifelse(glbl_model_base_data$MarketDescription=="CTA","Retailer","ROM")
  glbl_model_base_data$PPG_Item_No = paste0(glbl_model_base_data$PPG_Description,"_",glbl_model_base_data$PPG_Retailer)
  glbl_model_base_data$PPG_Key = NULL
  glbl_model_base_data$MarketDescription = NULL
  
  glbl_model_base_data_2$PPG_MFG = ifelse(glbl_model_base_data_2$`VENDOR(C)`=="NPPC", "NPP","Non-NPP")
  glbl_model_base_data_2$PPG_Retailer = ifelse(glbl_model_base_data_2$MarketDescription=="CTA","Retailer","ROM")
  glbl_model_base_data_2$PPG_Item_No = paste0(glbl_model_base_data_2$PPG_Description,"_",glbl_model_base_data_2$PPG_Retailer)
  glbl_model_base_data_2$PPG_Key = NULL
  glbl_model_base_data_2$MarketDescription = NULL
  
  model_data_set_1 = model_data
  model_data_set_1$PPG_Key = NULL
  
  model_data_set_2 = model_data_2
  model_data_set_2$PPG_Key = NULL
  
 
  Retailer_ID = unique(model_val$L5CustomerID)
  Quarterly_factor = UPC_Quarterly_module(Material_UPC_mapping,Material_PPG_mapping,Retailer,Retailer_ID)
  
  model_data_set_1$Retailer = Retailer
  model_data_set_2$Retailer = Retailer
  
  
  save(model_data_set_1,file = paste(base_dir,"/Output Files/",filtered_model_dataset_filename,".RData",sep=""))
  save(model_results_final,file = paste(base_dir,"/Output Files/",model_est_dat_filename,".RData",sep=""))
  save(glbl_model_cannibal_data,file = paste(base_dir,"/Output Files/",cannibal_dat_filename,".RData",sep=""))
  save(glbl_model_cannibal_data_2,file = paste(base_dir,"/Output Files/",cannibal_dat_filename_opt,".RData",sep=""))
  save(glbl_model_base_data,file = paste(base_dir,"/Output Files/",baselines_dat_filename,".RData",sep=""))
  save(model_data_set_2,file = paste(base_dir,"/Output Files/",filtered_model_dataset_filename_opt,".RData",sep=""))
  save(glbl_model_base_data_2,file = paste(base_dir,"/Output Files/",baselines_dat_filename_opt,".RData",sep=""))
  
  Product_Mapping = read.csv(paste0(support_dir,"/",product_mapping_filename))
  Product_Mapping1 = Product_Mapping[,c("modellingPPGClean","upcAdjusted","nppSegment","vendorAdjusted","nppBrandGroup")]
  setDT(Product_Mapping1)
  setnames(Product_Mapping1,c("modellingPPGClean","upcAdjusted","nppSegment","vendorAdjusted","nppBrandGroup"),
           c("PPG_Description","Item.Number","PPG_Cat","PPG_MFG","BRAND GROUP(C)"))
  Product_Mapping1$Item.Number1 = str_pad(Product_Mapping1$Item.Number, 12, pad = "0")
  Product_Mapping1$PPG_Description <- mapply(function(x,y)gsub(x,"",y) ,gsub(" ", "|",Product_Mapping1$Item.Number1),Product_Mapping1$PPG_Description)
  Product_Mapping1$PPG_MFG = ifelse(Product_Mapping1$PPG_MFG=="NPPC","NPP","Non-NPP")
  Product_Mapping1$Item.Number1 = NULL
  Product_Mapping1$Item.Number = NULL
  
  Quarterly_factor_fnl = merge(Quarterly_factor,Product_Mapping1,by=c("PPG_Description","PPG_Cat","PPG_MFG"))
  
  write.csv(Quarterly_factor_fnl,paste0(base_dir,"/Output Files/",upc_quarterly_filename))
  
  
}


# upc_final = read.csv("D:\\SAS Integration\\3b files\\upc_level_data_including_quarterly_factors_Piggly Wiggly Midwest.csv")
# upc_final$PPG_Description = gsub("\\.|-| ","", upc_final$PPG_Description)
# upc_final=upc_final[,c("PPG_Description","PPG_Cat","Market","Item.Number","Q1_2020","Q1_2021","Q1_2022","Q2_2019","Q2_2020",
#                        "Q2_2021","Q3_2019","Q3_2020","Q3_2021","Q4_2019","Q4_2020","Q4_2021")]
# 
# Mapping1$PPG_Description_temp = ifelse(Mapping1$PPG_Cat == 'DRY CAT',paste0('DC',gsub('^DC',"",Mapping1$PPG_Description)),
#                                       ifelse(Mapping1$PPG_Cat == 'DOG TREATS',paste0('DT',gsub('^DT',"",Mapping1$PPG_Description)),
#                                              ifelse(Mapping1$PPG_Cat == 'LITTER',paste0('CL',gsub('^CL',"",Mapping1$PPG_Description)),
#                                                     ifelse(Mapping1$PPG_Cat == 'WET DOG',paste0('DW',gsub('^DW',"",Mapping1$PPG_Description)),
#                                                            ifelse(Mapping1$PPG_Cat == 'CAT TREATS',paste0('CT',gsub('^CT',"",Mapping1$PPG_Description)),
#                                                                   ifelse(Mapping1$PPG_Cat == 'DRY DOG',paste0('DD',gsub('^DD',"",Mapping1$PPG_Description)),
#                                                                          paste0('CW',gsub('^CW',"",Mapping1$PPG_Description))))))))
# #Mapping1 = Mapping1[Mapping1$PPG_MFG=="NPP",]
# Mapping1 = Mapping1[Mapping1$PPG_Description %in% unique(model_val$PPG_Description),]
# Mapping1 = merge(Mapping1,unique(model_val[,c("PPG_Description","Market","PPG_Cat")]),by=c("PPG_Description","PPG_Cat"))
# Mapping1$Market = ifelse(Mapping1$Market=="CTA","Retailer","ROM")
# Mapping1$PPG_Cat = gsub(" ","_",Mapping1$PPG_Cat)
# Mapping2 = merge(Mapping1,upc_final,by.x=c("PPG_Description_temp","PPG_Cat","Market"),
#                      by.y=c("PPG_Description","PPG_Cat","Market"),all.x=T)
# Mapping2$PPG_Description_temp = NULL
# Mapping2$Item.Number.y = ifelse(is.na(Mapping2$Item.Number.y),Mapping2$Item.Number.x,Mapping2$Item.Number.y)
# Mapping2$Item.Number.x = NULL
# setnames(Mapping2,"Item.Number.y","Item.Number")
# Mapping2$PPG_Item_no = paste0(Mapping2$PPG_Description,"_",Mapping2$Market)
# 
# Mapping2$Retailer = Retailer
# 
# # upc_final1 = read.csv("D:\\SAS Integration\\3b files\\upc_level_data_including_quarterly_factors_Piggly Wiggly Midwest.csv")
# # 
# # upc_final1=upc_final1[,c("Item.Number","Market","Q1_2020","Q1_2021","Q1_2022","Q2_2019","Q2_2020",
# #                        "Q2_2021","Q3_2019","Q3_2020","Q3_2021","Q4_2019","Q4_2020","Q4_2021")]
# # Mapping3 = merge(Mapping1,upc_final1[upc_final1$Market=="Retailer",],by="Item.Number",all.x=T)
# # Mapping4= merge(Mapping1,upc_final1[upc_final1$Market=="ROM",],by="Item.Number",all.x=T)
# #Mapping3 = Mapping3[!Mapping3$PPG_Description %in% model_val$modellingPpg,]





