model_coeff_transform_fn<-function(model_est_dat_filename,
                                   model_est_transformed_op_filename,
                                   comp_prd_dat_filename,
                                   scenarioplannining_cannibalisation_working_filename,
                                   Retailer,base_dir){
  
  # Retailer & Category - Naming Convention ####
  
  category_format_fn <- function(df_col_name){
    df_col_name <- gsub("CAT_LITTER", "Litter", df_col_name)
    df_col_name <- gsub("SNACKS", "TREATS", df_col_name)
    df_col_name<- stri_trans_general(gsub("_", " ", df_col_name), id = "Title")
    return(df_col_name)
  }
  
  # retailer_format_fn <- function(df_col_name){
  #   df_col_name<- stri_trans_general(gsub("_", " ", df_col_name), id = "Title")
  #   return(df_col_name)
  # }
  
  ## ####
  file<-load(file=paste0(base_dir,"/Output Files/",model_est_dat_filename,".RData"))
  model_dat<-get(x = file)
  
  rm(model_results_final,file)
  model_coeff_dat<-model_dat
  model_coeff_dat$model_box<-ifelse(grepl("_ROM",model_coeff_dat$PPG_Item_No),"ROM","Retailer")
  model_coeff_dat$Modelled_PPG_Item_No<-model_coeff_dat$PPG_Description
  model_coeff_dat$acting_box<-ifelse(grepl("_RM",model_coeff_dat$model_coefficient_name),"ROM",
                                     ifelse(grepl("_CTA",model_coeff_dat$model_coefficient_name),"Retailer",
                                            model_coeff_dat$model_box))
  
  
  ### Regex to be changed to include "/" in acting items
  # model_coeff_dat$Acting_PPG_Item_No<-regmatches(model_coeff_dat$model_coefficient_name,gregexpr("(ITEM([0-9]|[a-z]|[A-Z]|\\.)+\\_)|(^[0-9]+\\_)",model_coeff_dat$model_coefficient_name))
  model_coeff_dat$Acting_PPG_Item_No<-ifelse(grepl("-",model_coeff_dat$model_coefficient_name),gsub(".*-","",as.character(model_coeff_dat$model_coefficient_name)),model_coeff_dat$Modelled_PPG_Item_No)
  model_coeff_dat$MarketDescription <- ifelse(model_coeff_dat$Market=="Retailer","CTA","RM")
  model_coeff_dat$Acting_PPG_Key<-paste0(model_coeff_dat$MarketDescription,"-",model_coeff_dat$Vendor,"-",model_coeff_dat$PPG_Description)
  
  #model_coeff_dat$Acting_PPG_Item_No<-gsub("\\_","",as.character(model_coeff_dat$Acting_PPG_Item_No))
  #model_coeff_dat$Acting_PPG_Item_No<-ifelse(model_coeff_dat$Acting_PPG_Item_No=="character(0)",model_coeff_dat$Modelled_PPG_Item_No,model_coeff_dat$Acting_PPG_Item_No)
  
  model_coeff_dat$model_coefficient_name=as.character(model_coeff_dat$model_coefficient_name)
  #Creating Standard Variable Names for Reporting
  model_coeff_dat$Variable<-ifelse(model_coeff_dat$model_coefficient_name %like% "\\*edlp","Interaction Regular Price",
                                   ifelse(model_coeff_dat$model_coefficient_name %like% "edlp_" |
                                            model_coeff_dat$model_coefficient_name %like% "wk_sold_median_base_price_byppg","Regular Price",
                                          ifelse(model_coeff_dat$model_coefficient_name %like% "tpr_discount_byppg_lag",
                                                 paste("Pantry Loading",gsub("tpr_discount_byppg_lag","",model_coeff_dat$model_coefficient_name)),
                                          ifelse(model_coeff_dat$model_coefficient_name %like% "\\*tpr","Interaction Promoted Discount",
                                                 ifelse(model_coeff_dat$model_coefficient_name=="tpr_discount_byppg" |
                                                          model_coeff_dat$model_coefficient_name %like% "tpr_","Promoted Discount",
                                                        ifelse(model_coeff_dat$model_coefficient_name=="flag_promotion_spend_yes","Promotion Spend",
                                                               ifelse(model_coeff_dat$model_coefficient_name=="flag_promotion_spend_no","No Promotion Spend",
                                                                      model_coeff_dat$model_coefficient_name)))))))
  
  
  ######## condition applied -  check if the model_results no npp retailer ppgs ########
  
  model_intercept_dat<-model_coeff_dat[which(model_coeff_dat$Variable=="(Intercept)"),]
  model_coeff_dat<-model_coeff_dat[which(model_coeff_dat$Variable!="(Intercept)"),]
  
  # model_coeff_dat$Acting_PPG_Item_No[model_coeff_dat$Variable=="Interaction Regular Price" | 
  #                                      model_coeff_dat$Variable=="Interaction Promoted Discount"]=
  #   gsub("[^0-9A-Za-z\\.///' ]", "",
  #        model_coeff_dat$Acting_PPG_Item_No[model_coeff_dat$Variable=="Interaction Regular Price" | 
  #                                             model_coeff_dat$Variable=="Interaction Promoted Discount"])
  # for(i in 1:nrow(model_coeff_dat)){
  #   if(model_coeff_dat$Variable[i]=="Interaction Regular Price" | model_coeff_dat$Variable[i]=="Interaction Promoted Discount"){
  #     model_coeff_dat$Acting_PPG_Item_No[i]=gsub(model_coeff_dat$Modelled_PPG_Item_No[i],"",model_coeff_dat$Acting_PPG_Item_No[i])
  #     model_coeff_dat$Acting_PPG_Item_No[i]=trimws(substr(model_coeff_dat$Acting_PPG_Item_No[i],2, nchar(model_coeff_dat$Acting_PPG_Item_No[i])))}
  # }
  #Assigning Key number to each acting item
  new_model_coeff<-data.frame()
  iter<-lapply(unique(model_coeff_dat$PPG_Item_No),function(x){
    #print(x)
    #x="ITEM11CTDENTALIFEDNT_Retailer"
    model_coeff_dat_tmp<-model_coeff_dat[which(model_coeff_dat$PPG_Item_No==x),]
    model_ppg<-unique(model_coeff_dat_tmp$Modelled_PPG_Item_No)
    #For Interaction
    i<-1
    act_index<-list()
    iter<-lapply(unique(model_coeff_dat_tmp$Acting_PPG_Item_No[
      which(model_coeff_dat_tmp$Acting_PPG_Item_No!=model_coeff_dat_tmp$Modelled_PPG_Item_No &
              model_coeff_dat_tmp$model_box==model_coeff_dat_tmp$acting_box & 
              model_coeff_dat_tmp$Variable=="Interaction Regular Price")]),
      function(x){
        names(x)<-i
        act_index<<-append(act_index,x)
        i<<-i+1
      })
    model_coeff_dat_tmp$CompKey_WB_Interaction<-lapply(model_coeff_dat_tmp$Acting_PPG_Item_No,
                                                       function(x){
                                                         names(act_index[act_index==x])})
    model_coeff_dat_tmp$CompKey_WB_Interaction<-as.numeric(as.character(model_coeff_dat_tmp$CompKey_WB_Interaction))
    
    i<-1
    act_index<-list()
    iter<-lapply(unique(model_coeff_dat_tmp$Acting_PPG_Item_No[
      which(model_coeff_dat_tmp$Acting_PPG_Item_No!=model_coeff_dat_tmp$Modelled_PPG_Item_No &
              model_coeff_dat_tmp$model_box==model_coeff_dat_tmp$acting_box & 
              model_coeff_dat_tmp$Variable=="Interaction Promoted Discount" )]),
      function(x){
        names(x)<-i
        act_index<<-append(act_index,x)
        i<<-i+1
      })
    model_coeff_dat_tmp$CompKey_WB_Interaction_TPR<-lapply(model_coeff_dat_tmp$Acting_PPG_Item_No,function(x){
      names(act_index[act_index==x])})
    model_coeff_dat_tmp$CompKey_WB_Interaction_TPR<-as.numeric(as.character(model_coeff_dat_tmp$CompKey_WB_Interaction_TPR))
    i<-1
    act_index<-list()
    # For EDLP
    iter<-lapply(unique(model_coeff_dat_tmp$Acting_PPG_Item_No[
      which(model_coeff_dat_tmp$model_box!=model_coeff_dat_tmp$acting_box & 
              model_coeff_dat_tmp$Variable=="Regular Price")]),function(x){
                names(x)<-i
                act_index<<-append(act_index,x)
                i<<-i+1
              })
    model_coeff_dat_tmp$CompKey_CB<-lapply(model_coeff_dat_tmp$Acting_PPG_Item_No,function(x){
      names(act_index[act_index==x])})
    model_coeff_dat_tmp$CompKey_CB<-as.numeric(as.character(model_coeff_dat_tmp$CompKey_CB))
    i<-1
    act_index<-list()
    iter<-lapply(unique(model_coeff_dat_tmp$Acting_PPG_Item_No[
      which(model_coeff_dat_tmp$Acting_PPG_Item_No!=model_coeff_dat_tmp$Modelled_PPG_Item_No &
              model_coeff_dat_tmp$model_box==model_coeff_dat_tmp$acting_box & 
              model_coeff_dat_tmp$Variable=="Regular Price")]),
      function(x){
        names(x)<-i
        act_index<<-append(act_index,x)
        i<<-i+1
      })
    model_coeff_dat_tmp$CompKey_WB<-lapply(model_coeff_dat_tmp$Acting_PPG_Item_No,function(x){
      names(act_index[act_index==x])})
    model_coeff_dat_tmp$CompKey_WB<-as.numeric(as.character(model_coeff_dat_tmp$CompKey_WB))
    # For TPR
    i<-1
    act_index<-list()
    iter<-lapply(unique(model_coeff_dat_tmp$Acting_PPG_Item_No[
      which(model_coeff_dat_tmp$model_box!=model_coeff_dat_tmp$acting_box & 
              model_coeff_dat_tmp$Variable=="Promoted Discount")]),
      function(x){
        names(x)<-i
        act_index<<-append(act_index,x)
        i<<-i+1
      })
    model_coeff_dat_tmp$CompKey_CB_TPR<-lapply(model_coeff_dat_tmp$Acting_PPG_Item_No,function(x){
      names(act_index[act_index==x])})
    model_coeff_dat_tmp$CompKey_CB_TPR<-as.numeric(as.character(model_coeff_dat_tmp$CompKey_CB_TPR))
    i<-1
    act_index<-list()
    iter<-lapply(unique(model_coeff_dat_tmp$Acting_PPG_Item_No[which(model_coeff_dat_tmp$Acting_PPG_Item_No!=model_coeff_dat_tmp$Modelled_PPG_Item_No &
                                                                       model_coeff_dat_tmp$model_box==model_coeff_dat_tmp$acting_box & model_coeff_dat_tmp$Variable=="Promoted Discount" )]),
                 function(x){
                   names(x)<-i
                   act_index<<-append(act_index,x)
                   i<<-i+1
                 })
    model_coeff_dat_tmp$CompKey_WB_TPR<-lapply(model_coeff_dat_tmp$Acting_PPG_Item_No,
                                               function(x){
                                                 names(act_index[act_index==x])})
    model_coeff_dat_tmp$CompKey_WB_TPR<-as.numeric(as.character(model_coeff_dat_tmp$CompKey_WB_TPR))
    model_coeff_dat_tmp$CompKey<-ifelse(model_coeff_dat_tmp$Modelled_PPG_Item_No!=model_coeff_dat_tmp$Acting_PPG_Item_No & model_coeff_dat_tmp$model_box==model_coeff_dat_tmp$acting_box & model_coeff_dat_tmp$Variable=="Interaction Regular Price",
                                        paste("WB_Comp_Interaction_RP_",model_coeff_dat_tmp$CompKey_WB_Interaction,sep=""),model_coeff_dat_tmp$Variable)
    
    model_coeff_dat_tmp$CompKey<-ifelse(model_coeff_dat_tmp$Modelled_PPG_Item_No!=model_coeff_dat_tmp$Acting_PPG_Item_No & model_coeff_dat_tmp$model_box==model_coeff_dat_tmp$acting_box & model_coeff_dat_tmp$Variable=="Interaction Promoted Discount",
                                        paste("WB_Comp_Interaction_TPR_",model_coeff_dat_tmp$CompKey_WB_Interaction_TPR,sep=""),model_coeff_dat_tmp$CompKey)
    
    
    model_coeff_dat_tmp$CompKey<-ifelse(model_coeff_dat_tmp$Modelled_PPG_Item_No!=model_coeff_dat_tmp$Acting_PPG_Item_No & model_coeff_dat_tmp$model_box==model_coeff_dat_tmp$acting_box & model_coeff_dat_tmp$Variable=="Regular Price",
                                        paste("WB_Comp_RP_",model_coeff_dat_tmp$CompKey_WB,sep=""),model_coeff_dat_tmp$CompKey)
    model_coeff_dat_tmp$CompKey<-ifelse(model_coeff_dat_tmp$model_box!=model_coeff_dat_tmp$acting_box & model_coeff_dat_tmp$Variable=="Regular Price",paste("CB_Comp_RP_",model_coeff_dat_tmp$CompKey_CB,sep=""),model_coeff_dat_tmp$CompKey)
    model_coeff_dat_tmp$CompKey<-ifelse(model_coeff_dat_tmp$Modelled_PPG_Item_No!=model_coeff_dat_tmp$Acting_PPG_Item_No & model_coeff_dat_tmp$model_box==model_coeff_dat_tmp$acting_box & model_coeff_dat_tmp$Variable=="Promoted Discount",
                                        paste("WB_Comp_TPR_",model_coeff_dat_tmp$CompKey_WB_TPR,sep=""),model_coeff_dat_tmp$CompKey)
    model_coeff_dat_tmp$CompKey<-ifelse(model_coeff_dat_tmp$model_box!=model_coeff_dat_tmp$acting_box & model_coeff_dat_tmp$Variable=="Promoted Discount",
                                        paste("CB_Comp_TPR_",model_coeff_dat_tmp$CompKey_CB_TPR,sep=""),model_coeff_dat_tmp$CompKey)
    
    new_model_coeff<<-rbind(new_model_coeff,model_coeff_dat_tmp)
    return(1)
  })
  # Interaction Key####
  InteractionCompKey=new_model_coeff
  InteractionCompKey=InteractionCompKey[InteractionCompKey$Variable%in%
                                          c("Interaction Promoted Discount",
                                            "Interaction Regular Price"),]
  InteractionCompKey$InteractionVariable=gsub("_RegularPrice|_PromotedDiscount","",
                                              InteractionCompKey$model_coefficient_name)
  for(i in 1:nrow(InteractionCompKey)){
    InteractionCompKey$InteractionVariable[i]=gsub(
      paste("_",InteractionCompKey$PPG_Item_No[i],"|",
            InteractionCompKey$PPG_Item_No[i],"_",sep=""),"",InteractionCompKey$InteractionVariable[i])
  }
  InteractionCompKey$InteractionVariable=gsub("_Retailer","",InteractionCompKey$InteractionVariable)
  
  InteractionCompKey1=InteractionCompKey[,c("InteractionVariable","CompKey","Variable","model_box","acting_box","Modelled_PPG_Item_No")]
  names(InteractionCompKey1)[1]="Acting_PPG_Item_No"
  InteractionCompKey1$Variable=gsub("Interaction ","",InteractionCompKey1$Variable)
  
  # RegularPrice_Interaction Key####
  RegularPrice_InteractionItem=new_model_coeff
  RegularPrice_InteractionItem=RegularPrice_InteractionItem[
    RegularPrice_InteractionItem$Variable%in%c("Regular Price"),]
  
  
  RegularPrice_InteractionItem=RegularPrice_InteractionItem[,c("CompKey","Variable","model_box","acting_box","Modelled_PPG_Item_No","Acting_PPG_Item_No")]
  names(RegularPrice_InteractionItem)[1]="ActingCompKey_RP"
  # TPR_Interaction Key####
  
  TPR_InteractionItem=new_model_coeff
  TPR_InteractionItem=TPR_InteractionItem[TPR_InteractionItem$Variable%in%c("Promoted Discount"),]
  TPR_InteractionItem=TPR_InteractionItem[,c("CompKey","Variable","model_box","acting_box","Modelled_PPG_Item_No","Acting_PPG_Item_No")]
  names(TPR_InteractionItem)[1]="ActingCompKey_TPR"
  
  
  new_model_coeff_melt<-new_model_coeff
  new_model_coeff_melt$Acting_PPG_Item_No<-NULL
  new_model_coeff_melt$Modelled_PPG_Item_No<-paste(new_model_coeff_melt$Modelled_PPG_Item_No,
                                                   new_model_coeff_melt$model_box,sep="_")
  new_model_coeff_melt$acting_box<-NULL
  new_model_coeff_melt$model_box<-NULL
  new_model_coeff_melt$Variable<-NULL
  new_model_coeff_melt$model_RSq<-NULL
  #new_model_coeff_melt$PPG_Cat<-NULL
  new_model_coeff_melt$PPG_MFG<-NULL
  new_model_coeff_melt$PPG_Item_No<-NULL
  new_model_coeff_melt$model_coefficient_name<-NULL
  new_model_coeff_melt$CompKey<-gsub("\\s+","_",new_model_coeff_melt$CompKey)
  new_model_coeff_melt$CompKey_CB<-NULL
  new_model_coeff_melt$CompKey_WB<-NULL
  new_model_coeff_melt$CompKey_CB_TPR<-NULL
  new_model_coeff_melt$CompKey_WB_TPR<-NULL
  new_model_coeff_melt<-new_model_coeff_melt[,c("PPG_Cat","Modelled_PPG_Item_No","PPG_Description","CompKey","model_coefficient_value")]
  new_model_coeff_melt<-reshape2::dcast(new_model_coeff_melt,PPG_Cat+Modelled_PPG_Item_No+PPG_Description~CompKey, value.var = "model_coefficient_value")
  
  wb_comp_Interaction_rp_cols<-sort(names(new_model_coeff_melt)[names(new_model_coeff_melt) %like% "WB_Comp_Interaction_RP_"])
  wb_comp_Interaction_tpr_cols<-sort(names(new_model_coeff_melt)[names(new_model_coeff_melt) %like% "WB_Comp_Interaction_TPR_"])
  wb_comp_rp_cols<-sort(names(new_model_coeff_melt)[names(new_model_coeff_melt) %like% "WB_Comp_RP_"])
  wb_comp_tpr_cols<-sort(names(new_model_coeff_melt)[names(new_model_coeff_melt) %like% "WB_Comp_TPR_"])
  cb_comp_rp_cols<-sort(names(new_model_coeff_melt)[names(new_model_coeff_melt) %like% "CB_Comp_RP_"])
  cb_comp_tpr_cols<-sort(names(new_model_coeff_melt)[names(new_model_coeff_melt) %like% "CB_Comp_TPR_"])
  
  model_cols<-c("Regular_Price", "Promoted_Discount",
                "Pantry_Loading_1", "Pantry_Loading_2",
                "ACV_Selling", "ACV_Feat_Only", "ACV_Disp_Only", "ACV_Feat_Disp",
                "Promotion_Spend", "category_trend",
                "flag_qtr2", "flag_qtr3", "flag_qtr4", "monthno")
  model_cols=model_cols[model_cols%in%colnames(new_model_coeff_melt)]
  new_model_coeff_melt<-data.frame(new_model_coeff_melt)
  new_model_coeff_melt<-new_model_coeff_melt[,c("PPG_Cat","Modelled_PPG_Item_No","PPG_Description",model_cols,
                                                wb_comp_rp_cols,wb_comp_tpr_cols,
                                                cb_comp_rp_cols,cb_comp_tpr_cols,
                                                wb_comp_Interaction_rp_cols,wb_comp_Interaction_tpr_cols)]
  new_model_coeff_melt[is.na(new_model_coeff_melt)]<-0
  
  new_model_coeff_melt$Modelled_PPG_Item_No<-gsub("\\_ROM","_ROM",new_model_coeff_melt$Modelled_PPG_Item_No)
  
  # model_intercept_dat<-unique(model_intercept_dat[,c("PPG_Item_No","model_coefficient_value")])
  # names(model_intercept_dat)<-c("Modelled_PPG_Item_No","Intercept")
  model_intercept_dat$Modelled_PPG_Item_No<-paste(model_intercept_dat$Modelled_PPG_Item_No,
                                                  model_intercept_dat$Market,sep="_")
  model_intercept_dat_1<-unique(model_intercept_dat[,c("PPG_Cat","Modelled_PPG_Item_No","PPG_Description","model_coefficient_value")])
  names(model_intercept_dat_1)<-c("PPG_Cat","Modelled_PPG_Item_No","PPG_Description","Intercept")
  
  new_model_coeff_melt<-merge(new_model_coeff_melt,
                              model_intercept_dat_1,by=c("PPG_Cat","Modelled_PPG_Item_No","PPG_Description"),all.y = T)
  
  new_model_coeff_melt[is.na(new_model_coeff_melt)] <- 0
  
  #new_model_coeff_melt<-merge(new_model_coeff_melt,model_intercept_dat,by="Modelled_PPG_Item_No")
  
  if(nrow(new_model_coeff_melt)!=0){
    new_model_coeff_melt$Retailer <-  Retailer
    new_model_coeff_melt$Market = ifelse(new_model_coeff_melt$Modelled_PPG_Item_No %like% "Retailer","Retailer","ROM")
    #new_model_coeff_melt$PPG_Cat <- Category
    new_model_coeff_melt$Retailer <- retailer_format_fn(new_model_coeff_melt$Retailer)
    new_model_coeff_melt$PPG_Cat <- category_format_fn(new_model_coeff_melt$PPG_Cat)
    # print("Model_Coefficients_EDLP_TPR_Tranformed file created")
    write.csv(new_model_coeff_melt,paste0(base_dir,"/Output Files/",model_est_transformed_op_filename),row.names = F)
  }
  
  
  
  
  #Store a version of the model est files with full names for acting items for scenario planning - computing cannibalisation
  # getwd()
  # file<-load(file=MappingFile_Description_path)
  # RMS_Data_PPG_Mapping_2<-get(x = file)
  # RMS_Data_PPG_Mapping_2$PPG_Description <- NULL #TBI
  # setnames(RMS_Data_PPG_Mapping_2,c("PPGName"),c("PPG_Description"))
  # rm(file)
  
  #adding intercept only model data to the new_model_coeff
  Intercept_only_models <- model_intercept_dat[!model_intercept_dat$PPG_Item_No %in% new_model_coeff$PPG_Item_No ]
  new_model_coeff1 <- rbind(new_model_coeff,Intercept_only_models,fill=T)
  
  new_model_coeff <- new_model_coeff1
  rm(new_model_coeff1)
  
  
  names(new_model_coeff)[names(new_model_coeff)=="PPG_Description"] = "Modelled_PPG_Description"
  #new_model_coeff <- merge(new_model_coeff,RMS_Data_PPG_Mapping_2[,c("PPG_Description","PPG_Item")],by.x = "Acting_PPG_Item_No",by.y="PPG_Item",all.x= T)
  new_model_coeff$Acting_PPG_Item = new_model_coeff$Acting_PPG_Item_No 
  new_model_coeff$Retailer = Retailer
  new_model_coeff$Market = ifelse(new_model_coeff$PPG_Item_No %like% "Retailer","Retailer","ROM")
  new_model_coeff$Retailer <- retailer_format_fn(new_model_coeff$Retailer)
  new_model_coeff$PPG_Cat <- category_format_fn(new_model_coeff$PPG_Cat)
  # print("ModelEstForCannibalization file created")
  save(new_model_coeff,file=paste0(base_dir,"/Output Files/",scenarioplannining_cannibalisation_working_filename))
  
  #------------------------------Competitor Products Information--------------------------------------------------
  if(nrow(new_model_coeff[CompKey %like% "Comp"]) > 0){
    new_model_coeff$Modelled_PPG_Item_No<-paste(new_model_coeff$Modelled_PPG_Item_No,new_model_coeff$model_box,sep="_")
    
    comp_prd_data<-reshape2::dcast(unique(new_model_coeff[which(new_model_coeff$CompKey %like% "Comp"),c("PPG_Cat","Modelled_PPG_Item_No","Modelled_PPG_Description","CompKey","Acting_PPG_Item")]),PPG_Cat+Modelled_PPG_Item_No+Modelled_PPG_Description~CompKey,value.var = "Acting_PPG_Item",fun.aggregate = NULL)
    
    wb_comp_interaction_rp_cols<-sort(names(comp_prd_data)[names(comp_prd_data) %like% "WB_Comp_Interaction_RP"])
    wb_comp_interaction_tpr_cols<-sort(names(comp_prd_data)[names(comp_prd_data) %like% "WB_Comp_Interaction_TPR"])
    wb_comp_rp_cols<-sort(names(comp_prd_data)[names(comp_prd_data) %like% "WB_Comp_RP"])
    wb_comp_tpr_cols<-sort(names(comp_prd_data)[names(comp_prd_data) %like% "WB_Comp_TPR"])
    
    cb_comp_rp_cols<-sort(names(comp_prd_data)[names(comp_prd_data) %like% "CB_Comp_RP"])
    cb_comp_tpr_cols<-sort(names(comp_prd_data)[names(comp_prd_data) %like% "CB_Comp_TPR"])
    comp_prd_data<-data.frame(comp_prd_data)
    comp_prd_data<-comp_prd_data[,c("PPG_Cat","Modelled_PPG_Item_No",wb_comp_rp_cols,wb_comp_tpr_cols,
                                    cb_comp_rp_cols,cb_comp_tpr_cols,wb_comp_interaction_rp_cols,wb_comp_interaction_tpr_cols)]
    comp_prd_data[is.na(comp_prd_data)]<-"Not Applicable"
    comp_prd_data_tmp<-data.frame(unique(new_model_coeff[which(!new_model_coeff$Modelled_PPG_Item_No %in% unique(comp_prd_data$Modelled_PPG_Item_No)),c("PPG_Cat","Modelled_PPG_Item_No")]))
    if(nrow(comp_prd_data_tmp)>0){
      
      if(sum(names(comp_prd_data) %like% "WB_Comp_RP") > 0){
        comp_prd_data_tmp[,paste("WB_Comp_RP_",1:sum(names(comp_prd_data) %like% "WB_Comp_RP"),sep="")]<-"Not Applicable"}
      if(sum(names(comp_prd_data) %like% "CB_Comp_RP") > 0){
        comp_prd_data_tmp[,paste("CB_Comp_RP_",1:sum(names(comp_prd_data) %like% "CB_Comp_RP"),sep="")]<-"Not Applicable"}
      if(sum(names(comp_prd_data) %like% "WB_Comp_TPR") > 0){
        comp_prd_data_tmp[,paste("WB_Comp_TPR_",1:sum(names(comp_prd_data) %like% "WB_Comp_TPR"),sep="")]<-"Not Applicable"}
      if(sum(names(comp_prd_data) %like% "CB_Comp_TPR") > 0){
        comp_prd_data_tmp[,paste("CB_Comp_TPR_",1:sum(names(comp_prd_data) %like% "CB_Comp_TPR"),sep="")]<-"Not Applicable"}
      if(sum(names(comp_prd_data) %like% "WB_Comp_Interaction_TPR") > 0){
        comp_prd_data_tmp[,paste("WB_Comp_Interaction_TPR_",1:sum(names(comp_prd_data) %like% "WB_Comp_Interaction_TPR"),sep="")]<-"Not Applicable"}
      if(sum(names(comp_prd_data) %like% "WB_Comp_Interaction_RP") > 0){
        comp_prd_data_tmp[,paste("WB_Comp_Interaction_RP_",1:sum(names(comp_prd_data) %like% "WB_Comp_Interaction_RP"),sep="")]<-"Not Applicable"}
      
      
      comp_prd_data<-rbind(comp_prd_data,comp_prd_data_tmp)
    }
    
    write.csv(comp_prd_data,paste0(base_dir,"/Output Files/",comp_prd_dat_filename),row.names = F)
    # print("Competitor_Products_EDLP_TPR_Transformed file created")
  }
}

#---------------------------------------------------------------------------------------------
#--------------------------------POS Flat Reports------------------------------
#---------------------------------------------------------------------------------------------
#base data flat reporting
base_transform_fn<-function(base_dat_filename,
                            raw_dat_filename,
                            MappingFile_Description_path,
                            cannibal_dat_filename,
                            scenarioplannining_cannibalisation_working_filename,
                            cannibal_dat_op_filename,
                            Retailer,base_dir,file_type){
  
  #cannibalisation flat reporting
  cannibal_report<-function(cannibal_dat_filename,
                            MappingFile_Description_path,
                            glbl_model_pred_transformed_dat,
                            cannibal_dat_op_filename,
                            Retailer,
                            Category, df_type){
    
    #load PPG descriptions
    # RMS_Data_PPG_Mapping_2<-get(x=load(file=MappingFile_Description_path))
    # RMS_Data_PPG_Mapping_2$PPG_Description <- NULL #TBI
    # setnames(RMS_Data_PPG_Mapping_2,c("PPGName"),c("PPG_Description"))
    
    #-------Cannibalisation data for Reporting----------------------
    file<-load(file=paste0(base_dir,"/Output Files/",cannibal_dat_filename,".RData"))
    Category_cannibal_dat<-get(x=file)
    cannibal_dat<-Category_cannibal_dat
    cannibal_dat$Cannibal_PPG <- paste0(cannibal_dat$Cannibal_PPG,"_Retailer")
    rm(Category_cannibal_dat,file)
    
    
    if(nrow(cannibal_dat) > 0){
      cannibal_dat$Cannibal_Doll<-as.numeric(as.character(cannibal_dat$Cannibal_Doll))
      cannibal_dat<-cannibal_dat[which(cannibal_dat$Cannibal_Doll>0),]
      cannibal_dat$box<-ifelse(cannibal_dat$Cannibalised_PPG %like% "\\_ROM","Cross","Within")
      cannibal_dat$Cannibalised_PPG<-as.character(cannibal_dat$Cannibalised_PPG)
      cannibal_dat$Cannibal_PPG<-as.character(cannibal_dat$Cannibal_PPG)
      cannibal_dat$Cannibalised_PPG<-ifelse(as.character(cannibal_dat$Cannibalised_PPG)==as.character(cannibal_dat$Cannibal_PPG) ,"Pantry Loading",cannibal_dat$Cannibalised_PPG)
      cannibal_dat$Cannibalised_PPG<-gsub("(\\_ROM)|\\_Retailer","",cannibal_dat$Cannibalised_PPG)
      
      cannibal_dat<-aggregate(Cannibal_Doll~PPG_Cat+Cannibal_PPG+Cannibalised_PPG+box+Date+Model_flag,data=cannibal_dat,sum)
      
      #cannibal_dat<-merge(cannibal_dat,RMS_Data_PPG_Mapping_2[,c("PPG_Description","PPG_Item")],by.x="Cannibalised_PPG",by.y="PPG_Item",all.x=TRUE)
      cannibal_dat$PPG_Description = cannibal_dat$Cannibalised_PPG
      
      cannibal_dat$Cannibalised_PPG<-ifelse(!cannibal_dat$Cannibalised_PPG %like% "Pantry Loading",
                                            cannibal_dat$PPG_Description,cannibal_dat$Cannibalised_PPG)
      
      cannibal_dat$PPG_Description<-NULL
      cannibal_dat<-data.table(cannibal_dat)
      
      cannibal_dat[,tot_box_bycannibalppg:=sum(Cannibal_Doll,na.rm=TRUE),by=list(Cannibal_PPG,box,Date)]
      cannibal_dat[,pct_box_bycannibalppg:=Cannibal_Doll/tot_box_bycannibalppg]
      cannibal_dat<-data.frame(cannibal_dat)
      box_cannibal_dat<-unique(cannibal_dat[,c("PPG_Cat","Cannibal_PPG",
                                               "Date",
                                               "box","tot_box_bycannibalppg")])
      box_cannibal_dat<-reshape2::dcast(box_cannibal_dat,PPG_Cat+Cannibal_PPG+Date~box)
      if("Within" %in% colnames(box_cannibal_dat)){
        box_cannibal_dat$Within<-as.numeric(as.character(box_cannibal_dat$Within))
      }else{
        box_cannibal_dat$Within = 0
      }
      if("Cross" %in% colnames(box_cannibal_dat)){
        box_cannibal_dat$Cross<-as.numeric(as.character(box_cannibal_dat$Cross))
      }else{
        box_cannibal_dat$Cross = 0
      }
      
      box_cannibal_dat$Date = as.character(box_cannibal_dat$Date)#TBI
      glbl_model_pred_transformed_dat<-merge(glbl_model_pred_transformed_dat,
                                             box_cannibal_dat,by.x=c("PPG_Cat","PPG_Item_No","Date"),
                                             by.y=c("PPG_Cat","Cannibal_PPG","Date"),all.x=TRUE)
      
      glbl_model_pred_transformed_dat$Within<-ifelse(is.na(glbl_model_pred_transformed_dat$Within),0,glbl_model_pred_transformed_dat$Within)
      glbl_model_pred_transformed_dat$Cross<-ifelse(is.na(glbl_model_pred_transformed_dat$Cross),0,glbl_model_pred_transformed_dat$Cross)
      glbl_model_pred_transformed_dat$true_lift_doll<-glbl_model_pred_transformed_dat$lift_doll-
        glbl_model_pred_transformed_dat$Within-
        glbl_model_pred_transformed_dat$Cross
      glbl_model_pred_transformed_dat$true_lift_doll<-ifelse(glbl_model_pred_transformed_dat$true_lift_doll<0,0,glbl_model_pred_transformed_dat$true_lift_doll)
      glbl_model_pred_transformed_dat$Within1<-ifelse(glbl_model_pred_transformed_dat$true_lift_doll==0,
                                                      glbl_model_pred_transformed_dat$lift_doll*(glbl_model_pred_transformed_dat$Within/(glbl_model_pred_transformed_dat$Within+glbl_model_pred_transformed_dat$Cross)),
                                                      glbl_model_pred_transformed_dat$Within)
      glbl_model_pred_transformed_dat$Cross1<-ifelse(glbl_model_pred_transformed_dat$true_lift_doll==0,
                                                     glbl_model_pred_transformed_dat$lift_doll*(glbl_model_pred_transformed_dat$Cross/(glbl_model_pred_transformed_dat$Within+glbl_model_pred_transformed_dat$Cross)),
                                                     glbl_model_pred_transformed_dat$Cross)
      
      # glbl_model_pred_transformed_dat$Within <- glbl_model_pred_transformed_dat$Within1
      # glbl_model_pred_transformed_dat$Cross <- glbl_model_pred_transformed_dat$Cross1
      # 
      # glbl_model_pred_transformed_dat$Within1 <- NULL
      # 
      
      setnames(glbl_model_pred_transformed_dat, c("Within1", "Cross1", "Within", "Cross"), c("Within", "Cross", "Within_non_normalised", "Cross_non_normalised"))
      box_cannibal_dat<-unique(glbl_model_pred_transformed_dat[,c("PPG_Cat","PPG_Item_No","Date",
                                                                  "Within","Cross")])
      box_cannibal_dat<-reshape2::melt(box_cannibal_dat,id=c("PPG_Cat","PPG_Item_No","Date"))
      names(box_cannibal_dat)<-c("PPG_Cat","PPG_Item_No","Date","box","Cannibal_Doll_New")
      cannibal_dat$Date <- as.character(cannibal_dat$Date)
      box_cannibal_dat$Date <- as.character(box_cannibal_dat$Date)
      cannibal_dat<-merge(cannibal_dat,box_cannibal_dat,
                          by.x=c("PPG_Cat","Cannibal_PPG","Date","box"),
                          by.y=c("PPG_Cat","PPG_Item_No","Date","box"),all.x=TRUE)
      cannibal_dat$Cannibal_Doll<-cannibal_dat$pct_box_bycannibalppg*cannibal_dat$Cannibal_Doll_New
      cannibal_dat$Cannibal_Doll_New<-NULL
      cannibal_dat$pct_box_bycannibalppg<-NULL
      cannibal_dat$tot_box_bycannibalppg<-NULL
      cannibal_dat<-cannibal_dat[!grepl("\\_ROM\\_Retailer",cannibal_dat$Cannibal_PPG),]
      cannibal_dat<-cannibal_dat[!is.na(cannibal_dat$Cannibal_Doll) & cannibal_dat$Cannibal_Doll>0,]
      if(nrow(cannibal_dat) > 0){
        cannibal_dat$Cannibal_PPG_temp<-gsub("(\\_ROM)|\\_Retailer","",cannibal_dat$Cannibal_PPG)
        #cannibal_dat<-merge(cannibal_dat,RMS_Data_PPG_Mapping_2[,c("PPG_Description","PPG_Item")],by.x="Cannibal_PPG_temp",by.y="PPG_Item",all.x=TRUE)
        cannibal_dat$PPG_Description = cannibal_dat$Cannibal_PPG_temp
        names(cannibal_dat)[names(cannibal_dat)=="PPG_Description"] = "Cannibal_PPG_Description"
        cannibal_dat$Cannibal_PPG_temp <- NULL
        #cannibal_dat$PPG_Cat <- Category
        cannibal_dat$Retailer <- Retailer
        cannibal_dat$PPG_Cat <- category_format_fn(cannibal_dat$PPG_Cat)
        cannibal_dat$Retailer <- retailer_format_fn(cannibal_dat$Retailer)
        # print("Cannibal_Data file created")
        if(df_type=="optimizer" | df_type=="actuals"){
          write.csv(cannibal_dat,paste0(base_dir,"/Output Files/",cannibal_dat_op_filename),row.names = F)
        }else if(df_type == "predicted"){
          write.csv(cannibal_dat,paste0(base_dir,"/Output Files/",cannibal_dat_op_filename1),row.names = F) 
        }
      }
    }else{
      glbl_model_pred_transformed_dat$Cross <- 0
      glbl_model_pred_transformed_dat$Within <- 0
      glbl_model_pred_transformed_dat[,c("Within_non_normalised", "Cross_non_normalised")]=0
      glbl_model_pred_transformed_dat$true_lift_doll <- glbl_model_pred_transformed_dat$lift_doll
    }
    return(glbl_model_pred_transformed_dat)
    
  }
  #Load Predictions
  file<-load(file=paste0(base_dir,"/Output Files/",baselines_dat_filename,".RData"))
  model_pred_dat<-get(x=file)
  model_pred_dat <- unique(model_pred_dat)
  rm(file)
  
  model_pred_dat = setDT(model_pred_dat)
  #add flag qtr for 1 yr ppg
  
  model_pred_dat[quarter(Date) == 2, flag_qtr2 := 1]	
  model_pred_dat[quarter(Date) == 3, flag_qtr3 := 1]	
	model_pred_dat[quarter(Date) == 4, flag_qtr4 := 1]
  
  #model_pred_dat <- model_pred_dat[model_pred_dat$PPG_Item_No=="ITEM52CTFRISKIESPARTYMIXCR_Retailer",]
  
  #Load Cannibalisation Working Data
  load(file=paste0(base_dir,"/Output Files/",scenarioplannining_cannibalisation_working_filename))
  #new_model_coeff <- NULL new_model_coeff[new_model_coeff$PPG_Item_No=="ITEM52CTFRISKIESPARTYMIXCR_Retailer",]
  
  Metrics_check=new_model_coeff[new_model_coeff$model_coefficient_name %like% "ACV" | 
                                  new_model_coeff$model_coefficient_name %in% c("wk_sold_median_base_price_byppg_log",
                                                                                "tpr_discount_byppg"),
                                c("PPG_Item_No","PPG_Cat","model_coefficient_name")]
  
  Metrics_check$RP_Elastic=ifelse(Metrics_check$model_coefficient_name %in% "wk_sold_median_base_price_byppg_log",1,0)
  Metrics_check$TPR_Elastic=ifelse(Metrics_check$model_coefficient_name%in%"tpr_discount_byppg",1,0)
  Metrics_check$ACV_Selling_Impact=ifelse(Metrics_check$model_coefficient_name%in%"ACV_Selling",1,0)
  Metrics_check$ACV_TPR_Impact=ifelse(Metrics_check$model_coefficient_name%in%"tpr_discount_byppg",1,0)
  Metrics_check$ACV_FT_Impact=ifelse(Metrics_check$model_coefficient_name%in%"ACV_Feat_Only",1,0)
  Metrics_check$ACV_DP_Impact=ifelse(Metrics_check$model_coefficient_name%in%"ACV_Disp_Only",1,0)
  Metrics_check$ACV_FT_DP_Impact=ifelse(Metrics_check$model_coefficient_name%in%"ACV_Feat_Disp",1,0)
  
  Metrics_check$model_coefficient_name=NULL
  Final_Metrics=Metrics_check%>%group_by(PPG_Cat,PPG_Item_No)%>%summarise_all(max)
  #Load Raw Data
  file<-load(file=paste0(base_dir,"/Output Files/",filtered_model_dataset_filename,".RData"))
  base_dat<-get(x=file)
  
  base_dat<-data.frame(base_dat)
  
  rm(file)
  #rm(file2)
  
  #Generating Flat reports for each PPG with relevant acting items pricing in the week
  glbl_model_pred_transformed_dat<-data.frame()
  iter<-lapply(unique(new_model_coeff$PPG_Item_No),function(x){
    #x = "DENTALIFEDT171207OZ_ROM"
    print(x)
    model_pred_dat_tmp<-model_pred_dat[which(model_pred_dat$PPG_Item_No==x),]
    
    #WB Interaction EDLP ####
    wb_Inter_comp_prd_data_tmp<-new_model_coeff[which(new_model_coeff$PPG_Item_No==x & 
                                                        new_model_coeff$CompKey %like% "WB_Comp_Interaction_RP"),]
    
    wb_Inter_comp_prd_data_tmp$Acting_PPG_Item_No<-ifelse(wb_Inter_comp_prd_data_tmp$acting_box=="Retailer",
                                                          paste(wb_Inter_comp_prd_data_tmp$Acting_PPG_Item_No,"_Retailer",sep=""),
                                                          paste(wb_Inter_comp_prd_data_tmp$Acting_PPG_Item_No,"_ROM",sep=""))
    wb_Inter_comp_prd_price_data<-base_dat[which(base_dat$PPG_Market %in% unique(wb_Inter_comp_prd_data_tmp$Acting_PPG_Key)),c("PPG_Item_No","Date","median_baseprice")]
    wb_Inter_comp_prd_price_data$CompKey<-as.character(lapply(wb_Inter_comp_prd_price_data$PPG_Item_No,
                                                              function(x) wb_Inter_comp_prd_data_tmp$CompKey[which(wb_Inter_comp_prd_data_tmp$Acting_PPG_Item_No==x)]))
    wb_Inter_comp_prd_price_data$PPG_Item_No<-NULL
    if(nrow(wb_Inter_comp_prd_price_data)>0){
      wb_Inter_comp_prd_price_data<-wb_Inter_comp_prd_price_data[,c("Date","CompKey","median_baseprice")]
      wb_Inter_comp_prd_price_data<-reshape2::dcast(wb_Inter_comp_prd_price_data,Date~CompKey,value.var="median_baseprice")
    }else{
      col<-t(data.frame(rep(NA,length(unique(new_model_coeff[CompKey %like% "WB_Comp_Interaction_RP"]$CompKey)))))
      if(length(col) > 0){
        colnames(col)<-paste("WB_Comp_Interaction_RP_",1:
                               length(unique(new_model_coeff[CompKey %like% "WB_Comp_Interaction_RP"]$CompKey)),sep="")}
      Date<-"0"
      wb_Inter_comp_prd_price_data<-cbind(Date,col)
    }
    wb_Inter_comp_prd_price_data<-data.frame(wb_Inter_comp_prd_price_data)
    if(sum(colnames(wb_Inter_comp_prd_price_data) %like% "WB_Comp_Interaction_RP_")<length(unique(new_model_coeff[CompKey %like% "WB_Comp_Interaction_RP"]$CompKey))){
      col<-t(data.frame(rep(NA,length(unique(new_model_coeff[CompKey %like% "WB_Comp_Interaction_RP"]$CompKey))-sum(names(wb_Inter_comp_prd_price_data) %like% "WB_Comp_Interaction_RP_"))))
      colnames(col)<-paste("WB_Comp_Interaction_RP_",(sum(names(wb_Inter_comp_prd_price_data) %like% "WB_Comp_Interaction_RP_")+1):
                             length(unique(new_model_coeff[CompKey %like% "WB_Comp_Interaction_RP"]$CompKey)),sep="")
      wb_Inter_comp_prd_price_data<-cbind(wb_Inter_comp_prd_price_data,col)
    }
    colwb<-sort(names(wb_Inter_comp_prd_price_data)[names(wb_Inter_comp_prd_price_data) %like% "WB_Comp_Interaction_RP_"])
    
    if(length(colwb) > 0){
      wb_Inter_comp_prd_price_data<-wb_Inter_comp_prd_price_data[,c("Date",colwb)]}
    # Interaction TPR ####
    wb_Inter_comp_prd_data_tmp<-new_model_coeff[which(new_model_coeff$PPG_Item_No==x & new_model_coeff$CompKey %like% "WB_Comp_Interaction_TPR"),]
    wb_Inter_comp_prd_data_tmp$Acting_PPG_Item_No<-ifelse(wb_Inter_comp_prd_data_tmp$acting_box=="Retailer",
                                                          paste(wb_Inter_comp_prd_data_tmp$Acting_PPG_Item_No,"_Retailer",sep=""),
                                                          paste(wb_Inter_comp_prd_data_tmp$Acting_PPG_Item_No,"_ROM",sep=""))
    wb_Inter_comp_prd_disc_data<-base_dat[which(base_dat$PPG_Market %in% unique(wb_Inter_comp_prd_data_tmp$Acting_PPG_Key)),c("PPG_Item_No","Date","tpr_discount_byppg")]
    wb_Inter_comp_prd_disc_data$CompKey<-as.character(lapply(wb_Inter_comp_prd_disc_data$PPG_Item_No,
                                                             function(x) wb_Inter_comp_prd_data_tmp$CompKey[which(wb_Inter_comp_prd_data_tmp$Acting_PPG_Item_No==x)]))
    wb_Inter_comp_prd_disc_data$PPG_Item_No<-NULL
    if(nrow(wb_Inter_comp_prd_disc_data)>0){
      wb_Inter_comp_prd_disc_data<-wb_Inter_comp_prd_disc_data[,c("Date","CompKey","tpr_discount_byppg")]
      wb_Inter_comp_prd_disc_data<-reshape2::dcast(wb_Inter_comp_prd_disc_data,Date~CompKey,value.var="tpr_discount_byppg")
    }else{
      col<-t(data.frame(rep(NA,length(unique(new_model_coeff[CompKey %like% "WB_Comp_Interaction_TPR"]$CompKey)))))
      if(length(col) > 0){
        colnames(col)<-paste("WB_Comp_Interaction_TPR_",1:
                               length(unique(new_model_coeff[CompKey %like% "WB_Comp_Interaction_TPR"]$CompKey)),sep="")}
      Date<-"0"
      wb_Inter_comp_prd_disc_data<-cbind(Date,col)
    }
    wb_Inter_comp_prd_disc_data<-data.frame(wb_Inter_comp_prd_disc_data)
    if(sum(colnames(wb_Inter_comp_prd_disc_data) %like% "WB_Comp_Interaction_TPR_")<length(unique(new_model_coeff[CompKey %like% "WB_Comp_Interaction_TPR"]$CompKey))){
      col<-t(data.frame(rep(NA,length(unique(new_model_coeff[CompKey %like% "WB_Comp_Interaction_TPR"]$CompKey))-sum(names(wb_Inter_comp_prd_disc_data) %like% "WB_Comp_Interaction_TPR_"))))
      colnames(col)<-paste("WB_Comp_Interaction_TPR_",(sum(names(wb_Inter_comp_prd_disc_data) %like% "WB_Comp_Interaction_TPR_")+1):
                             length(unique(new_model_coeff[CompKey %like% "WB_Comp_Interaction_TPR"]$CompKey)),sep="")
      wb_Inter_comp_prd_disc_data<-cbind(wb_Inter_comp_prd_disc_data,col)
    }
    colwb<-sort(names(wb_Inter_comp_prd_disc_data)[names(wb_Inter_comp_prd_disc_data) %like% "WB_Comp_Interaction_TPR_"])
    if(length(colwb) > 0){
      wb_Inter_comp_prd_disc_data<-wb_Inter_comp_prd_disc_data[,c("Date",colwb)]}
    
    # WB EDLP ####
    wb_comp_prd_data_tmp<-new_model_coeff[which(new_model_coeff$PPG_Item_No==x & new_model_coeff$CompKey %like% "WB_Comp_RP"),]
    wb_comp_prd_data_tmp$Acting_PPG_Item_No<-ifelse(wb_comp_prd_data_tmp$acting_box=="Retailer",
                                                    paste(wb_comp_prd_data_tmp$Acting_PPG_Item_No,"_Retailer",sep=""),
                                                    paste(wb_comp_prd_data_tmp$Acting_PPG_Item_No,"_ROM",sep=""))
    wb_comp_prd_price_data<-base_dat[which(base_dat$PPG_Market %in% unique(wb_comp_prd_data_tmp$Acting_PPG_Key)),c("PPG_Item_No","Date","median_baseprice")]
    wb_comp_prd_price_data$CompKey<-as.character(lapply(wb_comp_prd_price_data$PPG_Item_No,
                                                        function(x) wb_comp_prd_data_tmp$CompKey[which(wb_comp_prd_data_tmp$Acting_PPG_Item_No==x)]))
    wb_comp_prd_price_data$PPG_Item_No<-NULL
    if(nrow(wb_comp_prd_price_data)>0){
      wb_comp_prd_price_data<-wb_comp_prd_price_data[,c("Date","CompKey","median_baseprice")]
      wb_comp_prd_price_data<-reshape2::dcast(wb_comp_prd_price_data,Date~CompKey,value.var="median_baseprice")
    }else{
      col<-t(data.frame(rep(NA,length(unique(new_model_coeff[CompKey %like% "WB_Comp_RP"]$CompKey)))))
      if(length(col) > 0){
        colnames(col)<-paste("WB_Comp_RP_",1:
                               length(unique(new_model_coeff[CompKey %like% "WB_Comp_RP"]$CompKey)),sep="")}
      Date<-"0"
      wb_comp_prd_price_data<-cbind(Date,col)
    }
    wb_comp_prd_price_data<-data.frame(wb_comp_prd_price_data)
    if(sum(colnames(wb_comp_prd_price_data) %like% "WB_Comp_RP_")<length(unique(new_model_coeff[CompKey %like% "WB_Comp_RP"]$CompKey))){
      col<-t(data.frame(rep(NA,length(unique(new_model_coeff[CompKey %like% "WB_Comp_RP"]$CompKey))-sum(names(wb_comp_prd_price_data) %like% "WB_Comp_RP_"))))
      colnames(col)<-paste("WB_Comp_RP_",(sum(names(wb_comp_prd_price_data) %like% "WB_Comp_RP_")+1):
                             length(unique(new_model_coeff[CompKey %like% "WB_Comp_RP"]$CompKey)),sep="")
      wb_comp_prd_price_data<-cbind(wb_comp_prd_price_data,col)
    }
    colwb<-sort(names(wb_comp_prd_price_data)[names(wb_comp_prd_price_data) %like% "WB"])
    if(length(colwb) > 0){
      wb_comp_prd_price_data<-wb_comp_prd_price_data[,c("Date",colwb)]}
    #print(colwb)
    # WB TPR ####
    wb_comp_prd_data_tmp<-new_model_coeff[which(new_model_coeff$PPG_Item_No==x & new_model_coeff$CompKey %like% "WB_Comp_TPR"),]
    wb_comp_prd_data_tmp$Acting_PPG_Item_No<-ifelse(wb_comp_prd_data_tmp$acting_box=="Retailer",
                                                    paste(wb_comp_prd_data_tmp$Acting_PPG_Item_No,"_Retailer",sep=""),
                                                    paste(wb_comp_prd_data_tmp$Acting_PPG_Item_No,"_ROM",sep=""))
    wb_comp_prd_disc_data<-base_dat[which(base_dat$PPG_Market %in% unique(wb_comp_prd_data_tmp$Acting_PPG_Key)),c("PPG_Item_No","Date","tpr_discount_byppg")]
    wb_comp_prd_disc_data$CompKey<-as.character(lapply(wb_comp_prd_disc_data$PPG_Item_No,
                                                       function(x) wb_comp_prd_data_tmp$CompKey[which(wb_comp_prd_data_tmp$Acting_PPG_Item_No==x)]))
    wb_comp_prd_disc_data$PPG_Item_No<-NULL
    if(nrow(wb_comp_prd_disc_data)>0){
      wb_comp_prd_disc_data<-wb_comp_prd_disc_data[,c("Date","CompKey","tpr_discount_byppg")]
      wb_comp_prd_disc_data<-reshape2::dcast(wb_comp_prd_disc_data,Date~CompKey,value.var="tpr_discount_byppg")
    }else{
      col<-t(data.frame(rep(NA,length(unique(new_model_coeff[CompKey %like% "WB_Comp_TPR"]$CompKey)))))
      if(length(col) > 0){
        colnames(col)<-paste("WB_Comp_TPR_",1:
                               length(unique(new_model_coeff[CompKey %like% "WB_Comp_TPR"]$CompKey)),sep="")}
      Date<-"0"
      wb_comp_prd_disc_data<-cbind(Date,col)
    }
    wb_comp_prd_disc_data<-data.frame(wb_comp_prd_disc_data)
    if(sum(colnames(wb_comp_prd_disc_data) %like% "WB_Comp_TPR_")<length(unique(new_model_coeff[CompKey %like% "WB_Comp_TPR"]$CompKey))){
      col<-t(data.frame(rep(NA,length(unique(new_model_coeff[CompKey %like% "WB_Comp_TPR"]$CompKey))-sum(names(wb_comp_prd_disc_data) %like% "WB_Comp_TPR_"))))
      colnames(col)<-paste("WB_Comp_TPR_",(sum(names(wb_comp_prd_disc_data) %like% "WB_Comp_TPR_")+1):
                             length(unique(new_model_coeff[CompKey %like% "WB_Comp_TPR"]$CompKey)),sep="")
      wb_comp_prd_disc_data<-cbind(wb_comp_prd_disc_data,col)
    }
    colwb<-sort(names(wb_comp_prd_disc_data)[names(wb_comp_prd_disc_data) %like% "WB_Comp_TPR_"])
    if(length(colwb) > 0){
      wb_comp_prd_disc_data<-wb_comp_prd_disc_data[,c("Date",colwb)]}
    
    # CrossBox EDLP ####
    cb_comp_prd_data_tmp<-new_model_coeff[which(new_model_coeff$PPG_Item_No==x & new_model_coeff$CompKey %like% "CB_Comp_RP_"),]
    cb_comp_prd_data_tmp$Acting_PPG_Item_No<-ifelse(cb_comp_prd_data_tmp$acting_box=="Retailer",
                                                    paste(cb_comp_prd_data_tmp$Acting_PPG_Item_No,"_Retailer",sep=""),
                                                    paste(cb_comp_prd_data_tmp$Acting_PPG_Item_No,"_ROM",sep=""))
    
    cb_comp_prd_price_data<-base_dat[which(base_dat$PPG_Market %in% unique(cb_comp_prd_data_tmp$Acting_PPG_Key)),c("PPG_Item_No","Date","median_baseprice")]
    cb_comp_prd_price_data$CompKey<-as.character(lapply(cb_comp_prd_price_data$PPG_Item_No,
                                                        function(x) cb_comp_prd_data_tmp$CompKey[which(cb_comp_prd_data_tmp$Acting_PPG_Item_No==x)]))
    cb_comp_prd_price_data$PPG_Item_No<-NULL
    if(nrow(cb_comp_prd_price_data)>0){
      cb_comp_prd_price_data<-cb_comp_prd_price_data[,c("Date","CompKey","median_baseprice")]
      cb_comp_prd_price_data<-reshape2::dcast(cb_comp_prd_price_data,Date~CompKey,value.var="median_baseprice")
    }else{
      col<-t(data.frame(rep(NA,length(unique(new_model_coeff[CompKey %like% "CB_Comp_RP"]$CompKey)))))
      if(length(col) > 0){
        colnames(col)<-paste("CB_Comp_RP_",1:
                               length(unique(new_model_coeff[CompKey %like% "CB_Comp_RP"]$CompKey)),sep="")
        
      }
      Date<-"0"
      cb_comp_prd_price_data<-cbind(Date,col)
    }
    cb_comp_prd_price_data<-data.frame(cb_comp_prd_price_data)
    if(sum(colnames(cb_comp_prd_price_data) %like% "CB_Comp_RP_")<length(unique(new_model_coeff[CompKey %like% "CB_Comp_RP"]$CompKey))){
      col<-t(data.frame(rep(NA,length(unique(new_model_coeff[CompKey %like% "CB_Comp_RP"]$CompKey))-sum(names(cb_comp_prd_price_data) %like% "CB_Comp_RP_"))))
      colnames(col)<-paste("CB_Comp_RP_",(sum(names(cb_comp_prd_price_data) %like% "CB_Comp_RP_")+1):
                             length(unique(new_model_coeff[CompKey %like% "CB_Comp_RP"]$CompKey)),sep="")
      cb_comp_prd_price_data<-cbind(cb_comp_prd_price_data,col)
    }
    colcb<-sort(names(cb_comp_prd_price_data)[names(cb_comp_prd_price_data) %like% "CB_Comp_RP_"])
    if(length(colcb) > 0){
      cb_comp_prd_price_data<-cb_comp_prd_price_data[,c("Date",colcb)]}
    # CrossBox TPR ####
    cb_comp_prd_data_tmp<-new_model_coeff[which(new_model_coeff$PPG_Item_No==x & new_model_coeff$CompKey %like% "CB_Comp_TPR_"),]
    cb_comp_prd_data_tmp$Acting_PPG_Item_No<-ifelse(cb_comp_prd_data_tmp$acting_box=="Retailer",
                                                    paste(cb_comp_prd_data_tmp$Acting_PPG_Item_No,"_Retailer",sep=""),
                                                    paste(cb_comp_prd_data_tmp$Acting_PPG_Item_No,"_ROM",sep=""))
    
    cb_comp_prd_disc_data<-base_dat[which(base_dat$PPG_Market %in% unique(cb_comp_prd_data_tmp$Acting_PPG_Key)),c("PPG_Item_No","Date","tpr_discount_byppg")]
    cb_comp_prd_disc_data$CompKey<-as.character(lapply(cb_comp_prd_disc_data$PPG_Item_No,
                                                       function(x) cb_comp_prd_data_tmp$CompKey[which(cb_comp_prd_data_tmp$Acting_PPG_Item_No==x)]))
    cb_comp_prd_disc_data$PPG_Item_No<-NULL
    if(nrow(cb_comp_prd_disc_data)>0){
      cb_comp_prd_disc_data<-cb_comp_prd_disc_data[,c("Date","CompKey","tpr_discount_byppg")]
      cb_comp_prd_disc_data<-reshape2::dcast(cb_comp_prd_disc_data,Date~CompKey,value.var="tpr_discount_byppg")
    }else{
      col<-t(data.frame(rep(NA,length(unique(new_model_coeff[CompKey %like% "CB_Comp_TPR"]$CompKey)))))
      if(length(col) > 0){
        colnames(col)<-paste("CB_Comp_TPR_",1:
                               length(unique(new_model_coeff[CompKey %like% "CB_Comp_TPR"]$CompKey)),sep="")}
      Date<-"0"
      cb_comp_prd_disc_data<-cbind(Date,col)
    }
    cb_comp_prd_disc_data<-data.frame(cb_comp_prd_disc_data)
    if(sum(colnames(cb_comp_prd_disc_data) %like% "CB_Comp_TPR_")<length(unique(new_model_coeff[CompKey %like% "CB_Comp_TPR"]$CompKey))){
      col<-t(data.frame(rep(NA,length(unique(new_model_coeff[CompKey %like% "CB_Comp_TPR"]$CompKey))-sum(names(cb_comp_prd_disc_data) %like% "CB_Comp_TPR_"))))
      colnames(col)<-paste("CB_Comp_TPR_",(sum(names(cb_comp_prd_disc_data) %like% "CB_Comp_TPR_")+1):
                             length(unique(new_model_coeff[CompKey %like% "CB_Comp_TPR"]$CompKey)),sep="")
      cb_comp_prd_disc_data<-cbind(cb_comp_prd_disc_data,col)
    }
    colcb<-sort(names(cb_comp_prd_disc_data)[names(cb_comp_prd_disc_data) %like% "CB_Comp_TPR_"])
    if(length(colcb) > 0){
      cb_comp_prd_disc_data<-cb_comp_prd_disc_data[,c("Date",colcb)]}
    model_pred_dat_tmp = data.frame(model_pred_dat_tmp)
    # Merging DFs ####
    # wb_Inter_comp_prd_price_data$Date = as.character(wb_Inter_comp_prd_price_data$Date)
    # 
    # wb_Inter_comp_prd_disc_data$Date = as.character(wb_Inter_comp_prd_disc_data$Date)
    # 
    # wb_comp_prd_price_data$Date = as.character(wb_comp_prd_price_data$Date)
    # wb_comp_prd_disc_data$Date = as.character(wb_comp_prd_disc_data$Date)
    # 
    # cb_comp_prd_price_data$Date = as.character(cb_comp_prd_price_data$Date)
    # cb_comp_prd_disc_data$Date = as.character(cb_comp_prd_disc_data$Date)
    
    model_pred_dat_tmp<-merge(model_pred_dat_tmp,wb_Inter_comp_prd_price_data,by.x="Date",by.y="Date",all.x=TRUE)
    model_pred_dat_tmp<-merge(model_pred_dat_tmp,wb_Inter_comp_prd_disc_data,by.x="Date",by.y="Date",all.x=TRUE)
    
    model_pred_dat_tmp<-merge(model_pred_dat_tmp,wb_comp_prd_price_data,by.x="Date",by.y="Date",all.x=TRUE)
    model_pred_dat_tmp<-merge(model_pred_dat_tmp,wb_comp_prd_disc_data,by.x="Date",by.y="Date",all.x=TRUE)
    
    model_pred_dat_tmp<-merge(model_pred_dat_tmp,cb_comp_prd_price_data,by.x="Date",by.y="Date",all.x=TRUE)
    model_pred_dat_tmp<-merge(model_pred_dat_tmp,cb_comp_prd_disc_data,by.x="Date",by.y="Date",all.x=TRUE)
    model_pred_dat_tmp <- data.frame(lapply(model_pred_dat_tmp, as.character), stringsAsFactors=FALSE)
    glbl_model_pred_transformed_dat<<-rbind(glbl_model_pred_transformed_dat,model_pred_dat_tmp)
  })
  
  if(sum(colnames(glbl_model_pred_transformed_dat) %like% "Comp") > 0){
    cnames <- colnames(glbl_model_pred_transformed_dat[,c(colnames(glbl_model_pred_transformed_dat) %like% "Comp")])
    for (i in cnames)
    {
      glbl_model_pred_transformed_dat[[i]] <- as.numeric(as.character(glbl_model_pred_transformed_dat[[i]]))
    }
  }
  
  glbl_model_pred_transformed_dat[is.na(glbl_model_pred_transformed_dat)]<-""
  glbl_model_pred_transformed_dat$base_vol<-as.numeric(glbl_model_pred_transformed_dat$base_vol)
  glbl_model_pred_transformed_dat$pred_vol<- as.numeric(glbl_model_pred_transformed_dat$pred_vol)
  glbl_model_pred_transformed_dat$wk_sold_avg_price_byppg = as.numeric(glbl_model_pred_transformed_dat$wk_sold_avg_price_byppg)
  glbl_model_pred_transformed_dat_general = glbl_model_pred_transformed_dat
  #glbl_model_pred_transformed_dat = glbl_model_pred_transformed_dat_general
  
  if(file_type=="Optimizer"){
    load(paste0(base_dir,"/Output Files/RMS_Data_PPG_",Retailer,".RData"))
    RMS_Data_PPG_2 = data.frame(RMS_Data_PPG_2)
    RMS_Data_PPG_2=RMS_Data_PPG_2[,c("Date","PPG_Category","PPG_MFG","PPGName","PPG_Retailer","ListCost","OffInvoice","EachesLC","EachesOI","ImputedLC_retailer")]
    glbl_model_pred_transformed_dat$PPG_Retailer=ifelse((grepl("_ROM", glbl_model_pred_transformed_dat$PPG_Item_No)),"ROM","Retailer")
    RMS_Data_PPG_2$PPGName = gsub(" ","",RMS_Data_PPG_2$PPGName)
    RMS_Data_PPG_2$PPGName = gsub("\\.|_|-","",RMS_Data_PPG_2$PPGName)
    RMS_Data_PPG_2$PPG_Category = gsub("_"," ",RMS_Data_PPG_2$PPG_Category)
    RMS_Data_PPG_2$PPG_Category = ifelse(RMS_Data_PPG_2$PPG_Category=="LITTER","CAT LITTER",RMS_Data_PPG_2$PPG_Category)
    glbl_model_pred_transformed_dat$PPG_Description_temp = ifelse(glbl_model_pred_transformed_dat$PPG_Cat == 'DRY CAT',paste0('DC',gsub('^DC',"",glbl_model_pred_transformed_dat$PPG_Description)),
                                                                                 ifelse(glbl_model_pred_transformed_dat$PPG_Cat == 'DOG TREATS',paste0('DT',gsub('^DT',"",glbl_model_pred_transformed_dat$PPG_Description)),
                                                                                        ifelse(glbl_model_pred_transformed_dat$PPG_Cat == 'CAT LITTER',paste0('CL',gsub('^CL',"",glbl_model_pred_transformed_dat$PPG_Description)),
                                                                                               ifelse(glbl_model_pred_transformed_dat$PPG_Cat == 'WET DOG',paste0('DW',gsub('^DW',"",glbl_model_pred_transformed_dat$PPG_Description)),
                                                                                                      ifelse(glbl_model_pred_transformed_dat$PPG_Cat == 'CAT TREATS',paste0('CT',gsub('^CT',"",glbl_model_pred_transformed_dat$PPG_Description)),
                                                                                                             ifelse(glbl_model_pred_transformed_dat$PPG_Cat == 'DRY DOG',paste0('DD',gsub('^DD',"",glbl_model_pred_transformed_dat$PPG_Description)),
                                                                                                                    paste0('CW',gsub('^CW',"",glbl_model_pred_transformed_dat$PPG_Description))))))))
    glbl_model_pred_transformed_dat=unique(merge(RMS_Data_PPG_2,glbl_model_pred_transformed_dat,
                                                 by.y=c("PPG_Description_temp","Date","PPG_MFG","PPG_Retailer","PPG_Cat"),
                                                 by.x=c("PPGName","Date","PPG_MFG","PPG_Retailer","PPG_Category"),all.y=TRUE))
    setnames(glbl_model_pred_transformed_dat,"PPG_Category","PPG_Cat")
    glbl_model_pred_transformed_dat$PPGName = NULL
    glbl_model_pred_transformed_dat$ListCost=ifelse(glbl_model_pred_transformed_dat$ListCost=="",NA,glbl_model_pred_transformed_dat$ListCost)
    
    # glbl_model_pred_transformed_dat = glbl_model_pred_transformed_dat%>% group_by(PPG_Item_No) %>%  mutate(ListCost = zoo::na.locf0(ListCost))
    # glbl_model_pred_transformed_dat = glbl_model_pred_transformed_dat%>% group_by(PPG_Item_No) %>%  mutate(ListCost = zoo::na.locf0(ListCost,fromLast=T))
    # 
    glbl_model_pred_transformed_dat$ListCost = ifelse(glbl_model_pred_transformed_dat$Final_baseprice==0,0,glbl_model_pred_transformed_dat$ListCost)
    glbl_model_pred_transformed_dat$OffInvoice = ifelse(glbl_model_pred_transformed_dat$Final_baseprice==0,0,glbl_model_pred_transformed_dat$OffInvoice)
    glbl_model_pred_transformed_dat$wk_sold_doll_byppg = as.numeric(glbl_model_pred_transformed_dat$wk_sold_doll_byppg)
    
   
    # #newedit:subhash
    # glbl_model_pred_transformed_dat<- setDT(glbl_model_pred_transformed_dat)
    # temp<- glbl_model_pred_transformed_dat[(year(Date)==max(year(Date)))&(quarter(Date)==4),]
    # temp<- temp[, .('mean_FBP'= mean(as.numeric(Final_baseprice), na.rm=T),
    #                 'mean_LC'=mean(ListCost, na.rm=T)),
    #             by= c("PPG_Description","PPG_Retailer")]
    # temp$LC_as_percetange_of_BP<- temp$mean_LC/temp$mean_FBP
    # temp<- temp[(LC_as_percetange_of_BP<0.1|LC_as_percetange_of_BP>2),]
    # temp$LC_violation_flag<-1
    # temp<- temp[,-c("mean_FBP", "mean_LC")]
    # ####
    # 
    # glbl_model_pred_transformed_dat<- merge(glbl_model_pred_transformed_dat, temp,by= c('PPG_Description', "PPG_Retailer"), all.x=T)
    # glbl_model_pred_transformed_dat$LC_violation_flag[is.na(glbl_model_pred_transformed_dat$LC_violation_flag)] = 0
    # glbl_model_pred_transformed_dat<- setDT(glbl_model_pred_transformed_dat)[,'ListCost':=ifelse(LC_violation_flag==1,NA, ListCost)]
    # glbl_model_pred_transformed_dat<- glbl_model_pred_transformed_dat[, -c('LC_violation_flag')]
    
    temp1=glbl_model_pred_transformed_dat%>%filter(is.na(ImputedLC_retailer))%>%group_by(PPG_Cat,PPG_Item_No)%>%summarise(LC=sum(ListCost*EachesLC,na.rm=T)/
                                                                                                                            sum(EachesLC, na.rm=T),
                                                                                                                          OI=sum(OffInvoice*EachesOI,na.rm=T)/
                                                                                                                            sum(EachesOI, na.rm=T),
                                                                                                                          FBP=sum(as.numeric(Final_baseprice)*wk_sold_doll_byppg)/
                                                                                                                            sum(wk_sold_doll_byppg))
    
    temp2=glbl_model_pred_transformed_dat%>%filter(ImputedLC_retailer==1)%>%group_by(PPG_Cat,PPG_Item_No)%>%summarise(LC=mean(ListCost, na.rm=T),
                                                                                                                      OI=NA,
                                                                                                                      FBP=sum(as.numeric(Final_baseprice)*wk_sold_doll_byppg)/
                                                                                                                        sum(wk_sold_doll_byppg))
    
    temp=rbind(setDT(temp1),setDT(temp2))
    temp= setDT(temp)
    temp$OI= ifelse(is.nan(temp$OI)|(is.infinite(temp$OI)),NA, temp$OI)
    temp= temp[,'NetCost':=ifelse(is.na(OI),LC, LC+OI )]
    #temp$LC_as_percetange_of_BP=temp$LC/temp$FBP
    temp$NetCost_as_percentage_of_BP=temp$NetCost/temp$FBP
    temp$Flag= ifelse(temp$NetCost_as_percentage_of_BP>1.1 | temp$NetCost_as_percentage_of_BP<0.1,1,0)
    temp=setDT(temp)
    temp<- temp[,-c("LC","FBP","NetCost_as_percentage_of_BP","OI","EachesLC","EachesOI","NetCost")]
    
    glbl_model_pred_transformed_dat<- merge(glbl_model_pred_transformed_dat, temp,by= c('PPG_Item_No', "PPG_Cat"), all.x=T)
    glbl_model_pred_transformed_dat<- setDT(glbl_model_pred_transformed_dat)[,'ListCost':=ifelse(Flag==1,NA, ListCost)]
    glbl_model_pred_transformed_dat=setDT(glbl_model_pred_transformed_dat)
    
    glbl_model_pred_transformed_dat<- glbl_model_pred_transformed_dat[, -c('Flag')]
    
    glbl_model_pred_transformed_dat=as.data.frame(glbl_model_pred_transformed_dat)
    
    
    glbl_model_pred_transformed_dat$OffInvoice = ifelse(is.na(glbl_model_pred_transformed_dat$ListCost),0,
                                                        glbl_model_pred_transformed_dat$OffInvoice)
    glbl_model_pred_transformed_dat$OffInvoice = ifelse(glbl_model_pred_transformed_dat$ListCost==0,0,
                                                        glbl_model_pred_transformed_dat$OffInvoice)
    
    glbl_model_pred_transformed_dat$OffInvoice = ifelse(is.na(glbl_model_pred_transformed_dat$OffInvoice),0,glbl_model_pred_transformed_dat$OffInvoice)
    
    
    glbl_model_pred_transformed_dat$OffInvoice = ifelse(glbl_model_pred_transformed_dat$OffInvoice < 0, glbl_model_pred_transformed_dat$OffInvoice*(-1), glbl_model_pred_transformed_dat$OffInvoice)
    #setnames(glbl_model_pred_transformed_dat,"PPGName","PPG_Description")
    glbl_model_pred_transformed_dat$lift_vol<-glbl_model_pred_transformed_dat$pred_vol-glbl_model_pred_transformed_dat$base_vol
    glbl_model_pred_transformed_dat$base_vol<-ifelse(glbl_model_pred_transformed_dat$lift_vol<=0,
                                                     glbl_model_pred_transformed_dat$pred_vol,
                                                     glbl_model_pred_transformed_dat$base_vol)
    glbl_model_pred_transformed_dat$lift_vol<-ifelse(glbl_model_pred_transformed_dat$lift_vol<0,0,glbl_model_pred_transformed_dat$lift_vol)
    
    glbl_model_pred_transformed_dat$base_doll<-as.numeric(as.character(glbl_model_pred_transformed_dat$base_vol))*as.numeric(as.character(glbl_model_pred_transformed_dat$Final_baseprice))
    #glbl_model_pred_transformed_dat1 <- glbl_model_pred_transformed_dat[glbl_model_pred_transformed_dat$PPG_Item_No=="ITEM52CTFRISKIESPARTYMIXCR_Retailer",]
    #glbl_model_pred_transformed_dat1$wk_sold_doll_byppg_actual <- as.numeric(glbl_model_pred_transformed_dat1$wk_sold_qty_byppg)*as.numeric(glbl_model_pred_transformed_dat1$wk_sold_avg_price_byppg)
    
    glbl_model_pred_transformed_dat$wk_sold_doll_byppg<-glbl_model_pred_transformed_dat$pred_vol*glbl_model_pred_transformed_dat$wk_sold_avg_price_byppg
    glbl_model_pred_transformed_dat$lift_doll<-glbl_model_pred_transformed_dat$wk_sold_doll_byppg-glbl_model_pred_transformed_dat$base_doll
    glbl_model_pred_transformed_dat$lift_doll<-ifelse(glbl_model_pred_transformed_dat$lift_doll<0,0,glbl_model_pred_transformed_dat$lift_doll)
    glbl_model_pred_transformed_dat$base_doll<-ifelse(glbl_model_pred_transformed_dat$lift_doll<=0,
                                                      glbl_model_pred_transformed_dat$wk_sold_doll_byppg,
                                                      glbl_model_pred_transformed_dat$base_doll)
    
    ##purina sales and spend
    glbl_model_pred_transformed_dat$Purina_sales = glbl_model_pred_transformed_dat$ListCost * as.numeric(glbl_model_pred_transformed_dat$wk_sold_qty_byppg)
    glbl_model_pred_transformed_dat$Purina_spend = glbl_model_pred_transformed_dat$OffInvoice * as.numeric(glbl_model_pred_transformed_dat$wk_sold_qty_byppg)
    glbl_model_pred_transformed_dat$Purina_Base_sales = glbl_model_pred_transformed_dat$ListCost * as.numeric(glbl_model_pred_transformed_dat$base_vol)
    glbl_model_pred_transformed_dat$Purina_Base_sales<-ifelse(glbl_model_pred_transformed_dat$Purina_sales<glbl_model_pred_transformed_dat$Purina_Base_sales,
                                                              glbl_model_pred_transformed_dat$Purina_sales,
                                                              glbl_model_pred_transformed_dat$Purina_Base_sales)
    glbl_model_pred_transformed_dat$Purina_ratf = as.numeric(glbl_model_pred_transformed_dat$Purina_sales) - as.numeric(glbl_model_pred_transformed_dat$Purina_spend)
    
    #glbl_model_pred_transformed_dat$Purina_Gross_Lift = glbl_model_pred_transformed_dat$Purina_sales-as.numeric(glbl_model_pred_transformed_dat$Purina_Base_sales)
    glbl_model_pred_transformed_dat$Purina_Gross_Lift = (glbl_model_pred_transformed_dat$ListCost-(as.numeric(glbl_model_pred_transformed_dat$Estimated_baseprice)-as.numeric(glbl_model_pred_transformed_dat$wk_sold_avg_price_byppg)))*
                                                        (as.numeric(glbl_model_pred_transformed_dat$wk_sold_qty_byppg)-as.numeric(glbl_model_pred_transformed_dat$base_vol))
    
    glbl_model_pred_transformed_dat$Purina_Gross_Lift = ifelse(glbl_model_pred_transformed_dat$Purina_Gross_Lift<0,0,glbl_model_pred_transformed_dat$Purina_Gross_Lift)
    glbl_model_pred_transformed_dat$Purina_True_Lift = glbl_model_pred_transformed_dat$Purina_Gross_Lift
    
    
    glbl_model_pred_transformed_dat=as.data.frame(glbl_model_pred_transformed_dat)
    glbl_model_pred_transformed_dat<-cannibal_report(cannibal_dat_filename,
                                                     MappingFile_Description_path,
                                                     glbl_model_pred_transformed_dat,
                                                     cannibal_dat_op_filename,
                                                     Retailer,
                                                     Category, "optimizer")
    #d <- Output_Data_Hist[(Output_Data_Hist$Cross)=="",]
    glbl_model_pred_transformed_dat[is.na(glbl_model_pred_transformed_dat)]<-""
    
    
    glbl_model_pred_transformed_dat$Spend <- as.numeric(glbl_model_pred_transformed_dat$pred_vol) * (as.numeric(glbl_model_pred_transformed_dat$Final_baseprice) - as.numeric(glbl_model_pred_transformed_dat$wk_sold_avg_price_byppg))
    
    glbl_model_pred_transformed_dat$Retailer = Retailer
    glbl_model_pred_transformed_dat$Market = ifelse(glbl_model_pred_transformed_dat$PPG_Item_No %like% "Retailer","Retailer","ROM")
    
    Output_Data_Hist <- glbl_model_pred_transformed_dat
    Output_Data_Hist <- Output_Data_Hist[!colnames(Output_Data_Hist)%like% "_Comp_"]
    Output_Data_Hist$Retailer <- retailer_format_fn(Output_Data_Hist$Retailer)
    Output_Data_Hist$PPG_Cat <- category_format_fn(Output_Data_Hist$PPG_Cat)
    
    #### Imputing Missing weeks ####
    basedata <- copy(Output_Data_Hist) 
    setDT(basedata)
    ppg_data_frame <- data.frame() ## for creating ppg wise data base
    df_rbind <- data.frame() ## for all ppg value one data frame
    count_dates <- 0 ## for checking the number of dates to be imputed 
    for_date <- basedata[,list(no_of_weeks=n_distinct(Date)),by=c("PPG_Item_No")]
    bd_colnames <-  colnames(basedata)
    bd_colnames <- bd_colnames[!bd_colnames%in%c("PPG_Item_No","Date","PPG_Cat","PPG_MFG","PPG_Description","Retailer","Market",
                                                 "wk_sold_avg_price_byppg","median_baseprice","Final_baseprice","Estimated_baseprice")]
    
    ## pgg with highest number of weeks is considered
    req_ppg<-as.character(for_date[for_date$no_of_weeks==max(for_date$no_of_weeks),]$PPG_Item_No[1])
    # Start_date <- min(as.Date(basedata$Date))
    # End_date <- max(as.Date(basedata$Date))
    # date_range <- data.frame(seq(as.Date(Start_date), as.Date(End_date), by="weeks"))
    date_range<-as.Date(unique(basedata$Date)) ##
    overall_min_date <- min(date_range)
    for (p in unique(basedata$PPG_Item_No)){
      #p <- 	"ITEM30CTFRISKIESPARTYMIXCR_ROM"
      #print(p)
      ppg_data_frame <- basedata[basedata$PPG_Item_No==p,]
      ppg_data_frame$Date <- as.Date(ppg_data_frame$Date)
      date_var <- as.Date(ppg_data_frame$Date)
      current_min_date <- min(date_var)
      ###### date_values variable contains the dates to be imputed
      date_values <- date_range[!date_range%in%date_var]
      count_dates <- count_dates + as.numeric(length(date_values))
      n_rows_to_be_imputed<-length(date_range)-n_distinct(ppg_data_frame$Date)
      
      if (n_rows_to_be_imputed>0)
      {
        Temp_df <- ppg_data_frame[0:n_rows_to_be_imputed,]
        Temp_df$Date <- data.frame(date_values)
        Temp_df$PPG_Item_No <- p
        Temp_df$PPG_Cat <- unique(ppg_data_frame[ppg_data_frame$PPG_Item_No==p,]$PPG_Cat)
        Temp_df$PPG_Description <- unique(ppg_data_frame[ppg_data_frame$PPG_Item_No==p,]$PPG_Description)
        Temp_df$PPG_MFG <- unique(ppg_data_frame[ppg_data_frame$PPG_Item_No==p,]$PPG_MFG)
        Temp_df$PPG_Retailer <- unique(ppg_data_frame[ppg_data_frame$PPG_Item_No==p,]$PPG_Retailer)
        Temp_df$Model_flag <- unique(ppg_data_frame[ppg_data_frame$PPG_Item_No==p,]$Model_flag)
        Temp_df$Retailer <- unique(ppg_data_frame[ppg_data_frame$PPG_Item_No==p,]$Retailer)
        Temp_df$Market <- unique(ppg_data_frame[ppg_data_frame$PPG_Item_No==p,]$Market)

        
        Temp_df[,c("wk_sold_qty_byppg","tpr_discount_byppg","ACV_Feat_Only","ACV_Disp_Only","ACV_Feat_Disp",
                   "ACV_Selling","flag_qtr2","flag_qtr3","flag_qtr4","category_trend","tpr_discount_byppg_lag1",
                   "tpr_discount_byppg_lag2","monthno","pred_vol","base_vol","lift_vol","base_doll",
                   "wk_sold_doll_byppg","lift_doll",              
                   "Cross","Within","true_lift_doll","Spend","OffInvoice","Purina_spend")] <- 0
        
        Temp_df[,c("wk_sold_avg_price_byppg","median_baseprice","Final_baseprice",
                   "Estimated_baseprice","ListCost","Purina_sales","Purina_Base_sales",
                   "Purina_ratf","Purina_Gross_Lift","Purina_True_Lift")] <- NA
        
        ppg_data_frame<-rbind(ppg_data_frame,Temp_df)
        ### sorting it based on the date
        ppg_data_frame <- ppg_data_frame[order(as.Date(ppg_data_frame$Date, format="%Y/%m/%d")),]
        # If Starting weeks data is not avl making price points as 0
        if(current_min_date!=overall_min_date){
          
          Starting_missing_weeks <- date_range[date_range<current_min_date]
          #ppg_data_frame <- ppg_data_frame[!ppg_data_frame$Date %in% Starting_missing_weeks]
          ppg_data_frame$wk_sold_avg_price_byppg[ppg_data_frame$Date %in% Starting_missing_weeks] <- 0
          ppg_data_frame$median_baseprice[ppg_data_frame$Date %in% Starting_missing_weeks] <- 0
          ppg_data_frame$Final_baseprice[ppg_data_frame$Date %in% Starting_missing_weeks] <- 0
          ppg_data_frame$Estimated_baseprice[ppg_data_frame$Date %in% Starting_missing_weeks] <- 0
          
        }
        
        #Imputing succeeding weekS Price points for NA Price points
        ppg_data_frame = ppg_data_frame %>% group_by(PPG_Item_No) %>%  mutate(Final_baseprice = zoo::na.locf0(Final_baseprice))
        ppg_data_frame = ppg_data_frame %>% group_by(PPG_Item_No) %>%  mutate(wk_sold_avg_price_byppg = zoo::na.locf0(wk_sold_avg_price_byppg))
        ppg_data_frame = ppg_data_frame %>% group_by(PPG_Item_No) %>%  mutate(median_baseprice = zoo::na.locf0(median_baseprice))
        ppg_data_frame = ppg_data_frame %>% group_by(PPG_Item_No) %>%  mutate(Estimated_baseprice = zoo::na.locf0(Estimated_baseprice))
        ppg_data_frame = ppg_data_frame%>% group_by(PPG_Item_No) %>%  mutate(ListCost = zoo::na.locf0(ListCost))
        #ppg_data_frame = ppg_data_frame%>% group_by(PPG_Item_No) %>%  mutate(OffInvoice = zoo::na.locf0(OffInvoice,fromLast=T))
        ppg_data_frame$OffInvoice = ifelse(is.na(ppg_data_frame$OffInvoice),0,ppg_data_frame$OffInvoice)
        ppg_data_frame$Purina_sales = ifelse(is.na(ppg_data_frame$ListCost),NA,ppg_data_frame$Purina_sales)
        ppg_data_frame$Purina_Base_sales = ifelse(is.na(ppg_data_frame$ListCost),NA,ppg_data_frame$Purina_Base_sales)
        ppg_data_frame$Purina_ratf = ifelse(is.na(ppg_data_frame$ListCost),NA,ppg_data_frame$Purina_ratf)
        
        ppg_data_frame <- data.frame(ppg_data_frame)
      
        
        }
      
      df_rbind <- rbind(df_rbind,ppg_data_frame)
      #print("df_bind")
    }
    
    Output_Data_Hist <- copy(df_rbind)
    
    
  }else if(file_type=="General"){
    #### making actual dollar sales as a column
    load(paste0(base_dir,"/Output Files/RMS_Data_PPG_",Retailer,".RData"))
    RMS_Data_PPG_2 = data.frame(RMS_Data_PPG_2)
    RMS_Data_PPG_2=RMS_Data_PPG_2[,c("Date","PPG_Category","PPG_MFG","PPGName","PPG_Retailer","ListCost","OffInvoice","EachesLC","EachesOI","ImputedLC_retailer")]
    glbl_model_pred_transformed_dat$PPG_Retailer=ifelse((grepl("_ROM", glbl_model_pred_transformed_dat$PPG_Item_No)),"ROM","Retailer")
    RMS_Data_PPG_2$PPGName = gsub(" ","",RMS_Data_PPG_2$PPGName)
    RMS_Data_PPG_2$PPGName = gsub("\\.|_|-","",RMS_Data_PPG_2$PPGName)
    RMS_Data_PPG_2$PPG_Category = gsub("_"," ",RMS_Data_PPG_2$PPG_Category)
    RMS_Data_PPG_2$PPG_Category = ifelse(RMS_Data_PPG_2$PPG_Category=="LITTER","CAT LITTER",RMS_Data_PPG_2$PPG_Category)
    glbl_model_pred_transformed_dat$PPG_Description_temp = ifelse(glbl_model_pred_transformed_dat$PPG_Cat == 'DRY CAT',paste0('DC',gsub('^DC',"",glbl_model_pred_transformed_dat$PPG_Description)),
                                                                  ifelse(glbl_model_pred_transformed_dat$PPG_Cat == 'DOG TREATS',paste0('DT',gsub('^DT',"",glbl_model_pred_transformed_dat$PPG_Description)),
                                                                         ifelse(glbl_model_pred_transformed_dat$PPG_Cat == 'CAT LITTER',paste0('CL',gsub('^CL',"",glbl_model_pred_transformed_dat$PPG_Description)),
                                                                                ifelse(glbl_model_pred_transformed_dat$PPG_Cat == 'WET DOG',paste0('DW',gsub('^DW',"",glbl_model_pred_transformed_dat$PPG_Description)),
                                                                                       ifelse(glbl_model_pred_transformed_dat$PPG_Cat == 'CAT TREATS',paste0('CT',gsub('^CT',"",glbl_model_pred_transformed_dat$PPG_Description)),
                                                                                              ifelse(glbl_model_pred_transformed_dat$PPG_Cat == 'DRY DOG',paste0('DD',gsub('^DD',"",glbl_model_pred_transformed_dat$PPG_Description)),
                                                                                                     paste0('CW',gsub('^CW',"",glbl_model_pred_transformed_dat$PPG_Description))))))))
    
    glbl_model_pred_transformed_dat=unique(merge(RMS_Data_PPG_2,glbl_model_pred_transformed_dat,
                                                 by.y=c("PPG_Description_temp","Date","PPG_MFG","PPG_Retailer","PPG_Cat"),
                                                 by.x=c("PPGName","Date","PPG_MFG","PPG_Retailer","PPG_Category"),all.y=TRUE))
    setnames(glbl_model_pred_transformed_dat,"PPG_Category","PPG_Cat")
    glbl_model_pred_transformed_dat$PPGName = NULL
    glbl_model_pred_transformed_dat$ListCost=ifelse(glbl_model_pred_transformed_dat$ListCost=="",NA,glbl_model_pred_transformed_dat$ListCost)
    
    # glbl_model_pred_transformed_dat = glbl_model_pred_transformed_dat%>% group_by(PPG_Item_No) %>%  mutate(ListCost = zoo::na.locf0(ListCost))
    # glbl_model_pred_transformed_dat = glbl_model_pred_transformed_dat%>% group_by(PPG_Item_No) %>%  mutate(ListCost = zoo::na.locf0(ListCost,fromLast=T))
    # 
    glbl_model_pred_transformed_dat$ListCost = ifelse(glbl_model_pred_transformed_dat$Final_baseprice==0,0,glbl_model_pred_transformed_dat$ListCost)
    glbl_model_pred_transformed_dat$OffInvoice = ifelse(glbl_model_pred_transformed_dat$Final_baseprice==0,0,glbl_model_pred_transformed_dat$OffInvoice)
    glbl_model_pred_transformed_dat$wk_sold_doll_byppg = as.numeric(glbl_model_pred_transformed_dat$wk_sold_doll_byppg)
    
    temp1=glbl_model_pred_transformed_dat%>%filter(is.na(ImputedLC_retailer))%>%group_by(PPG_Cat,PPG_Item_No)%>%summarise(LC=sum(ListCost*EachesLC,na.rm=T)/
                                                                                                                            sum(EachesLC, na.rm=T),
                                                                                                                          OI=sum(OffInvoice*EachesOI,na.rm=T)/
                                                                                                                            sum(EachesOI, na.rm=T),
                                                                                                                          FBP=sum(as.numeric(Final_baseprice)*wk_sold_doll_byppg)/
                                                                                                                            sum(wk_sold_doll_byppg))
    
    temp2=glbl_model_pred_transformed_dat%>%filter(ImputedLC_retailer==1)%>%group_by(PPG_Cat,PPG_Item_No)%>%summarise(LC=mean(ListCost, na.rm=T),
                                                                                                                      OI=NA,
                                                                                                                      FBP=sum(as.numeric(Final_baseprice)*wk_sold_doll_byppg)/
                                                                                                                        sum(wk_sold_doll_byppg))
    
    
    temp=rbind(setDT(temp1),setDT(temp2))
    temp= setDT(temp)
    temp$OI= ifelse(is.nan(temp$OI)|(is.infinite(temp$OI)),NA, temp$OI)
    temp= temp[,'NetCost':=ifelse(is.na(OI),LC, LC+OI )]
    #temp$LC_as_percetange_of_BP=temp$LC/temp$FBP
    temp$NetCost_as_percentage_of_BP=temp$NetCost/temp$FBP
    temp$Flag= ifelse(temp$NetCost_as_percentage_of_BP>1.1 | temp$NetCost_as_percentage_of_BP<0.1,1,0)
    temp=setDT(temp)
    temp<- temp[,-c("LC","FBP","NetCost_as_percentage_of_BP","OI","EachesLC","EachesOI","NetCost")]
    
    glbl_model_pred_transformed_dat<- merge(glbl_model_pred_transformed_dat, temp,by= c('PPG_Item_No', "PPG_Cat"), all.x=T)
    glbl_model_pred_transformed_dat<- setDT(glbl_model_pred_transformed_dat)[,'ListCost':=ifelse(Flag==1,NA, ListCost)]
    glbl_model_pred_transformed_dat=setDT(glbl_model_pred_transformed_dat)
    glbl_model_pred_transformed_dat<- glbl_model_pred_transformed_dat[, -c('Flag')]
    
    glbl_model_pred_transformed_dat=as.data.frame(glbl_model_pred_transformed_dat)
    glbl_model_pred_transformed_dat$OffInvoice = ifelse(is.na(glbl_model_pred_transformed_dat$ListCost),0,
                                                        glbl_model_pred_transformed_dat$OffInvoice)
    glbl_model_pred_transformed_dat$OffInvoice = ifelse(glbl_model_pred_transformed_dat$ListCost==0,0,
                                                        glbl_model_pred_transformed_dat$OffInvoice)
    
    glbl_model_pred_transformed_dat$OffInvoice = ifelse(is.na(glbl_model_pred_transformed_dat$OffInvoice),0,glbl_model_pred_transformed_dat$OffInvoice)
    
    
    glbl_model_pred_transformed_dat$OffInvoice = ifelse(glbl_model_pred_transformed_dat$OffInvoice < 0, glbl_model_pred_transformed_dat$OffInvoice*(-1), glbl_model_pred_transformed_dat$OffInvoice)
    
    #setnames(glbl_model_pred_transformed_dat,"PPGName","PPG_Description")
    glbl_model_pred_transformed_dat$actual_dollar_sales = glbl_model_pred_transformed_dat$wk_sold_avg_price_byppg * as.numeric(glbl_model_pred_transformed_dat$wk_sold_qty_byppg)
    
    
    glbl_model_pred_transformed_dat$lift_vol<-glbl_model_pred_transformed_dat$pred_vol-glbl_model_pred_transformed_dat$base_vol
    glbl_model_pred_transformed_dat$base_vol<-ifelse(glbl_model_pred_transformed_dat$lift_vol<=0,
                                                     glbl_model_pred_transformed_dat$pred_vol,
                                                     glbl_model_pred_transformed_dat$base_vol)
    glbl_model_pred_transformed_dat$lift_vol<-ifelse(glbl_model_pred_transformed_dat$lift_vol<0,0,glbl_model_pred_transformed_dat$lift_vol)
    
    glbl_model_pred_transformed_dat$base_doll<-as.numeric(as.character(glbl_model_pred_transformed_dat$base_vol))*as.numeric(as.character(glbl_model_pred_transformed_dat$Final_baseprice))
    #glbl_model_pred_transformed_dat1 <- glbl_model_pred_transformed_dat[glbl_model_pred_transformed_dat$PPG_Item_No=="ITEM52CTFRISKIESPARTYMIXCR_Retailer",]
    #glbl_model_pred_transformed_dat1$wk_sold_doll_byppg_actual <- as.numeric(glbl_model_pred_transformed_dat1$wk_sold_qty_byppg)*as.numeric(glbl_model_pred_transformed_dat1$wk_sold_avg_price_byppg)
    
    glbl_model_pred_transformed_dat$wk_sold_doll_byppg<-glbl_model_pred_transformed_dat$pred_vol*glbl_model_pred_transformed_dat$wk_sold_avg_price_byppg
    
    ### creating a new column called any other sales component for actual - predicted, if actual > predicted
    # glbl_model_pred_transformed_dat$any_other_sales_component = ifelse(glbl_model_pred_transformed_dat$actual_dollar_sales > glbl_model_pred_transformed_dat$wk_sold_doll_byppg,glbl_model_pred_transformed_dat$actual_dollar_sales - glbl_model_pred_transformed_dat$wk_sold_doll_byppg,0)
    # 
    glbl_model_pred_transformed_dat$lift_doll<-glbl_model_pred_transformed_dat$wk_sold_doll_byppg-glbl_model_pred_transformed_dat$base_doll
    glbl_model_pred_transformed_dat$lift_doll<-ifelse(glbl_model_pred_transformed_dat$lift_doll<0,0,glbl_model_pred_transformed_dat$lift_doll)
    glbl_model_pred_transformed_dat$base_doll<-ifelse(glbl_model_pred_transformed_dat$lift_doll<=0,
                                                      glbl_model_pred_transformed_dat$wk_sold_doll_byppg,
                                                      glbl_model_pred_transformed_dat$base_doll)
    
    
    
    ### checking whether actual is more or less than predicted 
    # 0 : actual > predicted
    # 1 : actual< predicted
    # 2 : actual< baseline
    glbl_model_pred_transformed_dat$flag_check = ifelse(glbl_model_pred_transformed_dat$actual_dollar_sales> glbl_model_pred_transformed_dat$wk_sold_doll_byppg, 0, ifelse(glbl_model_pred_transformed_dat$actual_dollar_sales<glbl_model_pred_transformed_dat$wk_sold_doll_byppg & glbl_model_pred_transformed_dat$actual_dollar_sales>glbl_model_pred_transformed_dat$base_doll,1,2))
    glbl_model_pred_transformed_dat<-cannibal_report(cannibal_dat_filename,
                                                     MappingFile_Description_path,
                                                     glbl_model_pred_transformed_dat,
                                                     cannibal_dat_op_filename,
                                                     Retailer,
                                                     Category, "predicted")
    #d <- Output_Data_Hist[(Output_Data_Hist$Cross)=="",]
    #glbl_model_pred_transformed_dat[is.na(glbl_model_pred_transformed_dat)]<-""
    
    glbl_model_pred_transformed_dat=glbl_model_pred_transformed_dat[!glbl_model_pred_transformed_dat$pred_vol=="",]
    #### change gross lift, cannibalisation and true lift based on flag check
    glbl_model_pred_transformed_dat$within_percent = (as.numeric(glbl_model_pred_transformed_dat$Within))/glbl_model_pred_transformed_dat$lift_doll
    
    glbl_model_pred_transformed_dat$cross_percent = (as.numeric(glbl_model_pred_transformed_dat$Cross))/glbl_model_pred_transformed_dat$lift_doll
    
    glbl_model_pred_transformed_dat$true_lift_percent = (as.numeric(glbl_model_pred_transformed_dat$true_lift_doll))/glbl_model_pred_transformed_dat$lift_doll
    
    glbl_model_pred_transformed_dat$Cross_predicted_normalised = glbl_model_pred_transformed_dat$Cross
    glbl_model_pred_transformed_dat$Within_predicted_normalised = glbl_model_pred_transformed_dat$Within
    glbl_model_pred_transformed_dat$Cross_predicted_non_normalised = glbl_model_pred_transformed_dat$Cross_non_normalised
    glbl_model_pred_transformed_dat$Within_predicted_non_normalised = glbl_model_pred_transformed_dat$Within_non_normalised
    
    glbl_model_pred_transformed_dat[,c("Cross_non_normalised", "Within_non_normalised")] = NULL
    
    
    
    glbl_model_pred_transformed_dat$gross_lift_on_pred = glbl_model_pred_transformed_dat$lift_doll
    glbl_model_pred_transformed_dat$true_lift_on_pred = glbl_model_pred_transformed_dat$gross_lift_on_pred - (as.numeric(glbl_model_pred_transformed_dat$Cross_predicted_normalised)+as.numeric(glbl_model_pred_transformed_dat$Within_predicted_normalised))
    
    
    glbl_model_pred_transformed_dat$original_base_doll = glbl_model_pred_transformed_dat$base_doll
    glbl_model_pred_transformed_dat$original_base_vol = glbl_model_pred_transformed_dat$base_vol
    
    glbl_model_pred_transformed_dat$base_doll = as.numeric(ifelse(glbl_model_pred_transformed_dat$flag_check==2,glbl_model_pred_transformed_dat$actual_dollar_sales, glbl_model_pred_transformed_dat$base_doll))
    glbl_model_pred_transformed_dat$base_vol = as.numeric(ifelse(glbl_model_pred_transformed_dat$flag_check==2,glbl_model_pred_transformed_dat$wk_sold_qty_byppg, glbl_model_pred_transformed_dat$base_vol))
    
    glbl_model_pred_transformed_dat$lift_doll = ifelse(glbl_model_pred_transformed_dat$flag_check==0, glbl_model_pred_transformed_dat$actual_dollar_sales - glbl_model_pred_transformed_dat$base_doll,ifelse(glbl_model_pred_transformed_dat$flag_check==1,glbl_model_pred_transformed_dat$actual_dollar_sales - glbl_model_pred_transformed_dat$base_doll,0))
    glbl_model_pred_transformed_dat$wk_sold_qty_byppg = as.numeric(glbl_model_pred_transformed_dat$wk_sold_qty_byppg)
    glbl_model_pred_transformed_dat$lift_vol = ifelse(glbl_model_pred_transformed_dat$flag_check==0, glbl_model_pred_transformed_dat$wk_sold_qty_byppg - glbl_model_pred_transformed_dat$base_vol,ifelse(glbl_model_pred_transformed_dat$flag_check==1,glbl_model_pred_transformed_dat$wk_sold_qty_byppg - glbl_model_pred_transformed_dat$base_vol,0))
    
    glbl_model_pred_transformed_dat$actual_dollar_sales = as.numeric(glbl_model_pred_transformed_dat$actual_dollar_sales)
    
    ##purina sales and spend
    glbl_model_pred_transformed_dat$Purina_sales = glbl_model_pred_transformed_dat$ListCost * as.numeric(glbl_model_pred_transformed_dat$wk_sold_qty_byppg)
    glbl_model_pred_transformed_dat$Purina_spend = glbl_model_pred_transformed_dat$OffInvoice * as.numeric(glbl_model_pred_transformed_dat$wk_sold_qty_byppg)
    glbl_model_pred_transformed_dat$Purina_Base_sales = glbl_model_pred_transformed_dat$ListCost * as.numeric(glbl_model_pred_transformed_dat$base_vol)
    glbl_model_pred_transformed_dat$Purina_Base_sales<-ifelse(glbl_model_pred_transformed_dat$Purina_sales<glbl_model_pred_transformed_dat$Purina_Base_sales,
                                                              glbl_model_pred_transformed_dat$Purina_sales,
                                                              glbl_model_pred_transformed_dat$Purina_Base_sales)
    glbl_model_pred_transformed_dat$Purina_ratf = as.numeric(glbl_model_pred_transformed_dat$Purina_sales) - as.numeric(glbl_model_pred_transformed_dat$Purina_spend)
    
    #glbl_model_pred_transformed_dat$Purina_Gross_Lift = glbl_model_pred_transformed_dat$Purina_sales-as.numeric(glbl_model_pred_transformed_dat$Purina_Base_sales)
    glbl_model_pred_transformed_dat$Purina_Gross_Lift = (glbl_model_pred_transformed_dat$ListCost-(as.numeric(glbl_model_pred_transformed_dat$Estimated_baseprice)-as.numeric(glbl_model_pred_transformed_dat$wk_sold_avg_price_byppg)))*
                                                        (as.numeric(glbl_model_pred_transformed_dat$wk_sold_qty_byppg)-as.numeric(glbl_model_pred_transformed_dat$base_vol))
    
    glbl_model_pred_transformed_dat$Purina_Gross_Lift = ifelse(glbl_model_pred_transformed_dat$Purina_Gross_Lift<0,0,glbl_model_pred_transformed_dat$Purina_Gross_Lift)
    glbl_model_pred_transformed_dat$Purina_True_Lift = glbl_model_pred_transformed_dat$Purina_Gross_Lift
    
    
    glbl_model_pred_transformed_dat[,c("Within", "Cross")] = NULL
    glbl_model_pred_transformed_dat<-cannibal_report(cannibal_dat_filename,
                                                     MappingFile_Description_path,
                                                     glbl_model_pred_transformed_dat,
                                                     cannibal_dat_op_filename,
                                                     Retailer,
                                                     Category, "actuals")
    
    glbl_model_pred_transformed_dat[,c("Cross_non_normalised", "Within_non_normalised")] = NULL
    #d <- Output_Data_Hist[(Output_Data_Hist$Cross)=="",]
    glbl_model_pred_transformed_dat[is.na(glbl_model_pred_transformed_dat)]<-""
    
    # #### change gross lift, cannibalisation and true lift based on flag check
    # 
    # 
    # glbl_model_pred_transformed_dat$Within = ifelse(glbl_model_pred_transformed_dat$flag_check==0 |glbl_model_pred_transformed_dat$flag_check==1 , ifelse(glbl_model_pred_transformed_dat$Within_non_normalised+glbl_model_pred_transformed_dat$Cross_non_normalised<=glbl_model_pred_transformed_dat$lift_doll,glbl_model_pred_transformed_dat$Within_non_normalised,glbl_model_pred_transformed_dat$Within_non_normalised/(glbl_model_pred_transformed_dat$Within_non_normalised+glbl_model_pred_transformed_dat$Cross_non_normalised)*glbl_model_pred_transformed_dat$lift_doll),0)
    # 
    # 
    # glbl_model_pred_transformed_dat$Cross = ifelse(glbl_model_pred_transformed_dat$flag_check==0 | glbl_model_pred_transformed_dat$flag_check==1, ifelse(glbl_model_pred_transformed_dat$Within_non_normalised+glbl_model_pred_transformed_dat$Cross_non_normalised<=glbl_model_pred_transformed_dat$lift_doll,glbl_model_pred_transformed_dat$Cross_non_normalised,glbl_model_pred_transformed_dat$Cross_non_normalised/(glbl_model_pred_transformed_dat$Within_non_normalised+glbl_model_pred_transformed_dat$Cross_non_normalised)*glbl_model_pred_transformed_dat$lift_doll),0)
    glbl_model_pred_transformed_dat$Within = ifelse(!is.na(glbl_model_pred_transformed_dat$Within), glbl_model_pred_transformed_dat$Within, 0)
    glbl_model_pred_transformed_dat$Cross = ifelse(!is.na(glbl_model_pred_transformed_dat$Cross), glbl_model_pred_transformed_dat$Cross, 0)
    
    glbl_model_pred_transformed_dat$Within = as.numeric(glbl_model_pred_transformed_dat$Within)
    glbl_model_pred_transformed_dat$Cross = as.numeric(glbl_model_pred_transformed_dat$Cross)
    glbl_model_pred_transformed_dat$true_lift_doll = ifelse(glbl_model_pred_transformed_dat$flag_check==0 |glbl_model_pred_transformed_dat$flag_check==1, as.numeric(glbl_model_pred_transformed_dat$lift_doll)-as.numeric(glbl_model_pred_transformed_dat$Within)-as.numeric(glbl_model_pred_transformed_dat$Cross),0)
    glbl_model_pred_transformed_dat$true_lift_doll = as.numeric(ifelse(glbl_model_pred_transformed_dat$true_lift_doll<0,0,glbl_model_pred_transformed_dat$true_lift_doll))
    
    #### changing cannibal data file and choosing only required columns for historical view
    
    glbl_model_pred_transformed_dat$pred_vol_original = glbl_model_pred_transformed_dat$pred_vol
    glbl_model_pred_transformed_dat$wk_sold_doll_byppg_original = glbl_model_pred_transformed_dat$wk_sold_doll_byppg
    glbl_model_pred_transformed_dat$pred_vol = glbl_model_pred_transformed_dat$wk_sold_qty_byppg
    glbl_model_pred_transformed_dat$wk_sold_doll_byppg = glbl_model_pred_transformed_dat$actual_dollar_sales
    glbl_model_pred_transformed_dat[,c("actual_dollar_sales", "flag_check", "Cross_non_normalised", "Within_non_normalised", "within_percent", "cross_percent", "true_lift_percent")] = NULL
    glbl_model_pred_transformed_dat$Spend <- as.numeric(glbl_model_pred_transformed_dat$wk_sold_qty_byppg) * (as.numeric(glbl_model_pred_transformed_dat$Final_baseprice) - as.numeric(glbl_model_pred_transformed_dat$wk_sold_avg_price_byppg))
    
    
    glbl_model_pred_transformed_dat$Retailer = Retailer
    glbl_model_pred_transformed_dat$Market = ifelse(glbl_model_pred_transformed_dat$PPG_Item_No %like% "Retailer","Retailer","ROM")
    
    
    if(file.exists(paste0(cannibal_dat_op_filename))){
      cannibal_actuals = read.csv(paste0(base_dir,"/Output Files/",cannibal_dat_op_filename), stringsAsFactors = FALSE)
      if(file.exists(paste0(cannibal_dat_op_filename1))){
        cannibal_predicted = read.csv(paste0(base_dir,"/Output Files/",cannibal_dat_op_filename1), stringsAsFactors = FALSE)
        names(cannibal_predicted)[names(cannibal_predicted)=="Cannibal_Doll"] = "Cannibal_dollars_on_predicted"
        cannibal_actuals$X = NULL
        cannibal_predicted$X = NULL
        df = merge(cannibal_actuals, cannibal_predicted, all = TRUE, by=c("Cannibal_PPG", "Date", "box", "Cannibalised_PPG", "Model_flag","Cannibal_PPG_Description", "PPG_Cat", "Retailer"))
        df[is.na(df)]=0
      }else{
        df = cannibal_actuals
        df$Cannibal_dollars_on_predicted = 0
      }
      write.csv(df, paste0(base_dir,"/Output Files/",cannibal_dat_op_filename),row.names = F)
    }
    
    Output_Data_Hist <- glbl_model_pred_transformed_dat
    Output_Data_Hist <- Output_Data_Hist[!colnames(Output_Data_Hist)%like% "_Comp_"]
    Output_Data_Hist$Retailer <- retailer_format_fn(Output_Data_Hist$Retailer)
    Output_Data_Hist$PPG_Cat <- category_format_fn(Output_Data_Hist$PPG_Cat)
    
    
    #### Imputing Missing weeks ####
    basedata <- copy(Output_Data_Hist) 
    setDT(basedata)
    ppg_data_frame <- data.frame() ## for creating ppg wise data base
    df_rbind <- data.frame() ## for all ppg value one data frame
    count_dates <- 0 ## for checking the number of dates to be imputed 
    for_date <- basedata[,list(no_of_weeks=n_distinct(Date)),by=c("PPG_Item_No")]
    bd_colnames <-  colnames(basedata)
    bd_colnames <- bd_colnames[!bd_colnames%in%c("PPG_Item_No","Date","PPG_Cat","PPG_MFG","PPG_Description","Retailer","Market",
                                                 "wk_sold_avg_price_byppg","median_baseprice","Final_baseprice","Estimated_baseprice")]
    
    
    ## pgg with highest number of weeks is considered
    req_ppg<-as.character(for_date[for_date$no_of_weeks==max(for_date$no_of_weeks),]$PPG_Item_No[1])
    # Start_date <- min(as.Date(basedata$Date))
    # End_date <- max(as.Date(basedata$Date))
    # date_range <- data.frame(seq(as.Date(Start_date), as.Date(End_date), by="weeks"))
    date_range<-as.Date(unique(basedata$Date)) ##
    #date_range<-date_range[!is.na(date_range)]
    overall_min_date <- min(date_range)
    for (p in unique(basedata$PPG_Item_No)){
      ppg_data_frame <- basedata[basedata$PPG_Item_No==p,]
      ppg_data_frame$Date <- as.Date(ppg_data_frame$Date)
      date_var <- as.Date(ppg_data_frame$Date)
      current_min_date <- min(date_var)
      ###### date_values variable contains the dates to be imputed
      date_values <- date_range[!date_range%in%date_var]
      count_dates <- count_dates + as.numeric(length(date_values))
      n_rows_to_be_imputed<-length(date_range)-n_distinct(ppg_data_frame$Date)
      
      if (n_rows_to_be_imputed>0)
      {
        Temp_df <- ppg_data_frame[0:n_rows_to_be_imputed,]
        Temp_df$Date <- data.frame(date_values)
        Temp_df$PPG_Item_No <- p
        Temp_df$PPG_Cat <- unique(ppg_data_frame[ppg_data_frame$PPG_Item_No==p,]$PPG_Cat)
        Temp_df$PPG_Description <- unique(ppg_data_frame[ppg_data_frame$PPG_Item_No==p,]$PPG_Description)
        Temp_df$PPG_MFG <- unique(ppg_data_frame[ppg_data_frame$PPG_Item_No==p,]$PPG_MFG)
        Temp_df$PPG_Retailer <- unique(ppg_data_frame[ppg_data_frame$PPG_Item_No==p,]$PPG_Retailer)
        Temp_df$Model_flag <- unique(ppg_data_frame[ppg_data_frame$PPG_Item_No==p,]$Model_flag)
        Temp_df$Retailer <- unique(ppg_data_frame[ppg_data_frame$PPG_Item_No==p,]$Retailer)
        Temp_df$Market <- unique(ppg_data_frame[ppg_data_frame$PPG_Item_No==p,]$Market)

        Temp_df[,c("wk_sold_qty_byppg","tpr_discount_byppg","ACV_Feat_Only","ACV_Disp_Only","ACV_Feat_Disp",
                   "ACV_Selling","flag_qtr2","flag_qtr3","flag_qtr4","category_trend","tpr_discount_byppg_lag1",
                   "tpr_discount_byppg_lag2","monthno","pred_vol","base_vol","lift_vol","base_doll",
                   "wk_sold_doll_byppg","lift_doll","true_lift_doll", "Cross_predicted_normalised",
                   "Within_predicted_normalised"  ,  "Cross_predicted_non_normalised",
                   "Within_predicted_non_normalised",    "gross_lift_on_pred", "true_lift_on_pred",
                   "original_base_doll","original_base_vol"     
                   ,"Within","Cross", "pred_vol_original", "wk_sold_doll_byppg_original","Purina_spend","OffInvoice")] <- 0
        
        Temp_df[,c("wk_sold_avg_price_byppg","median_baseprice","Final_baseprice",
                   "Estimated_baseprice","ListCost","Purina_sales","Purina_Base_sales",
                   "Purina_ratf","Purina_Gross_Lift","Purina_True_Lift")] <- NA
        
        
        ppg_data_frame<-rbind(ppg_data_frame,Temp_df)
        ### sorting it based on the date
        ppg_data_frame <- ppg_data_frame[order(as.Date(ppg_data_frame$Date, format="%Y/%m/%d")),]
        # If Starting weeks data is not avl making price points as 0
        if(current_min_date!=overall_min_date){
          
          Starting_missing_weeks <- date_range[date_range<current_min_date]
          #ppg_data_frame <- ppg_data_frame[!ppg_data_frame$Date %in% Starting_missing_weeks]
          ppg_data_frame$wk_sold_avg_price_byppg[ppg_data_frame$Date %in% Starting_missing_weeks] <- 0
          ppg_data_frame$median_baseprice[ppg_data_frame$Date %in% Starting_missing_weeks] <- 0
          ppg_data_frame$Final_baseprice[ppg_data_frame$Date %in% Starting_missing_weeks] <- 0
          ppg_data_frame$Estimated_baseprice[ppg_data_frame$Date %in% Starting_missing_weeks] <- 0
          
        }
        
        #Imputing succeeding weekS Price points for NA Price points
        ppg_data_frame = ppg_data_frame %>% group_by(PPG_Item_No) %>%  mutate(Final_baseprice = zoo::na.locf0(Final_baseprice))
        ppg_data_frame = ppg_data_frame %>% group_by(PPG_Item_No) %>%  mutate(wk_sold_avg_price_byppg = zoo::na.locf0(wk_sold_avg_price_byppg))
        ppg_data_frame = ppg_data_frame %>% group_by(PPG_Item_No) %>%  mutate(median_baseprice = zoo::na.locf0(median_baseprice))
        ppg_data_frame = ppg_data_frame %>% group_by(PPG_Item_No) %>%  mutate(Estimated_baseprice = zoo::na.locf0(Estimated_baseprice))
        ppg_data_frame = ppg_data_frame%>% group_by(PPG_Item_No) %>%  mutate(ListCost = zoo::na.locf0(ListCost))
        #ppg_data_frame = ppg_data_frame%>% group_by(PPG_Item_No) %>%  mutate(OffInvoice = zoo::na.locf0(OffInvoice,fromLast=T))
        ppg_data_frame$OffInvoice = ifelse(is.na(ppg_data_frame$OffInvoice),0,ppg_data_frame$OffInvoice)
        ppg_data_frame$Purina_sales = ifelse(is.na(ppg_data_frame$ListCost),NA,ppg_data_frame$Purina_sales)
        ppg_data_frame$Purina_Base_sales = ifelse(is.na(ppg_data_frame$ListCost),NA,ppg_data_frame$Purina_Base_sales)
        ppg_data_frame$Purina_ratf = ifelse(is.na(ppg_data_frame$ListCost),NA,ppg_data_frame$Purina_ratf)
        
        ppg_data_frame <- data.frame(ppg_data_frame)
      }
      
      df_rbind <- rbind(df_rbind,ppg_data_frame)
      #print("df_bind")
    }
    
    Output_Data_Hist <- copy(df_rbind)
  }
  
  # Saving the base data file ####
  write.csv(Output_Data_Hist,paste0(base_dir,"/Output Files/",base_transform_op_filename),row.names = F)
  print("base data transformed file created")
  glbl_model_pred_transformed_dat$wk_sold_qty_byppg=NULL
  glbl_model_pred_transformed_dat$wk_sold_avg_price_byppg=NULL
  glbl_model_pred_transformed_dat$wk_sold_doll_byppg=NULL
  glbl_model_pred_transformed_dat$median_baseprice_byppg=glbl_model_pred_transformed_dat$median_baseprice
  glbl_model_pred_transformed_dat$median_baseprice=NULL
  glbl_model_pred_transformed_dat=data.frame(glbl_model_pred_transformed_dat)
  col_list=c("PPG_Item_No", "PPG_Cat", "PPG_MFG","Date","PPG_Description","Retailer","Market","PPG_Retailer")
  col_list2=colnames(glbl_model_pred_transformed_dat)
  col_list2=col_list2[!col_list2%in%col_list]
  for(i in col_list2){
    glbl_model_pred_transformed_dat[,i]=as.numeric(glbl_model_pred_transformed_dat[,i])
  }
  
  
  
  Retailer_Price_Data <- merge(glbl_model_pred_transformed_dat,base_dat[,c("PPG_Item_No","PPG_Cat","Date","ACV_TPR_Only")],by=c("PPG_Item_No","Date","PPG_Cat"),all.x=T)
  
  Retailer_Price_Data$tpr_discount_byppg[Retailer_Price_Data$tpr_discount_byppg == 0] <- NA
  
  
  
  ##########multiplying tpr and acv with 100 for price agg file
  acv_cols = colnames(Retailer_Price_Data)[colnames(Retailer_Price_Data) %like% "ACV"]
  Retailer_Price_Data[,acv_cols] = Retailer_Price_Data[,acv_cols]*100
  Retailer_Price_Data$tpr_discount_byppg = Retailer_Price_Data$tpr_discount_byppg*100
  Retailer_price_agg <- Retailer_Price_Data %>% group_by(PPG_Item_No, PPG_Cat, PPG_MFG,PPG_Description) %>% summarise(
    median_baseprice = max(Final_baseprice, na.rm = TRUE),
    min.baseprice = max(Final_baseprice, na.rm = TRUE),
    max.baseprice = max(Final_baseprice, na.rm = TRUE),
    max.TPR = max(tpr_discount_byppg, na.rm = TRUE),
    max.RP = max(median_baseprice_byppg, na.rm = TRUE),
    min.TPR = min(tpr_discount_byppg, na.rm = TRUE),
    min.RP = min(median_baseprice_byppg, na.rm = TRUE),
    mean.TPR = mean(tpr_discount_byppg, na.rm = TRUE),
    mean.RP = mean(median_baseprice_byppg, na.rm = TRUE),
    ACV_TPR_Min=min(ACV_TPR_Only,na.rm=T),
    ACV_TPR_Max=max(ACV_TPR_Only,na.rm=T),
    ACV_FT_Min=min(ACV_Feat_Only,na.rm=T),
    ACV_FT_Max=max(ACV_Feat_Only,na.rm=T),
    ACV_DP_Min=min(ACV_Disp_Only,na.rm=T),
    ACV_DP_Max=max(ACV_Disp_Only,na.rm=T),
    ACV_FT_DP_Min=min(ACV_Feat_Disp,na.rm=T),
    ACV_FT_DP_Max=max(ACV_Feat_Disp,na.rm=T),
    min.ListCost=min(ListCost,na.rm=T),
    max.ListCost=max(ListCost,na.rm=T),
    ListCost=mean(ListCost,na.rm=T),
    min.OffInvoice=min(OffInvoice,na.rm=T),
    max.OffInvoice=max(OffInvoice,na.rm=T),
    OffInvoice=(sum(OffInvoice * EachesOI,na.rm=T)/ sum(EachesOI, na.rm=T))
  )
  maxweek=max(as.Date(Retailer_Price_Data$Date))
  
  
  Retailer_price_agg2 <- Retailer_Price_Data%>%filter(as.Date(Date)>(maxweek-49)) %>% group_by(PPG_Cat,PPG_Item_No) %>% summarise(
    
    ACV_Selling_Mean=max(ACV_Selling,na.rm=T)
  )				
  
  
  #Imputing Mean ACV Selling for PPGs , which dont have ACV selling for last 7weeks, we will be taking last ACV selling is present  
  Retailer_price_agg_temp <- Retailer_Price_Data%>%filter(as.Date(Date)>(maxweek-49))
  Retailer_price_agg_temp <- unique(Retailer_price_agg_temp$PPG_Item_No)
  
  Retailer_price_agg_temp1 <- Retailer_Price_Data[!Retailer_Price_Data$PPG_Item_No %in% Retailer_price_agg_temp,c("PPG_Item_No","PPG_Cat","Date","ACV_Selling")]
  Retailer_price_agg_temp1 <- Retailer_price_agg_temp1[is.na(Retailer_price_agg_temp1$ACV_Selling)==F,]
  Retailer_price_agg_temp1a <- Retailer_price_agg_temp1 %>%group_by(PPG_Item_No,PPG_Cat) %>% filter(as.Date(Date)==max(as.Date(Date)))
  
  Retailer_price_agg_temp1a$Date <- NULL 
  colnames(Retailer_price_agg_temp1a)  <-c("PPG_Item_No","PPG_Cat","ACV_Selling_Mean")
  Retailer_price_agg2 <- as.data.frame(Retailer_price_agg2)
  Retailer_price_agg_temp1a <- as.data.frame(Retailer_price_agg_temp1a)
  Retailer_price_agg2 <- rbind(Retailer_price_agg2,Retailer_price_agg_temp1a)
  
  # Adjusting the Price ranges
  Retailer_price_agg$min.baseprice <- floor(Retailer_price_agg$min.baseprice - (Retailer_price_agg$median_baseprice / 10))
  Retailer_price_agg$max.baseprice <- ceiling(Retailer_price_agg$max.baseprice + (Retailer_price_agg$median_baseprice / 10))
  Retailer_price_agg$median_baseprice <- round(Retailer_price_agg$median_baseprice,1)
  Retailer_price_agg$min.TPR <- 0
  Retailer_price_agg$max.TPR <- ifelse(ceiling(Retailer_price_agg$max.TPR + (Retailer_price_agg$mean.TPR / 10))>100,ceiling(Retailer_price_agg$max.TPR ), ceiling(Retailer_price_agg$max.TPR + (Retailer_price_agg$mean.TPR / 10)))
  Retailer_price_agg$min.RP <- floor(Retailer_price_agg$min.RP - (Retailer_price_agg$mean.RP / 10))
  Retailer_price_agg$max.RP <- ceiling(Retailer_price_agg$max.RP + (Retailer_price_agg$mean.RP / 10))
  Retailer_price_agg$mean.RP <- ifelse(Retailer_price_agg$mean.RP > Retailer_price_agg$median_baseprice,Retailer_price_agg$median_baseprice,Retailer_price_agg$mean.RP)
  Retailer_price_agg$max.RP <- ifelse(Retailer_price_agg$max.RP > Retailer_price_agg$max.baseprice, Retailer_price_agg$max.baseprice, Retailer_price_agg$max.RP)
  Retailer_price_agg$min.ListCost <- floor(Retailer_price_agg$min.ListCost - (Retailer_price_agg$ListCost / 10))
  Retailer_price_agg$max.ListCost <- ceiling(Retailer_price_agg$max.ListCost + (Retailer_price_agg$ListCost / 10))
  Retailer_price_agg$min.OffInvoice <- floor(Retailer_price_agg$min.OffInvoice - (Retailer_price_agg$OffInvoice / 10))
  Retailer_price_agg$max.OffInvoice <- ceiling(Retailer_price_agg$max.OffInvoice + (Retailer_price_agg$OffInvoice / 10))
  Retailer_price_agg$ListCost <- round(Retailer_price_agg$ListCost,1)
  Retailer_price_agg$OffInvoice <- round(Retailer_price_agg$OffInvoice,1)
  Retailer_price_agg$min.ListCost <- ifelse(Retailer_price_agg$min.ListCost=="NaN",0,Retailer_price_agg$min.ListCost)
  Retailer_price_agg$min.OffInvoice <- ifelse(Retailer_price_agg$min.OffInvoice=="NaN",0,Retailer_price_agg$min.OffInvoice)
  Retailer_price_agg$ListCost <- ifelse(Retailer_price_agg$ListCost=="NaN",0,Retailer_price_agg$ListCost)
  Retailer_price_agg$OffInvoice <- ifelse(Retailer_price_agg$OffInvoice=="NaN",0,Retailer_price_agg$OffInvoice)
  
  Retailer_price_agg$max.ListCost <- ifelse(Retailer_price_agg$max.ListCost=="NaN",Retailer_price_agg$mean.RP,Retailer_price_agg$max.ListCost)
  Retailer_price_agg$max.ListCost <- ifelse(Retailer_price_agg$max.ListCost==0,Retailer_price_agg$mean.RP,Retailer_price_agg$max.ListCost)
  Retailer_price_agg$max.OffInvoice <- ifelse(Retailer_price_agg$max.OffInvoice=="NaN",Retailer_price_agg$max.ListCost,Retailer_price_agg$max.OffInvoice)
  Retailer_price_agg$max.OffInvoice <- ifelse(Retailer_price_agg$max.OffInvoice==0,Retailer_price_agg$max.ListCost,Retailer_price_agg$max.OffInvoice)
  
  Retailer_price_agg$min.OffInvoice <- ifelse(Retailer_price_agg$min.OffInvoice < 0,0,Retailer_price_agg$min.OffInvoice)
  
  
  Retailer_price_agg=merge(Retailer_price_agg,Retailer_price_agg2,by=c("PPG_Cat","PPG_Item_No"),all.x=T)
  Retailer_price_agg$PPG_Cat = category_format_fn(Retailer_price_agg$PPG_Cat)
  Retailer_price_agg=merge(Retailer_price_agg,Final_Metrics,by=c("PPG_Cat","PPG_Item_No"),all.x=T)
  
  if(sum(is.na(Retailer_price_agg[,colnames(Final_Metrics)]))>0){
    
    Retailer_price_agg1 <- Retailer_price_agg
    Retailer_price_agg1a <- Retailer_price_agg1[is.na(Retailer_price_agg1$RP_Elastic)==F,]
    Retailer_price_agg1b <- Retailer_price_agg1[!is.na(Retailer_price_agg1$RP_Elastic)==F,]
    Retailer_price_agg1b[,(colnames(Final_Metrics)[-1])]<- 0
    Retailer_price_agg <- rbind(Retailer_price_agg1a,Retailer_price_agg1b)
    rm(Retailer_price_agg1a,Retailer_price_agg1b)
  }
  
  
  Retailer_price_agg$mean.TPR[Retailer_price_agg$mean.TPR == 'NaN'] <- 0
  Retailer_price_agg$max.TPR[is.na(Retailer_price_agg$max.TPR)] <- 0
  #stop()
  #Retailer_price_agg <- na.omit(Retailer_price_agg)
  Retailer_price_agg$Retailer = Retailer
  Retailer_price_agg$Market = ifelse(Retailer_price_agg$PPG_Item_No %like% "Retailer","Retailer","ROM")
  setnames(Retailer_price_agg,c("PPG_Description"),c("PPG_Item"))
  Retailer_price_agg$Retailer <- retailer_format_fn(Retailer_price_agg$Retailer)
  
  # Filter_ppg_file <- read.csv(paste0(base_dir1,Filtered_PPGs))
  # Filter_ppg_file$TotalSales <- NULL 
  # Filter_ppg_file$Reason <- NULL 
  # Filter_ppg_file$PPG_Retailer <- NULL
  # colnames(Filter_ppg_file)
  # colnames(Retailer_price_agg)
  # Retailer_price_agg1 <- merge(Retailer_price_agg,Filter_ppg_file,
  #                              by.x = c("PPG_Item_No","PPG_MFG"),
  #                              by.y = c("PPG_Item_No","PPG_MFG"),all.x= T )
  Model_est = get(load(paste0(base_dir,"/Output Files/",model_est_dat_filename,".RData")))
  Model_est = unique(Model_est[,c("PPG_Item_No","PPG_Cat","model_RSq","TrainMAPE","Model_flag")])
  Model_est$PPG_Cat = category_format_fn(Model_est$PPG_Cat)
  Retailer_price_agg1 <- merge(Retailer_price_agg,Model_est,
                                by.x = c("PPG_Item_No","PPG_Cat"),
                                by.y = c("PPG_Item_No","PPG_Cat"),all.x= T )
  #adding TPR events count
  TPR_Event_cnt <- Output_Data_Hist[,c("PPG_Cat","PPG_Item_No","PPG_MFG","tpr_discount_byppg")]
  TPR_Event_cnt$TPR_Flag <- ifelse(TPR_Event_cnt$tpr_discount_byppg>0,1,0)
  TPR_Event_cnt <- TPR_Event_cnt[TPR_Event_cnt$TPR_Flag==1,]
  TPR_Event_cnt <- TPR_Event_cnt %>% group_by(PPG_Cat,PPG_Item_No,PPG_MFG) %>% summarise(TPR_Events=n())
  
  #merging TPR event
  Retailer_price_agg1a <- merge(Retailer_price_agg1,TPR_Event_cnt,by = c("PPG_Cat","PPG_Item_No","PPG_MFG"),all.x = T)
  Retailer_price_agg1a$TPR_Events[is.na(Retailer_price_agg1a$TPR_Events)] <- 0
  
  #print("Price agg file will be created")
  #ACV_Selling_Mean_col_ref <- grep("ACV_Selling_Mean", colnames(Retailer_price_agg))
  #Retailer_price_agg1 <- Retailer_price_agg
  #Retailer_price_agg1$ACV_Selling_Mean <- as.numeric(as.character(Retailer_price_agg1$ACV_Selling_Mean))
  write.csv(Retailer_price_agg1a,paste0(base_dir,"/Output Files/",price_agg_op_filename),row.names = F)
  
  # print("Price agg file created")
  if(file_type=="General"){
    glbl_model_pred_transformed_dat_temp = copy(glbl_model_pred_transformed_dat)
    max_year = max(year(glbl_model_pred_transformed_dat_temp$Date))
    
    glbl_model_pred_transformed_dat_temp$monthno = ifelse(year(glbl_model_pred_transformed_dat_temp$Date)==max_year, as.numeric(as.character(glbl_model_pred_transformed_dat_temp$monthno))+12,as.numeric(as.character(glbl_model_pred_transformed_dat_temp$monthno))+24)
  }else{
    glbl_model_pred_transformed_dat_temp = copy(glbl_model_pred_transformed_dat)
    
  }
  glbl_model_pred_transformed_dat_temp <- glbl_model_pred_transformed_dat_temp%>%filter(as.Date(Date)>(maxweek-52*7))
  #glbl_model_pred_transformed_dat2 <- glbl_model_pred_transformed_dat_temp %>% group_by(PPG_Item_No,PPG_Description, PPG_Cat, PPG_MFG,flag_qtr2,flag_qtr3,flag_qtr4,Market,Retailer)%>%summarise_all(funs(mean(.,na.rm=T)))
  # Custom aggregations
  glbl_model_pred_transformed_dat2_1 <- glbl_model_pred_transformed_dat_temp %>% 
    group_by(PPG_Item_No,PPG_Description, PPG_Cat, PPG_MFG,flag_qtr2,flag_qtr3,flag_qtr4,Market,Retailer)%>%
    summarise(Date = max(Date),
              OffInvoice = (sum(OffInvoice * EachesOI, na.rm=T)/ sum(EachesOI, na.rm=T)),
    )
  glbl_model_pred_transformed_dat2 <- glbl_model_pred_transformed_dat_temp %>%
    group_by(PPG_Item_No,PPG_Description, PPG_Cat, PPG_MFG,flag_qtr2,flag_qtr3,flag_qtr4,Market,Retailer) %>%
    summarise_at(vars(-c(Date, OffInvoice)), funs(mean))%>%
    left_join(glbl_model_pred_transformed_dat2_1, by = c("PPG_Item_No","PPG_Description", "PPG_Cat", "PPG_MFG","flag_qtr2","flag_qtr3","flag_qtr4","Market","Retailer"))%>%
    data.frame()
  
  glbl_model_pred_transformed_dat2$qtr=ifelse(glbl_model_pred_transformed_dat2$flag_qtr2==1,2,
                                              ifelse(glbl_model_pred_transformed_dat2$flag_qtr3==1,3,
                                                     ifelse(glbl_model_pred_transformed_dat2$flag_qtr4==1,4,1
                                                     )))
  glbl_model_pred_transformed_dat2$Retailer <- retailer_format_fn(glbl_model_pred_transformed_dat2$Retailer)
  glbl_model_pred_transformed_dat2$PPG_Cat <- category_format_fn(glbl_model_pred_transformed_dat2$PPG_Cat)
  
  fix_for_3b <- function(dt){
    all_qtr_filling_dt <- copy(dt)

    qtrData<- expand.grid(unique(all_qtr_filling_dt$PPG_Item_No),c(1,2,3,4))
    names(qtrData) <- c("PPG_Item_No","qtr")

    # expanding the base data to have all quarter (4) information for every ppg (ROM included)
    all_qtr_filling_inter_dt <-full_join(all_qtr_filling_dt,qtrData)

    # sort by the date
    all_qtr_filling_inter_dt[,"Date"] <- as.Date(all_qtr_filling_inter_dt[,"Date"])
    all_qtr_filling_inter_dt <- all_qtr_filling_inter_dt[order(all_qtr_filling_inter_dt[,"PPG_Item_No"], -as.numeric(all_qtr_filling_inter_dt[,"Date"])),]

    # Separating the existent data
    all_qtr_filling_inter_dt$borrowedqtr <- all_qtr_filling_inter_dt$qtr


    # forward fill from the last available observation
    all_qtr_filling_inter_dt$missing_qtrs <- ifelse(is.na(all_qtr_filling_inter_dt$Date),1,0)
    all_qtr_filling_inter_dt$borrowedqtr <- ifelse(all_qtr_filling_inter_dt$missing_qtrs == 1,NA,all_qtr_filling_inter_dt$borrowedqtr)

    all_qtr_filling_ext_dt <- all_qtr_filling_inter_dt[is.na(all_qtr_filling_inter_dt$Date)==F,]

    cols_to_fill <- names(all_qtr_filling_inter_dt)[!names(all_qtr_filling_inter_dt)%in%"PPG_Item_No"]
    all_qtr_filling_inter_dt <- all_qtr_filling_inter_dt %>%
      group_by(PPG_Item_No)%>%
      fill_(fill_cols = cols_to_fill, .direction = "updown")%>%
      data.frame()

    all_qtr_filling_inter_dt <- rbind(all_qtr_filling_ext_dt,all_qtr_filling_inter_dt[all_qtr_filling_inter_dt$missing_qtrs==1,])
    all_qtr_filling_inter_dt <- all_qtr_filling_inter_dt[order(all_qtr_filling_inter_dt[,"PPG_Item_No"], -as.numeric(all_qtr_filling_inter_dt[,"Date"])),]

    all_qtr_filling_inter_dt$flag_qtr2 <- ifelse(all_qtr_filling_inter_dt$qtr==2,1,0)
    all_qtr_filling_inter_dt$flag_qtr3 <- ifelse(all_qtr_filling_inter_dt$qtr==3,1,0)
    all_qtr_filling_inter_dt$flag_qtr4 <- ifelse(all_qtr_filling_inter_dt$qtr==4,1,0)

    return(all_qtr_filling_inter_dt)
  }

  glbl_model_pred_transformed_dat2 = fix_for_3b(glbl_model_pred_transformed_dat2)

  
  write.csv(glbl_model_pred_transformed_dat2,paste0(base_dir,"/Output Files/",base_transform_op_filename2),row.names = F)
  # print("Base data transformed 2 is created")
  #setwd("..")
  #return(TRUE)
}