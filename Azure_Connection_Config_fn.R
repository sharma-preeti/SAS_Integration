
Data_Extraction <- function(azure_path){
  ##File Location
  ## https://npusdvdatalakesta.blob.core.windows.net/restricted-dataoperations/workspace/tiger_analytics
  
  configuration_setup <- function() {
    # Configuration inputs
    account_name = "npusdvdatalakesta"
    vault_name = "npusdvtpoidkey"
    client_secret_name = "npus-dv-srm-trade-promo-tool-secret"
    client_id_name = "npus-dv-srm-trade-promo-tool-spn-id"
    tenant_id_name = "tenant-id"
    p = '/workspace/tiger_analytics/trade_promotion_optimization'
    
    # Extracting the credentials required for setting up connection
    vault <- key_vault(url = sprintf("https://%s.vault.azure.net",vault_name), as_managed_identity = T)
    client_secret=vault$secrets$get(client_secret_name)$value
    client_id=vault$secrets$get(name =client_id_name)$value
    tenant_id=vault$secrets$get(name = tenant_id_name)$value  
    
    # Creating the token (for 1 hour) for accessing the data lake domain
    token <- AzureAuth::get_azure_token(sprintf("https://%s.blob.core.windows.net",account_name),
                                        tenant_id, 
                                        client_id, 
                                        client_secret)
    return(list(account_name,token))
  }
  
  auth_list_file_names<-function(path){
    p<-path
    # Establish endpoint connection
    cont<-AzureStor::storage_container(sprintf("https://%s.blob.core.windows.net/restricted-dataoperations",account_name), token=token)
    nmes <- AzureStor::list_storage_files(container = cont, p)$name
    # Clean the retailer names - remove the URL path
    cl_nmes <- sub(pattern = p, replacement = "",x = nmes)
    
    return(cl_nmes)
  }
  
  auth_read<-function(path, file ,header=T,data.table=T,quote = ""){
    p<-path
    f<-file
    
    endp<-AzureStor::adls_endpoint(sprintf("https://%s.blob.core.windows.net/restricted-dataoperations",account_name), 
                                   token=token)
    cont <- AzureStor::storage_container(endp, p)
    fname <- tempfile()
    AzureStor::storage_download(cont, f, fname)
    return(fread(fname,header=header,data.table=data.table,quote = quote))
  }
  
  config <- configuration_setup()
  account_name <- config[[1]]
  token <- config[[2]]
  
  # list_file_names <- auth_list_file_names(path=azure_path)
  # 
  # for(hive_path in list_file_names){
  #   if(grepl("FERT_MasterXREF",hive_path)){
  #     Material_UPC_mapping=auth_read(path=hive_path,file=azure_path,header=T,data.table=T)
  #   }else if(grepl("fmActualsDecomp",hive_path)){
  #     Material_PPG_mapping=auth_read(path=hive_path,file=azure_path,header=T,data.table=T)
  #   }else if(grepl("modelCoefficients_RM_update",hive_path)){
  #     model_val=auth_read(path=hive_path,file=azure_path,header=T,data.table=T)
  #   }else if(grepl("timeSeriesPPG",hive_path)){
  #     model_data=auth_read(path=hive_path,file=azure_path,header=T,data.table=T)
  #   }
  # }
  # 
  
  model_val=read_excel("E:\\Tiger\\preeti\\SAS Integration\\modelCoefficients_RM_update.xlsx")
  model_data=read.csv("E:\\Tiger\\preeti\\SAS Integration\\timeSeriesPPG.csv")
  #Product_Mapping=read.csv("D:\\SAS Integration\\PigglyWiggly\\PRODUCTMAPPING_MODELLINGPPG.csv")
  Material_UPC_mapping = read.csv("E:\\Tiger\\preeti\\SAS Integration\\FERT_MasterXREF.csv",skip=1)
  Material_PPG_mapping = read.csv("E:\\Tiger\\preeti\\SAS Integration\\fmActualsDecomp.csv")
  return(list(model_data,model_val,Material_UPC_mapping,Material_PPG_mapping))
  
}
