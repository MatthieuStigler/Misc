gs_call_any <- function(x, quiet=TRUE){
  
  gsi_path <- util_get_gs_path()
  ## call
  call <- paste(gsi_path, x)
  if(!quiet) print(call)
  call_out <- system(call, intern=TRUE)
  call_out
}


gc_upload <- function(local_path, gs_path, quiet=TRUE, run=TRUE){
  
  gs_path <- util_check_add_gs(gs_path)
  if(!file.exists(local_path)) warning("local_path not found?")
  local_path <- str_replace(local_path, "\\.shp$", "\\.\\*")
  gs_call <- paste("gsutil -m cp", local_path,  gs_path)
  if(!quiet) print(gs_call)
  if(run) system(gs_call, intern=TRUE)
}

gs_download <- function(input = "gs://general_from_ee/US_soils/*",
                        output,
                        output_base_dir=NULL,
                        quiet=TRUE,
                        cmd = c("rsync -r", "cp"),
                        run=TRUE) {
  cmd <-  match.arg(cmd)
  if(!is.null(output_base_dir)) {
    out_path <- paste0(output_base_dir,  "/", output) %>% 
      str_replace( "//", "/")
  } else {
    out_path <- output
  }
  if(str_detect(out_path, " ")) out_path <-  paste0("'", out_path, "'")
  cmd <- paste0("-m ",  cmd, " ", input, " ", out_path)
  gs_call_any(cmd, quiet=quiet, run=run)
}


gs_check_is_there <- function(path, quiet=TRUE) {
  path <- util_check_add_gs(path)
  gs_call <- paste("ls", path)
  
  out <- gs_call_any(gs_call, quiet=quiet)
  
  stat <- attr(out, "status")
  res <- ifelse(is.null(stat), TRUE, FALSE) 
  res
}


util_check_add_gs <- function(gs_path) {
  if(!str_detect(gs_path, "^gs://")) gs_path <- paste("gs://", gs_path, sep="")
  gs_path
}

util_get_gs_path <- function(){
  out <- system("which gsutil", intern=TRUE)
  out
}

util_get_ee_path <- function(){
  out <- system("which earthengine", intern=TRUE)
  out
}


###
ee_call_any <- function(x, user= c("mmstigler", "mat"), quiet=TRUE){
  
  ee_path <- util_get_ee_path()
  
  ## call
  call <- paste(ee_path, x)
  if(!quiet) print(call)
  call_out <- system(call, intern=TRUE)
  call_out
}


ee_check_has_asset <- function(ee_id, user_name, quiet=TRUE) {
  
  ee_id_last <- dirname(ee_id)
  
  # call
  ee_call <- paste("ls", ee_id_last)
  if(!quiet) print(ee_call)
  out_check <- ee_call_any(ee_call, user=user_name, quiet=quiet)
  
  # ana
  any(str_detect(out_check, ee_id))
}

ee_rm <- function(ee_id=NULL, user_name, quiet=TRUE) {
  
  call <- paste("rm",  user_name, sep=" ")
  call <- paste(call, ee_id, sep="/")
  
  ee_call_any(x= call, user=user_name, quiet=quiet)
}


ee_upload <- function(user_name,
                      gs_file, ee_id, 
                      quiet=TRUE, type=c("table", "image"),
                      delete=TRUE) {
  
  type <- match.arg(type)
  
  gs_file <- util_check_add_gs(gs_file)
  
  ## check if not already there on ee
  is_there_ee <- ee_check_has_asset(ee_id, user_name, quiet)
  if(delete & is_there_ee) ee_rm(ee_id, user_name, quiet)
  
  ## check if there on gs
  if(!gs_check_is_there(gs_file)) warning("File not there on gs?")
  
  ## upload
  call <- paste(util_get_ee_path(),
                " upload ",
                type, " --asset_id=users/", 
                user_name, "/", ee_id,
                " ", gs_file, sep="")
  if(!quiet) print(call)
  out <- system(call, intern=TRUE)
  tibble(gs=gs_file, id=ee_id, task=capt_task(out=out))
}



if(FALSE){
  gc_upload("../ProtectedAreas/conservation_units_legal_amazon_INPE/conservation_units_legal_amazon_CLEAN.shp",
            gs_path = "cropins_yield_us/transfer_forest/to_ee", 
            quiet=TRUE)
}