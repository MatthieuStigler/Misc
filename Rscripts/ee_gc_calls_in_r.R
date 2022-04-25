gs_call_any <- function(x, quiet=TRUE, run=TRUE){

  gsi_path <- util_get_gs_path()
  ## call
  call <- paste(gsi_path, x)
  if(!quiet) print(call)
  if(run) system(call, intern=TRUE)
}


gc_upload <- function(...) {
  warning("Use rather gs_upload")
  gs_upload(...)
}

gs_upload <- function(local_path, gs_path, quiet=TRUE, run=TRUE,
                      cmd = "cp", normalize=FALSE){

  gs_path <- util_check_add_gs(gs_path)

  ## check file(s) exist
  has_wildcard <- stringr::str_detect(basename(local_path), "\\*")
  if(!file.exists(local_path) && !has_wildcard) warning("local_path not found?")
  if(has_wildcard && !dir.exists(dirname(local_path))) warning("basename of local_path not found?")


  local_path <- stringr::str_replace(local_path, "\\.shp$", "\\.\\*")

  ## Replace spaces
  if(normalize) {
    local_path <- base::normalizePath(local_path)
  }
  if(stringr::str_detect(local_path, " ")) {
    local_path <- paste0("'", local_path, "'")
  }
  if(stringr::str_detect(gs_path, " ")) {
    gs_path <- paste0("'", gs_path, "'")
  }

  ##
  gs_call <- paste("gsutil -m ", cmd, local_path,  gs_path)
  if(!quiet) print(gs_call)
  if(run) system(gs_call, intern=TRUE)
}

gs_download <- function(input = "gs://bucket_name/folder/*",
                        output,
                        quiet=TRUE,
                        cmd = "rsync -r",
                        run=TRUE) {

  if(stringr::str_detect(input, "\\*") && stringr::str_detect(cmd, "rsync")) {
    warning("cannot do 'rsync' with wildcard... use rather cp -n !?")
    }

  if(stringr::str_detect(output, " ")) output <-  paste0("'", output, "'")
  cmd <- paste0("-m ",  cmd, " ", input, " ", output)
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
  if(!stringr::str_detect(gs_path, "^gs://")) gs_path <- paste("gs://", gs_path, sep="")
  if(!stringr::str_detect(gs_path, " ")) gs_path <- paste0("'", gs_path, "'")
  gs_path
}

util_get_gs_path <- function(){
  out <- system("which gsutil", intern=TRUE)
  if("status" %in% names(attributes(out)) && attr(out, "status")==1) warning("Path not found?")
  out
}

util_get_ee_path <- function(){

  renviron_path <- Sys.getenv("EARTHENGINE_PYTHON")
  if(renviron_path!="") {
    renviron_path <- stringr::str_replace(renviron_path, "python$", "earthengine")
    return(renviron_path)
  }
  system_path <- suppressWarnings(system("which earthengine", intern=TRUE))
  is_inpath <- is.null(attr(system_path, "status"))
  if(is_inpath) return(system_path)
  warning("No earthengine found, set env variable 'EARTHENGINE_PYTHON'")
}


###
ee_call_any <- function(call, quiet=TRUE, run = TRUE){

  ee_path <- util_get_ee_path()

  ## call
  call_full <- paste(ee_path, call)
  if(!quiet) print(call_full)
  if(run) system(call_full, intern=TRUE)
}


ee_check_has_asset <- function(ee_id, user_name, quiet=TRUE, run=TRUE) {

  ee_id_base <- dirname(ee_id)
  ## if returns just ., this means info is stored in user_name  
  if(ee_id_base==".") ee_id_base <- user_name
    
  # call
  ee_call <- paste("ls", ee_id_base)
  out_check <- ee_call_any(ee_call, quiet=quiet, run = run)

  # ana
  any(stringr::str_detect(out_check, ee_id))
}

#' @examples
#' util_format_id("test", "samOne")
#' util_format_id("users/samOne/test")
#' util_format_id("projects/proj/test")
util_format_id <- function(ee_id=NULL, user_name=NULL) {
  if(grepl(ee_id, "^projects|^user")) return(ee_id)
  if(is.null(user_name)) return(ee_id)
  paste(user_name, ee_id, sep="/")
}

ee_rm <- function(ee_id=NULL, user_name, quiet=TRUE) {

  call <- paste("rm",  user_name, sep=" ")
  call <- paste(call, ee_id, sep="/")

  ee_call_any(x= call, quiet=quiet)
}


ee_upload <- function(ee_id,
                      user_name=NULL,
                      gs_file,
                      quiet=TRUE, type=c("table", "image"),
                      delete=FALSE,
                      check_ee_asset = TRUE,
                      run=TRUE) {

  type <- match.arg(type)

  gs_file <- util_check_add_gs(gs_file)

  ## check if there on gs
  if(!gs_check_is_there(gs_file)) warning("File not there on gs?")

  ## check if not already there on ee
  if(check_ee_asset) {
    is_there_ee <- ee_check_has_asset(ee_id, user_name, quiet, run=run)
  } else {
    is_there_ee <- FALSE
  }
  if(is_there_ee) {
    if(!quiet) print("Asset already there!")
    if(delete) {
      print("Deleting assert first")
      ee_rm(ee_id, user_name, quiet)
    } else {
      if(!quiet) print("Delete first asset with delete=TRUE")
      return(FALSE)
    }
  }


  ## upload
  ee_id_clean <- util_format_id(ee_id, user_name )
  call <- paste(" upload ",
                type, " --asset_id=",
                ee_id_clean,
                " ", gs_file, sep="")
  out <- ee_call_any(call, run=run, quiet=quiet)
  if(run) {
    return(tibble::tibble(gs=gs_file, id=ee_id, task=util_capt_task(out=out)))
  }
}




util_clean_ee_return <- function(out){
  if(any(stringr::str_detect(out, "Running command using"))){
    out <- out[!stringr::str_detect(out, "Running command using") & out!=""]
  }
  out
}

util_capt_task <- function(out) {
  out <- util_clean_ee_return(out)

  if(!stringr::str_detect(out, "Started upload task with ID")) {
    res <- out
  } else {
    res <- stringr::str_replace(out, "Started upload task with ID: ", "")
  }

  res
}


if(FALSE){
  gc_upload("../ProtectedAreas/conservation_units_legal_amazon_INPE/conservation_units_legal_amazon_CLEAN.shp",
            gs_path = "cropins_yield_us/transfer_forest/to_ee",
            quiet=TRUE)
}
