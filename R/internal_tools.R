## Filter metaweb by checklist ----------------------------
##' @name .filter_df_by_checklist
##' 
##' @title filter metaweb to keep only valid species code
##' 
##' @description Filter metaweb to keep only valid species code
##' 
##' @param df a data.frame to be checked
##' @param col.name a character determining the column name to check
##' @inheritParams taxonomic_conflict
##' @return a data.frame
##' @keywords internal
##' 
##' @importFrom cli cli_alert_warning

.filter_df_by_checklist <- function(df, checklist, col.name, project.name){
  df.name <- deparse(substitute(df))
  checklist.name <- deparse(substitute(checklist))
  
  to.be.filtered <- !df[,col.name] %in% checklist$Code
  if (any(to.be.filtered)) {
    filtered.values <- df[to.be.filtered, col.name]
    logfile <- paste0("filter_", df.name, "_" , col.name, ".log")
    cli::cli_alert_warning("{length(filtered.values)} {col.name} values were not found in {checklist.name} and will be removed from {df.name}. Filtered values will be written to log/{logfile}", wrap = TRUE)
    .write_logfile(out.log = filtered.values, 
                   logfile = logfile, 
                   project.name = project.name)
  } 
  df[!to.be.filtered, ]
}

## Check metaweb ----------------------------
##' @name .check_metaweb
##' 
##' @title Check metaweb data.frame
##' 
##' @description Check whether a data.frame is a valid metaweb
##' 
##' @param x data.frame to be checked
##' @return a boolean
##' @keywords internal

.check_metaweb <- function(x){
  x.name <- deparse(substitute(x))
  metaweb.colnames <- c("Prey_Code_new", "Pred_Code_new")
  
  .fun_testIfInherits(x, "data.frame")
  .fun_testdfcolnames(x, metaweb.colnames)
  .fun_testIfInherits(x$Prey_Code_new, "character")
  .fun_testIfInherits(x$Pred_Code_new, "character")
  return(TRUE)
}

## Check checklist ----------------------------
##' @name .check_checklist
##' 
##' @title Check checklist data.frame
##' 
##' @description Check whether a data.frame is a valid 
##' checklist
##' 
##' @param x data.frame to be checked
##' @return a boolean
##' @keywords internal

.check_checklist <- function(x){
  x.name <- deparse(substitute(x))
  checklist.colnames <- c("Code", "Class", "Order", "Family", "SpeciesName")
  
  .fun_testIfInherits(x, "data.frame")
  .fun_testdfcolnames(x, checklist.colnames)
  .fun_testIfInherits(x$Code, "character")
  .fun_testIfInherits(x$Class, "character")
  .fun_testIfInherits(x$Order, "character")
  .fun_testIfInherits(x$Family, "character")
  .fun_testIfInherits(x$SpeciesName, "character")
  return(TRUE)
}


## Check trophic.groups ----------------------------
##' @name .check_trophic.groups
##' 
##' @title Check trophic.groups data.frame
##' 
##' @description Check whether a data.frame is a valid 
##' trophic.groups
##' 
##' @param x data.frame to be checked
##' @return a boolean
##' @keywords internal

.check_trophic.groups <- function(x){
  x.name <- deparse(substitute(x))
  trophic.groups.colnames <- c("group", "Code")
  
  .fun_testIfInherits(x, "data.frame")
  .fun_testdfcolnames(x, trophic.groups.colnames)
  .fun_testIfNum(x$group)
  .fun_testIfInherits(x$Code, "character")
  return(TRUE)
}

## Check class inheritance ----------------------------
##' @name .fun_testIfInherits
##' 
##' @title Check class of an object
##' 
##' @description Check whether an object inherits a given class
##' 
##' @param x object to be checked
##' @param thisclass a class to check
##' @return a boolean
##' @keywords internal

.fun_testIfInherits <- function(x, thisclass){
  x.name <- deparse(substitute(x))
  if (!inherits(x, thisclass)){
    stop(paste0(x.name, " must be a valid ", thisclass))
  }
  TRUE
}

## Check positive integer ----------------------------
##' @name .fun_testIfPosInt
##' 
##' @title Check if object is a positive integer
##' 
##' @description Check if object is a positive integer
##' 
##' @param x object to be checked
##' @return a boolean
##' @keywords internal
.fun_testIfPosInt <- function(x)
{
  x.name <- deparse(substitute(x))
  if (!is.numeric(x)) {
    stop(paste0("\n", x.name, "must be a integer"))
  } else if (any(x < 0) || any(x %% 1 != 0)) {
    stop(paste0("\n", x.name, "must be a positive integer"))
  }
  TRUE
}

## Check numeric ----------------------------
##' @name .fun_testIfNum
##' 
##' @title Check if object is a numeric
##' 
##' @description Check whether an object is a numeric
##' 
##' @param x object to be checked
##' @return a boolean
##' @keywords internal
.fun_testIfNum <- function(x)
{
  x.name <- deparse(substitute(x))
  if (!is.numeric(x)) {
    stop(paste0("\n", x.name, "must be a numeric"))
  }
  TRUE
}

## Check object values ----------------------------
##' @name .fun_testIfIn
##' 
##' @title Check object values
##' 
##' @description Check whether an object only contains a set of values
##' 
##' @param x object to be checked
##' @param values a set of values
##' @return a boolean
##' @keywords internal

.fun_testIfIn <- function(x, values)
{
  x.name <- deparse(substitute(x))
  if (any(!(x %in% values))) {
    stop(paste0("\n", x.name, " must be '", 
                ifelse(length(values) > 1, 
                       paste0(paste0(values[1:(length(values) -1)], collapse = "', '"),
                              "' or '", values[length(values)])
                       , paste0(values,"'"))))
  }
  TRUE
}

## Check data.frame column names ----------------------------
##' @name .fun_testdfcolnames
##' 
##' @title Check data.frame column names
##' 
##' @description Check whether a data.frame have valid column names
##' 
##' @param x object to be checked
##' @param thiscolnames a set of column nmaes
##' @return a boolean
##' @keywords internal

.fun_testdfcolnames <- function(x, thiscolnames)
{
  x.name <- deparse(substitute(x))
  if (!all(thiscolnames %in% colnames(x))) {
    stop(paste0(x.name, " must have all the following columns: ", paste0(thiscolnames, collapse = " ; ")))
  }
  TRUE
}


## Write logfile ----------------------------
##' @name .write_logfile
##' 
##' @title Write a logfile
##' 
##' @description Write a logfile and create appropriate folders if needbe
##' 
##' @param out.log character to be written to the logfile
##' @param logfile file name
##' @inheritParams taxonomic_conflict
##' @return NULL
##' @keywords internal

.write_logfile <- function(out.log, logfile, project.name){
  logdir <- paste0(project.name,"/log/")
  if (!dir.exists(logdir)) {
    dir.create(logdir, recursive = TRUE, showWarnings = FALSE)
  }
  fileConn <- file(paste0(logdir,logfile))
  writeLines(out.log, fileConn)
  close(fileConn)
  invisible(NULL)
}

## Check folder existance ----------------------------
##' @name .fun_testIfDirExists
##' 
##' @title Check folder existence
##' 
##' @description Check whether a folder exists
##' @param x folder to check
##' @return a boolean
##' @keywords internal

.fun_testIfDirExists <- function(x){
  x.name <- deparse(substitute(x))
  if (!dir.exists(x)){
    stop(paste0(x.name, " must be an existing folder"))
  }
  TRUE
}



## Register cluster ----------------------------
##' @name .register_cluster
##' 
##' @title Register a cluster if need be
##' 
##' @description Register a cluster to be used with \code{foreach}
##' @param nb.cpu number of CPU to use
##' @return NULL
##' @keywords internal

.register_cluster <- function(nb.cpu){
  if (nb.cpu > 1) {
    if (.getOS() != "windows") {
      if (!isNamespaceLoaded("doParallel")) { 
        if(!requireNamespace('doParallel', quietly = TRUE)) stop("Package 'doParallel' not found")
      }
      doParallel::registerDoParallel(cores = nb.cpu)
    } else {
      warning("Parallelisation with `foreach` is not available for Windows. Sorry.")
    }
  }
  invisible(NULL)
}

## Get OS name ----------------------------
##' @name .getOS
##' 
##' @title get OS type
##' 
##' @description get OS type
##' @return character
##' @keywords internal

.getOS = function()
{
  sysinf = Sys.info()
  if (!is.null(sysinf))  {
    os = sysinf['sysname']
    if (os == 'Darwin') os = "mac"
  } else  {
    os = .Platform$OS.type
    if (grepl("^darwin", R.version$os)) os = "mac"
    if (grepl("linux-gnu", R.version$os)) os = "linux"
  }
  return(tolower(os))
}

## Emulate ggplot2 color scale ----------------------------
##' @name gg_color_hue
##' 
##' @title Emulate ggplot2 color scale
##' 
##' @description Emulate ggplot2 color scale
##' @param n number of colors
##' @return character
##' @keywords internal
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}