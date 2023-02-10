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
    cli::cli_alert_warning("{length(filtered.values)} {col.name} values were not found in {checklist.name} and will be removed from {df.name}.", wrap = TRUE)
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
  metaweb.colnames <- c("Prey_Code_new", "Pred_Code_new", "Co_occur")
  
  .fun_testIfInherits(x, "data.frame")
  .fun_testdfcolnames(x, metaweb.colnames)
  .fun_testIfInherits(x$Prey_Code_new, "character")
  .fun_testIfInherits(x$Pred_Code_new, "character")
  
  passed.check <- TRUE
  
  # Duplicated Prey_Name
  this.check <- 
    x %>% 
    group_by(Prey_Code_new) %>% 
    summarize(Prey_Name = unique(Prey_Name),
              .groups = 'keep') %>% 
    mutate(n = n()) %>% 
    filter(n > 1) %>% 
    summarize(Prey_Code_new = first(Prey_Code_new))
  
  if (nrow(this.check) > 0) {
    cli_alert_danger("The following Prey_Code_new have duplicated Prey_Name: {this.check$Prey_Code_new}")
    passed.check <- FALSE
  }
  
  # Duplicated Prey_Code_new
  this.check <- 
    x %>% 
    group_by(Prey_Name) %>% 
    summarize(Prey_Code_new = unique(Prey_Code_new),
              .groups = 'keep') %>% 
    mutate(n = n()) %>% 
    filter(n > 1) %>% 
    summarize(Prey_Name = first(Prey_Name))
  
  if (nrow(this.check) > 0) {
    cli_alert_danger("The following Prey_Name have duplicated Prey_Code_new: {this.check$Prey_Name}")
    passed.check <- FALSE
  }
  
  # Duplicated Pred_Name
  this.check <- 
    x %>% 
    group_by(Pred_Code_new) %>% 
    summarize(Pred_Name = unique(Pred_Name),
              .groups = 'keep') %>% 
    mutate(n = n()) %>% 
    filter(n > 1) %>% 
    summarize(Pred_Code_new = first(Pred_Code_new))
  
  if (nrow(this.check) > 0) {
    cli_alert_danger("The following Pred_Code_new have duplicated Pred_Name: {this.check$Pred_Code_new}")
    passed.check <- FALSE
  }
  
  # Duplicated Pred_Code_new
  this.check <- 
    x %>% 
    group_by(Pred_Name) %>% 
    summarize(Pred_Code_new = unique(Pred_Code_new),
              .groups = 'keep') %>% 
    mutate(n = n()) %>% 
    filter(n > 1) %>% 
    summarize(Pred_Name = first(Pred_Name))
  
  if (nrow(this.check) > 0) {
    cli_alert_danger("The following Pred_Name have duplicated Pred_Code_new: {this.check$Pred_Name}")
    passed.check <- FALSE
  }
  if (!passed.check) {
    stop("Duplicated code or species name in metaweb")
  }
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
  
  this.check <- x$SpeciesName[duplicated(x$SpeciesName)]
  if (length(this.check) > 0) {
    cli_alert_danger("The following SpeciesName in {x.name} are duplicated: \\
                     {this.check}")
    stop("Duplicated SpeciesName in checklist")
  }
  
  this.check <- x$Code[duplicated(x$Code)]
  if (length(this.check) > 0) {
    cli_alert_danger("The following Code in {x.name} are duplicated: \\
                     {this.check}")
    stop("Duplicated Code in checklist")
  }

  return(TRUE)
}


## .check_metaweb_checklist  ----------------------------
##' @name .check_metaweb_checklist
##' 
##' @title Check metaweb and checklist compatibility
##' 
##' @description Check that checklist and metaweb have the same code and 
##' species name
##' @inheritParams trophic_dataset
##' @return a boolean

.check_metaweb_checklist <- function(metaweb, checklist){
  metaweb.name <- deparse(substitute(metaweb))
  checklist.name <- deparse(substitute(checklist))
  checklist_metaweb_prey <- 
    group_by(metaweb, Prey_Code_new) %>% 
    summarize(Prey_Name = first(Prey_Name))
  checklist_metaweb_pred <- 
    group_by(metaweb, Pred_Code_new) %>% 
    summarize(Pred_Name = first(Pred_Name))
  passed.check <- TRUE
  # Prey ; Merge = Code 
  this.check <-
    checklist %>% 
    select(Code, SpeciesName) %>% 
    inner_join(checklist_metaweb_prey, by = c("Code" = "Prey_Code_new"))
  
  which.name <- which(this.check$SpeciesName != this.check$Prey_Name)
  if (length(which.name) > 0) {
    cli_alert_danger("Some values for {metaweb.name}$Prey_Name are different from \\
                     {checklist.name}$SpeciesName.
                     {metaweb.name}: {this.check$Prey_Name[which.name]}
                     {checklist.name}: {this.check$SpeciesName[which.name]}")
    passed.check <- FALSE
  }
  # Prey ; Merge = SpeciesName 
  this.check <-
    checklist %>% 
    select(Code, SpeciesName) %>% 
    inner_join(checklist_metaweb_prey, by = c("SpeciesName" = "Prey_Name"))
  
  which.name <- which(this.check$Code != this.check$Prey_Code_new)
  if (length(which.name) > 0) {
    cli_alert_danger("Some values for {metaweb.name}$Prey_Code_new are different from \\
                     {checklist.name}$Code
                     {metaweb.name}: {this.check$Prey_Code_new[which.name]}
                     {checklist.name}: {this.check$Code[which.name]}")
    passed.check <- FALSE
  }
  
  # Predator ; Merge = Code 
  this.check <-
    checklist %>% 
    select(Code, SpeciesName) %>% 
    inner_join(checklist_metaweb_pred, by = c("Code" = "Pred_Code_new"))
  
  which.name <- which(this.check$SpeciesName != this.check$Pred_Name)
  if (length(which.name) > 0) {
    cli_alert_danger("Some values for {metaweb.name}$Pred_Name are different from \\
                     {checklist.name}$SpeciesName.
                     {metaweb.name}: {this.check$Pred_Name[which.name]}
                     {checklist.name}: {this.check$SpeciesName[which.name]}")
    passed.check <- FALSE
  }
  # Predator ; Merge = SpeciesName 
  this.check <-
    checklist %>% 
    select(Code, SpeciesName) %>% 
    inner_join(checklist_metaweb_pred, by = c("SpeciesName" = "Pred_Name"))
  
  which.name <- which(this.check$Code != this.check$Pred_Code_new)
  if (length(which.name) > 0) {
    cli_alert_danger("Some values for {metaweb.name}$Pred_Code_new are different from \\
                     {checklist.name}$Code
                     {metaweb.name}: {this.check$Pred_Code_new[which.name]}
                     {checklist.name}: {this.check$Code[which.name]}")
    passed.check <- FALSE
  }
  
  if (!passed.check) {
    stop(paste0(metaweb.name, " and ", checklist.name, " have incompatible species name or code"))
  }
  TRUE
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


## Check positive numeric ----------------------------
##' @name .fun_testIfPosNum
##' 
##' @title Check if object is a positive numeric
##' 
##' @description Check if object is a positive numeric
##' 
##' @param x object to be checked
##' @return a boolean
##' @keywords internal
.fun_testIfPosNum <- function(x) {
  x.name <- deparse(substitute(x))
  if (!is.numeric(x)) {
    stop(paste0("\n", x.name, "must be a numeric"))
  } else if (any(x < 0)) {
    stop(paste0("\n", x.name, "must be a positive numeric"))
  }
  TRUE
}




## Check 0-1 numeric ----------------------------
##' @name .fun_testIf01
##' 
##' @title Check if object is included in 0-1
##' 
##' @description Check if object is included in 0-1
##' 
##' @param x object to be checked
##' @return a boolean
##' @keywords internal
.fun_testIf01 <- function(x){
  x.name <- deparse(substitute(x))
  .fun_testIfPosNum(x)
  if (any(x > 1)) {
    stop(paste0("\n", x.name, "must be a 0 to 1 numeric"))
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
##' @param open type of opening ('w' for standard, 'a' for appending)
##' @inheritParams taxonomic_conflict
##' @return NULL
##' @keywords internal
##' 
##' @importFrom cli cli_alert_info
##' @importFrom utils write.csv

.write_logfile <- function(out.log, logfile, project.name, open = "w", silent = FALSE){
  logdir <- paste0(project.name,"/log/")
  if (!dir.exists(logdir)) {
    dir.create(logdir, recursive = TRUE, showWarnings = FALSE)
  }
  
  thisfilename <- paste0(logdir,logfile)
  
  if (inherits(out.log, "data.frame")) {
    write.csv(out.log, paste0(logdir,logfile))
  } else {
    fileConn <- file(thisfilename, open = open)
    writeLines(out.log, fileConn)
    close(fileConn)
  }
  if (!silent) {
    cli_alert_info("Logs written to {thisfilename}")
  }
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
      return(TRUE)
    } else {
      warning("Parallelisation with `foreach` is not available for Windows. Sorry.")
      return(FALSE)
    }
  }
  FALSE
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
##' @importFrom grDevices hcl
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


## Check whether a name is present in sampling effort  ----------------------------
##' @name check_name_config
##' 
##' @title Check if a name is present in a sampling effort configuration
##' 
##' @description Check if a name is present in a sampling effort configuration
##' @inheritParams sampling_effort
##' @param thisname name to be checked
##' @return vector
##' @keywords internal

check_name_config <- function(sampling.effort.config, thisname)
{
  sapply(sampling.effort.config, function(x){
    thisname %in% x
  })
}

## Check taxonomic group  ----------------------------
##' @name check_taxa
##' 
##' @title Check if a taxonomic group is present in the checklist
##' 
##' @description Check if a taxonomic group is present in the checklist
##' @inheritParams sampling_effort
##' @param this.taxa \code{character} taxonomic group to be checked
##' @return a boolean
##' @keywords internal

check_taxa <- function(this.taxa, checklist){
  Species.range <- unique(checklist$SpeciesName)
  Family.range <- unique(checklist$Family)
  Order.range <- unique(checklist$Order)
  Class.range <- unique(checklist$Class)
  any(sapply(list(Species.range,
                  Family.range,
                  Order.range,
                  Class.range), function(z){
                    this.taxa %in% z
                  }))
}

## find_iucn_index  ----------------------------
##' @name find_iucn_index
##' 
##' @title Find a species in IUCN range data.frame
##' 
##' @description Find a species in IUCN range data.frame
##' @param df a \code{data.frame}
##' @param this.species a \code{character}, species to be found
##' @param species_col a \code{character}, column name for the species name
##' @param capitalized \code{boolean}, TRUE if column names are capitalized
##' @return a boolean
##' @keywords internal

find_iucn_index <- function(df, this.species, species_col, capitalized){
  if(capitalized){
    presence_col <- "PRESENCE"
    season_col <- "SEASONAL"
  } else {
    presence_col <- "presence"
    season_col <- "seasonal"
  } 
  which(df[,species_col] == this.species &
          df[,presence_col] %in% c(1,2) &
          df[,season_col] %in% c(1,2))
}


## get_predator_summary  ----------------------------
##' @name get_predator_summary
##' 
##' @title Get summary of predator occurrences
##' 
##' @description Get prevalence and summary of occurrences inside and outside
##' IUCN range for a given predator.
##' @param this.trophic a \code{data.frame} with occurrence data: columns cell,
##' x, y, presence, inside_iucn, SpeciesName, Code and all prey occurrences as 
##' columns 
##' @return a data.frame
##' @export

get_predator_summary <- function(this.trophic){
  tmp.inside <- this.trophic %>% 
    group_by(SpeciesName, presence, inside_iucn) %>% 
    summarize(n = n())
  tmp.prevalence <- this.trophic %>% 
    group_by(SpeciesName, presence) %>% 
    summarize(n = n())
  
  # presence
  presence <- tmp.prevalence$n[which(tmp.prevalence$presence == 1)]
  if (length(presence) == 0) presence <- 0
  # total absences
  absence_tot <- tmp.prevalence$n[which(tmp.prevalence$presence == 0)]
  if (length(absence_tot) == 0) absence_tot <- 0
  # absence inside IUCN range
  absence_inside <- tmp.inside$n[which(tmp.inside$presence == 0 &
                                         tmp.inside$inside_iucn)]
  if (length(absence_inside) == 0) absence_inside <- 0
  # absence outside IUCN range
  absence_outside <- absence_tot - absence_inside
  prevalence <- presence/(presence+absence_tot)
  nprey <- ncol(this.trophic)-7
  return(
    data.frame(
      "SpeciesName" = first(this.trophic$SpeciesName),
      "Code" = first(this.trophic$Code),
      "presence" = presence,
      "absence_tot" = absence_tot,
      "absence_inside" = absence_inside,
      "absence_outside" = absence_outside,
      "prevalence" = prevalence,
      "nprey" = nprey
    )
  )
}

## get_prey_summary  ----------------------------
##' @name get_prey_summary
##' 
##' @title Get summary of prey occurrences
##' 
##' @description Get prevalence and summary of occurrences inside and outside
##' IUCN range for the prey of a given predator.
##' @param this.trophic a \code{data.frame} with occurrence data: columns cell,
##' x, y, presence, inside_iucn, SpeciesName, Code and all prey occurrences as 
##' columns 
##' @return a data.frame
##' @export

get_prey_summary <- function(this.trophic){
  list.prey <- colnames(this.trophic[,-(1:7)])
  prey.summary <- foreach(this.prey = list.prey, .combine = 'rbind') %do% {
    tmp_prevalence <- 
      this.trophic %>% 
      filter(presence == 1) %>% 
      .[,..this.prey] 
    prevalence_pred1 <- sum(tmp_prevalence)/nrow(tmp_prevalence)
    prevalence <- sum(this.trophic[,..this.prey])/nrow(this.trophic[,..this.prey])
    absence <- length(which(this.trophic[,..this.prey] == 0))
    presence <- length(which(this.trophic[,..this.prey] == 1))
    data.frame(
      "SpeciesName" = first(this.trophic$SpeciesName),
      "Code" = first(this.trophic$Code),
      "Prey_Code" = this.prey,
      "prevalence" = prevalence,
      "absence" = absence,
      "presence" = presence,
      "prevalence_pred1" = prevalence_pred1
    )
  }
  prey.summary
}
  