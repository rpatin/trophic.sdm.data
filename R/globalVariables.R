
# calc_sampling_effort ----------------------------------------------------
utils::globalVariables(names = c("this.layer",
                                 "this.species"))

# detect_gbif_outsider ----------------------------------------------------
utils::globalVariables(names = c("distance_to_iucn",
                                 "this.species",
                                 "outside.iucn",
                                 "Class","Order","Family",
                                 "q5","q25","q75","q95", "median",
                                 "thiscode",
                                 "presence"))

# detect_taxonomic_conflict ----------------------------------------------------
utils::globalVariables(names = c("Code",
                                 "group",
                                 "Class",
                                 "Order",
                                 "Family",
                                 "Prey_Code_new",
                                 "Pred_Code_new",
                                 "Prey_Name",
                                 "Pred_Name",
                                 "nClass",
                                 "nOrder",
                                 "nFamily",
                                 "ntot"))
# load_gbif_data ----------------------------------------------------
utils::globalVariables(names = c("X", "Y", "species", 
                                 "coordinateUncertaintyInMeters",
                                 "coordinatePrecision",
                                 "distance_to_iucn"))

# summary_conflict ----------------------------------------------------
utils::globalVariables(names = c("X", "Y", "species",
                                 "coordinateUncertaintyInMeters",
                                 "coordinatePrecision",
                                 "distance_to_iucn"))



# buffer_IUCN -------------------------------------------------------------

utils::globalVariables(names = c("species.buffer"))

# rasterize_IUCN -------------------------------------------------------------
utils::globalVariables(names = c("species.buffer"))


# get_trophic_summary -------------------------------------------------------------

utils::globalVariables(names = c("SpeciesName",
                                 "inside_iucn"))

# get_prey_summary -------------------------------------------------------------

utils::globalVariables(names = c(".",
                                 "..this.prey",
                                 "this.prey"))

# prepare_dataset -------------------------------------------------------------

utils::globalVariables(names = c("param.filter",
                                 "species.buffer",
                                 "species.quantile",
                                 "inside_iucn",
                                 "mask.grid",
                                 "layer",
                                 "Co_occur",
                                 "Score_DD",
                                 "Stage",
                                 "status",
                                 "this.prey",
                                 "cell","x","y",
                                 "percent_certain",
                                 "occurrence.dir",
                                 "occurrence.rast.dir",
                                 "status.rast.dir",
                                 "dataset.dir",
                                 "raw.dir",
                                 "presence_unfiltered",
                                 "absence.inside.certain",
                                 "absence_inside",
                                 "absence.outside",
                                 "absence_outside",
                                 "nprey.unfiltered",
                                 "presence.filtered",
                                 "absence.filtered.inside",
                                 "absence.filtered.outside",
                                 "absence.inside.uncertain",
                                 "nprey.filtered",
                                 "nprey"))

# filter_dataset -------------------------------------------------------------
utils::globalVariables(names = c("checklist",
                                 "absence",
                                 "SpeciesName",
                                 "Prey_Code",
                                 "prevalence",
                                 "prevalence_pred1"))

# assemble_trophic -------------------------------------------------------------
utils::globalVariables(names = c("sppname",
                                 "this.group"))

# check_gbif_duplicata -------------------------------------------------------------
utils::globalVariables(names = c("URI_GBIF"))

# extract_gbif -------------------------------------------------------------
utils::globalVariables(names = c("last",
                                 "n.presences",
                                 "n.absences",
                                 "n.absences.outside",
                                 "n.absences.inside.certain",
                                 "n.absences.inside.uncertain"))

# extract_iucn -------------------------------------------------------------
utils::globalVariables(names = c("n.presences",
                                 "n.absences",
                                 "n.absences.outside",
                                 "n.absences.inside.certain",
                                 "n.absences.inside.uncertain"))


# map_effort -------------------------------------------------------------
utils::globalVariables(names = c("effort_char"))

# map_prey_filtering -------------------------------------------------------------
utils::globalVariables(names = c("effort_char"))

# map_uncertain -------------------------------------------------------------
utils::globalVariables(names = c("effort_char"))

# summary_outsider -----------------------------------------------------------

utils::globalVariables(names = c("q80",
                                 "q85",
                                 "q90"))

# plot_dataset ---------------------------------------------------------------
utils::globalVariables(names = c("presence_plot"))

# plot_trophic ---------------------------------------------------------------
utils::globalVariables(names = c("datatype"))

# plot_uncertain ---------------------------------------------------------------
utils::globalVariables(names = c("datatype"))

# rasterize_prey_filtering ---------------------------------------------------------------
utils::globalVariables(names = c("this.taxa"))

# rasterize_uncertain ---------------------------------------------------------------
utils::globalVariables(names = c("this.taxa"))


# subsample ---------------------------------------------------------------
utils::globalVariables(names = c("aim_presence",
                                 "aim_absence_inside",
                                 "tot_inside",
                                 "aim_outside",
                                 "filter_presence",
                                 "filter_absence_inside",
                                 "filter_absence_outside"))

# summary_trophic ---------------------------------------------------------------
utils::globalVariables(names = c("absence.inside.uncertain",
                                 "tot.iucn"))



