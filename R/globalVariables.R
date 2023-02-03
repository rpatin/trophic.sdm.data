
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
                                 "coordinatePrecision",
                                 "distance_to_iucn"))

# summary_conflict ----------------------------------------------------
utils::globalVariables(names = c("X", "Y", "species",
                                 "coordinatePrecision",
                                 "distance_to_iucn"))





