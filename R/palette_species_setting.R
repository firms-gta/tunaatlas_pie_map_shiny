# https://www.rapidtables.com/convert/color/hex-to-rgb.html
# https://www.r-bloggers.com/2020/03/how-to-standardize-group-colors-in-data-visualizations-in-r/
palette3_info <- brewer.pal.info[brewer.pal.info$category == "qual", ]  
palette3_all <- unlist(mapply(brewer.pal, 
                              palette3_info$maxcolors,
                              rownames(palette3_info)))
set.seed(2643598)  
palette3 <- sample(palette3_all, nrow(target_flag), replace=TRUE)
names(palette3) = target_flag$fishing_fleet
palette3

palette3_speciesinfo <- brewer.pal.info[brewer.pal.info$category == "qual", ]  
palette3_species <- unlist(mapply(brewer.pal, 
                                  palette3_speciesinfo$maxcolors,
                                  rownames(palette3_speciesinfo)))
set.seed(2643598)  
# palette3 <- sample(palette3_all, nrow(unique(df_i11_map$fishing_fleet)), replace=TRUE)
palette_species <- sample(palette3_species, nrow(target_species), replace=TRUE)
names(palette_species) = target_species$species