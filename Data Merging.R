# Read in raw data ----
MASTER <- read.csv('Raw Data/Master Facility List.csv')
DENOM_FACILITIES <- read.delim('Raw Data/Denominator Facilities.txt')
DENOM_TRANSFERS <- read.delim('Raw Data/Denominator Transfers.txt')
CASES <- read.csv('Raw Data/ARI Cases.csv')

# Preliminary data cleaning
MASTER$CCN[which(MASTER$CCN == '')] <- NA

# Create merged facilities dataframe (nodes)
merge_facilities <- function(){

  # Find master list indices for ARI facilties and facilities in HD3
  unique_gaids <- unique(c(levels(CASES$ari_residence), levels(CASES$ari_collect), levels(CASES$ari_dxto)))
  masterlist_indices <- which(MASTER$facility_id %in% unique_gaids)
  no_ari_indices <- which(MASTER$Area == 'HD3' & MASTER$CCN %in% DENOM_FACILITIES$numid)
  masterlist_indices <- unique(c(masterlist_indices, no_ari_indices))

  # Create an initial dataframe
  merged <- data.frame('id' = MASTER$facility_id[masterlist_indices],
                       'ccn' = MASTER$CCN[masterlist_indices],
                       'type' = MASTER$facility_type[masterlist_indices],
                       'name' = MASTER$facility_name[masterlist_indices],
                       'city' = MASTER$city[masterlist_indices],
                       'state' = MASTER$state[masterlist_indices],
                       'zip' = MASTER$zip[masterlist_indices],
                       'county' = MASTER$county[masterlist_indices],
                       'area' = MASTER$Area[masterlist_indices])

  # Add in the GA IDs that aren't in the master list
  not_in_masterlist_ids <- unique_gaids[which(!(unique_gaids %in% MASTER$facility_id))]
  not_in_masterlist <- data.frame('id' = not_in_masterlist_ids, 'ccn' = NA, 'type' = NA, 'name' = NA, 'city' = NA, 'state' = NA, 'zip' = NA, 'county' = NA, 'area' = NA)
  merged <- rbind(merged, not_in_masterlist)

  # Merge the relevant DENOM data, and keep ARI data that doesn't match DENOM
  merged <- merge(merged, DENOM_FACILITIES[,c('numid', 'stays', 'fac_type', 'patient_days')], by.x = 'ccn', by.y = 'numid', all.x = TRUE)

  # Add in a prevalence metric, which is how many cases a facility appears in
  prevalence <- vapply(as.character(merged$id), function(x){
    length(which(CASES$ari_residence == x | CASES$ari_collect == x | CASES$ari_dxto == x))
  }, integer(1), USE.NAMES = FALSE)

  merged <- cbind(merged, prevalence)

  return(merged)

}

# Create merged transfers dataframe (edges)
merge_transfers <- function(){

  # Create a merged facilities list to create edges from
  merged_facilities <- merge_facilities()

  # Convert from cases to edges (numerator data)
  merged_edges <- data.frame('from' = c(as.character(CASES$ari_residence), as.character(CASES$ari_collect)),
                             'to' = c(as.character(CASES$ari_collect), as.character(CASES$ari_dxto)))

  # Aggregate the edge counts
  merged_edges <- aggregate(list('ari' = rep(1,nrow(merged_edges))), merged_edges, length)

  # Find relevant DENOM edges (origin and destination facilities are in the merged facilities list and include indirects)
  relevant_denom_transfers <- DENOM_TRANSFERS[which(DENOM_TRANSFERS$numid1 %in% merged_facilities$ccn
                                                & DENOM_TRANSFERS$numid2 %in% merged_facilities$ccn
                                                & DENOM_TRANSFERS$timeinterval == 365),]

  # Merge the origin and destination IDs with their CCNs
  relevant_denom_transfers <- merge(relevant_denom_transfers, merged_facilities[,c('id', 'ccn')], by.x = 'numid1', by.y = 'ccn')
  relevant_denom_transfers <- merge(relevant_denom_transfers, merged_facilities[,c('id', 'ccn')], by.x = 'numid2', by.y = 'ccn', suffixes = c('_from' ,'_to'))

  # Merge in DENOM transfers
  merged_edges <- merge(merged_edges, relevant_denom_transfers, by.x = c('from', 'to'), by.y = c('id_from', 'id_to'), all = TRUE)

  # Organize the columns (and trim off timeinterval)
  merged_edges <- merged_edges[,c('from', 'to', 'numid1', 'numid2', 'ari', 'transfers')]

  # NA ari should actually be 0
  merged_edges[which(is.na(merged_edges$ari)), 'ari'] <- 0

  # Create a parameter for ARI transmissions as a percentage of total transmissions
  percent_ari <- merged_edges$ari / abs(merged_edges$transfers)  # Absolute value to account for DENOM negatives
  percent_ari[which(percent_ari >= 1)] <- percent_ari[which(percent_ari >= 1)] / 10  # And then divide those by 10
  merged_edges <- cbind(merged_edges, percent_ari)

  return(merged_edges)

}

# Write resulting nodes and edges to CSV
write.csv(merge_transfers(), 'Cleaned Data/Merged Transfers.csv', row.names = FALSE)
write.csv(merge_facilities(), 'Cleaned Data/Merged Facilities.csv', row.names = FALSE)
