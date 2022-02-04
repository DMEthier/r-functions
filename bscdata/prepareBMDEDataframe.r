# this function takes a BSC-specific BMDE dataframe and makes it compatible with BMDE fields.
# If keepnsf is TRUE, fields that are not part of the BMDE standard (non-standard fields) are removed

bscdata.prepareBMDEDataframe <- function(df, keepnsf=FALSE) {

  fields <- bscdata.getBMDEFieldConversions()

  # rename the BSC index fields to BMDE standard names
  for (i in 1:length(fields$bmdefield)) {
    if (fields$bscfield[i] %in% names(df)) {
      # removes the existing BMDE fields that will be replaced, then rename the BSC field
      df <- subset(df, select=!(names(df) == fields$bmdefield[i]))
      names(df)[which(names(df) == fields$bscfield[i])] <- as.character(fields$bmdefield[i])
    }
  }

  if (!keepnsf) {
    # removes the fields not conforming to BMDE standards
    df <- subset(df, select=!(names(df) %in% c("record_id", "protocol_id", "protocol_type", "species_id", "survey_week", "bcr")))
  }

  return(df)

}

