# return a list of project_id values that can be used to filter bmde data

bscdata.listBMDEProjects <- function(showall=FALSE, datasource="bmdedata") {

 sql <- paste("select project_id, project_code, project_name from projects",
    ifelse(showall,""," where exists (select record_id from bmde_data where bmde_data.project_id = projects.project_id)"), sep="")
 channel <- odbcConnect(datasource)
 table1 <- sqlQuery(channel, sql)
 odbcClose(channel)
 rm(channel)
 return(table1)

}
