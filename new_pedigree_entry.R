rm(list=ls())
#Overall Purpose:
#1. To build germplasm list and design import file for BMS from establishment report
#   Adds new pedigree entries to BMS before building germplasm list 

#1. new_pedigree_entry.R
#2. design_import_bms.R
#3. trial_grid_build.R 

#Pre-requisites
#Transfer design files from 1Trials to "C:\Users\MarkPaget\Documents\Design Import\BMS\YYYY\Trial Abbr"
#Add new clone IDs to the design file (Tree) before loading
#First add new pedigree entries to BMS (new_pedigree_entry.R)
#Need to add Tid to the design files. This is currently done manually, but the current Tid range in BMS can be found using 'dbReadTable(con,"nd_experimentprop")' 
#Use 'design_import_BMS.R' to build germplasm list and design file
#Use 'trial_grid_build.R' to add trial rows ('FIELDMAP RANGE') and trial columns ('FIELDMAP COLUMN')
#After trial row and columns have been added, load trial design with plot coordinates to BMS

library(readxl)
library(writexl)
library(openxlsx)
library(tidyverse)
library(DBI)

source("C:/Users/MarkPaget/Documents/Evaluations/Production/pedigree_fetch.R")

year <- 2025
fpath <- "C:/Users/MarkPaget/Documents/Design Import/BMS/2025"
list.files(fpath)
(trialname <- "BC25TU11CFC")
(strialname <- "25TU11")
fpath.full <- paste0("C:/Users/MarkPaget/Documents/Design Import/BMS/2025/",strialname,"/")
#df <- read.csv("S:/Data/Katmandoo/RPBC/Design Import/2024/24BT01_design_with_Replacements.csv")
df <- read_excel(paste0(fpath.full,strialname,"_Design_with_Replacements.xlsx"), sheet='25TU11_design')
head(df)

max(df$Pset)
max(df$Pset)*36

#Tid assignment (can be done manually but check current range below)
#need a Tid as Tree ID here for OP replacements 
#get current Tid range

con <- dbConnect(RMySQL::MySQL(),
                 dbname = Sys.getenv("DB_NAME"),
                 host = Sys.getenv("DB_HOST"),
                 port = 23306,
                 user = Sys.getenv("DB_USER"),
                 password = Sys.getenv("DB_PASSWORD"))

tables <- dbListTables(con)

design <- dbReadTable(con,"nd_experimentprop")  #type_id
head(design)

tid_value <- design[design$type_id %in% '100768',] #type_id 100768 == TID (value = TID)
head(tid_value)
length(unique(tid_value$nd_experiment_id))

head(tid_value)
range(tid_value$value)
#[1] ""         "73140620"
tid_value$Tid <- as.numeric(tid_value$value)
#tid_value_noNA <- tid_value[!tid_value$value %in% '',]
range(tid_value$Tid,na.rm=TRUE)
max(tid_value$Tid,na.rm=TRUE)
(latest_Tid <- max(tid_value$Tid,na.rm=TRUE)+1)

df$Tid <- seq(from = latest_Tid , length.out = nrow(df), by = 1)
range(df$Tid)

cat("#",min(df$Tid), "to",max(df$Tid),"-",strialname, "\n")
#min "18000001" 
#73098885 to 73102988 - 24BT01
#73102989 to 73106228 - 24TU02
#73106229 to 73109828 - 24BP03
#73109829 to 73113068 - 24BP04
#73113069 to 73117388 - 24HB05
#73117389 to 73121348 - 24NE06
#73121349 to 73124948 - 24OT07
#73124949 to 73128548 - 24SD08
#73128549 to 73130060 - 24BP09
#73130061 to 73131572 - 24NE10
#73131573 to 73133072 - 24OT11
#73133073 to 73134572 - 24WK12
#73134573 to 73137308 - 24BP17
#73137309 to 73140620 - 24CA18
#73140621 to 73143500 - 25AK01
#73143501 to 73146380 - 25HB02
#73146381 to 73149260 - 25OT03
#73149261 to 73152860 - 25WK04
#73152861 to 73155740 - 25WP05
#73155741 to 73159340 - 25BT10
#73159341 to 73163588 - 25TU11

dbDisconnect(con)

#get pedigree
pedigree_df <- extract_pedigree(
  db_host = Sys.getenv("DB_HOST"),
  db_port = 23306,
  db_name = Sys.getenv("DB_NAME"),
  db_user = Sys.getenv("DB_USER"),
  db_password = Sys.getenv("PASSWORD")
)


head(df)
df[is.na(df$Tree),c('Tree','Tid','Familycode','Replacement_line')] #list missing trees
unique(df$Familycode[is.na(df$Tree)])

unique(df[!df$Tree %in% pedigree_df$Tree,c("Tree","Fieldcode")])

df$Tree.tmp <- df$Tree #create temp Tree id column
df$Tree[is.na(df$Tree)]
df$Tree[is.na(df$Tree)] <- df$Tid[is.na(df$Tree)]

new_ped_entry <- df[!df$Tree.tmp %in% pedigree_df$Tree,]
head(new_ped_entry)

unique(df$Familycode[is.na(df$Tree.tmp)])
unique(df$Fieldcode[is.na(df$Tree.tmp)])

table(df$Familycode)
nrow(df[df$Familycode %in% 'GF19',])
table(df$PType)
table(df$Clone)
#new clones may need manually handling (allocating new Tree IDs)

new_ped_entry <- new_ped_entry[,c('Tree','Tree.tmp','Fieldcode','Familycode','Fcln','Mcln')]
new_ped_entry$ENTRY_NO <- seq(1,nrow(new_ped_entry))
new_ped_entry$NURSERY_CODE <- new_ped_entry$Fieldcode
new_ped_entry$COLL_SOU_AP_text <- ""#"13/645/A" #seedlot GF19 only
new_ped_entry$`PREFERRED NAME` <- "TREE"
new_ped_entry$`LOCATION ABBR` <- "AUS"
new_ped_entry$REFERENCE <- "RPBCpedigree"
new_ped_entry$`BREEDING METHOD` <- "C2W"
new_ped_entry$`CREATION DATE` <- ""
#new_ped_entry$NOTE_AA_text <- "GF19"
new_ped_entry$ORI_COUN_AP_text <- "NZL"
new_ped_entry$GENUS_AP_text <- "Pinus"
new_ped_entry$SPECIES_AP_text <- "Pinus radiata"
new_ped_entry$SPP_CODE_AP_text <- "PR"
new_ped_entry$BRDR_AA_text <- "RPBC"
new_ped_entry$CATEGORY_AA_text <- "Ortet"
new_ped_entry$SOW_DATE_AP_text <- "2022"
new_ped_entry$`STORAGE LOCATION ABBR` <- "" 
new_ped_entry$UNITS <- ""
new_ped_entry$AMOUNT <- ""
new_ped_entry$`STOCK ID` <- ""
new_ped_entry$PUI <- ""

write.csv(new_ped_entry,"./BMS/2025/25TU11_new_ped_entries.csv",row.names=FALSE) #upload to BMS (import germplasm)
new_ped_entry[new_ped_entry$ENTRY_NO %in% 10,]

#reload pedigree
pedigree_df <- extract_pedigree(
  db_host = Sys.getenv("DB_HOST"),
  db_port = 23306,
  db_name = Sys.getenv("DB_NAME"),
  db_user = Sys.getenv("DB_USER"),
  db_password = Sys.getenv("DB_PASSWORD")
)

#PROGENITORS may have to be add to spreadsheet manually or use code below if Fcln and Mcln are supplied

newg <- read_excel("./BMS/2025/25TU11_new_ped_entries.xlsx") #include Fcln and Mcln in this file
head(newg)
head(pedigree_df)
updateg <- merge(newg,pedigree_df[,c('Tree','gid','germplsm_uuid')],by='Tree')
head(updateg)
colnames(updateg)[colnames(updateg) == "germplsm_uuid"] <- "GUID"
colnames(updateg)[colnames(updateg) == "gid"] <- "GID"

pedigree_df$Fcln_Tree <- pedigree_df$Tree
colnames(updateg)[colnames(updateg) == "Fcln"] <- "Fcln_Tree"
updateg.1 <- merge(updateg,pedigree_df[,c('Fcln_Tree','gid','germplsm_uuid')],by='Fcln_Tree',all.x=TRUE)
head(updateg.1)
colnames(updateg.1)[colnames(updateg.1) == "germplsm_uuid"] <- "PROGENITOR_1_GUID"
colnames(updateg.1)[colnames(updateg.1) == "gid"] <- "PROGENITOR 1"

pedigree_df$Mcln_Tree <- pedigree_df$Tree
colnames(updateg.1)[colnames(updateg.1) == "Mcln"] <- "Mcln_Tree"
updateg.2 <- merge(updateg.1,pedigree_df[,c('Mcln_Tree','gid','germplsm_uuid')],by='Mcln_Tree',all.x=TRUE)
head(updateg.2)
colnames(updateg.2)[colnames(updateg.2) == "germplsm_uuid"] <- "PROGENITOR_2_GUID"
colnames(updateg.2)[colnames(updateg.2) == "gid"] <- "PROGENITOR 2"

head(updateg.2,7)


updateg.3 <- updateg.2[,c('Tree','PREFERRED NAME','LOCATION ABBR','REFERENCE','BREEDING METHOD','CREATION DATE',
                      'GID','GUID','PROGENITOR 1','PROGENITOR 2')]

#write.csv(updateg,"25AK01_new_ped_entries_UPDATE.csv",row.names=FALSE) #upload to BMS
wb <- createWorkbook()

# Add a sheet and write data
addWorksheet(wb, "Observation")
writeData(wb, "Observation", updateg.3)

# Save workbook
strialname
getwd()
saveWorkbook(wb,paste0("./BMS/",year,"/",strialname,"_new_ped_entries_UPDATE.xlsx"),
             overwrite = TRUE)

#upload the pedigree update to BMS (import germplasm updates)

#now add new ids to design file
head(df)
head(newg)

df[is.na(df$Tree.tmp)&df$Clone==1,c('Tree','Tid','Familycode')]
unique(df$Familycode[is.na(df$Tree.tmp)&df$Clone==1])
df[is.na(df$Tree.tmp)&df$Clone==2,c('Tree','Tid','Familycode')]

df$Tree[is.na(df$Tree.tmp)]

idx_na <- is.na(df$Tree.tmp)
match_idx <- match(df$Fieldcode[idx_na], newg$NURSERY_CODE)
df$Tree[idx_na] <- ifelse(is.na(match_idx), df$Tree[idx_na], newg$Tree[match_idx])
#rows in df with is.na(Tree.tmp) had a matching Tid in newg, 
#and df$Tree has been updated accordingly (previously 
#Tree equaled Tid as a placeholder; now it's replaced 
#with the correct Tree value from newg

df$Tree[is.na(df$Tree.tmp)]

#save design file with Tid for 'design_import_BMS.R

wb1 <- createWorkbook()

# Add a sheet and write data
addWorksheet(wb1, paste0(strialname,"_Tid"))
writeData(wb1, paste0(strialname,"_Tid"), df)

# Save workbook
strialname
getwd()
saveWorkbook(wb1,paste0("./BMS/",year,"/",strialname,"/",strialname,"_Design_with_Tid.xlsx"),
             overwrite = TRUE)

#end