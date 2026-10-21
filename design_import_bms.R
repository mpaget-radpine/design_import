rm(list=ls())
#creates germplasm list and design import file for BMS from establishment report
#GG (seedlings) trial import file from #225

#Transfer design files from 1Trials to "C:\Users\MarkPaget\Documents\Design Import\BMS\2025"
#First add new pedigree entries to BMS (new_pedigree_entry.R)
#Need to add Tid to the design files. It is probably better to do this manually. But the current Tid range in BMS can be found using 'dbReadTable(con,"nd_experimentprop")' 
#Use 'trial_grid_build.R' to add trial rows ('FIELDMAP RANGE') and trial columns ('FIELDMAP COLUMN')
#After trial row and columns have been added, load trial with plot coordinates to BMS

#1. new_pedigree_entry.R
#2. design_import_BMS.R
#3. trial_grid_build.R 

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

df <- read_excel(paste0(fpath.full,strialname,"_Design_with_Tid.xlsx"), sheet=paste0(strialname,"_Tid"))

head(df)
max(df$Pset)
max(df$Pset)*36
nrow(df)==length(unique(df$Tid))

#extract pedigree function if new analysis, then save below
pedigree_df <- extract_pedigree(
  db_host = Sys.getenv("DB_HOST"),
  db_port = 23306,
  db_name = Sys.getenv("DB_NAME"),
  db_user = Sys.getenv("DB_USER"),
  db_password = Sys.getenv("DB_PASSWORD")
)

head(pedigree_df)
head(df)

nrow(df[!df$Tree %in% pedigree_df$Tree,]) #0 all Trees in df are in pedigree
df[!df$Tree %in% pedigree_df$Tree,]

nrow(df[df$Fieldcode %in% 'GF19',])
nrow(df[df$Familycode %in% 'GF19',])

df$ENTRY_TYPE <- "T"
df$ENTRY_TYPE[df$Fieldcode %in% 'GF19'] <- "C"
df$Familycode[df$Fieldcode %in% 'GF19'] <- "GF19"
table(df$ENTRY_TYPE)
#df$Clone <- 2
#df$PType <- 3
#df$PType[substring(df$Tree,1,3) %in% "401"] <- 4
table(df$PType)
table(df$Clone)

head(df)
#df[df$PType==4,c("Tree","FieldCode","Replacement","Label")]

df_gid <- merge(df,pedigree_df,by='Tree',all.x=TRUE)
head(df_gid)
tail(df_gid)

range(df_gid$Tid)
length(unique(df_gid$Tid))

df_list <- df_gid[!duplicated(df_gid$Tree),]
length(unique(df_list$gid))
str(df_list)
df_list$ENTRY_NO <- seq(1,nrow(df_list))
range(df_list$ENTRY_NO)
df_list <- df_list[,c('ENTRY_NO','Tree','germplsm_uuid','gid','ENTRY_TYPE','PType','Clone','Familycode','Clonecode','Fieldcode')]

colnames(df_list)[colnames(df_list) == "Tree"] <- "DESIGNATION"
colnames(df_list)[colnames(df_list) == "germplsm_uuid"] <- "GUID"
colnames(df_list)[colnames(df_list) == "gid"] <- "GID"

head(df_list)

#Create workbook
wb <- createWorkbook()

# Add a sheet and write data
addWorksheet(wb, "Observation")
writeData(wb, "Observation", df_list)

# Save workbook
strialname
saveWorkbook(wb,paste0("./BMS/",year,"/",strialname,"/",strialname,"_grmplsm.xlsx"),overwrite = TRUE)

#Expt design for import

colnames(df_list)[colnames(df_list) == "DESIGNATION"] <- "Tree"
head(df_list)
df_entry_design <- merge(df_gid,df_list[,c('Tree','ENTRY_NO')],by='Tree')
head(df_entry_design)
range(df_entry_design$ENTRY_NO)

df_entryno <- df_entry_design[,c('Tree','Replicate','Eset','Pset','Prow','Pcol','Tpos','Fieldlabel_design','Replacement_line','Tid','ENTRY_NO',
                                 'gid','germplsm_uuid','Fcln.y','Mcln.y')] 
head(df_entryno)

colnames(df_entryno)[colnames(df_entryno) == "germplsm_uuid"] <- "GUID"
colnames(df_entryno)[colnames(df_entryno) == "gid"] <- "GID"
colnames(df_entryno)[colnames(df_entryno) == "Fcln.y"] <- "Fcln"
colnames(df_entryno)[colnames(df_entryno) == "Mcln.y"] <- "Mcln"


df_entryno$TRIAL_INSTANCE <- 1
df_entryno <- df_entryno[order(df_entryno$Pset,df_entryno$Tpos),]
head(df_entryno,10)
df_entryno$PLOT_NO <- seq(1,nrow(df_entryno))
tail(df_entryno)

#check for windrows/linerakes
head(df_entryno)
#df_entryno$Linerake <- 0
#df_entryno$Linerake[df_entryno$Pcol %in% "a"] <- 1
#df_entryno$Linerake[df_entryno$Pcol %in% "f"] <- 1
table(df_entryno$Linerake)

df_entryno$Replacement_line[is.na(df_entryno$Replacement_line)] <- ''
unique(df_entryno$Replacement_line)

df_entryno <- df_entryno[ , !(names(df_entryno) %in% "Replacement")] #removes column 'Replacement'
names(df_entryno) %in% "Replacement"
length(unique(df_entryno$Tid)) == nrow(df_entryno)

head(df_entryno)

#Create workbook
wbx <- createWorkbook()

# Add a sheet and write data
addWorksheet(wbx, "Observation")
writeData(wbx, "Observation", df_entryno)

# Save workbook
strialname
saveWorkbook(wbx,paste0("./BMS/",year,"/",strialname,"/",strialname,"_Design_import_with_parents.xlsx"),
             overwrite = TRUE)


df_import <- df_entryno[ , !(names(df_entryno) %in% c('Fcln','Mcln'))]

wby <- createWorkbook()

# Add a sheet and write data
addWorksheet(wby, "Observation")
writeData(wby, "Observation", df_import)

# Save workbook
strialname
saveWorkbook(wby,paste0("./BMS/",year,"/",strialname,"/",strialname,"_Design_import.xlsx"),
             overwrite = TRUE)



##save Tid list
head(df_entryno)
newTid <- df_gid[,c('Tid','Tree')]
(newTid$Trial <- trialname)
newTid$Year <- '2025'
newTid <- newTid[,c('Trial','Tid','Tree','Year')]
head(newTid)
range(newTid$Tid)
strialname
write.csv(newTid,paste0("./BMS/",year,"/","TID_",strialname,".csv"),
                        row.names=FALSE)



#end
#now got to trial_grid_build.R to add plot coordinates

################################################################################



##loading GG trials - germplasm import

head(pedigree_df)

pedigree_df[pedigree_df$Tree %in% "SL13129",]

sl <- c("SL14652",
"SL14655",
"SL15265",
"SL15023",
"SL14022",
"SL14027",
"SL14158",
"SL19001",
"SL22061",
"SL14110",
"SL20066",
"SL14169",
"SL14051",
"SL14656")

#ped <- pedigree_df[pedigree_df$Tree %in% sl,]
#ped
#names(ped)[names(ped) == "Tree"] <- "BMSSeedlotID"

names(ped)[names(ped) == "Tree"] <- "BMSSeedlotID"
head(df)
#df$BMSSeedlotID <- paste0("SL",df$Familycode)
names(df)[names(df) == "BMScode"] <- "BMSSeedlotID"

sl <- unique(df$BMSSeedlotID)
sl

ped <- pedigree_df[pedigree_df$Tree %in% df$LNAME,]
head(ped)
names(ped)[names(ped) == "Tree"] <- "LNAME"
head(df)

df$Tree <- seq(from = 73134573 , length.out = nrow(df), by = 1)
range(df$Tree)
df$Tid <- df$Tree
range(df$Tid)

#BMS germplasm import file - load at line #20
imp <- df[,c('ENTRY_NO','Tree','Familycode','BMSSeedlotID','Fieldcode','PType','Clone')]

#all.equal(df$LNAME,df$Fieldcode)
imp <- df[,c('ENTRY_NO','LNAME','Tree','Familycode')] #for trees already in pedigree

imp <- merge(imp,ped,by="LNAME")

head(imp)
names(imp)[names(imp) == "Familycode"] <- "Seedlot"
#names(imp)[names(imp) == "SowingYear"] <- "SOW_DATE_AP_text"
imp$SOW_DATE_AP_text <- 2023
names(imp)[names(imp) == "BMSSeedlotID"] <- "COLL_SOU_AP_text"
#names(imp)[names(imp) == "SeedlingID"] <- "LNAME"
#names(imp)[names(imp) == "BMSSeedlotID"] <- "Seedlot"
names(imp)[names(imp) == "Fieldcode"] <- "NURSERY_CODE"

imp$GENUS_AP_text <- "Pinus"
imp$SPECIES_AP_text <- "Pinus radiata"
imp$SPP_CODE_AP_text <- "PR"
imp$ORI_COUN_AP_text <- "NZL"
imp$BRDR_AA_text <- "RPBC"
imp$CATEGORY_AA_text <- "Seedling"
imp$`BREEDING METHOD` <- "C2W"
imp$`PREFERRED NAME` <- "TREE"
imp$`CREATION DATE` <- ""
imp$`LOCATION_ABBR` <- "NZL"
imp$REFERENCE <- "GSseedling24"
imp$NOTE_AA_text <- ""
imp$`PROGENITOR 1` <- ""
imp$`PROGENITOR 2` <- ""
imp$`STORAGE LOCATION ABBR` <- ""
imp$UNITS <- ""
imp$AMOUNT <- ""
imp$`STOCK ID` <- ""
imp$PUI <- ""


head(imp)



tmp_imp <- imp[,c("ENTRY_NO","Seedlot","NURSERY_CODE","Tree","PREFERRED NAME","BREEDING METHOD","LOCATION",              
                      "CREATION DATE","REFERENCE","PROGENITOR 1","PROGENITOR 2","NOTE_AA_text","SOW_DATE_AP_text",   
                      "GENUS_AP_text","SPECIES_AP_text","SPP_CODE_AP_text","ORI_COUN_AP_text","BRDR_AA_text","CATEGORY_AA_text",   
                      "COLL_SOU_AP_text")]
head(tmp_imp)

onedrive <- "C:/Users/mark.paget/OneDrive - Radiata Pine Breeding Company/Documents/"

write.xlsx(tmp_imp,
           paste0(onedrive,"Breeding Programme/BMS/BMS Import/Germplasm import/24WK12_GG.xlsx"),
           sheetName="Observation")
#upload germplasm to BMS

#then export data and labels

tdat <- read.xlsx(paste0(onedrive,"Breeding Programme/BMS/BMS Import/Germplasm import/Germplasm update/24WK12_GG_update.xlsx"))
head(tdat)

head(pedigree_df)

names(pedigree_df)[names(pedigree_df) == "Tree"] <- "COLL_SOU_AP_text"

tdat1 <- merge(tdat,pedigree_df[,c("COLL_SOU_AP_text","Fcln","Mcln",'gpid1',"gpid2")],
               by="COLL_SOU_AP_text",all.x=TRUE)

tail(tdat1)
tdat1[tdat1$COLL_SOU_AP_text %in% 'SL14158',]

head(tdat1)


tdat_imp <- tdat1[,c("TREE","GID","GUID","Preferred.Name","Breeding.Method.Name","Reference","Location.Abbr","Creation.Date","gpid1","gpid2")]
names(tdat_imp)[names(tdat_imp) == "Breeding.Method.Name"] <- "BREEDING METHOD"
names(tdat_imp)[names(tdat_imp) == "Reference"] <- "REFERENCE"
names(tdat_imp)[names(tdat_imp) == "gpid1"] <- "PROGENITOR 1"
names(tdat_imp)[names(tdat_imp) == "gpid2"] <- "PROGENITOR 2"
names(tdat_imp)[names(tdat_imp) == "Creation.Date"] <- "CREATION DATE"
names(tdat_imp)[names(tdat_imp) == "Location.Abbr"] <- "LOCATION ABBR"
names(tdat_imp)[names(tdat_imp) == "Preferred.Name"] <- "PREFERRED NAME"
names(tdat_imp)[names(tdat_imp) == "TREE"] <- "Tree"
tdat_imp$`PREFERRED NAME` <- 'TREE'

head(tdat_imp)
#tdat_imp$`PREFERRED NAME` <- "TREE"
tdat_imp$NURSERY_NAME <- "Southern Cypresses"
unique(tdat_imp$`BREEDING METHOD`)
table(tdat_imp$`BREEDING METHOD`)

tdat_imp$`BREEDING METHOD`[tdat_imp$`BREEDING METHOD` %in% "Open pollination"] <- "PPO"
tdat_imp$`BREEDING METHOD`[tdat_imp$`BREEDING METHOD` %in% "Single cross"] <- "C2W"
table(tdat_imp$`BREEDING METHOD`)

write.xlsx(tdat_imp,
           paste0(onedrive,"Breeding Programme/BMS/BMS Import/Germplasm import/Germplasm update/24BP17_DR_updateImport.xlsx"),
           sheetName="Observation")

#upload update to BMS

head(tdat_imp)
newTid <- tdat_imp[,c('Tree','GID')]
trialname
newTid$Trial <- trialname
newTid$Year <- '2024'
newTid$Tid <- newTid$Tree

newTid <- newTid[,c('Trial','Tid','Tree','Year')]
head(newTid)
range(newTid$Tid)
strialname

newTid <- newTid[order(newTid[,'Tid']),]
head(newTid)
tail(newTid)


write.csv(newTid,paste0("S:/Data/Katmandoo/RPBC/Design Import/2024/TID_",strialname,".csv"),
          row.names=FALSE)


head(tdat)
head(imp)


#create seedling germplasm list

#colnames(tdat_imp)[colnames(tdat_imp) == "Tree"] <- "DESIGNATION"
colnames(tdat_imp)[colnames(tdat_imp) == "germplsm_uuid"] <- "GUID"
colnames(tdat_imp)[colnames(tdat_imp) == "gid"] <- "GID"

head(df)
head(tdat_imp)
#head(tt)
#head(pedigree_df)
df1 <- merge(df,tdat_imp[,c('Tree','GID','GUID')],by='Tree')
#df1 <- merge(df,pedigree_df[,c('Tree','gid','germplsm_uuid')],by='Tree')


head(df1)
df1$ENTRY_TYPE <- 'T'
df1$Seedlot <- df1$Familycode
#colnames(df1)[colnames(df1) == "germplsm_uuid"] <- "GUID"
#colnames(df1)[colnames(df1) == "gid"] <- "GID"
head(df1)
df1$Fieldcode <- df1$LNAME

df2 <- df1[,c('ENTRY_NO','Tree','GUID','GID','ENTRY_TYPE','PType','Clone','Familycode','Seedlot','Fieldcode')]
#df2 <- df1[,c('PLOT_NO','TRIAL_INSTANCE','ENTRY_NO','Tree','GUID','GID','ENTRY_TYPE','PType','Clone','Familycode',
#              'Fieldcode')]

head(df2)

colnames(df2)[colnames(df2) == "Tree"] <- "DESIGNATION"
#colnames(df2)[colnames(df2) == "germplsm_uuid"] <- "GUID"
#colnames(df2)[colnames(df2) == "gid"] <- "GID"

#Create workbook
wb <- createWorkbook()

# Add a sheet and write data
addWorksheet(wb, "Observation")
writeData(wb, "Observation", df2)

# Save workbook
strialname
saveWorkbook(wb,paste0("S:/Data/Katmandoo/RPBC/Design Import/2024/",strialname,"_grmplsm.xlsx"),overwrite = TRUE)


head(df2)


#####design file for seedlings #with existing PLOT_No, TRIAL_INSTANCE columns
head(df1)


df.exp <- df1[,c('Tree','Tid','TRIAL_INSTANCE','PLOT_NO','ENTRY_NO','Replicate','Eset','Pset','Tpos',
                 'Fieldcode','Fieldlabel','GID','GUID','Replacement')]
#df1$Pcol <- df1$Bcol
df.exp <- df1[,c('Tree','Tid','TRIAL_INSTANCE','PLOT_NO','ENTRY_NO','Replicate','Eset','Pset','Tpos','Prow','Pcol',
                 'Fieldcode','Familycode','Clonecode','GID','GUID','Fieldlabel','Windrow','Replacement')] 
#colnames(df.exp)[colnames(df.exp) == "Fieldcode"] <- "Fieldlabel"

length(unique(df.exp$Tid)) == nrow(df.exp)
all.equal(df.exp$Tree,df.exp$Tid)

head(df.exp)
df.exp$Replacement[is.na(df.exp$Replacement)] <- ''
unique(df.exp$Replacement)
 
#Create workbook
wbx <- createWorkbook()

# Add a sheet and write data
addWorksheet(wbx, "Observation")
writeData(wbx, "Observation", df.exp)

# Save workbook
strialname
saveWorkbook(wbx,paste0("S:/Data/Katmandoo/RPBC/Design Import/2024/",strialname,"_Design_import.xlsx"),overwrite = TRUE)
##


#design file without existing PLOT_NO, ENTRY_NO, TRIAL_INSTANCE
head(df.exp)
head(df2)

df2$Tree <- df2$DESIGNATION
df.exp <- merge(df.exp,df2[,c('Tree','ENTRY_NO','GUID')],by='Tree')
head(df.exp)
all.equal(df.exp$GUID,df.exp$germplsm_uuid)

df.exp$TRIAL_INSTANCE <- 1
df.exp <- df.exp[order(df.exp$Pset,df.exp$Tpos),]
head(df.exp,10)
df.exp$PLOT_NO <- seq(1,nrow(df.exp))
tail(df.exp)

df.exp$Tid <- seq(from = 73130061 , length.out = nrow(df.exp), by = 1)
range(df.exp$Tid)

#check for windrows

head(df.exp)
#df.exp$Windrow <- 0
#df.exp$Windrow[df.exp$Pcol %in% "a"] <- 1
#df.exp$Windrow[df.exp$Pcol %in% "f"] <- 1
table(df.exp$Windrow)

df.exp$Replacement[is.na(df.exp$Replacement)] <- ''

colnames(df.exp)[colnames(df.exp) == "Tree"] <- "DESIGNATION"
colnames(df.exp)[colnames(df.exp) == "gid"] <- "GID"
df.exp <- df.exp[ , !(names(df.exp) %in% "germplsm_uuid")]
length(unique(df.exp$Tid)) == nrow(df.exp)

head(df.exp)

#Create workbook
wbx <- createWorkbook()

# Add a sheet and write data
addWorksheet(wbx, "Observation")
writeData(wbx, "Observation", df.exp)

# Save workbook
strialname
saveWorkbook(wbx,paste0("S:/Data/Katmandoo/RPBC/Design Import/2024/",strialname,"_Design_import.xlsx"),overwrite = TRUE)
##





##combine Tid files


# Folder containing the CSVs
path <- "S:/Data/Katmandoo/RPBC/Design Import/2024"

# Get all CSV file names
#files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)
files <- list.files(path, pattern = "^TID.*\\.csv$", full.names = TRUE)

# ^TID → filename must start with “TID”
# .* → anything after that
# \\.csv$ → file extension ends with .csv

# Read and combine with rbind
tid_all <- do.call(rbind, lapply(files, read.csv))
range(tid_all$Tid)
#[1] 73098885 73140620

tid_all <- tid_all[order(tid_all$Tid),]
head(tid_all)
tail(tid_all)

write.csv(tid_all,paste0("S:/Data/Katmandoo/RPBC/Design Import/2024/2024_TID_","all",".csv"),
          row.names=FALSE)



##ad hoc

dat <- read.xlsx(paste0(onedrive,"Breeding Programme/BMS/BMS Import/Germplasm import/Germplasm update/GS26_update.xlsx"))
head(dat)

head(pedigree_df)

names(pedigree_df)[names(pedigree_df) == "Tree"] <- "COLL_SOU_AP_text"

dat1 <- merge(dat,pedigree_df[,c("COLL_SOU_AP_text","Fcln","Mcln",'gpid1',"gpid2")],
               by="COLL_SOU_AP_text",all.x=TRUE)

tail(dat1)
dat1[dat1$COLL_SOU_AP_text %in% 'SL13098',]

head(dat1)

dat_imp <- dat[,c("GID","GUID","Preferred.Name","Breeding.Method","Reference","Location.Abbr","Creation.Date","gpid1","gpid2","Fcln","Mcln")]
names(dat_imp)[names(dat_imp) == "Breeding.Method"] <- "BREEDING METHOD"
names(dat_imp)[names(dat_imp) == "Reference"] <- "REFERENCE"
names(dat_imp)[names(dat_imp) == "gpid1"] <- "PROGENITOR 1"
names(dat_imp)[names(dat_imp) == "gpid2"] <- "PROGENITOR 2"
names(dat_imp)[names(dat_imp) == "Creation.Date"] <- "CREATION DATE"
names(dat_imp)[names(dat_imp) == "Location.Abbr"] <- "LOCATION ABBR"
dat_imp$LNAME <- dat_imp$Preferred.Name
dat_imp$Preferred.Name <- "LNAME"
names(dat_imp)[names(dat_imp) == "Preferred.Name"] <- "PREFERRED NAME"
head(dat_imp)
#tdat_imp$`PREFERRED NAME` <- "TREE"
dat_imp$NURSERY_NAME <- "Southern Cypresses"

write.xlsx(dat_imp,
           paste0(onedrive,"Breeding Programme/BMS/BMS Import/Germplasm import/Germplasm update/GS26_update_List.xlsx"),
           sheetName="Observation")



# df <- read.csv("../2026geno_extras.csv",header=TRUE)
# head(df)
# names(df)[names(df) == "LNAME"] <- "Tree"
# head(pedigree_df)
# pedigree_df[pedigree_df$Tree %in% '26594_1A',]
# 
# df1 <- merge(df,pedigree_df[,c("Tree","gid","germplsm_uuid")],)
# head(df1)
# write.csv(df1,"../2026geno_extras_gid.csv",row.names=FALSE,quote=FALSE)
