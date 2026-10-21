rm(list=ls())
source("./trial_grid_functions.R")
library(readxl)
library(writexl)
library(openxlsx)

(trialname <- "BC25TU11CFC")
(strialname <- "25TU11")
year <- 2025

grid_plot_coords <- build_trial_grid(
  n_block_rows = 15,   # number of grid rows (blocks)
  n_block_cols = 11,    # number of grid columns (blocks)
  sub_rows = 6,        # plots per grid, vertically
  sub_cols = 6,        # plots per grid, horizontally
  exclude_blocks = c(15,46,61,76,91,106,113:121,128:137,143:153,155:165), # grid numbers that don't physically exist
#   exclude_blocks = NA,
  grid_start = "bottomleft",    # corner the initial 1:91 grid_number pass starts from
  grid_direction = "updown",    # "updown" (column-major) or "leftright" (row-major)
  grid_pattern = "straight",    #default is serpentine
  pset_start = "bottomleft",   # corner the post-exclusion 1:80 Pset renumbering starts from
  pset_direction = "updown",    # "updown" (column-major) or "leftright" (row-major)
  pset_pattern = "straight",
  sub_orientation = "row_vertical", # same Pcol (sub_col) -> same plot_row; plot_col varies 1:sub_rows
  # keep this in sync with the `orientation` arg of plot_block_grid() below (line ~85)
  within_grid_direction = "vertical", # "vertical" (default) or "horizontal" serpentine for within_grid_number, from bottom-left plot
  sub_start = "bottomleft" # corner sub_row/sub_col/within_grid_number originate from; keep in sync with `start` arg of plot_block_grid() below
)

grid_plot_coords$Pset.new <- grid_plot_coords$Pset
head(grid_plot_coords)

####correction for 25OT03 and other trials where Psets are out of order##################
#grid_plot_coords$Pset.new[grid_plot_coords$Pset == 19] <- 34
#grid_plot_coords$Pset.new[grid_plot_coords$Pset %in% 20:34] <-
#grid_plot_coords$Pset[grid_plot_coords$Pset %in% 20:34] - 1
#grid_plot_coords$Pset <- grid_plot_coords$Pset.new
#grid_plot_coords$plot_label <- paste0(grid_plot_coords$Pset,"_",grid_plot_coords$within_grid_number)
######################################

head(grid_plot_coords)
grid_plot_coords |> arrange(Pset, within_grid_number)
grid_plot_coords |> arrange(Pset, within_grid_number) |> head(40)
grid_plot_coords |> arrange(Pset, within_grid_number) |> tail(20)


block_borders <- grid_plot_coords |>
  filter(!is.na(Pset.new)) |>
  group_by(Pset.new) |>
  summarise(
    xmin = min(plot_col) - 0.5, xmax = max(plot_col) + 0.5,
    ymin = min(plot_row) - 0.5, ymax = max(plot_row) + 0.5
  )

p <- ggplot(grid_plot_coords, aes(plot_col, plot_row)) +
  geom_tile(color = "grey70", fill = "white", linewidth = 0.2) +
  geom_rect(
    data = block_borders,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE, fill = NA, color = "black", linewidth = 0.6
  ) +
  geom_text(
    data = block_borders,
    aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2, label = Pset.new),
    inherit.aes = FALSE, size = 3
  ) +
  coord_equal() +
  theme_minimal() +
  theme(panel.grid = element_blank(), axis.title = element_blank())
print(p)

p$layers[[1]]$aes_params$fill <- "lightblue"
p <- p + theme(axis.title = element_blank())
p

p <- p + theme(axis.text = element_blank())
p

table(grid_plot_coords$within_grid_number)
table(grid_plot_coords$sub_col)
table(grid_plot_coords$sub_row)
table(grid_plot_coords$Pset, useNA="ifany")

grid_plot_coords <- grid_plot_coords |> select(-grid_number)

strialname
write.csv(grid_plot_coords, file = paste0("./BMS/",year,"/",strialname,"/grid_plot_coords_",strialname,".csv"),row.names=FALSE)

#plot_block_grid(6, 6, "letter", orientation = "row_vertical") #check against establishment report
# keep `orientation` in sync with the `sub_orientation` arg of build_trial_grid() above (line ~22)
plot_block_grid(6, 6, "letter", orientation = "row_vertical", start = "bottomleft") #check against establishment report; where row = a to f and column = 1 to 6
plot_block_grid(6, 6, "number", orientation = "row_vertical", start = "bottomleft") 

grid_plot_coords <- grid_plot_coords %>%
  arrange(Pset, sub_col, sub_row)

head(grid_plot_coords,13)
tail(grid_plot_coords,13)
range(grid_plot_coords$plot_row) #'FIELDMAP RANGE'
range(grid_plot_coords$plot_col) #'FIELDMAP COLUMN'
grid_plot_coords$plot_pos <- paste0(grid_plot_coords$sub_col,grid_plot_coords$sub_row)

names(grid_plot_coords)[names(grid_plot_coords) == "Pset"] <- "Pset.x"

#match trial design with plot coordinates
strialname
df <- read_excel(paste0("./BMS/",year,"/",strialname,"/",strialname,"_Design_import.xlsx"))
head(df)

df$plot_label <- paste0(df$Pset, "_", df$Tpos)
df$plot_pos_df <- paste0(df$Pcol,df$Prow)

head(df)
head(grid_plot_coords)

df_grid <- merge(df,grid_plot_coords[,c("Pset.x","within_grid_number","plot_row","plot_col","plot_label","plot_pos")],
                 by="plot_label",all.y=TRUE)
head(df_grid)

head(df_grid)

#check
all.equal(df_grid$plot_pos_df,df_grid$plot_pos)
nrow(df_grid[is.na(df_grid$Pset.x),])
head(df_grid[is.na(df_grid$Pset.x),])
idx <- !(is.na(df_grid$plot_pos_df) | is.na(df_grid$plot_pos))
all.equal(df_grid$plot_pos_df[idx], df_grid$plot_pos[idx]) #if there is padding (NAs)
all.equal(as.numeric(df_grid$Pset),df_grid$Pset.x)
idx.p <- !(is.na(df_grid$Pset) | is.na(df_grid$Pset.x))
all.equal(df_grid$Pset[idx.p], df_grid$Pset.x[idx.p]) #if there is padding (NAs)
idx.tpos <- !(is.na(df_grid$Tpos) | is.na(df_grid$within_grid_number))
all.equal(df_grid$Tpos[idx.tpos], df_grid$within_grid_number[idx.tpos]) #if there is padding (NAs)
#write.csv(df_grid,"df_grid_check.csv",row.names = FALSE)

str(df_grid)

# Each Pset block spans a contiguous 6x6 region of the grid; draw
# boundaries between blocks and label each block at its centroid.
row_range <- range(df_grid$plot_row, na.rm = TRUE)
col_range <- range(df_grid$plot_col, na.rm = TRUE)
row_breaks <- seq(row_range[1] - 0.5, row_range[2] + 0.5, by = 6)
col_breaks <- seq(col_range[1] - 0.5, col_range[2] + 0.5, by = 6)

block_labels <- df_grid |>
  filter(!is.na(Pset.x)) |>
  group_by(Pset.x) |>
  summarise(plot_row = mean(plot_row, na.rm = TRUE), plot_col = mean(plot_col, na.rm = TRUE))


ggplot(df_grid, aes(x = plot_col, y = plot_row, fill = is.na(Pset.x))) +
  geom_tile(color = "white", linewidth = 0.2) +
  geom_hline(yintercept = row_breaks, color = "black", linewidth = 0.4) +
  geom_vline(xintercept = col_breaks, color = "black", linewidth = 0.4) +
  geom_text(data = block_labels, aes(label = Pset.x), size = 3, inherit.aes = FALSE, x = block_labels$plot_col, y = block_labels$plot_row) +
  scale_fill_manual(values = c(`FALSE` = "lightblue", `TRUE` = "grey60")) +
  scale_x_continuous(expand = c(0, 0),breaks = seq(0, max(df_grid$plot_col, na.rm = TRUE), by = 6)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_equal() +
  theme_minimal() +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), legend.position = "none") +
  labs(x = "Trial column", y = "Trial row", title = trialname)


ggplot(df_grid, aes(x = plot_col, y = plot_row, fill = is.na(Pset.x))) +
  geom_tile(color = "white", linewidth = 0.2) +
  geom_hline(yintercept = row_breaks, color = "black", linewidth = 0.4) +
  geom_vline(xintercept = col_breaks, color = "black", linewidth = 0.4) +
  geom_text(data = block_labels, aes(label = Pset.x), size = 3, inherit.aes = FALSE, x = block_labels$plot_col, y = block_labels$plot_row) +
  scale_fill_manual(values = c(`FALSE` = "lightblue", `TRUE` = "grey60")) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, max(df_grid$plot_col, na.rm = TRUE), by = 6)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, max(df_grid$plot_row, na.rm = TRUE), by = 6)) +
  coord_equal() +
  theme_minimal() +
  theme(axis.ticks = element_blank(), axis.text = element_text(size = 6), legend.position = "none") +
  labs(x = "Trial column", y = "Trial row", title = trialname)


#library(desplot)
#desplot(df_grid,trait_num~plot_col+plot_row,out1=Pset)

head(df_grid)

head(df_grid[df_grid$Pset.x %in% "10",])

#grid_plot$Pset.x <- grid_plot$Pset
plot_selected_block(df_grid, "118") #default start = bottomleft. Also accepts orientation and pattern
head(df_grid[df_grid$Pset %in% 10,],13)

#if trial has linerakes########################
head(df_design)
#df_design$Linerake <- 0
#df_design$Linerake[df_design$Pcol %in% "a"] <- 1
#df_design$Linerake[df_design$Pcol %in% "f"] <- 1
table(df_design$Linerake)
###############################################

df_design <- df_grid |> select(Tree,Replicate,Eset,Pset,Prow,Pcol,Tpos,Fieldlabel_design,Replacement_line,Linerake,Tid,
                               ENTRY_NO,GID,GUID,TRIAL_INSTANCE,PLOT_NO,plot_row,plot_col)
names(df_design)[names(df_design) == "plot_row"] <- "FIELDMAP RANGE"
names(df_design)[names(df_design) == "plot_col"] <- "FIELDMAP COLUMN"

df_design <- df_design[order(df_design$PLOT_NO),]
head(df_design)
range(df_design$`FIELDMAP RANGE`)
range(df_design$`FIELDMAP COLUMN`)



df_design$Replacement_line[is.na(df_design$Replacement_line)] <- ''
unique(df_design$Replacement_line)
head(df_design)

#save full rectangle
wb <- createWorkbook()

# Add a sheet and write data
addWorksheet(wb, "Observation")
writeData(wb, "Observation", df_design)

# Save workbook
strialname
year
saveWorkbook(wb,paste0("C:/Users/MarkPaget/Documents/Design Import/BMS/",year,"/",strialname,"/",strialname,"_Design_import_full_rectangle.xlsx"),
             overwrite = TRUE)

df_trial_design <- df_design[!is.na(df_design$Pset),]
table(df_trial_design$Pset)
names(df_trial_design)[names(df_trial_design) == "Tree"] <- "DESIGNATION"

#save trial design with plot coordinates
wbz <- createWorkbook()

# Add a sheet and write data
addWorksheet(wbz, "Observation")
writeData(wbz, "Observation", df_trial_design)

# Save workbook
strialname
year
saveWorkbook(wbz,paste0("C:/Users/MarkPaget/Documents/Design Import/BMS/",year,"/",strialname,"/",strialname,"_Design_import_with_plotcoords.xlsx"),
             overwrite = TRUE)

#end

#copy all new files to S:\Data\Katmandoo\RPBC\Design Import\YYYY\strialname"
#import grmplsm list ("strialname,"_grmplsm.xlsx) to BMS lists and add germplasm to trial in studies
#import "strialname,"_Design_import_with_plotcoords.xlsx" to BMS
 