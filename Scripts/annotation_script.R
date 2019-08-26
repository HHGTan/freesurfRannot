
FREESURFER_HOME <- "/Applications/freesurfer"
Lannot <- read_annotation(file = paste0(FREESURFER_HOME, "/subjects/fsaverage/label/lh.aparc.annot"))
Rannot <- read_annotation(file = paste0(FREESURFER_HOME, "/subjects/fsaverage/label/rh.aparc.annot"))

p1 <-  read.table("/Users/htan4/Documents/Rprojects/ClustMR/Testzone/Output/4a_out1.txt", header =  TRUE)

ctab1 <- create_p_ctab(p1, structure_col = "domain", direction_col = "estimate", pval_col = "p.adjust")

Lannot_new <- replace_ctab(object = Lannot, new_ctab = ctab1$lh, labelcol = "structure", RGBAcols = c("red", "green","blue", "transp"))
Rannot_new <- replace_ctab(object = Rannot, new_ctab = ctab1$rh, labelcol = "structure", RGBAcols = c("red", "green","blue", "transp"))

write_annotation(Lannot_new, file = "/Users/htan4/Documents/Rprojects/ClustMR/Testzone/Output/lh.aparc4a.annot")
write_annotation(Rannot_new, file = "/Users/htan4/Documents/Rprojects/ClustMR/Testzone/Output/rh.aparc4a.annot")


p1 <-  read.table("/Users/htan4/Documents/Rprojects/ClustMR/Testzone/Output/4b_out1.txt", header =  TRUE)

ctab1 <- create_p_ctab(p1, structure_col = "domain", direction_col = "estimate", pval_col = "p.adjust")

Lannot_new <- replace_ctab(object = Lannot, new_ctab = ctab1$lh, labelcol = "structure", RGBAcols = c("red", "green","blue", "transp"))
Rannot_new <- replace_ctab(object = Rannot, new_ctab = ctab1$rh, labelcol = "structure", RGBAcols = c("red", "green","blue", "transp"))

write_annotation(Lannot_new, file = "/Users/htan4/Documents/Rprojects/ClustMR/Testzone/Output/lh.aparc4b.annot")
write_annotation(Rannot_new, file = "/Users/htan4/Documents/Rprojects/ClustMR/Testzone/Output/rh.aparc4b.annot")


p1 <-  read.table("/Users/htan4/Documents/Rprojects/ClustMR/Testzone/Output/4c_out1.txt", header =  TRUE)

ctab1 <- create_p_ctab(p1, structure_col = "domain", direction_col = "estimate", pval_col = "p.adjust")

Lannot_new <- replace_ctab(object = Lannot, new_ctab = ctab1$lh, labelcol = "structure", RGBAcols = c("red", "green","blue", "transp"))
Rannot_new <- replace_ctab(object = Rannot, new_ctab = ctab1$rh, labelcol = "structure", RGBAcols = c("red", "green","blue", "transp"))

write_annotation(Lannot_new, file = "/Users/htan4/Documents/Rprojects/ClustMR/Testzone/Output/lh.aparc4c.annot")
write_annotation(Rannot_new, file = "/Users/htan4/Documents/Rprojects/ClustMR/Testzone/Output/rh.aparc4c.annot")


p1 <-  read.table("/Users/htan4/Documents/Rprojects/ClustMR/Testzone/Output/4d_out1.txt", header =  TRUE)

ctab1 <- create_p_ctab(p1, structure_col = "domain", direction_col = "estimate", pval_col = "p.adjust")

Lannot_new <- replace_ctab(object = Lannot, new_ctab = ctab1$lh, labelcol = "structure", RGBAcols = c("red", "green","blue", "transp"))
Rannot_new <- replace_ctab(object = Rannot, new_ctab = ctab1$rh, labelcol = "structure", RGBAcols = c("red", "green","blue", "transp"))

write_annotation(Lannot_new, file = "/Users/htan4/Documents/Rprojects/ClustMR/Testzone/Output/lh.aparc4d.annot")
write_annotation(Rannot_new, file = "/Users/htan4/Documents/Rprojects/ClustMR/Testzone/Output/rh.aparc4d.annot")


p1 <-  read.table("/Users/htan4/Documents/Rprojects/ClustMR/Testzone/Output/4e_out1.txt", header =  TRUE)

ctab1 <- create_p_ctab(p1, structure_col = "domain", direction_col = "estimate", pval_col = "p.adjust")

Lannot_new <- replace_ctab(object = Lannot, new_ctab = ctab1$lh, labelcol = "structure", RGBAcols = c("red", "green","blue", "transp"))
Rannot_new <- replace_ctab(object = Rannot, new_ctab = ctab1$rh, labelcol = "structure", RGBAcols = c("red", "green","blue", "transp"))

write_annotation(Lannot_new, file = "/Users/htan4/Documents/Rprojects/ClustMR/Testzone/Output/lh.aparc4e.annot")
write_annotation(Rannot_new, file = "/Users/htan4/Documents/Rprojects/ClustMR/Testzone/Output/rh.aparc4e.annot")


p1 <-  read.table("/Users/htan4/Documents/Rprojects/ClustMR/Testzone/Output/4f_out1.txt", header =  TRUE)

ctab1 <- create_p_ctab(p1, structure_col = "domain", direction_col = "estimate", pval_col = "p.adjust")

Lannot_new <- replace_ctab(object = Lannot, new_ctab = ctab1$lh, labelcol = "structure", RGBAcols = c("red", "green","blue", "transp"))
Rannot_new <- replace_ctab(object = Rannot, new_ctab = ctab1$rh, labelcol = "structure", RGBAcols = c("red", "green","blue", "transp"))

write_annotation(Lannot_new, file = "/Users/htan4/Documents/Rprojects/ClustMR/Testzone/Output/lh.aparc4f.annot")
write_annotation(Rannot_new, file = "/Users/htan4/Documents/Rprojects/ClustMR/Testzone/Output/rh.aparc4f.annot")

