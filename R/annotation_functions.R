
#' Read Freesurfer annotaton files
#' @export
read_annotation <- function(file, ...) {

  # open connection
  to.read = file(file, "rb")

  # 1. read first entry: vertex count
  vtxct <- readBin(to.read, integer(), endian = "big")

  # 2. read vertex data, based on vtxct
  #    - First column is vertex id, seccond column is annotation value (RGB related)
  vertex_data <- matrix(readBin(to.read, integer(), n = vtxct*2, endian = "big"),ncol = 2, byrow = TRUE)

  # 3. read other descriptive entries
  tag <- readBin(to.read, integer(), n=1, endian = "big")
  ctab_version <- readBin(to.read, integer(), n=1, endian = "big")
  maxstruc <- readBin(to.read, integer(), n=1, endian = "big")
  len <- readBin(to.read, integer(), n=1, endian = "big")
  fname <- readBin(to.read, character(), n=1, endian = "big")
  num_entries <- readBin(to.read, integer(), n=1, endian = "big")

  # 4. read look-up table (LUT), line by line
  LUT_Label <- list()
  LUT_len <- list()
  LUT_labelname <- list()
  LUT_red <- list()
  LUT_green <- list()
  LUT_blue <- list()
  LUT_transp <- list()

  for (i in 1:num_entries){
    LUT_Label <- c(LUT_Label, readBin(to.read, integer(), n=1, endian = "big"))
    LUT_len <- c(LUT_len, readBin(to.read, integer(), n=1, endian = "big"))
    LUT_labelname <- c(LUT_labelname, readBin(to.read, character(), n=1, endian = "big"))
    LUT_red <- c(LUT_red, readBin(to.read, integer(), n=1, endian = "big"))
    LUT_green <- c(LUT_green, readBin(to.read, integer(), n=1, endian = "big"))
    LUT_blue <- c(LUT_blue, readBin(to.read, integer(), n=1, endian = "big"))
    LUT_transp <- c(LUT_transp, readBin(to.read, integer(), n=1, endian = "big"))
  }
  close(to.read)

  LUT <- data.frame(Label = unlist(LUT_Label),
                    len = unlist(LUT_len),
                    labelname = unlist(LUT_labelname),
                    red = unlist(LUT_red),
                    green = unlist(LUT_green),
                    blue = unlist(LUT_blue),
                    transp = unlist(LUT_transp),
                    stringsAsFactors = FALSE)


  # 5. combine to single list and change class
  annot_construct <- list(vtxct = vtxct, vertex_data= vertex_data, tag = tag, ctab_version = ctab_version,
                          maxstruc = maxstruc, len = len, fname = fname, num_entries = num_entries, LUT = LUT)
  class(annot_construct) <- "annotation_construct"
  return(annot_construct)
}


#' Write annotation_construct objects to Freesurfer annotation file
#' @export
write_annotation <- function(object, file, convert2integer = FALSE, ...){

  staticnames <- c("vtxct", "vertex_data", "tag", "ctab_version", "maxstruc", "len", "fname", "num_entries", "LUT")

  if(class(object) != "annotation_construct"){
    warning("Object structure is not of class 'annotation_construct'.")
  }

  if(!identical(names(object),staticnames)){
    warning("Object structure does not match regular structure of class 'annotation_construct'.")
  }

  # numerics are different from integers when written in binaries
  if (convert2integer){
    for (i in names(object)){
      if (is.matrix(object[[i]]) | is.data.frame(object[[i]])){
        for (j in 1:ncol(object[[i]])) {
          if(is.numeric(object[[i]][,j])){
            object[[i]][,j] <- as.integer(object[[i]][,j])
          }
        }
      } else if (is.numeric(object[[i]])){
        object[[i]]<- as.integer(object[[i]])
      }
    }
  }

  to.write <- file(file, "wb")
  for (k in names(object)){
    if (k == "vertex_data") {
      writeBin(as.vector(t(object[[k]])), to.write, endian = "big")
    } else if (k == "LUT") {
      for (x in 1:nrow(object[[k]])) {
        for (y in 1:ncol(object[[k]])){
          writeBin(object[[k]][x,y], to.write, endian = "big")
        }
      }
    } else {
      writeBin(object[[k]], to.write, endian = "big")
    }
  }
  close(to.write)
}






## Nog toevoegen: Warning voor als ctab labels niet overeenkomen met annotation labels

#' Insert new color table in annotation_construct
#' @export
replace_ctab <- function(object, new_ctab, labelcol, RGBAcols, ...){

  staticnames <- c("vtxct", "vertex_data", "tag", "ctab_version", "maxstruc", "len", "fname", "num_entries", "LUT")

  if(class(object) != "annotation_construct"){
    warning("Object structure is not of class 'annotation_construct'")
  }
  if(!identical(names(object),staticnames)){
    warning("Object structure does not match regular structure of class 'annotation_construct'")
  }

  if (length(RGBAcols) != 4){
    stop("RGBAcols should be 4 items long")
  }

  names(new_ctab)[names(new_ctab) == labelcol] <- "labelname"
  new_ctab$labelname <- as.character(new_ctab$labelname)

  new_RGBAcols <- c("red", "green", "blue", "transp")
  for (i in 1:length(RGBAcols)) names(new_ctab)[names(new_ctab) == RGBAcols[i]] = new_RGBAcols[i]


  # Labelnames to vertices using vertex labels
  orig_ctabcols <- colnames(object$LUT)
  object$LUT$vlabel <-  ((object$LUT$blue * 256^2) + (object$LUT$green * 256) + object$LUT$red )
  colnames(object$vertex_data) <- c("vno", "vlabel")
  object$vertex_data <- merge(object$vertex_data, object$LUT[c("vlabel", "labelname")], all.x = TRUE)


  # Link new vertexlabels to labelnames
  new_ctab$vlabel_new <-  as.integer((new_ctab$blue * 256^2) + (new_ctab$green * 256) + new_ctab$red )
  new_ctab[new_ctab$transp == 128L, "vlabel_new"] <- 2147483647L
  object$vertex_data <-  merge(object$vertex_data, new_ctab[c("labelname", "vlabel_new")],
                               by = "labelname", all.x = TRUE )
  object$vertex_data <-  object$vertex_data[order(object$vertex_data$vno),]

  object$vertex_data[is.na(object$vertex_data$vlabel_new), "vlabel_new"] <-
    object$vertex_data[is.na(object$vertex_data$vlabel_new), "vlabel"]

  object$vertex_data <- as.matrix(object$vertex_data[, c("vno", "vlabel_new")])
  colnames(object$vertex_data) <- NULL
  rownames(object$vertex_data) <- NULL

  # update new ctab
  ## remove duplicate labelnames, each coloured labelname should have a unique vertex label
  count_dupl <- data.frame(table(new_ctab[new_ctab$transp != "128", "vlabel_new"]))
  dupl <- count_dupl[count_dupl$Freq > 1, "Var1"]
  new_ctab$duplicate <- new_ctab$vlabel_new %in% dupl

  replaceLabels <- lapply(dupl, function(i){
    RGBA <- unique(new_ctab[new_ctab$vlabel_new == i,c("red", "green", "blue", "transp")])
    label_no <- which(dupl==i)

    labelname_orig <- new_ctab[which(new_ctab$vlabel_new==i), "labelname"]
    labelname_new <- paste0("label",LETTERS[label_no])
    out1 <- cbind(labelname = labelname_new, RGBA,stringsAsFactors =FALSE)

    writeLines(paste("NOTE:", labelname_new, "is composed of the following structures of identical colour:\n",
                     paste(labelname_orig, collapse = ", ")))
    return(out1)
  })

  replaceRows <- do.call(rbind, replaceLabels)
  keepRows <- new_ctab[!new_ctab$duplicate,c("labelname", "red", "green", "blue", "transp")]
  rep_ctab <- rbind(replaceRows, keepRows)

  # move unknown to first row
  if (any(grepl("unknown", rep_ctab$labelname))) {
    pos_unknown <- which(rep_ctab$labelname == "unknown")
    rep_ctab <- rbind(rep_ctab[pos_unknown,], rep_ctab[-pos_unknown,], stringsAsFactors = FALSE)
  } else {
    warning("Label 'unknown' is not found in the look-up table. This may sometimes lead to incorrect annotations.")
  }

  rep_ctab$Label <- 0L:(nrow(rep_ctab)-1)
  rep_ctab$len <- nchar(rep_ctab$labelname) +1L

  rep_ctab <- rep_ctab[, orig_ctabcols]
  replace_num_entries <- nrow(rep_ctab)

  object$LUT <- rep_ctab
  object$num_entries <- replace_num_entries

  return(object)
}
