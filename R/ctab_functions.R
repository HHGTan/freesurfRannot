
#x <- read.table("/Users/htan4/Documents/Rprojects/Test_stage/2a_out1.txt", header =T)
#  @ colramp_pos/neg colors to interpolate; must be a valid argument to col2rgb().

# output: list of lh, rh or unknown, if no prefixes of ^[lr]h or ^left/^right are found

#' Create a color table based on p-values and positive/negative direction of effect
#' @export
create_p_ctab <-  function(x, structure_col, direction_col, pval_col,
                           colramp_pos = c("yellow","red"),  colramp_neg = c("cyan","blue"),
                           p_range =  c(0.00001, 0.05), default_unknown = TRUE, ...){

  # color palette
  palette_pos <- colorRampPalette(colramp_pos)(256)
  palette_neg <- colorRampPalette(colramp_neg)(256)

  # check over which p.value range colours should change gradually
  if(length(p_range) < 2){
    stop("p_range should contain at least 2 values")
  }
  min_p <- min(p_range)
  max_p <- max(p_range)

  if (missing(x)){
    x <- do.call(cbind, list(structure_col,direction_col,pval_col))
    x <- as.data.frame(x)
    colnames(x) <-  c("structure","direction","pval")
  } else{
    if (length(c(structure_col,direction_col,pval_col)) != 3){
      stop ("Not all required columns of x are properly defined")
    }
    x <- x[, c(structure_col,direction_col,pval_col)]
    colnames(x) <-  c("structure","direction","pval")
  }

  # remove _thickness or _volume suffixes
  x$structure <- gsub("_thickness$|_volume$","", x$structure)


  # calculate normalized pval (in log10)
  x$norm_pval <- 1+ round(255* ( log10(x[,"pval"]) - log10(min_p))/(log10(max_p)-log10(min_p)))

  # if pval < min_p, give most extreme value (=1)
  x$norm_pval <- pmax(x$norm_pval,1)
  x$hexcol <- ifelse(x$direction > 0, palette_pos[x$norm_pval], palette_neg[x$norm_pval]) # to add: in case of undefined, will give negative

  # transform to RGBA. Note: colnames are derived from Freesurfer's annotation file documentation
  x$red <- col2rgb(x$hexcol)["red",]
  x$green <- col2rgb(x$hexcol)["green",]
  x$blue <- col2rgb(x$hexcol)["blue",]
  x$transp <- 0L #set alpha to zero

  # if hexcol is out of range from pallet_pos or pallette_neg, make grey and transparent
  for (i in 1:nrow(x)) {
    if (is.na(x$hexcol[i])){
      x[i, c("red","green","blue")] <- 160L
      x[i,"transp"] <- 128L
    }
  }

  #
  x$hemisphere <- ifelse(grepl("^lh|^left", x$structure, ignore.case = TRUE), "lh",
                         ifelse(grepl("^rh|^right", x$structure, ignore.case = TRUE),"rh", "unknown"))

  ctab_list <- sapply(unique(x$hemisphere), function(j){
    ctab <- x[x$hemisphere == j,c("structure", "red", "green", "blue", "transp")]
    ctab$structure <- gsub("^lh_|^left_|^rh_|^right_", "", ctab$structure)

    if (default_unknown){
      add_unknown <- data.frame(structure = "unknown",
                                red = 160L, green = 160L, blue = 160L, transp = 128L, stringsAsFactors = FALSE)
      ctab <- rbind(add_unknown,ctab)
    }
    rownames(ctab) <- 0L:(nrow(ctab)-1L)
    return(ctab)
  }, simplify = FALSE, USE.NAMES = TRUE)

  return(ctab_list)
}



