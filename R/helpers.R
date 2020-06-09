
# escape all characters that need escaping
regex_escape <- function(string,n = 1) {
  for(i in seq_len(n)){
    string <- gsub("([][{}().+*^$|\\?])", "\\\\\\1", string)
  }
  string
}

pattern_match <- function(x,patterns){
  # build a boolean matrix with patterns as columns and x elements as rows
  # lapply + do.call + cbind guarantee that we get a matrix output
  m <- do.call(cbind,lapply(patterns, grepl, x))
  # get indices of 1st relevant pattern
  i <- apply(m, 1, which.max)
  # but which.max returns 1 on a vector of FALSE so need to adjust
  i[!rowSums(m)] <- NA
  # return pattern indices
  i
}


# simulate bind_rows to spare a dependency
# bind_rows2(list(mtcars[1:2, 1:3], mtcars[1:2, 2:4]))
bind_rows2 <- function(x){
  # rbind of several df without columns outputs 0 row df so need special case
  if(all(sapply(x,ncol)==0)) return(data.frame(row.names = seq_along(x)))
  # all names found in all data frames
  nms <- unique(unlist(c(sapply(x, names))))
  # add NA columns so we can rbind them
  x[] <- lapply(x,function(x) {x[nms[!nms %in% names(x)]] <- NA; x})
  do.call(rbind,x)
}

setNames <-function (object = nm, nm) {
  names(object) <- nm
  object
}




# converts a list to a data frame without creating new columns, trying to unnest etc
as.data.frame2 <- function(l){
  nrows <- unique(lengths(l))
  if(length(nrows) !=1) {
    stop("list elements should have the same length to be converted")
  }
  structure(l, class ="data.frame", row.names = seq.int(nrows))
}

parse_brackets <- function(patterns, matched, use_multiple, named_capture = FALSE) {

  # loop on patterns and matched in parallel to get a list containing for
  # each pattern a vector of bracket content
  bracket_content <-
    Map(function(x,y) substring(x, y+1, y + attr(y, "match.length") - 2),
        patterns, matched)

  # parse these bracket contents into names (sometime "") and subpatterns
  # (the actual regex)

  L <- lapply(bracket_content, parse_bracket, use_multiple = use_multiple, named_capture = named_capture)
}

parse_bracket <- function(x, use_multiple, named_capture = FALSE){
  # wether bracket content contains "="
  eq <- grepl("=", x)
  # names are either the lhs if `=` is found, otherwise bracket content itself
  nms <- x
  nms[eq] <- gsub("^(.*?)\\=.*","\\1", nms[eq])
  empty_nms_lgl <- nms ==""
  # subpattern is either the rhs if `=` is found, otherwise "(.*?)"
  subpat <- rep.int("(.*?)", length(x))
  # if names were given we put the pattern inside a group (unsing parentheses)
  if(named_capture) {
    subpat[eq & !empty_nms_lgl]  <- gsub("^(.*?)\\=(.*)", "(?<\\1>\\2)", x[eq & nms!=""])
    subpat[!eq & !empty_nms_lgl] <- paste0("(?<",x[!eq & nms!=""],">.*?)")
  } else
    subpat[eq & !empty_nms_lgl] <- gsub("^.*?\\=(.*)","(\\1)", x[eq & nms!=""])
  # if names are not given we omit parens so the value will be matched but
  # won't be extracted
  subpat[eq & empty_nms_lgl] <- gsub("^.*?\\=(.*)",  "\\1", x[eq & nms==""])
  nms <- nms[!empty_nms_lgl]

  # this should be only in the default case,
  # subpat should be altered for the cases where we have duplicate names

  if(!use_multiple) {
    dupes_lgl <- duplicated(nms)

    subpat[dupes_lgl] <- ""
    n_unescaped_parens <- nchar(gsub("[^(]|\\\\\\(", "", subpat))
    group_indices <- cumsum(c(1,n_unescaped_parens))
    group_indices <- group_indices[-length(group_indices)]
    for (dupe in unique(nms[dupes_lgl])){
      i_first <- which.max(nms == dupe)
      i_dupes <- setdiff(which(nms == dupe), i_first)
      if(!identical(x[i_dupes], nms[i_dupes]))
        stop(sprintf(
          "'%s' name is repeated, regex can only be defined for the first instance",
          nms[i_first]))
      subpat[i_dupes] <- paste0("\\",group_indices[i_first])
      nms <- nms[-i_dupes]
    }
    group_indices <- group_indices[!empty_nms_lgl][!dupes_lgl]
  } else {
    n_unescaped_parens <- nchar(gsub("[^(]|\\\\\\(", "", subpat))
    group_indices <- cumsum(c(1,n_unescaped_parens))
    group_indices <- group_indices[-length(group_indices)]
    group_indices <- group_indices[!empty_nms_lgl]
  }

  list(names = nms, subpatterns= subpat, group_indices = group_indices)
}
