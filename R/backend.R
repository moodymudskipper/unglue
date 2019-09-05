unglue_data0 <- function(
  x, patterns_regex, convert = FALSE, multiple = NULL,
  output = "data.frame"){

  # assign a pattern to each element
  pattern_indices <- pattern_match(x, patterns_regex)

  # initiate a list of results
  res <- as.list(rep.int(NA,length(x)))

  # assign a tibble of matches to each result from each pattern
  for(i in seq_along(patterns_regex)){
    subset_ind <- which(pattern_indices == i)
    matched <- gregexpr(patterns_regex[i], x[subset_ind], perl = T)
    groups  <- attr(patterns_regex, "groups")[[i]]
    nms     <- rep(names(groups),lengths(groups))
    # res_i is the list of result for i-th pattern
    res_i <- Map(
      x[subset_ind], matched,
      f = function(x, m) {
        # cs like captured string
        cs <- substring(x, attr(m, "capture.start") , attr(
          m, "capture.start") + attr(m, "capture.length") - 1)
        cs <- cs[unlist(groups)]
        if(is.null(multiple)) {
          cs <- setNames(as.list(cs), nms)
          return(as.data.frame(cs, stringsAsFactors = FALSE))
        }

        # aggregate names
        cs <- tapply(
          simplify = FALSE,
          cs, nms,
          function(x) {
            if(length(x)==1) x else {
              # call `multiple` on all values
              agg_val <- do.call(multiple, as.list(x))
              # if the result is not a scalar, wrap it in a list
              if(is.list(agg_val) || length(agg_val) > 1)
                agg_val <- list(agg_val)
              agg_val
            }
          })
        # reorder
        cs <- cs[unique(nms)]
        # convert
        as.data.frame2(cs)
      }
    )
    # update results
    res[subset_ind] <- res_i
  }
  # replace NA elements by an empty single row data.frame
  na_indices <- is.na(pattern_indices)
  res[na_indices] <- replicate(sum(na_indices), data.frame(row.names = 1))


  # bind everything
  res <- bind_rows2(res)
  rownames(res) <- NULL

  # convert if relevant
  if (isTRUE(convert)) {
    res <- utils::type.convert(res, as.is = TRUE)
  } else if(is.function(convert)) {
    res <- convert(res)
  } else if(inherits(convert, "formula")){
    if(!requireNamespace("rlang"))
      stop("rlang package must be installed to use formula notation in `convert` argument of unglue functions")
    convert <- rlang::as_function(convert)
    res <- convert(res)
  }

  if (output == "list"){
    res <- split(res, seq_len(nrow(res)))
    nms <- names(groups)
    nms <- lapply(attr(patterns_regex, "groups"),names)[pattern_indices]
    res <- Map(`[`, res, nms)
  }

  res
}


unglue_vec0 <- function(x, patterns_regex, var, convert){
  groups <- attr(patterns_regex, "groups")
  nms <- lapply(groups, names)
  # assign a pattern to each element
  pattern_indices <- pattern_match(x, patterns_regex)

  # initiate a list of results
  res <- rep.int(NA_character_, length(x))
  if(is.numeric(var)){
    for(i in seq_along(patterns_regex)){
      subset_ind <- which(pattern_indices == i)
      if (var > length(groups[[i]])) {
        res_i <- rep_len(NA_character_, length(subset_ind))
      } else {
        matched <- gregexpr(patterns_regex[i], x[subset_ind], perl = TRUE)
        # res_i is the list of result for i-th pattern
        res_i <- Map(function(x,y) substring(x, attr(y, "capture.start") , attr(y, "capture.start") + attr(y, "capture.length") - 1),
                     x[subset_ind], matched)
        res_i <- sapply(res_i, `[[`, groups[[i]][var])
      }
      res[subset_ind] <- res_i
    }
  } else {
    for(i in seq_along(patterns_regex)){
      subset_ind <- which(pattern_indices == i)
      if (! var %in% nms[[i]]) {
        res_i <- rep_len(NA_character_, length(subset_ind))
      } else {
        matched <- gregexpr(patterns_regex[i], x[subset_ind], perl = TRUE)
        # res_i is the list of result for i-th pattern
        res_i <- Map(function(x,y) substring(x, attr(y, "capture.start") , attr(y, "capture.start") + attr(y, "capture.length") - 1),
                     x[subset_ind], matched)
        res_i <- sapply(res_i, `[[`, groups[[i]][which.max(var==nms[[i]])])
      }
      res[subset_ind] <- res_i
    }
  }


  # convert if relevant
  if (isTRUE(convert)) {
    res <- utils::type.convert(res, as.is = TRUE)
  } else if(is.function(convert)) {
    res <- convert(res)
  } else if(inherits(convert, "formula")){
    if(!requireNamespace("rlang"))
      stop("rlang package must be installed to use formula notation in `convert` argument of unglue functions")
    convert <- rlang::as_function(convert)
    res <- convert(res)
  }

  res
}
