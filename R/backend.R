unglue_data0 <- function(
  x, patterns_regex, convert = FALSE, multiple = NULL,
  output = "data.frame"){

  # get numeric index of relevant pattern for each element of x
  pattern_indices <- pattern_match(x, patterns_regex)
  if (output == "logical") return(!is.na(pattern_indices))

  # initiate a list of results
  res <- as.list(rep.int(NA,length(x)))

  if(inherits(multiple, "formula")) {
    if(!requireNamespace("rlang"))
      stop("rlang package must be installed to use formula notation in `convert` argument of unglue functions")
    multiple <- rlang::as_function(multiple)
  }

  # assign a tibble of matches to each result from each pattern
  for(i in seq_along(patterns_regex)){
    # get numeric index of elements of x that are matched by i-th pattern
    subset_ind <- which(pattern_indices == i)
    x_subset <- x[subset_ind]
    matched <- gregexpr(patterns_regex[i], x_subset, perl = T)
    # `groups` are indices or relevant named captured groups
    groups  <- attr(patterns_regex, "groups")[[i]]
    nms     <- rep(names(groups),lengths(groups))
    # res_i is the list of result for i-th pattern
    res_i <- get_ith_pattern_res(x_subset, matched, groups, nms, multiple)

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


get_ith_pattern_res <- function(x_subset, matched, groups, nms, multiple){
  Map(
    x_subset, matched,
    f = function(x, m) {
      # browser()
      # cs like captured string(s)
      cs <- substring(x, attr(m, "capture.start") , attr(
        m, "capture.start") + attr(m, "capture.length") - 1)
      # keep only named groups
      cs <- cs[unlist(groups)]

      # if no multiples, make a one row data frame of captured strings
      if(is.null(multiple)) {
        cs <- setNames(as.list(cs), nms)
        return(as.data.frame(cs, stringsAsFactors = FALSE))
      }

      # if we have multiples, aggregate by name
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
}


unglue_vec0 <- function(x, patterns_regex, var, convert, multiple){
  groups <- attr(patterns_regex, "groups")
  nms <- lapply(groups, names)
  # assign a pattern to each element
  pattern_indices <- pattern_match(x, patterns_regex)

  if(inherits(multiple, "formula")) {
    if(!requireNamespace("rlang"))
      stop("rlang package must be installed to use formula notation in `convert` argument of unglue functions")
    multiple <- rlang::as_function(multiple)
  }

  # initiate a list of results
  res <- rep.int(NA_character_, length(x))
  if(is.numeric(var)){
    for(i in seq_along(patterns_regex)){
      groups  <- attr(patterns_regex, "groups")[[i]]
      nms     <- rep(names(groups),lengths(groups))
      subset_ind <- which(pattern_indices == i)
      if (var > length(groups)) {
        # if var is too big of an index, result is NA
        res_i <- rep_len(NA_character_, length(subset_ind))
      } else {
        x_subset <- x[subset_ind]
        matched <- gregexpr(patterns_regex[i], x_subset, perl = TRUE)
        res_i <- get_ith_pattern_res(x_subset, matched, groups, nms, multiple)
        res_i <- sapply(res_i, `[[`, var)
      }
      res[subset_ind] <- res_i
    }
  } else {
    for(i in seq_along(patterns_regex)){
      groups  <- attr(patterns_regex, "groups")[[i]]
      nms     <- rep(names(groups),lengths(groups))
      subset_ind <- which(pattern_indices == i)
      if (! var %in% nms) {
        res_i <- rep_len(NA_character_, length(subset_ind))
      } else {
        x_subset <- x[subset_ind]
        matched <- gregexpr(patterns_regex[i], x_subset, perl = TRUE)
        res_i <- get_ith_pattern_res(x_subset, matched, groups, nms, multiple)
        res_i <- sapply(res_i, `[[`, var)
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
