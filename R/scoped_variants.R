# unglue_chr  <- function(
#   x, patterns, var = 1, open = "{", close = "}", multiple = NULL){
#   patterns_regex <- unglue_to_regex(
#     patterns, open = open, close = close, multiple = multiple,
#     named_capture = FALSE, attributes = TRUE)
#   if((!is.character(var) && !is.numeric(var)) || length(var) != 1){
#     stop("var should be a character or numeric of length 1")
#   }
#   unglue_vec0(x, patterns_regex, var)
# }
#
# # maybe parameter to error on NA coercion ?
# unglue_dbl  <-
#   function(x, patterns, var = 1, open = "{", close = "}", multiple = NULL){
#   patterns_regex <- unglue_to_regex(
#     patterns, open = open, close = close, multiple = multiple,
#     named_capture = FALSE, attributes = TRUE)
#   if((!is.character(var) && !is.numeric(var)) || length(var) != 1){
#     stop("var should be a character or numeric of length 1")
#   }
#   as.double(unglue_vec0(x, patterns_regex, var))
# }
#
# unglue_int  <-
#   function(x, patterns, var = 1, open = "{", close = "}", multiple = NULL, strict = TRUE){
#   patterns_regex <- unglue_to_regex(
#     patterns, open = open, close = close, multiple = multiple,
#     named_capture = FALSE, attributes = TRUE)
#   if((!is.character(var) && !is.numeric(var)) || length(var) != 1){
#     stop("var should be a character or numeric of length 1")
#   }
#   res <- unglue_vec0(x, patterns_regex, var)
#   if(strict) as.integer2(res) else as.integer(res)
# }
#
# unglue_lgl  <-
#   function(x, patterns, var = 1, open = "{", close = "}",
#            multiple = NULL, strict = TRUE){
#   patterns_regex <- unglue_to_regex(
#     patterns, open = open, close = close, multiple = multiple,
#     named_capture = FALSE, attributes = TRUE)
#   if((!is.character(var) && !is.numeric(var)) || length(var) != 1){
#     stop("var should be a character or numeric of length 1")
#   }
#   res <- unglue_vec0(x, patterns_regex, var)
#   if(strict) as.logical2(res) else as.logical(res)
# }
#
# # unglue validate with own list or condition ?
#
# as.logical2 <- function(x){
#   stopifnot(all(x %in% c("true","false","TRUE","FALSE","T","F","0","1")))
#   as.logical(x)
# }
#
# as.integer2 <-
#   function(x, tol = .Machine$double.eps^0.5) {
#     x <- as.numeric(x)
#     stopifnot(all(abs(x - round(x)) < tol, na.rm=TRUE))
#     as.integer(x)
#   }
