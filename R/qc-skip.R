
## This function identifies questions in a table that might have been skipped, and
## reports which questions might have caused that skipping.
## This assumes that the order questions are asked is the order they are reported in the codebook.

##' Identify questions that may have been skipped based on response to a
##' previous question.
##'
##' @param table Character string giving the name of an NHANES table
##' @return A data frame with columns:
##' \describe{
#'   \item{Table}{The name of the input table.}
#'   \item{Variable}{One row for each question in the documentation file.}
#'   \item{MaybeSkipped}{A logical value indicating whether that question might have been skipped.}
#'.  \item{SkippedDueTo}{A vector of the questions, delivered earlier, that might have caused this question to be skipped.}
#' }
##' @author Deepayan Sarkar
##' @examples
##' sk1 = get_skip_info("DEMO_J")
##' table(sk1$MaybeSkipped)
##' sk1[sk1$MaybeSkipped,]
##' @export
get_skip_info <- function(table)
{
    ## Look at codebook for a table and decide which variables may get
    ## skipped over
    if(length(table)>1) stop("argument 'table' must have length one")
    var <- metadata_var(table = table)
    cb <- metadata_cb(table = table)[, c("Variable", "SkipToItem")]
    cb <- dplyr::mutate(cb, SkipToItem = toupper(SkipToItem))
    cb <- unique(cb)
    ##SkipToItem is character - and not NA
    skipvars <- subset(cb, nchar(SkipToItem) > 0)

    ## Sanity check: non-NA values of SkipToItem should be either
    ## another variable, or "End of Section"

    uvars <- unique(var$Variable) # not always same as cb$Variable; see BPQ_D
    stopifnot(all(skipvars$SkipToItem %in% c(uvars, "END OF SECTION")))

    ## For each variable, we want to know (a) if this variable _might_
    ## have been skipped based on response to a previous question, and
    ## (b) if so, which question. Part (b) can have multiple
    ## answers. For now, we will only count the number of variables
    ## that could cause such skipping, without recording what they
    ## are. Hopefully this is a good starting point; we can add more
    ## information later if necessary.

    maybe_skipped <- structure(numeric(length(uvars)), names = uvars)
    due_to <- structure(vector(mode = "list", length = length(uvars)),
                        names = uvars)

    ## Example: table = "WHQ_B"
    ##    Variable     SkipToItem
    ## 25   WHQ060        WHD080A
    ## 31   WHQ070         WHQ090
    ## 32   WHQ070         WHQ090
    ## 33   WHQ070         WHQ090
    ## 61  WHD080M End of Section
    ## 63  WHD080N End of Section
    ## 66   WHQ090         WHD110
    ## 67   WHQ090         WHD110
    ## 68   WHQ090         WHD110
    ## 96  WHD100M End of Section
    ## 98  WHD100N End of Section

    ## First map to integers
    iskipvars <- list(Variable = match(skipvars$Variable, uvars),
                      SkipToItem = match(skipvars$SkipToItem, uvars,
                                         nomatch = length(uvars) + 1L))

    ## Only those strictly in-between are potentially skipped
    for (i in seq_len(nrow(skipvars))) {
        ind <- seq(iskipvars$Variable[i] + 1L,
                   iskipvars$SkipToItem[i] - 1L)
        maybe_skipped[ind] <- maybe_skipped[ind] + 1
        for (j in ind) {
            due_to[[j]] <- c(due_to[[j]], skipvars$Variable[[i]])
        }
    }
    data.frame(Table = table, Variable = uvars,
               MaybeSkipped = maybe_skipped > 0,
               SkippedDueTo = sapply(due_to, function(x) paste(unique(x), collapse = ", ")),
               row.names = NULL)
}

