#' Test Corpus
#'
#' This function tests the parquet files from a specified directory and prints a message for each file.
#' The function does not return anything, the essential are the disgnostig messages
#' @importFrom arrow read_parquet
#' @param path The path to the directory containing the parquet files.
#' @return NULL
#' @export
#'
#' @examples
#' test_corpus()
#'
#' @keywords internal
test_corpus <- function(
    path = file.path("data", "corpus")) {
    p <- list.files(
        path = "data/corpus",
        pattern = ".parquet$",
        recursive = TRUE,
        full.names = TRUE
    )
    oldOpt <- options(warn = 1)
    try(
        invisible(
            lapply(
                p,
                function(x) {
                    message(x)
                    read_parquet(x)
                    invisible(NULL)
                }
            )
        )
    )
    options(oldOpt)
}

#' Assess Search Term
#'
#' This function assesses the search term by counting the number of occurrences in a given text corpus.
#'
#' @param st The search term to be assessed. Each line should be one sub-term. Usually, these are combined by `OR`.
#' @param remove A regular expression pattern to remove from the search term.
#' @param excl_others Logical indicating whether to exclude other search terms from the count.
#' @param mc.cores The number of CPU cores to use for parallel processing.
#'
#' @return A data frame with the search term and the corresponding count.
#'
#' @importFrom pbmcapply pbmclapply
#' @importFrom openalexR oa_fetch
#'
#' @md
#'
#' @examples
#' assess_search_term(list("climate OR", "change"))
#'
#' @keywords internal
assess_search_term <- function(
    st = NULL,
    remove = " OR$",
    excl_others = FALSE,
    mc.cores = 8) {
    st <- gsub(pattern = remove, replacement = "", st)
    result <- data.frame(
        term = st,
        count = pbmcapply::pbmclapply(
            st,
            function(x) {
                if (excl_others) {
                    excl <- st[!(st %in% x)]
                    searchterm <- paste0("(", x, ") NOT (", paste0(excl, collapse = " OR "), ")")
                } else {
                    searchterm <- x
                }
                openalexR::oa_fetch(
                    title_and_abstract.search = searchterm,
                    output = "list",
                    count_only = TRUE
                )$count
            },
            mc.cores = mc.cores
        ) |>
            unlist()
    )
    return(result)
}
