#' Get Corpus
#'
#' This function opens a dataset from a specified path and returns a corpus object.
#'
#' @param path The path to the dataset.
#' @importFrom arrow open_dataset
#' @return A corpus object.
#' @examples
#' get_corpus("data/corpus/")
get_corpus <- function(
    path = file.path("data", "corpus"),
    unify_schemas = FALSE) {
    con <- arrow::open_dataset(
        sources = path,
        unify_schemas = unify_schemas
    )
    return(con)
}


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

#' Serialize Arrow
#'
#' This function serializes the 'topics' and 'author' columns of a data frame using base64 encoding.
#'
#' @param data A data frame containing 'topics' and 'author' columns.
#' @return A modified data frame with serialized 'topics' and 'author' columns.
#' @importFrom purrr map_chr
#' @importFrom base64enc base64encode
#' @examples
#' data <- data.frame(topics = list("topic1", "topic2"), author = list("author1", "author2"))
#' serialize_arrow(data)
serialize_arrow <- function(data) {
    data |>
        mutate(
            topics = purrr::map_chr(topics, ~ serialize(.x, NULL) |> base64enc::base64encode()),
            author = purrr::map_chr(author, ~ serialize(.x, NULL) |> base64enc::base64encode())
        )
}

#' Unserialize Arrow
#'
#' This function unserializes the 'topics' and 'author' columns of a data frame using base64 decoding.
#'
#' @param data A data frame containing serialized 'topics' and 'author' columns.
#' @return A modified data frame with unserialized 'topics' and 'author' columns.
#' @importFrom purrr map
#' @importFrom base64enc base64decode
#' @examples
#' data <- data.frame(topics = list("dHlwZTE=", "dHlwZTI="), author = list("YXV0aG9yMQ==", "YXV0aG9yMg=="))
#' unserialize_arrow(data)
unserialize_arrow <- function(data) {
    data |>
        mutate(
            topics = purrr::map(topics, ~ .x |>
                base64enc::base64decode() |>
                unserialize()),
            author = purrr::map(author, ~ .x |>
                base64enc::base64decode() |>
                unserialize())
        )
}

#' Compact
#'
#' This function removes newlines, asterisks, extra spaces, and unnecessary spaces around parentheses from a string.
#'
#' @param x A character string.
#' @return A modified string with newlines, asterisks, extra spaces, and unnecessary spaces around parentheses removed.
#' @examples
#' x <- "This is a\nstring with *extra* spaces (and unnecessary spaces)."
#' compact(x)
compact <- function(x) {
    x |>
        gsub(pattern = "\n", replacement = " ") |>
        gsub(pattern = "\\*", replacement = "") |>
        gsub(pattern = "\\s+", replacement = " ") |>
        gsub(pattern = "\\( ", replacement = "(") |>
        gsub(pattern = " )", replacement = ")")
}


assess_search_term <- function(
    st = NULL,
    remove = " OR$",
    mc.cores = 8) {
    st <- gsub(pattern = remove, replacement = "", st)
    result <- data.frame(
        term = st,
        count = pbmcapply::pbmclapply(
            st,
            function(x) {
                excl <- st[!(st %in% x)]
                r <- openalexR::oa_fetch(
                    title_and_abstract.search = paste0("(", x, ") NOT (", paste0(excl, collapse = " OR "), ")"),
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
