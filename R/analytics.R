#' Whether saved text holds a visible contribution
#'
#' The single blank-text rule for analytics, weighting, protocols and views.
#' Whitespace follows Unicode (spaces, tabs, line breaks, no-break and ideographic
#' spaces); invisible format characters such as zero-width spaces count as blank.
#' Intentional line breaks inside real text are kept by callers.
#' @param text Character vector; missing values are blank.
#' @return Logical vector.
#' @keywords internal
#' @noRd
bw_has_text <- function(text) {
  text <- as.character(text)
  !is.na(text) & grepl("[^\\h\\v\\p{Cf}]", text, perl = TRUE)
}

#' Tokenize German contributions without stemming
#'
#' Splits on Unicode nonletters, lowercases, removes tokens shorter than three
#' letters and German Snowball stopwords. Repeated terms remain repeated.
#' @param text Character vector; missing values are treated as empty text.
#' @param extra_stopwords Additional terms to omit, compared in lowercase.
#' @return A list of character vectors, one per input string.
#' @keywords internal
#' @noRd
bw_tokenize <- function(text, extra_stopwords = character()) {
  text <- as.character(text)
  text[is.na(text)] <- ""
  omit <- unique(stringi::stri_trans_tolower(c(
    stopwords::stopwords("de", source = "snowball"), extra_stopwords
  ), locale = "de"))
  split <- stringi::stri_split_regex(
    stringi::stri_trans_tolower(text, locale = "de"), "[^\\p{L}]+"
  )
  lapply(split, function(tokens) {
    tokens[stringi::stri_length(tokens) >= 3L & !tokens %in% omit]
  })
}

#' Tokenize entries with topic-specific prompt exclusion
#' @param entries Raw entries table.
#' @param topics Optional topics table containing id, title, q1 and q2.
#' @param exclude_prompt Whether to omit the corresponding topic vocabulary.
#' @return One token vector per entry.
#' @keywords internal
#' @noRd
bw_entry_tokens <- function(entries, topics = NULL, exclude_prompt = TRUE) {
  tokens <- bw_tokenize(entries$text)
  if (!exclude_prompt || is.null(topics) || !nrow(topics) || !nrow(entries)) {
    return(tokens)
  }
  for (i in seq_len(nrow(topics))) {
    questions <- topic_questions(topics[i, , drop = FALSE])
    prompt <- unique(unlist(bw_tokenize(c(topics$title[i], questions)),
                            use.names = FALSE))
    rows <- which(entries$topic_id == topics$id[i])
    tokens[rows] <- lapply(tokens[rows], function(x) x[!x %in% prompt])
  }
  tokens
}

#' Count filtered terms by topic or by topic and question
#' @param entries Raw entries table.
#' @param by Grouping level: topic, or topic and question.
#' @param topics Optional topics table for prompt vocabulary exclusion.
#' @param exclude_prompt Whether to exclude prompt vocabulary.
#' @return Data frame with topic_id, optionally question, term and n. Counts
#'   include repeated occurrences within an entry, sorted by grouping then term.
#' @keywords internal
#' @noRd
bw_term_counts <- function(entries, by = c("topic", "question"),
                           topics = NULL, exclude_prompt = TRUE) {
  by <- match.arg(by)
  result <- data.frame(topic_id = integer(), term = character(), n = integer())
  if (by == "question") result <- cbind(result["topic_id"], question = integer(),
                                        result[c("term", "n")])
  tokens <- bw_entry_tokens(entries, topics, exclude_prompt)
  if (!sum(lengths(tokens))) return(result)
  rows <- rep(seq_len(nrow(entries)), lengths(tokens))
  long <- data.frame(topic_id = entries$topic_id[rows],
                     term = unlist(tokens, use.names = FALSE), n = 1L)
  keys <- "topic_id"
  if (by == "question") {
    long$question <- entries$question[rows]
    keys <- c(keys, "question")
  }
  keys <- c(keys, "term")
  result <- stats::aggregate(long["n"], long[keys], sum)
  result <- result[do.call(order, result[keys]), c(keys, "n")]
  rownames(result) <- NULL
  result
}

#' Count within-entry term co-occurrences
#' @param entries Raw entries table.
#' @param min_cooc Minimum number of entries containing an edge.
#' @param max_nodes Maximum number of candidate terms, selected by occurrence
#'   frequency with alphabetical tie breaking.
#' @param topics Optional topics table for prompt vocabulary exclusion.
#' @param exclude_prompt Whether to exclude prompt vocabulary.
#' @return Data frame with from, to and weight. Attribute nodes contains term and
#'   n for all selected terms, including isolated nodes. Each pair is counted at
#'   most once per entry; repeated words cannot increase its weight.
#' @keywords internal
#' @noRd
bw_cooccurrence <- function(entries, min_cooc = 2, max_nodes = 40,
                            topics = NULL, exclude_prompt = TRUE) {
  check_index(min_cooc, "min_cooc")
  check_index(max_nodes, "max_nodes")
  if (length(min_cooc) != 1L || length(max_nodes) != 1L) {
    stop("min_cooc and max_nodes must be scalar integers.", call. = FALSE)
  }
  tokens <- bw_entry_tokens(entries, topics, exclude_prompt)
  counts <- table(unlist(tokens, use.names = FALSE))
  nodes <- data.frame(term = as.character(names(counts)), n = as.integer(counts))
  nodes <- utils::head(nodes[order(-nodes$n, nodes$term), , drop = FALSE], max_nodes)
  rownames(nodes) <- NULL
  pairs <- lapply(tokens, function(x) {
    terms <- sort(intersect(unique(x), nodes$term))
    if (length(terms) < 2L) return(NULL)
    m <- utils::combn(terms, 2L)
    data.frame(from = m[1L, ], to = m[2L, ], weight = 1L)
  })
  edges <- data.frame(from = character(), to = character(), weight = integer())
  pairs <- do.call(rbind, pairs)
  if (!is.null(pairs)) {
    edges <- stats::aggregate(pairs["weight"], pairs[c("from", "to")], sum)
    edges <- edges[edges$weight >= min_cooc, , drop = FALSE]
    edges <- edges[order(edges$from, edges$to), , drop = FALSE]
    rownames(edges) <- NULL
  }
  attr(edges, "nodes") <- nodes
  edges
}

#' Measure lexical continuity between consecutive rounds
#'
#' For each topic, sheet and question, pools all contributors in each round into
#' a token set and computes Jaccard overlap with the immediately preceding round.
#' Missing or empty sets produce NA. A topic's score is the arithmetic mean of
#' available comparisons; a topic without valid comparisons has NA. This
#' describes shared vocabulary, not proof of idea quality or causal influence.
#' @param entries Raw entries table.
#' @param topics Optional topics table for prompt vocabulary exclusion.
#' @param exclude_prompt Whether to exclude prompt vocabulary.
#' @return Data frame with topic_id and buildon (0 to 1 or NA).
#' @keywords internal
#' @noRd
bw_buildon <- function(entries, topics = NULL, exclude_prompt = TRUE) {
  ids <- sort(unique(c(entries$topic_id, topics$id)))
  result <- data.frame(topic_id = ids, buildon = rep(NA_real_, length(ids)))
  tokens <- bw_entry_tokens(entries, topics, exclude_prompt)
  for (i in seq_along(ids)) {
    rows <- which(entries$topic_id == ids[i])
    groups <- split(rows, interaction(entries$sheet[rows], entries$question[rows], drop = TRUE))
    scores <- unlist(lapply(groups, function(indices) {
      rounds <- sort(unique(entries$round[indices]))
      later <- rounds[rounds > 1L]
      vapply(later, function(r) {
        previous <- unique(unlist(tokens[indices[entries$round[indices] == r - 1L]],
                                  use.names = FALSE))
        current <- unique(unlist(tokens[indices[entries$round[indices] == r]],
                                 use.names = FALSE))
        if (!length(previous) || !length(current)) return(NA_real_)
        length(intersect(previous, current)) / length(union(previous, current))
      }, numeric(1))
    }), use.names = FALSE)
    if (any(!is.na(scores))) result$buildon[i] <- mean(scores, na.rm = TRUE)
  }
  result
}

#' Summarize stored nonblank contributions
#'
#' Words are all Unicode letter sequences before stopword and prompt filtering.
#' Submission rate is submitted nonblank entries divided by all stored nonblank
#' entries, not the proportion of theoretical assignments completed. Empty data
#' have zero contributions and terms, with undefined means and rates (NA).
#' @param entries Raw entries table.
#' @param topics Optional topics table for prompt vocabulary exclusion.
#' @param exclude_prompt Whether to exclude prompt vocabulary from distinct terms.
#' @return One-row data frame with contributions, mean_words, submission_rate and
#'   distinct_terms.
#' @keywords internal
#' @noRd
bw_kpis <- function(entries, topics = NULL, exclude_prompt = TRUE) {
  keep <- bw_has_text(entries$text)
  entries <- entries[keep, , drop = FALSE]
  n <- nrow(entries)
  words <- stringi::stri_count_regex(entries$text, "\\p{L}+")
  tokens <- bw_entry_tokens(entries, topics, exclude_prompt)
  data.frame(
    contributions = n,
    mean_words = if (n) mean(words) else NA_real_,
    submission_rate = if (n) mean(entries$submitted == 1L, na.rm = TRUE) else NA_real_,
    distinct_terms = length(unique(unlist(tokens, use.names = FALSE)))
  )
}

#' Summarize each topic using the shared analytics definitions
#' @param entries Raw entries table.
#' @param topics Optional topics table; includes topics without contributions.
#' @param exclude_prompt Whether to exclude prompt vocabulary.
#' @return Data frame with topic_id, KPI columns and buildon.
#' @keywords internal
#' @noRd
bw_topic_summary <- function(entries, topics = NULL, exclude_prompt = TRUE) {
  ids <- sort(unique(c(entries$topic_id, topics$id)))
  if (!length(ids)) {
    result <- bw_kpis(entries, topics, exclude_prompt)[FALSE, , drop = FALSE]
    return(cbind(topic_id = integer(), result, buildon = numeric()))
  }
  result <- do.call(rbind, lapply(ids, function(id) {
    cbind(topic_id = id, bw_kpis(entries[entries$topic_id == id, , drop = FALSE],
                                 topics, exclude_prompt))
  }))
  buildon <- bw_buildon(entries, topics, exclude_prompt)
  result$buildon <- buildon$buildon[match(result$topic_id, buildon$topic_id)]
  rownames(result) <- NULL
  result
}
