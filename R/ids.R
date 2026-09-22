#' Draw random characters from the operating system's secure generator
#'
#' Identifiers never use R's global random number generator, which plotting
#' code may reseed. Rejection sampling keeps every character equally likely.
#' @param n Number of characters.
#' @param alphabet Character vector of single characters.
#' @return One string of `n` characters.
#' @keywords internal
#' @noRd
bw_random_chars <- function(n, alphabet) {
  check_index(n, "n")
  k <- length(alphabet)
  limit <- 256L - 256L %% k
  drawn <- integer()
  while (length(drawn) < n) {
    bytes <- as.integer(openssl::rand_bytes(2L * n))
    drawn <- c(drawn, bytes[bytes < limit])
  }
  paste(alphabet[drawn[seq_len(n)] %% k + 1L], collapse = "")
}

#' Persistent participant identity stored on the participant's device
#' @keywords internal
#' @noRd
bw_participant_id <- function() bw_random_chars(14L, c(letters, 0:9))

#' Short session address without easily confused characters
#' @keywords internal
#' @noRd
bw_session_code <- function() {
  bw_random_chars(6L, strsplit("23456789abcdefghjkmnpqrstuvwxyz", "")[[1]])
}

#' Moderator login token
#' @keywords internal
#' @noRd
bw_token <- function() bw_random_chars(32L, c(letters, LETTERS, 0:9))
