file_stem <- function(filename) {
  sub("\\.[^.]+$", "", basename(filename))
}

timestamp_prefix <- function(filename) {
  stem <- file_stem(filename)
  match <- regexpr("^[0-9]{8}_[0-9]{4}", stem, perl = TRUE)
  if (match[[1]] == -1L) {
    return(NA_character_)
  }
  regmatches(stem, match)
}

normalized_descriptor <- function(filename) {
  value <- tolower(file_stem(filename))
  value <- sub("^[0-9]{8}_[0-9]{4}[_ -]*", "", value, perl = TRUE)
  value <- gsub(
    "light[ _-]*fluc[ _-]*(script|data)?",
    "",
    value,
    perl = TRUE
  )
  value <- gsub("post[ _-]*blackout", "", value, perl = TRUE)
  value <- gsub("with[ _-]*area[ _-]*[0-9.]+", "", value, perl = TRUE)
  value <- gsub("area[ _-]*[0-9.]+", "", value, perl = TRUE)
  value <- gsub("\\([0-9.]+\\)", "", value, perl = TRUE)
  value <- gsub("[^a-z0-9]+", "_", value, perl = TRUE)
  gsub("^_+|_+$", "", value, perl = TRUE)
}

protocol_match_result <- function(status, protocol = NULL, method = NULL, message) {
  list(
    status = status,
    protocol = protocol,
    method = method,
    message = message
  )
}

match_protocol <- function(measurement_name, protocols) {
  if (!is.data.frame(protocols) || !"name" %in% names(protocols)) {
    stop("Protocols must be a data frame with a name column.", call. = FALSE)
  }

  if (nrow(protocols) == 0L) {
    return(protocol_match_result(
      "missing",
      message = "No protocol TXT files are available in the protocols folder."
    ))
  }

  measurement_stem <- file_stem(measurement_name)
  protocol_stems <- vapply(protocols$name, file_stem, character(1))
  exact <- which(tolower(protocol_stems) == tolower(measurement_stem))

  if (length(exact) == 1L) {
    return(protocol_match_result(
      "matched",
      protocols[exact, , drop = FALSE],
      "exact filename stem",
      sprintf(
        paste0(
          "Protocol match: %s has the same filename stem as the measurement ",
          "(ignoring the .csv/.txt extension). No fuzzy matching was used."
        ),
        protocols$name[[exact]]
      )
    ))
  }

  measurement_timestamp <- timestamp_prefix(measurement_name)
  protocol_timestamps <- vapply(protocols$name, timestamp_prefix, character(1))
  candidates <- which(
    !is.na(measurement_timestamp) &
      !is.na(protocol_timestamps) &
      protocol_timestamps == measurement_timestamp
  )

  if (length(candidates) == 1L) {
    return(protocol_match_result(
      "matched",
      protocols[candidates, , drop = FALSE],
      "unique timestamp prefix",
      sprintf(
        paste0(
          "Protocol match: %s is the only TXT protocol with the shared %s ",
          "timestamp prefix. No fuzzy matching was used."
        ),
        protocols$name[[candidates]],
        measurement_timestamp
      )
    ))
  }

  if (length(candidates) > 1L) {
    measurement_descriptor <- normalized_descriptor(measurement_name)
    candidate_descriptors <- vapply(
      protocols$name[candidates],
      normalized_descriptor,
      character(1)
    )
    descriptor_matches <- candidates[candidate_descriptors == measurement_descriptor]

    if (length(descriptor_matches) == 1L) {
      matched <- descriptor_matches[[1]]
      return(protocol_match_result(
        "matched",
        protocols[matched, , drop = FALSE],
        "timestamp and normalized descriptor",
        sprintf(
          paste0(
            "Protocol match: %s matched by the shared timestamp and an exact ",
            "normalized descriptor. Normalization removes only known technical ",
            "variants such as lightFlucScript, postblackout, and area annotations. ",
            "No fuzzy matching was used."
          ),
          protocols$name[[matched]]
        )
      ))
    }

    candidate_names <- paste(protocols$name[candidates], collapse = ", ")
    return(protocol_match_result(
      "ambiguous",
      message = sprintf(
        paste0(
          "Several protocols share this timestamp and no unique descriptor match was found: %s. ",
          "No protocol was guessed."
        ),
        candidate_names
      )
    ))
  }

  protocol_match_result(
    "missing",
    message = paste0(
      "No protocol matched this measurement by exact filename stem or timestamp. ",
      "No protocol was guessed."
    )
  )
}
