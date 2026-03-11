build_s3_client <- function() {
  s3_endpoint_raw <- Sys.getenv("AWS_S3_ENDPOINT", Sys.getenv("S3_ENDPOINT", ""))
  if (!nzchar(s3_endpoint_raw)) {
    return(NULL)
  }

  endpoint <- if (grepl("^https?://", s3_endpoint_raw)) s3_endpoint_raw else paste0("https://", s3_endpoint_raw)
  region <- Sys.getenv("AWS_DEFAULT_REGION", "waw3-1")
  access_key <- Sys.getenv("AWS_ACCESS_KEY_ID", "")
  secret_key <- Sys.getenv("AWS_SECRET_ACCESS_KEY", "")
  session_token <- Sys.getenv("AWS_SESSION_TOKEN", "")

  if (!nzchar(access_key) || !nzchar(secret_key)) {
    return(NULL)
  }

  creds <- list(
    access_key_id = access_key,
    secret_access_key = secret_key
  )
  if (nzchar(session_token)) {
    creds$session_token <- session_token
  }

  paws::s3(config = list(
    credentials = list(creds = creds),
    endpoint = endpoint,
    region = region
  ))
}

resolve_bucket <- function() {
  bucket <- Sys.getenv("S3_BUCKET", "")
  if (nzchar(bucket)) {
    return(bucket)
  }
  edito_user <- Sys.getenv("EDITO_USERNAME", "")
  if (nzchar(edito_user)) {
    return(paste0("oidc-", edito_user))
  }
  ""
}

safe_s3_key <- function(prefix, filename) {
  if (!nzchar(prefix)) {
    return(filename)
  }
  paste0(gsub("/+$", "", prefix), "/", gsub("^/+", "", filename))
}

download_s3_object <- function(s3, bucket, key, dest_path) {
  if (is.null(s3) || !nzchar(bucket) || !nzchar(key)) {
    return(FALSE)
  }

  dir.create(dirname(dest_path), recursive = TRUE, showWarnings = FALSE)

  ok <- tryCatch({
    obj <- s3$get_object(Bucket = bucket, Key = key)
    body <- obj$Body

    if (is.raw(body)) {
      writeBin(body, dest_path)
    } else if (is.character(body)) {
      writeBin(charToRaw(paste(body, collapse = "")), dest_path)
    } else {
      stop("Unsupported response body type from S3")
    }
    TRUE
  }, error = function(e) {
    cat(">>> S3 download failed for", paste0("s3://", bucket, "/", key), "-", conditionMessage(e), "\n")
    FALSE
  })

  if (ok) {
    cat(">>> Downloaded", paste0("s3://", bucket, "/", key), "->", dest_path, "\n")
  }
  ok
}

upload_to_s3 <- function(s3, bucket, local_path, s3_key) {
  if (is.null(s3) || !nzchar(bucket) || !file.exists(local_path)) {
    return(FALSE)
  }

  ok <- tryCatch({
    s3$put_object(
      Bucket = bucket,
      Key = s3_key,
      Body = readBin(local_path, what = "raw", n = file.info(local_path)$size)
    )
    TRUE
  }, error = function(e) {
    cat(">>> S3 upload failed for", paste0("s3://", bucket, "/", s3_key), "-", conditionMessage(e), "\n")
    FALSE
  })

  if (ok) {
    cat(">>> Uploaded", local_path, "->", paste0("s3://", bucket, "/", s3_key), "\n")
  }
  ok
}

upload_dir_to_s3 <- function(s3, bucket, local_dir, s3_prefix) {
  if (is.null(s3) || !nzchar(bucket) || !dir.exists(local_dir)) {
    return(invisible(NULL))
  }

  local_dir_norm <- normalizePath(local_dir, winslash = "/", mustWork = FALSE)
  files <- list.files(local_dir, recursive = TRUE, full.names = TRUE)

  for (f in files) {
    file_norm <- normalizePath(f, winslash = "/", mustWork = FALSE)
    prefix <- paste0(local_dir_norm, "/")
    rel <- if (startsWith(file_norm, prefix)) substring(file_norm, nchar(prefix) + 1) else basename(file_norm)
    key <- if (!nzchar(s3_prefix)) rel else safe_s3_key(s3_prefix, rel)
    upload_to_s3(s3, bucket, f, key)
  }
}