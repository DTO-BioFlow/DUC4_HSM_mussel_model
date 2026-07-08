#!/bin/bash
set -euo pipefail

# ---------------------------------------------------------------------------
# Entrypoint: syncs the S3_SCRIPTS_PREFIX folder from S3 (including PARAMS),
# then executes SCRIPT_NAME.  All run parameters come from the PARAMS file.
#
# Required environment variables:
#   SCRIPT_NAME           R script filename to run (e.g. VSC_CB2_HSM_18.R)
#   S3_BUCKET             S3 bucket name
#   AWS_ACCESS_KEY_ID     S3-compatible access key
#   AWS_SECRET_ACCESS_KEY S3-compatible secret key
#   AWS_S3_ENDPOINT       Custom S3 endpoint URL (e.g. s3.waw3-1.cloudferro.com)
#
# Optional environment variables (defaults set in Dockerfile):
#   S3_SCRIPTS_PREFIX     S3 key prefix for scripts folder (default: scripts)
#   AWS_DEFAULT_REGION    S3 region (default: waw3-1)
#   AWS_SESSION_TOKEN     Session token if using temporary credentials
#   PARAMS                Path to params file (default: /app/scripts/PARAMS)
# ---------------------------------------------------------------------------

SCRIPTS_LOCAL_DIR="/app/scripts"
S3_SCRIPTS_PREFIX="${S3_SCRIPTS_PREFIX:-scripts}"

# ---------------------------------------------------------------------------
# Validate required env vars
# ---------------------------------------------------------------------------
if [[ -z "${SCRIPT_NAME:-}" ]]; then
    echo "Error: SCRIPT_NAME is not set."
    exit 1
fi

S3_BUCKET="${AWS_S3_Bucket_Name:-}"
if [[ -z "${S3_BUCKET:-}" ]]; then
    echo "Error: S3_BUCKET is not set."
    exit 1
fi

if [[ -z "${AWS_S3_ENDPOINT:-}" ]]; then
    echo "Error: AWS_S3_ENDPOINT is not set."
    exit 1
fi

if [[ -z "${AWS_ACCESS_KEY_ID:-}" ]] || [[ -z "${AWS_SECRET_ACCESS_KEY:-}" ]]; then
    echo "Error: AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY must both be set."
    exit 1
fi

# ---------------------------------------------------------------------------
# Build the --endpoint-url argument (prepend https:// if no scheme present)
# ---------------------------------------------------------------------------
ENDPOINT_ARG=""
if [[ -n "${AWS_S3_ENDPOINT:-}" ]]; then
    if [[ "${AWS_S3_ENDPOINT}" != http* ]]; then
        ENDPOINT_ARG="--endpoint-url https://${AWS_S3_ENDPOINT}"
    else
        ENDPOINT_ARG="--endpoint-url ${AWS_S3_ENDPOINT}"
    fi
fi

# ---------------------------------------------------------------------------
# Sync the entire scripts prefix from S3.
# This downloads SCRIPT_NAME, PARAMS, and any helper files.
# ---------------------------------------------------------------------------
SCRIPT_PATH="${SCRIPTS_LOCAL_DIR}/${SCRIPT_NAME}"
S3_SCRIPTS_URI="s3://${S3_BUCKET}/$(echo "${S3_SCRIPTS_PREFIX}" | sed 's|/*$||')/"
mkdir -p "${SCRIPTS_LOCAL_DIR}"

echo ">>> Syncing scripts folder from S3: ${S3_SCRIPTS_URI} -> ${SCRIPTS_LOCAL_DIR}/"
# shellcheck disable=SC2086
aws s3 sync ${ENDPOINT_ARG} "${S3_SCRIPTS_URI}" "${SCRIPTS_LOCAL_DIR}/"
echo ">>> Sync complete"

if [[ ! -f "${SCRIPT_PATH}" ]]; then
    echo "Error: SCRIPT_NAME '${SCRIPT_NAME}' not found in ${SCRIPTS_LOCAL_DIR} after sync."
    echo "  Check that s3://${S3_BUCKET}/${S3_SCRIPTS_PREFIX}/${SCRIPT_NAME} exists."
    exit 1
fi

export PARAMS="${PARAMS:-${SCRIPTS_LOCAL_DIR}/PARAMS}"
echo ">>> PARAMS file: ${PARAMS}"

# ---------------------------------------------------------------------------
# Execute the R script
# ---------------------------------------------------------------------------
echo ">>> Running: Rscript ${SCRIPT_PATH}"
exec Rscript "${SCRIPT_PATH}"
