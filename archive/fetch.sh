#!/bin/bash
## Simple wrapper to fetch results from Condor

REMOTE_DIR=""
MODEL="all"
JOB="model"
LOCAL_DIR="model"

## Parse arguments
for arg in "$@"; do
  case $arg in
    --model=*)
      MODEL="${arg#*=}"
      ;;
    --job=*)
      JOB="${arg#*=}"
      ;;
    --local-dir=*)
      LOCAL_DIR="${arg#*=}"
      ;;
    *)
      if [ -z "$REMOTE_DIR" ]; then
        REMOTE_DIR="$arg"
      else
        echo "Unknown option: $arg"
        echo ""
        echo "Usage: ./fetch.sh <remote_dir> [options]"
        echo ""
        echo "Arguments:"
        echo "  <remote_dir>        Remote directory (e.g., develop/Feb_03_2026_model)"
        echo ""
        echo "Options:"
        echo "  --model=<name>      Model to fetch (default: all)"
        echo "  --job=<type>        Job type: model, hessian, prof, jitter, all (default: model)"
        echo "  --local-dir=<path>  Local directory (default: model)"
        echo ""
        echo "Examples:"
        echo "  ./fetch.sh develop/Feb_03_2026_model"
        echo "  ./fetch.sh develop/Feb_03_2026_hessian --job=hessian"
        echo "  ./fetch.sh develop/Feb_03_2026_all --job=all --model=base"
        echo ""
        exit 1
      fi
      ;;
  esac
done

if [ -z "$REMOTE_DIR" ]; then
  echo "Error: Remote directory required"
  echo ""
  echo "Usage: ./fetch.sh <remote_dir> [options]"
  echo "Example: ./fetch.sh develop/Feb_03_2026_model"
  echo ""
  exit 1
fi

echo "=========================================="
echo "Fetching results from Condor"
echo "=========================================="
echo "Remote dir: $REMOTE_DIR"
echo "Models: $MODEL"
echo "Job type: $JOB"
echo "Local dir: $LOCAL_DIR"
echo ""

Rscript fetch_results.R "$REMOTE_DIR" --model=$MODEL --job=$JOB --local-dir=$LOCAL_DIR
