#!/bin/bash
## Simple wrapper to launch jitter jobs

MODEL="base"
RUNS=100
DIR=""

## Parse arguments
for arg in "$@"; do
  case $arg in
    --model=*)
      MODEL="${arg#*=}"
      ;;
    --runs=*)
      RUNS="${arg#*=}"
      ;;
    --dir=*)
      DIR="--dir=${arg#*=}"
      ;;
    --all)
      MODEL="all"
      ;;
    *)
      echo "Unknown option: $arg"
      echo ""
      echo "Usage: ./launch_jitter.sh [options]"
      echo ""
      echo "Options:"
      echo "  --model=<name>   Model to run (default: base)"
      echo "  --all            Run all models"
      echo "  --runs=<n>       Number of jitter runs (default: 100)"
      echo "  --dir=<path>     Custom remote directory"
      echo ""
      echo "Examples:"
      echo "  ./launch_jitter.sh"
      echo "  ./launch_jitter.sh --model=M1 --runs=50"
      echo "  ./launch_jitter.sh --all"
      echo ""
      exit 1
      ;;
  esac
done

echo "=========================================="
echo "Launching JITTER jobs"
echo "=========================================="
echo "Models: $MODEL"
echo "Runs: $RUNS"
echo ""

Rscript launch.R jitter --model=$MODEL --njitter=$RUNS $DIR
