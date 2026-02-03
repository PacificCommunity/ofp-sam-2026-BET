#!/bin/bash
## Simple wrapper to launch model runs

MODEL="base"
DIR=""

## Parse arguments
for arg in "$@"; do
  case $arg in
    --model=*)
      MODEL="${arg#*=}"
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
      echo "Usage: ./launch_model.sh [options]"
      echo ""
      echo "Options:"
      echo "  --model=<name>   Model to run (default: base)"
      echo "  --all            Run all models"
      echo "  --dir=<path>     Custom remote directory"
      echo ""
      echo "Examples:"
      echo "  ./launch_model.sh"
      echo "  ./launch_model.sh --model=M1"
      echo "  ./launch_model.sh --all"
      echo "  ./launch_model.sh --model=base --dir=develop/test_run"
      echo ""
      exit 1
      ;;
  esac
done

echo "=========================================="
echo "Launching MODEL jobs"
echo "=========================================="
echo "Models: $MODEL"
echo ""

Rscript launch.R model --model=$MODEL $DIR
