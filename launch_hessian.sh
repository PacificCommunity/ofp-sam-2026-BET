#!/bin/bash
## Simple wrapper to launch hessian jobs

MODEL="base"
PARTS=200
DIR=""

## Parse arguments
for arg in "$@"; do
  case $arg in
    --model=*)
      MODEL="${arg#*=}"
      ;;
    --parts=*)
      PARTS="${arg#*=}"
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
      echo "Usage: ./launch_hessian.sh [options]"
      echo ""
      echo "Options:"
      echo "  --model=<name>   Model to run (default: base)"
      echo "  --all            Run all models"
      echo "  --parts=<n>      Number of parts (default: 200)"
      echo "  --dir=<path>     Custom remote directory"
      echo ""
      echo "Examples:"
      echo "  ./launch_hessian.sh"
      echo "  ./launch_hessian.sh --model=M1 --parts=150"
      echo "  ./launch_hessian.sh --all"
      echo ""
      exit 1
      ;;
  esac
done

echo "=========================================="
echo "Launching HESSIAN jobs"
echo "=========================================="
echo "Models: $MODEL"
echo "Parts: $PARTS"
echo ""

Rscript launch.R hessian --model=$MODEL --nsplit=$PARTS $DIR
