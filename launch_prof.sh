#!/bin/bash
## Simple wrapper to launch profile jobs

MODEL="base"
SCALERS="100,90,80,70,60,50"
DIR=""

## Parse arguments
for arg in "$@"; do
  case $arg in
    --model=*)
      MODEL="${arg#*=}"
      ;;
    --scalers=*)
      SCALERS="${arg#*=}"
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
      echo "Usage: ./launch_prof.sh [options]"
      echo ""
      echo "Options:"
      echo "  --model=<name>      Model to run (default: base)"
      echo "  --all               Run all models"
      echo "  --scalers=<list>    Comma-separated scalers (default: 100,90,80,70,60,50)"
      echo "  --dir=<path>        Custom remote directory"
      echo ""
      echo "Examples:"
      echo "  ./launch_prof.sh"
      echo "  ./launch_prof.sh --model=M1 --scalers=70,80,90,100"
      echo "  ./launch_prof.sh --all"
      echo ""
      exit 1
      ;;
  esac
done

echo "=========================================="
echo "Launching PROFILE jobs"
echo "=========================================="
echo "Models: $MODEL"
echo "Scalers: $SCALERS"
echo ""

Rscript launch.R prof --model=$MODEL --scalers=$SCALERS $DIR
