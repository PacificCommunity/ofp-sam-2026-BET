#!/bin/bash
## Migration script to reorganize files into new structure

echo "==================================="
echo "Reorganizing MFCL Assessment Files"
echo "==================================="

## Backup old files
echo ""
echo "Creating backup..."
mkdir -p .backup
cp -r *.R .backup/ 2>/dev/null
echo "✓ Backed up to .backup/"

## Move tool scripts to tools/
echo ""
echo "Moving analysis tools to tools/..."
mv -f collate_hessian.R tools/ 2>/dev/null && echo "  ✓ collate_hessian.R"
mv -f collate_hessian_mfcl.R tools/ 2>/dev/null && echo "  ✓ collate_hessian_mfcl.R"
mv -f verify_hessian.R tools/ 2>/dev/null && echo "  ✓ verify_hessian.R"
mv -f read_hessian.R tools/ 2>/dev/null && echo "  ✓ read_hessian.R"
mv -f inspect_hessian.R tools/ 2>/dev/null && echo "  ✓ inspect_hessian.R"
mv -f test_hessian_split.R tools/ 2>/dev/null && echo "  ✓ test_hessian_split.R"

## Rename old launch scripts
echo ""
echo "Archiving old launch scripts..."
mkdir -p .old_launch
mv -f launch_condor*.R .old_launch/ 2>/dev/null && echo "  ✓ Archived launch_condor*.R"

## Rename old run scripts
echo ""
echo "Archiving old run scripts..."
mkdir -p .old_scripts
mv -f run_model.R .old_scripts/ 2>/dev/null && echo "  ✓ run_model.R"
mv -f run_hessian.R .old_scripts/ 2>/dev/null && echo "  ✓ run_hessian.R"
mv -f run_prof.R .old_scripts/ 2>/dev/null && echo "  ✓ run_prof.R"
mv -f run_jitter.R .old_scripts/ 2>/dev/null && echo "  ✓ run_jitter.R"
mv -f run_mfcl.R .old_scripts/ 2>/dev/null && echo "  ✓ run_mfcl.R"

## Update makefile
echo ""
echo "Updating makefile..."
if [ -f makefile.new ]; then
  mv makefile makefile.old
  mv makefile.new makefile
  echo "  ✓ makefile updated (old saved as makefile.old)"
fi

## Summary
echo ""
echo "==================================="
echo "Migration Complete!"
echo "==================================="
echo ""
echo "New structure:"
echo "  config.R          - Central configuration"
echo "  launch.R          - Unified launcher"
echo "  makefile          - Updated makefile"
echo "  scripts/          - Execution scripts"
echo "  tools/            - Analysis utilities"
echo ""
echo "Old files saved in:"
echo "  .backup/          - Full backup"
echo "  .old_launch/      - Old launch scripts"
echo "  .old_scripts/     - Old run scripts"
echo ""
echo "Next steps:"
echo "  1. Review config.R settings"
echo "  2. Test locally: make model"
echo "  3. Submit to Condor: Rscript launch.R model"
echo ""
