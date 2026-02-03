
#!/usr/bin/env bash

# Execute the clone script
source clone_job.sh

# Load environment variables from job_env.txt if present
if [[ -f "job_env.txt" ]]; then
  grep -E '^[A-Za-z_][A-Za-z0-9_]*=' "job_env.txt" | sed 's/^/export /' > env_exports.sh
  source env_exports.sh
fi

# Determine working directory
if [[ -n "$GITHUB_TARGET_FOLDER" ]]; then
    WORK_DIR="$GITHUB_TARGET_FOLDER"
else
    WORK_DIR="$GITHUB_REPO"
fi

# Unset GitHub PAT for security
unset GITHUB_PAT

# Change to working directory and run make
cd "$WORK_DIR" || exit 1
echo "Running make with options: hessian hessian_part=7 nsplit=200"
make hessian hessian_part=7 nsplit=200

# Archive the results
cd ..
echo "Archiving folder: $WORK_DIR..."
tar -czvf output_archive.tar.gz "$WORK_DIR"

