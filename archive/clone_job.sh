
#!/bin/bash
export GITHUB_PAT='REMOVED_SECRET'
export GITHUB_USERNAME='kyuhank'
export GITHUB_ORGANIZATION='PacificCommunity'
export GITHUB_REPO='ofp-sam-2026-bet'
export GITHUB_BRANCH='reorg'


if [[ -n "$GITHUB_TARGET_FOLDER" ]]; then
    git init
    git remote add origin https://$GITHUB_USERNAME:$GITHUB_PAT@github.com/$GITHUB_ORGANIZATION/$GITHUB_REPO.git
    git config core.sparseCheckout true
    echo "$GITHUB_TARGET_FOLDER/" >> .git/info/sparse-checkout
    git pull origin $GITHUB_BRANCH
else
    git clone -b $GITHUB_BRANCH https://$GITHUB_USERNAME:$GITHUB_PAT@github.com/$GITHUB_ORGANIZATION/$GITHUB_REPO.git
fi

