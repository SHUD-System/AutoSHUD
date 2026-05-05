#!/usr/bin/env bash
# AutoSHUD post-merge tag helper for the v2.x release line.
#
# Default mode is a dry run. It prints the intended v2.5.0 release action and
# checks whether the tag already exists, but it does not create or push a tag.
# Use --execute only after the release-preparation PR has been merged into
# master and the merged commit has passed the release checklist.

set -euo pipefail

TAG="v2.5.0"
CREATE_MAINT=false
EXECUTE=false

usage() {
  cat <<'USAGE'
Usage: ./scripts/tag-v2-freeze.sh [tag] [--execute] [--maint]

Defaults:
  tag        v2.5.0
  mode       dry run; no tag is created or pushed

Examples:
  ./scripts/tag-v2-freeze.sh
  ./scripts/tag-v2-freeze.sh --execute
  ./scripts/tag-v2-freeze.sh v2.5.0 --execute
  ./scripts/tag-v2-freeze.sh v2.5.0 --execute --maint

Rules:
  - Run --execute only after the release PR is merged into master.
  - Tags and GitHub Releases must be created only from merged master.
  - This script creates/pushes the git tag only in --execute mode.
  - This script never creates the GitHub Release; use docs/release-v2.5.0.md.
USAGE
}

for arg in "$@"; do
  case "$arg" in
    --maint) CREATE_MAINT=true ;;
    --execute) EXECUTE=true ;;
    -h|--help)
      usage
      exit 0
      ;;
    v*) TAG="$arg" ;;
    *)
      echo "Error: unknown argument: $arg" >&2
      usage >&2
      exit 2
      ;;
  esac
done

ROOT="$(git rev-parse --show-toplevel 2>/dev/null)" || {
  echo "Error: run this script inside the AutoSHUD repository." >&2
  exit 1
}
cd "$ROOT"

echo "== Repository root: $ROOT"
echo "== Tag: $TAG"
if $EXECUTE; then
  echo "== Mode: execute post-merge tag publication"
else
  echo "== Mode: dry run; no tag will be created or pushed"
fi
echo ""

if ! git rev-parse --verify master >/dev/null 2>&1; then
  echo "Error: local branch master does not exist." >&2
  exit 1
fi

if ! $EXECUTE; then
  current_branch="$(git branch --show-current 2>/dev/null || true)"
  echo "Current branch: ${current_branch:-detached HEAD}"
  if git rev-parse -q --verify "refs/tags/$TAG" >/dev/null; then
    echo "Local tag exists: $TAG"
  else
    echo "Local tag not found: $TAG"
  fi
  if git ls-remote --exit-code --tags origin "$TAG" >/dev/null 2>&1; then
    echo "Remote tag exists on origin: $TAG"
  else
    echo "Remote tag not found on origin: $TAG"
  fi
  echo ""
  echo "Dry run complete. After merge, run:"
  echo "  ./scripts/tag-v2-freeze.sh $TAG --execute"
  echo "Then create the GitHub Release using docs/release-v2.5.0.md."
  exit 0
fi

echo ">> [1/6] Fetching remote updates..."
git fetch origin --tags

echo ">> [2/6] Checking out master and fast-forwarding to origin/master..."
git checkout master
git pull --ff-only origin master

echo ">> [3/6] Checking that the working tree is clean..."
if ! git diff-index --quiet HEAD --; then
  echo "Error: uncommitted changes exist. Commit or stash them before tagging." >&2
  git status -s
  exit 1
fi

if git rev-parse -q --verify "refs/tags/$TAG" >/dev/null; then
  echo "Error: local tag already exists: $TAG" >&2
  exit 1
fi

if git ls-remote --exit-code --tags origin "$TAG" >/dev/null 2>&1; then
  echo "Error: remote tag already exists on origin: $TAG" >&2
  exit 1
fi

echo ">> [4/6] Creating annotated tag $TAG ..."
git tag -a "$TAG" -m "AutoSHUD $TAG"

echo ">> [5/6] Pushing tag to origin ..."
git push origin "$TAG"

if $CREATE_MAINT; then
  MAINT_BRANCH="maint/v2.5.x"
  if git rev-parse --verify "$MAINT_BRANCH" >/dev/null 2>&1; then
    echo ">> Local branch $MAINT_BRANCH already exists; skipping creation."
  else
    echo ">> [optional] Creating maintenance branch $MAINT_BRANCH from $TAG ..."
    git branch "$MAINT_BRANCH" "$TAG"
    git push -u origin "$MAINT_BRANCH"
  fi
fi

echo ">> [6/6] Tag publication complete."
echo ""
echo "------------------------------------------------------------"
echo "Next steps:"
echo "  1. Open: https://github.com/SHUD-System/AutoSHUD/releases/new"
echo "  2. Choose tag: $TAG"
echo "  3. Release title: AutoSHUD ${TAG}"
echo "  4. Use the draft release notes in docs/release-v2.5.0.md"
echo "  5. Publish the GitHub Release"
echo ""
echo "Do not create a v3.0.0 release from this flow; v3.0.0 remains team-review gated."
echo "------------------------------------------------------------"
