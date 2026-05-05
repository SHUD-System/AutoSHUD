#!/usr/bin/env bash
# AutoSHUD post-merge tag helper for the v2.5.0 release line.
#
# Default mode is a dry run. It prints the intended v2.5.0 release action and
# checks whether the tag and GitHub Release already exist, but it does not
# create or push a tag. Use --execute only after the release-preparation PR has
# been merged into master and the merged commit has passed the release checklist.

set -euo pipefail

TAG="v2.5.0"
GH_REPO="SHUD-System/AutoSHUD"
CREATE_MAINT=false
EXECUTE=false
EXPECTED_REVIEWED_SHA="${AUTOSHUD_EXPECTED_REVIEWED_SHA:-}"

usage() {
  cat <<'USAGE'
Usage: ./scripts/tag-v2-freeze.sh [v2.5.0] [--execute] [--maint] [--expected-sha SHA]

Defaults:
  tag        v2.5.0
  mode       dry run; no tag is created or pushed

Environment:
  AUTOSHUD_EXPECTED_REVIEWED_SHA
             reviewed commit that must be contained in updated master before
             execute mode creates/pushes the tag

Examples:
  ./scripts/tag-v2-freeze.sh
  ./scripts/tag-v2-freeze.sh --execute --expected-sha <reviewed-sha>
  AUTOSHUD_EXPECTED_REVIEWED_SHA=<reviewed-sha> ./scripts/tag-v2-freeze.sh --execute
  ./scripts/tag-v2-freeze.sh v2.5.0 --execute --expected-sha <reviewed-sha> --maint

Rules:
  - Only v2.5.0 is supported; all other tags, including v3.x, are rejected.
  - Run --execute only after the release PR is merged into master.
  - Tags and GitHub Releases must be created only from merged master.
  - This script creates/pushes the git tag only in --execute mode.
  - This script never creates the GitHub Release; use docs/release-v2.5.0.md.
  - This helper has no v3.0.0 executable path; v3.0.0 remains team-review gated.
USAGE
}

die() {
  echo "Error: $*" >&2
  exit 1
}

validate_expected_sha_format() {
  local sha="$1"

  if [ -z "$sha" ]; then
    die "expected reviewed SHA is empty."
  fi
  if [ "${#sha}" -lt 7 ] || [ "${#sha}" -gt 40 ]; then
    die "expected reviewed SHA must be 7 to 40 hex characters: $sha"
  fi
  case "$sha" in
    *[!0-9a-fA-F]*)
      die "expected reviewed SHA must contain only hex characters: $sha"
      ;;
  esac
}

require_marker() {
  local file="$1"
  local marker="$2"
  local description="$3"

  if [ ! -f "$file" ]; then
    die "missing required release file: $file"
  fi
  if ! grep -Fq -- "$marker" "$file"; then
    die "missing release marker in $file: $description"
  fi
}

verify_release_markers() {
  require_marker "docs/release-v2.5.0.md" "# AutoSHUD v2.5.0 Release Preparation" "release preparation title"
  require_marker "docs/release-v2.5.0.md" "Release target: AutoSHUD \`v2.5.0\`." "v2.5.0 release target"
  require_marker "docs/release-v2.5.0.md" "--verify-tag" "GitHub Release tag verification"
  require_marker "docs/release-v2.5.0.md" "no v3.0.0 executable path" "v3.0.0 remains gated"
  require_marker "README.md" "AutoSHUD \`v2.5.0\` release candidate" "README v2.5.0 release target"
}

require_file() {
  local file="$1"
  local description="$2"

  if [ ! -f "$file" ]; then
    die "missing $description: $file"
  fi
}

verify_release_evidence_files() {
  local base
  local ext
  local sidecar

  require_file "Example/9035800.autoshud.txt" "example release config"
  require_file "Example/ljy.autoshud.txt" "example release config"

  for base in \
    Example/9035800/wbd \
    Example/9035800/stm_dem \
    Example/Lijiayan/Data/wbd_dem \
    Example/Lijiayan/Data/stm_dem
  do
    for ext in shp shx dbf prj
    do
      sidecar="$base.$ext"
      require_file "$sidecar" "required shapefile sidecar"
    done
  done
}

check_local_tag_absent() {
  if git rev-parse -q --verify "refs/tags/$TAG" >/dev/null; then
    die "local tag already exists: $TAG"
  fi
  echo "Local tag not found: $TAG"
}

check_remote_tag_absent() {
  local output
  local status

  if output="$(git ls-remote --exit-code --tags origin "refs/tags/$TAG" 2>&1)"; then
    die "remote tag already exists on origin: $TAG"
  else
    status=$?
    if [ "$status" -eq 2 ]; then
      echo "Remote tag not found on origin: $TAG"
      return 0
    fi
    echo "Error: unable to confirm remote tag absence on origin for $TAG (git ls-remote exit $status)." >&2
    if [ -n "$output" ]; then
      echo "$output" >&2
    fi
    exit 1
  fi
}

check_remote_tag_present() {
  local output
  local status

  if output="$(git ls-remote --exit-code --tags origin "refs/tags/$TAG" 2>&1)"; then
    echo "Remote tag confirmed on origin: $TAG"
    return 0
  else
    status=$?
    if [ "$status" -eq 2 ]; then
      die "remote tag is not visible on origin after push: $TAG"
    fi
    echo "Error: unable to confirm remote tag on origin for $TAG (git ls-remote exit $status)." >&2
    if [ -n "$output" ]; then
      echo "$output" >&2
    fi
    exit 1
  fi
}

check_github_release_absent() {
  local output
  local api_path
  local status

  if ! command -v gh >/dev/null 2>&1; then
    die "GitHub CLI 'gh' is required to confirm that the $TAG GitHub Release is absent."
  fi

  api_path="repos/$GH_REPO/releases/tags/$TAG"
  if output="$(gh api -i "$api_path" 2>&1)"; then
    die "GitHub Release already exists: $TAG"
  else
    status=$?
    if printf '%s\n' "$output" | grep -Eq '^HTTP/[0-9.]+ 404 '; then
      echo "GitHub Release not found: $TAG"
      return 0
    fi
    echo "Error: unable to confirm GitHub Release absence for $TAG (gh api exit $status)." >&2
    if [ -n "$output" ]; then
      echo "$output" >&2
    fi
    exit 1
  fi
}

verify_expected_sha_in_master() {
  local sha="$1"

  if [ -z "$sha" ]; then
    die "execute mode requires --expected-sha SHA or AUTOSHUD_EXPECTED_REVIEWED_SHA=SHA."
  fi
  validate_expected_sha_format "$sha"
  if ! git rev-parse --verify --quiet "${sha}^{commit}" >/dev/null; then
    die "expected reviewed SHA is not available after fetching: $sha"
  fi
  if ! git merge-base --is-ancestor "$sha" master; then
    die "expected reviewed SHA is not contained in updated master: $sha"
  fi
  echo "Reviewed SHA is contained in updated master: $(git rev-parse --short "$sha")"
}

verify_master_matches_origin() {
  local local_master
  local origin_master

  local_master="$(git rev-parse master)"
  origin_master="$(git rev-parse refs/remotes/origin/master)"
  if [ "$local_master" != "$origin_master" ]; then
    die "local master does not match origin/master after update; refusing to tag unpublished or stale local state."
  fi
  echo "Updated master matches origin/master: $(git rev-parse --short master)"
}

show_dry_run_expected_sha_status() {
  local sha="$1"

  if [ -z "$sha" ]; then
    echo "Reviewed SHA guard: not set; execute mode will require --expected-sha or AUTOSHUD_EXPECTED_REVIEWED_SHA."
    return 0
  fi

  validate_expected_sha_format "$sha"
  if ! git rev-parse --verify --quiet "${sha}^{commit}" >/dev/null; then
    echo "Reviewed SHA guard: $sha is not present locally yet; execute mode fetches before enforcing containment."
    return 0
  fi
  if git merge-base --is-ancestor "$sha" master; then
    echo "Reviewed SHA guard: $(git rev-parse --short "$sha") is contained in current local master."
  else
    echo "Reviewed SHA guard: $(git rev-parse --short "$sha") is not contained in current local master; execute mode updates master before enforcing containment."
  fi
}

while [ "$#" -gt 0 ]; do
  case "$1" in
    --maint)
      CREATE_MAINT=true
      shift
      ;;
    --execute)
      EXECUTE=true
      shift
      ;;
    --expected-sha)
      if [ "$#" -lt 2 ]; then
        die "--expected-sha requires a SHA value."
      fi
      EXPECTED_REVIEWED_SHA="$2"
      shift 2
      ;;
    --expected-sha=*)
      EXPECTED_REVIEWED_SHA="${1#--expected-sha=}"
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    v2.5.0)
      shift
      ;;
    v*)
      die "this helper only supports v2.5.0; refusing tag $1."
      ;;
    *)
      echo "Error: unknown argument: $1" >&2
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

echo "== Repository: AutoSHUD"
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
  show_dry_run_expected_sha_status "$EXPECTED_REVIEWED_SHA"
  verify_release_markers
  verify_release_evidence_files
  echo "Release marker and evidence checks passed."
  check_local_tag_absent
  check_remote_tag_absent
  check_github_release_absent
  echo ""
  echo "Dry run complete. After merge, run:"
  echo "  ./scripts/tag-v2-freeze.sh --execute --expected-sha <reviewed-sha>"
  echo "Then create the GitHub Release using docs/release-v2.5.0.md."
  exit 0
fi

echo ">> [1/8] Fetching remote updates..."
git fetch origin --tags

echo ">> [2/8] Checking out master and fast-forwarding to origin/master..."
git checkout master
git pull --ff-only origin master
verify_master_matches_origin

echo ">> [3/8] Checking that the working tree is clean..."
if [ -n "$(git status --porcelain)" ]; then
  echo "Error: uncommitted changes exist. Commit or stash them before tagging." >&2
  git status -s
  exit 1
fi

echo ">> [4/8] Verifying reviewed SHA containment and release markers..."
verify_expected_sha_in_master "$EXPECTED_REVIEWED_SHA"
verify_release_markers
verify_release_evidence_files
echo "Release marker and evidence checks passed."

echo ">> [5/8] Confirming tag and release names are unused..."
check_local_tag_absent
check_remote_tag_absent
check_github_release_absent

echo ">> [6/8] Creating annotated tag $TAG ..."
git tag -a "$TAG" -m "AutoSHUD $TAG"

echo ">> [7/8] Pushing tag to origin ..."
git push origin "$TAG"
check_remote_tag_present

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

echo ">> [8/8] Tag publication complete."
echo ""
echo "------------------------------------------------------------"
echo "Next steps:"
echo "  1. Verify the remote tag immediately before release creation:"
echo "     git ls-remote --exit-code --tags origin refs/tags/$TAG >/dev/null"
echo "  2. Create the verified-tag GitHub Release:"
echo "     gh release create $TAG \\"
echo "       --repo $GH_REPO \\"
echo "       --verify-tag \\"
echo "       --title \"AutoSHUD $TAG\""
echo "  3. Use the draft release notes in docs/release-v2.5.0.md"
echo ""
echo "Do not create a v3.0.0 release from this helper; v3.0.0 remains team-review gated."
echo "------------------------------------------------------------"
