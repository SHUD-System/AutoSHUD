# AutoSHUD v2.5.0 Release Preparation

This document prepares the AutoSHUD side of the `v2.5.0` release candidate. It
is a post-merge checklist and release-note draft for maintainers. It must not be
used to publish a local tag, remote tag, or GitHub Release from an unmerged PR
branch.

## Scope

- Release target: AutoSHUD `v2.5.0`.
- Paired dependency target: rSHUD `v2.5.0`.
- Runtime expectations: R `>= 4.0`, `terra >= 1.7-0`, and `sf >= 1.0-0`.
- Retired spatial packages: the modern path does not require `rgeos` or
  `rgdal`.
- ERA5 forcing scope: `Forcing 0.7` writes classic SHUD local forcing CSV files
  and `meteoCov` shapefiles. It is not SHUD-NC direct NetCDF forcing.
- v3.0.0 status: formal `v3.0.0` release remains a future team-review-gated
  action.

No runtime R scripts, generated model outputs, or example geospatial data are
changed by this release-preparation document.

## Pre-Merge Evidence

Run these checks while preparing the PR. If an R package or system dependency is
missing, record the exact blocker instead of claiming the check passed.

```sh
Rscript tests/test-era5-forcing.R
```

```sh
test -f Example/9035800.autoshud.txt &&
test -f Example/ljy.autoshud.txt &&
for base in \
  Example/9035800/wbd \
  Example/9035800/stm_dem \
  Example/Lijiayan/Data/wbd_dem \
  Example/Lijiayan/Data/stm_dem
do
  for ext in shp shx dbf prj
  do
    test -f "$base.$ext"
  done
done
```

Before merge, also confirm that release publication has not already happened:

```sh
! git rev-parse -q --verify refs/tags/v2.5.0
! git ls-remote --exit-code --tags origin v2.5.0 >/dev/null 2>&1
! gh -R SHUD-System/AutoSHUD release view v2.5.0 >/dev/null 2>&1
```

The `gh` check requires the GitHub CLI and repository access. If `gh` is not
available, record that as an environment limitation and verify the GitHub
Releases page manually during review.

## Post-Merge Release Checklist

Run these steps only after the release-preparation PR has been reviewed and
merged into `master`.

1. Start from the merged `master` commit.

```sh
git fetch origin --tags
git checkout master
git pull --ff-only origin master
git status --short
```

`git status --short` must print no changes before tagging.

2. Repeat the release smoke checks from the merged commit.

```sh
Rscript tests/test-era5-forcing.R
```

```sh
test -f Example/9035800.autoshud.txt &&
test -f Example/ljy.autoshud.txt &&
for base in \
  Example/9035800/wbd \
  Example/9035800/stm_dem \
  Example/Lijiayan/Data/wbd_dem \
  Example/Lijiayan/Data/stm_dem
do
  for ext in shp shx dbf prj
  do
    test -f "$base.$ext"
  done
done
```

3. Confirm that `v2.5.0` does not already exist locally, remotely, or as a
   GitHub Release.

```sh
! git rev-parse -q --verify refs/tags/v2.5.0
! git ls-remote --exit-code --tags origin v2.5.0 >/dev/null 2>&1
! gh -R SHUD-System/AutoSHUD release view v2.5.0 >/dev/null 2>&1
```

4. Create and push the annotated tag from merged `master`.

```sh
git tag -a v2.5.0 -m "AutoSHUD v2.5.0"
git push origin v2.5.0
```

The helper script can be used instead after merge:

```sh
./scripts/tag-v2-freeze.sh --execute
```

5. Create the GitHub Release from the tag and the draft notes below.

```sh
gh release create v2.5.0 \
  --repo SHUD-System/AutoSHUD \
  --title "AutoSHUD v2.5.0"
```

Paste the release-note draft into the GitHub Release body, then publish.

## Draft GitHub Release Notes

### AutoSHUD v2.5.0

AutoSHUD `v2.5.0` is the release-candidate line prepared to align with rSHUD
`v2.5.0`.

Highlights:

- Documents the expected rSHUD `v2.5.0` pairing.
- Documents the modern spatial dependency baseline: R `>= 4.0`,
  `terra >= 1.7-0`, and `sf >= 1.0-0`.
- Confirms that the modern path does not require retired `rgeos` or `rgdal`.
- Keeps ERA5 `Forcing 0.7` scoped to classic SHUD local forcing CSV files plus
  `meteoCov` shapefiles; it is not SHUD-NC direct NetCDF forcing.
- Confirms that example `.autoshud.txt` templates and required shapefile
  sidecars are present for the documented examples.

Validation for this release:

- `Rscript tests/test-era5-forcing.R`
- Example config and shapefile sidecar presence check for `Example/9035800` and
  `Example/Lijiayan`.

Not included:

- No runtime behavior changes.
- No regenerated example geospatial data or model outputs.
- No formal `v3.0.0` release. The `v3.0.0` release remains blocked on future
  team review.
