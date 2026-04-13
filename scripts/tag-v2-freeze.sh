#!/usr/bin/env bash
# AutoSHUD：把当前 master 冻结为 V2.x 稳定标签（AutoSHUD-V2.x），master 切换为 V3 开发线；可选创建 V2 维护分支。
#
# 用法（在仓库根目录执行；任意子目录也可，脚本会 cd 到仓库根）：
#   ./scripts/tag-v2-freeze.sh
#   ./scripts/tag-v2-freeze.sh v2.1.0
#   ./scripts/tag-v2-freeze.sh --maint
#   ./scripts/tag-v2-freeze.sh v2.1.0 --maint
#   ./scripts/tag-v2-freeze.sh --help
#
# 不会替你创建 GitHub Release，脚本结束时会打印网页上要点哪几步。

set -euo pipefail

TAG="v2.0.0"
CREATE_MAINT=false
for arg in "$@"; do
  case "$arg" in
    --maint) CREATE_MAINT=true ;;
    -h|--help)
      echo "用法: $0 [标签名，默认 v2.0.0 如 v2.1.0] [--maint]"
      echo "示例: $0"
      echo "       $0 v2.1.0"
      echo "       $0 --maint"
      echo "       $0 v2.1.0 --maint"
      exit 0
      ;;
    v*) TAG="$arg" ;;
  esac
done

ROOT="$(git rev-parse --show-toplevel 2>/dev/null)" || {
  echo "错误：请在 AutoSHUD 仓库内执行本脚本（或子目录亦可）。"
  exit 1
}
cd "$ROOT"

echo "== 仓库根目录: $ROOT"
echo "== 将使用的标签: $TAG"
echo ""

if ! git rev-parse --verify master >/dev/null 2>&1; thecd /Volumes/CloudDisk/CloudDrive/Development/AutoSHUD
git statusn
  echo "错误：当前仓库没有本地分支 master。"
  exit 1
fi

echo ">> [1/6] 获取远程更新..."
git fetch origin

echo ">> [2/6] 切换到 master 并与 origin/master 对齐..."
git checkout master
git pull --ff-only origin master

echo ">> [3/6] 检查工作区是否干净（有未提交改动则停止，避免标签钉在错误状态）..."
if ! git diff-index --quiet HEAD --; then
  echo "错误：存在未提交的修改。请先 git commit 或 git stash，再重新运行本脚本。"
  git status -s
  exit 1
fi

if git rev-parse "$TAG" >/dev/null 2>&1; then
  echo "警告：标签 $TAG 已存在。"
  read -r -p "是否仍要推送到远程？(y/N) " ans
  if [[ "${ans:-}" != "y" && "${ans:-}" != "Y" ]]; then
    echo "已取消。"
    exit 0
  fi
else
  echo ">> [4/6] 创建附注标签 $TAG ..."
  git tag -a "$TAG" -m "AutoSHUD-V2.x ${TAG} (V2 稳定版冻结，master 进入 V3.x 开发)"
fi

echo ">> [5/6] 推送标签到 origin ..."
git push origin "$TAG"

if $CREATE_MAINT; then
  MAINT_BRANCH="maint/v2.x"
  if git rev-parse --verify "$MAINT_BRANCH" >/dev/null 2>&1; then
    echo ">> 本地已存在分支 $MAINT_BRANCH，跳过创建。"
  else
    echo ">> [可选] 从 $TAG 创建维护分支 $MAINT_BRANCH ..."
    git branch "$MAINT_BRANCH" "$TAG"
    git push -u origin "$MAINT_BRANCH"
  fi
fi

echo ">> [6/6] 完成。"
echo ""
echo "------------------------------------------------------------"
echo "接下来请在浏览器完成（本脚本无法代替网页操作）："
echo "  1. 打开: https://github.com/SHUD-System/AutoSHUD/releases/new"
echo "  2. Choose a tag → 选择 $TAG"
echo "  3. Release title 例如: AutoSHUD ${TAG}"
echo "  4. 说明中写清: 这是一个 AutoSHUD-V2.x 稳定发布。master 分支现在进入 V3.x 开发。"
echo "  5. Publish release"
echo ""
echo "建议在 README 顶部增加版本说明：V2 稳定版请使用 GitHub Release 或 --branch $TAG 下载；默认 clone master 为 V3.x 开发版。"
echo "验证 V2: git clone --branch $TAG https://github.com/SHUD-System/AutoSHUD.git"
echo "验证 V3: git clone https://github.com/SHUD-System/AutoSHUD.git  # 默认 master = V3.x"
echo "------------------------------------------------------------"
