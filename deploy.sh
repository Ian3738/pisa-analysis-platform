#!/usr/bin/env bash
# 把建置產物複製到 docs/（GitHub Pages 的來源目錄）
set -euo pipefail
cd "$(dirname "$0")"

echo "→ 產出 Pages 版網頁"
python3 R/build_site.py --target pages >/dev/null

echo "→ 複製到 docs/"
cp web/index-pages.html               docs/index.html
cp "output/PISA分析報告_APA7.docx"     docs/PISA-analysis-report-APA7.docx
touch docs/.nojekyll                   # 關閉 Jekyll，避免底線開頭的檔案被忽略

printf '   docs/index.html                      %s\n' "$(du -h docs/index.html | cut -f1)"
printf '   docs/PISA-analysis-report-APA7.docx  %s\n' "$(du -h docs/PISA-analysis-report-APA7.docx | cut -f1)"
echo "完成。commit 並 push 後 GitHub Pages 會自動更新。"
