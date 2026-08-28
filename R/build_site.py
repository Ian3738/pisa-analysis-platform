#!/usr/bin/env python3
"""把 R 產出的 SVG、估計結果與 Word 報告組進網站樣板。

兩個目標：
  artifact  claude.ai Artifact。沙箱會擋掉頁面自行觸發的下載，故改用 downloads
            執行期能力，Word 報告以 base64 內嵌（頁面約 8 MB）。
  pages     GitHub Pages。一般網站，用 <a download> 直接連到並存的 .docx 即可，
            不需內嵌（頁面約 3 MB）。

用法：
  python3 build_site.py                      # artifact（預設）
  python3 build_site.py --target pages       # GitHub Pages
"""
import argparse, json, re, pathlib, sys

ROOT = pathlib.Path.home() / "PISA" / "web"
FIGS = ROOT / "figs"

FONT_STACK = '"Noto Sans TC", "PingFang TC", "Microsoft JhengHei", "Heiti TC", sans-serif'

DOCX_ASCII = "PISA-analysis-report-APA7.docx"   # 網址用 ASCII 檔名，避免編碼問題
DOCX_ZH    = "PISA分析報告_APA7.docx"           # download 屬性指定另存時的中文檔名


def load_svg(name, mode):
    p = FIGS / f"{name}_{mode}.svg"
    if not p.exists():
        raise SystemExit(f"缺少圖檔：{p}")
    s = p.read_text()
    s = re.sub(r"<\?xml[^>]*\?>\s*", "", s)          # 內嵌 HTML 不能有 XML 宣告
    # 移除固定寬高，改由 CSS 控制；保留 viewBox 維持比例
    s = re.sub(r"(<svg\b[^>]*?)\s+width='[^']*'", r"\1", s, count=1)
    s = re.sub(r"(<svg\b[^>]*?)\s+height='[^']*'", r"\1", s, count=1)
    # 每張圖的樣式區塊加上唯一前綴，避免多張 SVG 的 CSS 互相覆蓋
    uid = f"sv-{name}-{mode}"
    s = s.replace("class='svglite'", f"class='svglite {uid}'", 1)
    s = re.sub(r"\.svglite\b", f".{uid}", s)
    s = re.sub(r"(<svg\b)", r"\1 preserveAspectRatio='xMidYMid meet' role='img'", s, count=1)
    # svglite 會把本機解析到的實體字型家族名寫死（macOS 上是 PingFang HK），
    # 在 Windows／Linux 上會 fallback，也用不到頁面已載入的 Noto Sans TC。
    # 改寫成完整字型堆疊，讓各平台都有合理結果。
    s = re.sub(r'font-family:\s*"[^"]*"\s*;', f"font-family: {FONT_STACK};", s)
    return s.strip()


def fig_block(name):
    return (f"<div class='fig-l'>{load_svg(name,'light')}</div>"
            f"<div class='fig-d'>{load_svg(name,'dark')}</div>")


def figset_block(prefix):
    names = sorted({p.name.rsplit("_", 1)[0] for p in FIGS.glob(f"{prefix}_*.svg")
                    if not p.name.startswith("desc_")})
    if not names:
        raise SystemExit(f"FIGSET 找不到任何圖：{prefix}")
    out = []
    for i, n in enumerate(names):
        cls = "figitem on" if i == 0 else "figitem"
        out.append(f"<div class='{cls}' data-fig='{n}'>{fig_block(n)}</div>")
    return "".join(out), names


# ---------------------------------------------------------------- 下載區塊
def dl_markup_artifact(n):
    pad = ' style="margin-top:18px"' if n == 2 else ""
    return (f'<div class="dl"{pad}>'
            f'<button class="dl-btn" id="dlBtn{n}" type="button">'
            f'<span class="ic">↓</span>下載完整報告（Word）</button>'
            f'<span class="dl-meta" id="dlMeta{n}"></span></div>'
            f'<div class="dl-msg" id="dlMsg{n}" role="status" aria-live="polite"></div>')


def dl_markup_pages(n, size_mb):
    pad = ' style="margin-top:18px"' if n == 2 else ""
    return (f'<div class="dl"{pad}>'
            f'<a class="dl-btn" href="{DOCX_ASCII}" download="{DOCX_ZH}">'
            f'<span class="ic">↓</span>下載完整報告（Word）</a>'
            f'<span class="dl-meta">{size_mb:.1f} MB · 24 圖 · 5 表 · APA 7</span></div>')


DL_JS_ARTIFACT = r"""
/* ---------- Word 報告下載（Artifact：透過 downloads 執行期能力）---------- */
const DOCX_B64 = "/*__DOCX_B64__*/";
const DOCX_NAME = "PISA分析報告_APA7.docx";

(function initDownload() {
  const pairs = [1, 2]
    .map(n => ({ btn: $('#dlBtn' + n), meta: $('#dlMeta' + n), msg: $('#dlMsg' + n) }))
    .filter(x => x.btn);
  if (!pairs.length) return;

  const bytes = Math.floor(DOCX_B64.length * 3 / 4);
  const sizeText = (bytes / (1024 * 1024)).toFixed(1) + ' MB · 24 圖 · 5 表 · APA 7';
  pairs.forEach(p => { if (p.meta) p.meta.textContent = sizeText; });

  const say = (p, text, cls) => {
    if (!p.msg) return;
    p.msg.textContent = text;
    p.msg.className = 'dl-msg' + (cls ? ' ' + cls : '');
  };

  function decode() {
    const bin = atob(DOCX_B64);
    const out = new Uint8Array(bin.length);
    for (let i = 0; i < bin.length; i++) out[i] = bin.charCodeAt(i);
    return out;
  }

  const ERRORS = {
    extension_not_enabled: '這個檢視環境未開放 Word 檔下載。請改用另外提供的檔案。',
    declined: '已取消下載。',
    rate_limited: '請稍候幾秒再試一次。',
    too_large: '檔案超過大小上限。',
    bad_request: '下載請求有誤，請重新整理頁面後再試。'
  };

  let busy = false;
  async function run(p) {
    if (busy) return;
    busy = true;
    pairs.forEach(q => { q.btn.disabled = true; });
    say(p, '準備檔案……');
    try {
      const downloads = await claude.use('downloads');
      if (!downloads) {
        say(p, '這個檢視環境不支援檔案下載。請改用另外提供的檔案。', 'err');
        return;
      }
      await downloads.save({ filename: DOCX_NAME, data: decode() });
      say(p, '已儲存 ' + DOCX_NAME, 'ok');
      pairs.forEach(q => { q.btn.disabled = false; });
    } catch (e) {
      const code = e && e.code;
      say(p, ERRORS[code] || '下載未完成，請再試一次。', code === 'declined' ? '' : 'err');
      pairs.forEach(q => { q.btn.disabled = false; });
    } finally {
      busy = false;
    }
  }

  pairs.forEach(p => p.btn.addEventListener('click', () => run(p)));

  // 能力不可用時直接停用按鈕，不要讓讀者按了才失望
  claude.use('downloads').then(d => {
    if (!d) pairs.forEach(p => {
      p.btn.disabled = true;
      say(p, '這個檢視環境不支援檔案下載。報告檔已另外提供。');
    });
  }).catch(() => {});
})();"""

DL_JS_PAGES = """
/* GitHub Pages 版本以一般超連結提供報告，此處不需 JavaScript。 */"""


# ---------------------------------------------------------------- 組裝
def build(target):
    html = (ROOT / "template.html").read_text()

    for m in re.findall(r"<!--FIGSET:([a-z0-9_]+)-->", html):
        block, names = figset_block(m)
        html = html.replace(f"<!--FIGSET:{m}-->", block)
        print(f"  FIGSET {m:10s} → {len(names)} 張")

    for m in re.findall(r"<!--FIG:([a-z_0-9]+)-->", html):
        html = html.replace(f"<!--FIG:{m}-->", fig_block(m))
        print(f"  FIG    {m}")

    if target == "artifact":
        b64p = ROOT / "report_b64.txt"
        if not b64p.exists():
            raise SystemExit(f"缺少 {b64p}；請先執行 10_report_docx.R 並 base64 編碼")
        b64 = "".join(b64p.read_text().split())   # base64 只含 A-Za-z0-9+/=，可安全放進 JS 字串
        for n in (1, 2):
            html = html.replace(f"<!--DLBLOCK:{n}-->", dl_markup_artifact(n))
        html = html.replace("/*__DL_JS__*/", DL_JS_ARTIFACT)
        html = html.replace("/*__DOCX_B64__*/", b64)
        print(f"  DOCX   內嵌 {len(b64)/2**20:.2f} MB base64")
        out = ROOT / "pisa-platform.html"
    else:
        docx = ROOT.parent / "output" / DOCX_ZH
        if not docx.exists():
            raise SystemExit(f"缺少 {docx}；請先執行 10_report_docx.R")
        size_mb = docx.stat().st_size / 2**20
        for n in (1, 2):
            html = html.replace(f"<!--DLBLOCK:{n}-->", dl_markup_pages(n, size_mb))
        html = html.replace("/*__DL_JS__*/", DL_JS_PAGES)
        print(f"  DOCX   以連結提供（{size_mb:.2f} MB，須與網頁並存）")
        out = ROOT / "index-pages.html"

    html = re.sub(r"/\*__PISA_DATA__\*/.*?/\*__END__\*/",
                  lambda _: (ROOT / "pisa_data.json").read_text(), html, flags=re.S)
    html = re.sub(r"/\*__ZH_DATA__\*/.*?/\*__END__\*/",
                  lambda _: (ROOT / "country_zh.json").read_text(), html, flags=re.S)
    html = re.sub(r"/\*__SCHOOL_DATA__\*/.*?/\*__END__\*/",
                  lambda _: (ROOT / "school_data.json").read_text(), html, flags=re.S)

    left = re.findall(r"<!--(?:FIG|FIGSET|DLBLOCK):[^>]+-->", html)
    left += re.findall(r"/\*__[A-Z_]+__\*/", html)
    if left:
        raise SystemExit(f"仍有未填的佔位符：{left}")

    out.write_text(html)
    print(f"\n寫出 {out}  {len(html)/2**20:.2f} MB")
    return out


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("--target", choices=["artifact", "pages"], default="artifact")
    build(ap.parse_args().target)
