# pandoc-aozora-ruby

青空文庫風のルビを使えるようにするPandocフィルターです。

## インストール

```sh
$ git clone https://github.com/minoki/pandoc-aozora-ruby.git
$ cd pandoc-aozora-ruby
$ stack install
```

```sh
$ pandoc --filter pandoc-aozora-ruby-filter -o foo.html foo.md
```

## 入力フォーマット

ルビ対象の文字列（親文字、ベーステキスト）の前に開始記号として全角縦棒｜を置き、その後に二重山括弧《》で括った読み仮名（ルビテキスト）を書きます。
ルビ対象が漢字のみからなる場合は、全角縦棒は省略できます。

今のところ、開始記号が省略できるのはルビ対象が漢字のみの場合であり、ルビ対象が「ひらがなのみ」「カタカナのみ」「アルファベットのみ」の場合の開始記号の省略には対応していません。

例：

```markdown
｜単語《よみがな》

とあるアーベル圏の図式追跡《ダイアグラムチェース》

最小｜多項式《たこうしき》

ホム集合《セット》

```

漢字として扱われる文字は、

- U+4E00-U+9FFF (CJK Unified Ideographs)
    - 「仝」 U+4EDD を含む
- U+F900-U+FAFF (CJK Compatibility Ideographs)
- U+3400-U+4DBF (CJK Unified Ideographs Extension A)
- U+20000-U+2FA1F (CJK Unified Ideogarphs Extension B - F, Supplement)
- 「々」 U+3005 Ideographic Iteration Mark
- 「〆」 U+3006 Ideographic Closing Mark
- 「〇」 U+3007 Ideographic Number Zero
- 「〻」 U+303B Vertical Ideographic Iteration Mark
- 「ヶ」 U+30F6 Katakana Letter Small KE

です。

（TODO: 異体字選択符号？）

## 出力フォーマット

HTML系 (`html`, `html5`, `epub`, `epub3`) とLaTeX (`latex`) に対応しています。

```html
<ruby>単語<rp>《</rp><rt>よみがな</rt><rp>》</rp></ruby>
```

```latex
\ruby{単語}{よみがな}
```

LaTeXの場合は、`\ruby`コマンドを提供するLaTeXパッケージを別途読み込んでください。
例：`okumacro`, `pxrubrica`, `luatexja-ruby`
