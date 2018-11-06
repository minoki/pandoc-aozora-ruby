module Text.Pandoc.Filter.AozoraRuby where
import Text.Pandoc.Definition
-- import Text.Pandoc.XML (escapeStringForXML)
-- depending on the entire 'pandoc' package just for a single function is overkill...

isKanji :: Char -> Bool
isKanji c = (0x4E00 <= i && i <= 0x9FFF)      -- CJK Unified Ideographs
            || (0xF900 <= i && i <= 0xFAFF)   -- CJK Compatibility Ideographs
            || (0x3400 <= i && i <= 0x4DBF)   -- CJK Unified Ideographs Extension A
            || (0x20000 <= i && i <= 0x2FA1F) -- CJK Unified Ideographs Extension B - F, Supplement
            || i == 0x3005 -- '々' Ideographic Iteration Mark
            || i == 0x3006 -- '〆' Ideographic Closing Mark
            || i == 0x3007 -- '〇' Ideographic Number Zero
            || i == 0x303B -- '〻' Vertical Ideographic Iteration Mark
            || i == 0x30F6 -- 'ヶ' Katakana Letter Small KE
  where i = fromEnum c

transformAozoraRubyInString
  :: (String -> String -> Inline) -- ^ Function to build a ruby in the target format
  -> String                       -- ^ Input text
  -> [Inline]
transformAozoraRubyInString makeSimpleRuby s = loop "" s
  where
    loop :: String -> String -> [Inline]
    loop acc [] = prependRev acc []

    -- Ruby notation with explicit starting mark ('｜')
    -- e.g. "最小｜多項式《たこうしき》"
    loop acc s@('｜':xs)
      | (base,_:xs') <- break (== '《') xs
      , (read,_:rest) <- break (== '》') xs'
      = prependRev acc $ makeSimpleRuby base read : loop "" rest

      -- Since there is no matching '《 》', no need to worry for ruby text
      | otherwise = [Str (reverse acc ++ s)]

    -- Ruby notation without explicit starting mark
    -- e.g. "多項式《たこうしき》"
    loop acc@(a0:_) ('《':xs)
      | isKanji a0
      , (read,_:rest) <- break (== '》') xs
      , (revbase,acc') <- span isKanji acc
      = prependRev acc' $ makeSimpleRuby (reverse revbase) read : loop "" rest

    loop acc (x:xs) = loop (x:acc) xs

    prependRev :: String -> [Inline] -> [Inline]
    prependRev "" xs = xs
    prependRev s xs = Str (reverse s) : xs

-- | Build a LaTeX text for ruby text
makeSimpleRubyLaTeX :: String -> String -> Inline
makeSimpleRubyLaTeX base read = RawInline (Format "latex")
                                ("\\ruby{" ++ base ++ "}{" ++ read ++ "}")

-- | Build a HTML text for ruby text
makeSimpleRubyHTML :: String -> String -> Inline
makeSimpleRubyHTML base read = RawInline (Format "html")
                               ("<ruby>" ++ escapeStringForXML base ++ "<rp>《</rp><rt>" ++ escapeStringForXML read ++ "</rt><rp>》</rp></ruby>")

-- | Convert text in ruby
aozoraRubyFilter :: Maybe Format -> Inline -> [Inline]
aozoraRubyFilter (Just (Format "latex")) (Str s) = transformAozoraRubyInString makeSimpleRubyLaTeX s
aozoraRubyFilter (Just (Format "html"))  (Str s) = transformAozoraRubyInString makeSimpleRubyHTML s
aozoraRubyFilter (Just (Format "html5")) (Str s) = transformAozoraRubyInString makeSimpleRubyHTML s
aozoraRubyFilter (Just (Format "epub"))  (Str s) = transformAozoraRubyInString makeSimpleRubyHTML s
aozoraRubyFilter (Just (Format "epub3")) (Str s) = transformAozoraRubyInString makeSimpleRubyHTML s
aozoraRubyFilter _ x = [x]

escapeCharForXML :: Char -> String
escapeCharForXML c = case c of
  '<' -> "&lt;"
  '>' -> "&gt;"
  '"' -> "&quot;"
  '&' -> "&amp;"
  c   -> [c]

escapeStringForXML :: String -> String
escapeStringForXML = concatMap escapeCharForXML
