{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Filter.AozoraRuby where
import Text.Pandoc.Definition
import Data.String
import TextCompat
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

stringToInlines :: String -> [Inline]
stringToInlines [] = []
stringToInlines (' ' : xs) = Space : stringToInlines xs
stringToInlines (s : xs) = case span (/= ' ') xs of
                             (xs0, xs1) -> Str (fromString (s : xs0)) : stringToInlines xs1

transformAozoraRubyInString
  :: (String -> String -> Inline) -- ^ Function to build a ruby in the target format
  -> String                       -- ^ Input text
  -> [Inline]
transformAozoraRubyInString makeSimpleRuby = loop ""
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
      | otherwise = stringToInlines (reverse acc ++ s)

    -- Ruby notation without explicit starting mark
    -- e.g. "多項式《たこうしき》"
    loop acc@(a0:_) ('《':xs)
      | isKanji a0
      , (read,_:rest) <- break (== '》') xs
      , (revbase,acc') <- span isKanji acc
      = prependRev acc' $ makeSimpleRuby (reverse revbase) read : loop "" rest

    loop acc (x:xs) = loop (x:acc) xs

    prependRev :: String -> [Inline] -> [Inline]
    prependRev s xs = stringToInlines (reverse s) ++ xs

extractTextual :: [Inline] -> (String,[Inline])
extractTextual = loop ""
  where loop s0 (Str s : xs) = loop (s0 <> s) xs
        loop s0 (Space : xs) = loop (s0 <> " ") xs
        loop s0 xs = (toString s0, xs)

transformAozoraRubyInInlines
  :: (String -> String -> Inline) -- ^ Function to build a ruby in the target format
  -> [Inline]                     -- ^ Input
  -> [Inline]
transformAozoraRubyInInlines makeSimpleRuby = loop
  where
    loop [] = []
    loop xs = case extractTextual xs of
                (str@(_:_), rest) -> transformAozoraRubyInString makeSimpleRuby str ++ loop rest
                ("", x : xss) -> x : loop xss
                (_, _) -> [] -- should not reach here

-- | Build a LaTeX text for ruby text
makeSimpleRubyLaTeX :: String -> String -> Inline
makeSimpleRubyLaTeX base read = RawInline (Format "latex")
                                ("\\ruby{" <> fromString base <> "}{" <> fromString read <> "}")
                                -- Should escape special characters?

-- | Build a HTML text for ruby text
makeSimpleRubyHTML :: String -> String -> Inline
makeSimpleRubyHTML base read = RawInline (Format "html")
                               ("<ruby>" <> fromString (escapeStringForXML base) <> "<rp>《</rp><rt>" <> fromString (escapeStringForXML read) <> "</rt><rp>》</rp></ruby>")

-- | Convert text in ruby
aozoraRubyFilter :: Maybe Format -> [Inline] -> [Inline]
aozoraRubyFilter (Just (Format "latex")) = transformAozoraRubyInInlines makeSimpleRubyLaTeX
aozoraRubyFilter (Just (Format "html"))  = transformAozoraRubyInInlines makeSimpleRubyHTML
aozoraRubyFilter (Just (Format "html5")) = transformAozoraRubyInInlines makeSimpleRubyHTML
aozoraRubyFilter (Just (Format "epub"))  = transformAozoraRubyInInlines makeSimpleRubyHTML
aozoraRubyFilter (Just (Format "epub3")) = transformAozoraRubyInInlines makeSimpleRubyHTML
aozoraRubyFilter _ = id

escapeCharForXML :: Char -> String
escapeCharForXML c = case c of
  '<' -> "&lt;"
  '>' -> "&gt;"
  '"' -> "&quot;"
  '&' -> "&amp;"
  c   -> [c]

escapeStringForXML :: String -> String
escapeStringForXML = concatMap escapeCharForXML
