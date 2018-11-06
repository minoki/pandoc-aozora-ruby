{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit
import Text.Pandoc (Format(..),runPure)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Writers.HTML (writeHtml5String)
import Text.Pandoc.Writers.LaTeX (writeLaTeX)
import Text.Pandoc.Walk (walk)
import Text.Pandoc.Filter.AozoraRuby
import Data.Default (def)
import Data.Text (Text)

mdToHtml :: Text -> Text
mdToHtml text = case runPure m of
                  Left e -> error (show e)
                  Right x -> x
  where m = do ast <- readMarkdown def text
               let transformed = walk (aozoraRubyFilter (Just (Format "html5"))) ast
               writeHtml5String def transformed

mdToLaTeX :: Text -> Text
mdToLaTeX text = case runPure m of
                   Left e -> error (show e)
                   Right x -> x
  where m = do ast <- readMarkdown def text
               let transformed = walk (aozoraRubyFilter (Just (Format "latex"))) ast
               writeLaTeX def transformed

test1 = TestCase $ assertEqual "simple" "<p><ruby>最小多項式<rp>《</rp><rt>さいしょうたこうしき</rt><rp>》</rp></ruby></p>" $ mdToHtml "最小多項式《さいしょうたこうしき》"
test2 = TestCase $ assertEqual "simple" "\\ruby{最小多項式}{さいしょうたこうしき}" $ mdToLaTeX "最小多項式《さいしょうたこうしき》"
test3 = TestCase $ assertEqual "simple" "\\ruby{最小 多項式}{さいしょう たこうしき}" $ mdToLaTeX "｜最小 多項式《さいしょう たこうしき》"

tests = TestList [TestLabel "Test 1" test1
                 ,TestLabel "Test 2" test2
                 ,TestLabel "Test 3" test3
                 ]

main = runTestTT tests
