import Text.Pandoc.JSON
import Text.Pandoc.Filter.AozoraRuby

main :: IO ()
main = toJSONFilter aozoraRubyFilter
