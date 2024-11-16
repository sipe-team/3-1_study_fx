
import StringUtils
import qualified StringUtils as SU

main :: IO ()
main = do
    print (capitalize "hello")
    print (reverseString "hello")
    print (countWords "hello")

    print (SU.capitalize "hello")
    print (SU.reverseString "hello")
    print (SU.countWords "hello")