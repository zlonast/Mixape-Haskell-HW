import Test.Hspec (hspec)

import HW1.TestHW1 (testHW1)
import HW2.TestHW2 (testHW2)

main :: IO ()
main =
  do
    hspec testHW1
    hspec testHW2
