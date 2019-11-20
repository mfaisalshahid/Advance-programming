import Data.List
import Data.List.Split
import Data.Char
import Control.Monad (join)
import Control.Monad (msum)
import Data.Monoid (mconcat)

--splitLine x
variable = ["He", "who", "controls" , "the" , "world"]
enHyp = [("creative", ["cr","ea","ti","ve"]), ("controls", ["co","nt","ro","ls"]), ("achieve", ["ach","ie","ve"]), ("future", ["fu","tu","re"]), ("present", ["pre","se","nt"]), ("motivated", ["mot","iv","at","ed"]), ("desire", ["de","si","re"]), ("others", ["ot","he","rs"])]

 
newFunc = lineBreaks enHyp 12 variable