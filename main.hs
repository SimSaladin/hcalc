
import Hcalc
import System.Environment

main :: IO ()
main = do
    (fileName:_) <- getArgs
    sheet' <- fromCSV fileName
    case sheet' of
        Left _ -> error "no parse"
        Right sh -> putStrLn $ evalSheet sh
