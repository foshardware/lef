
import Control.Monad

import Distribution.Simple

import System.IO
import System.Environment
import System.Process



main :: IO ()
main = do
  args <- getArgs
  when ("--enable-tests" `elem` args) $ pullSubmodules
  defaultMain



pullSubmodules :: IO ()
pullSubmodules = git ["submodule", "update", "--init", "--recursive"] $ \ h -> putStr =<< hGetContents h



git :: [String] -> (Handle -> IO ()) -> IO ()
git xs p = withCreateProcess (proc "git" xs) { std_out = CreatePipe } (\ _ (Just i) _ _ -> p i)

