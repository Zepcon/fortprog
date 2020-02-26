module Interact where

import Parser
import Data.List
import Type
import Pretty
import SLD
import Subst



-- Die interaktive Umgebung soll solange aktiv sein, bis sie vom Benutzer beendet wird.
  -- Umgebung erstellen ??
-- Meldung nach dem Starten:
  -- "Welcome! \n"
  -- "Type ":h" for help."
-- :q beendet die Umgebung

-- Es soll eine Hilfe angezeigt werden können, die alle Funktionen
-- der interaktiven Umgebung auflistet.
help :: String
help = "Commands available from the prompt: \n" ++
       " <goal>      Solves/proves the specified goal. \n" ++
       " :h          Shows this help message. \n" ++
       " :l <file>   Loads the specified file. \n" ++
       " :q          Exits the interactive environment. \n" ++
       " :s <strat>  Sets the specified search strategy where <strat> is one of 'dfs' or 'bfs'."



main = do
        putStrLn("Welcome!  \nType \"h\" for help.")
        mainloop (Prog []) dfs


mainloop :: Prog -> Strategy -> IO ()
mainloop prog stra = do
                               putStr "?- "
                               input <- getLine
                               case (words input) of  -- input holen
                                [":h"] -> do
                                 putStrLn help
                                [":help"] -> do
                                 putStrLn help
                                 mainloop prog stra
                                [":quit"] -> do
                                 putStrLn "Bye"
                                 return()
                                [":q"] -> do
                                 putStrLn "Bye. "
                                 return()
                                [":s","bfs"] -> do
                                 putStrLn "Strategy set to breadth-first search."
                                 mainloop prog bfs
                                [":set","bfs"] -> do
                                 putStrLn "Strategy set to breadth-first search."
                                 mainloop prog bfs
                                [":s","dfs"] -> do
                                 putStrLn "Strategy set to depth-first search."
                                 mainloop prog dfs
                                [":set","dfs"] -> do
                                 putStrLn "Strategy set to depth-first search."
                                 mainloop prog dfs
                                (":l":[filename]) -> do
                                  x <- parseFile filename
                                  case x of
                                   (Left err) -> do
                                     putStrLn err
                                     mainloop prog stra
                                   (Right suc) -> do
                                     putStrLn "Loaded. "
                                     mainloop suc stra
                                _ -> do
                                  let y = parse input :: Either String Goal
                                  case y of
                                   (Left err) -> do
                                    putStrLn "Wrong input!"
                                    mainloop prog stra
                                   (Right suc) -> do
                                    let erg = (solve stra prog suc)
                                    if ((length erg) /= 0) then solvePrint(erg) else putStrLn("No solutions.")
                                    mainloop prog stra



-- Substitutionen nur auf Semikolon Eingabe printen
solvePrint :: [Subst] -> IO ()
solvePrint [] = return () -- Fertig geprintet
solvePrint (x:xs) = do
                     putStr(pretty x)
                     input <- getLine
                     if (input == ";") then do
                                             solvePrint(xs)
                                        else return ()  -- Nutzer hat keinen Bock mehr auf weitere Ergebnisse
