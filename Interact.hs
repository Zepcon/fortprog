module Interact where

import Parser



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
" :s <strat>  Sets the specified search strategy
where <strat> is one of 'dfs' or 'bfs'."

-- Der Benutzer soll ein Programm laden können, das solange geladen bleibt,
-- bis ein neues geladen wird.
-- :l <file> läd die spezifizierte Datei
-- parseFile ??
-- Meldung nach dem Laden:
  -- "Loaded."


-- Der Benutzer soll zwischen Anfragen jederzeit die Suchstrategie wechseln können.
-- :s <strat> setzt die Suchstrategie auf dfs oder bfs
-- Parse Prog ??
-- Meldung nach dem Setzen der Suchstrategie:
  -- "Strategy set to " ++ case <start> of
  --                            dfs -> "depth-first search."
  --                            bfs -> "breadth-first search."
  -- es folgt die Anfrage:

-- Es sollen Anfragen gestellt werden können, deren Lösungen nacheinander angefordert werden können.
  -- <goal> löst/beweist die Anfrage goal
  -- Parse Goal ??
-- mithilfe von ; kann der User weitere Lösungen anfordern
-- Meldung, wenn keine weitere Lösung vorhanden ist:
  -- "No more solutions."

-- Die interaktive Umgebung soll robust gegenüber Fehlern sein
-- und stets sinnvolle Rückmeldungen ausgeben.
