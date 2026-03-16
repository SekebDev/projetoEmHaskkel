module Logs where
import Data.Time


data Evento = CadastroUsuario | CadastroItem | Emprestimo | Devolucao 
    deriving (Show)

data Resultado = Sucesso | Erro String
    deriving (Show)

data Log = Log {
    evento :: Evento,
    texto :: String,
    resultado :: Resultado
} deriving (Show)

-- cria um log a partir de um evento e um texto descritivo
criarLog :: Evento -> String -> Resultado -> Log
criarLog evento texto resultado = Log { evento, texto, resultado }

-- formata o log para ser escrito no arquivo de logs, incluindo a data e hora do evento
formatarLog :: Log -> IO String
formatarLog log = do
    tempo <- getCurrentTime 

    let tempoFormatado = "[" ++ formatTime defaultTimeLocale "%Y-%m-%d %H:%M" tempo ++ "]" 
    let logFinal = case resultado log of
            Sucesso -> tempoFormatado ++ " " ++ show (evento log) ++ ": " ++ texto log ++ " (Sucesso)\n"
            Erro msg -> tempoFormatado ++ " " ++ show (evento log) ++ ": " ++ texto log ++ " (Erro - " ++ msg ++ ")\n"
    
    return logFinal

-- escreve o log no arquivo de logs
arquivarLog :: Log -> IO ()
arquivarLog log = do
    logFormatado <- formatarLog log
    appendFile "logs.txt" logFormatado
    return ()

-- imprime os logs no terminal
imprimirLogs :: IO ()
imprimirLogs = do
    logs <- readFile "logs.txt"
    putStrLn logs