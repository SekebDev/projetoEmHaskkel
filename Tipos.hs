module Tipos(Usuario(..), Item(..), DB(..), TipoMidia(..), Emprestimo(..), StatusEmprestimo(..), FilaEspera(..)) where

-- tipo para os Usuários
data Usuario = Usuario {
    nome      :: String,
    matricula :: String,
    email     :: String
} deriving (Show, Eq)


-- tipos de mídia disponíveis
data TipoMidia = Livro | Filme | Jogo deriving (Show, Eq)

-- tipo para Livros, Filmes e Jogos
data Item = Item {
    titulo      :: String,
    autor       :: String,
    ano         :: Int,
    codigoUnico :: String,
    tipoMidia   :: TipoMidia
} deriving (Show, Eq)


-- status do empréstimo
data StatusEmprestimo = Emprestado | Devolvido | Atrasado
    deriving (Show, Eq)

-- tipo para os Empréstimos
data Emprestimo = Emprestimo {
    codItemEmprestimo          :: String,
    matriculaUsuarioEmprestimo :: String,
    dataRetirada               :: String,
    dataDevolucao              :: String,
    status                     :: StatusEmprestimo
} deriving (Show, Eq)

-- tipo para gerenciar a fila de espera de um item
data FilaEspera = FilaEspera {
    codItemFila  :: String,
    usuariosFila :: [String] -- matrículas dos usuários na fila
} deriving (Show, Eq)

-- tipo para o Banco de Dados
data DB = DB {
    usuarios :: [Usuario],
    itens    :: [Item],
    emprestimos :: [Emprestimo],
    esperas     :: [FilaEspera]
} deriving (Show)