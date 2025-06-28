module Tabuleiro where

import Posicao

-- Define novos tipos de dados
data Peca = PecaJogador1 | PecaJogador2 | DamaJogador1 | DamaJogador2 | Semicapturada Peca
    deriving (Eq, Show)

data Casa = Vazia | Ocupada Peca
    deriving (Eq, Show)

-- Types para tornar o código mais legível
type Linha = [Casa]
type Tabuleiro = [Linha]

-- Função para checar se a peça que será capturada é uma adversaria ou nao
ehPecaAdversaria :: Peca -> Peca -> Bool
ehPecaAdversaria PecaJogador1  p = p == PecaJogador2 || p == DamaJogador2
ehPecaAdversaria DamaJogador1  p = p == PecaJogador2 || p == DamaJogador2
ehPecaAdversaria PecaJogador2  p = p == PecaJogador1 || p == DamaJogador1
ehPecaAdversaria DamaJogador2  p = p == PecaJogador1 || p == DamaJogador1

-- Função para verificar se é Dama
ehDama :: Peca -> Bool
ehDama DamaJogador1 = True
ehDama DamaJogador2 = True
ehDama _            = False

-- Função para promover para Dama
promoverParaDama :: (Int, Peca) -> Peca
promoverParaDama (0, PecaJogador1) = DamaJogador1
promoverParaDama (7, PecaJogador2) = DamaJogador2
promoverParaDama (_, peca)         = peca

-- Função que promove ou não a peça para Dama
avaliarPromocaoParaDama :: (Int, Char) -> Peca -> Peca
avaliarPromocaoParaDama destino peca =
    case linhaParaIndice (fst destino) of
        Just li -> promoverParaDama (li, peca)
        Nothing -> peca

-- Função que gera o tabuleiro inicial da partida
tabuleiroInicial :: Tabuleiro
tabuleiroInicial =
    [ linhaJogador2 i | i <- [0..2] ] ++ -- Posiciona as peças do jogador 2
    replicate 2 linhaVazia ++ -- Deixa as duas linhas do meio vazias
    [ linhaJogador1 i | i <- [0..2] ] -- Posiciona as peças do jogador 1
    where
        linhaJogador2 i = [if odd (i + j) then Ocupada PecaJogador2 else Vazia | j <- [0..7]]
        linhaJogador1 i = [if even (i + j) then Ocupada PecaJogador1 else Vazia | j <- [0..7]]
        linhaVazia = replicate 8 Vazia

-- Cores e plano de fundo usando código ANSI
corTexto :: String -> String -> String
corTexto cor s = cor ++ s ++ "\x1b[0m"

bgPreto :: String -> String
bgPreto s = "\x1b[40m" ++ s ++ "\x1b[0m"

bgBranco :: String -> String
bgBranco s = "\x1b[47m" ++ s ++ "\x1b[0m"

-- Função auxliar para mostrar a casa do tabuleiro
mostrarCasa :: Int -> (Int, Casa) -> String
mostrarCasa linhaIndex (colIndex, casa) =
    let bg = if even (linhaIndex + colIndex) then bgBranco else bgPreto -- Define a cor branca ou preta para a Casa
        texto = case casa of
            Vazia -> "   "
            Ocupada (Semicapturada DamaJogador1) -> corTexto "\x1b[37;1m" " D "
            Ocupada (Semicapturada DamaJogador2) -> corTexto "\x1b[37;1m" " D "
            Ocupada (Semicapturada _)            -> corTexto "\x1b[37;1m" " o " -- Branco
            Ocupada PecaJogador1   -> corTexto "\x1b[34;1m" " o " -- Azul
            Ocupada PecaJogador2   -> corTexto "\x1b[33;1m" " o " -- Amarelo
            Ocupada DamaJogador1   -> corTexto "\x1b[34;1m" " D "
            Ocupada DamaJogador2   -> corTexto "\x1b[33;1m" " D "
    in bg texto ++ "|" -- Define o separador lateral das casas

-- Função auxiliar para mostrar a linha do tabuleiro
mostrarLinha :: Int -> Linha -> String
mostrarLinha numeroLinha linha =
    " " ++ show numeroLinha ++ " |" ++ concatMap (mostrarCasa numeroLinha) (zip [0..] linha)
    -- Pega a função mostrarCasa com o primeiro argumento fixado (linhaIndex) e aplica ela a cada elemento da lista gerada por zip [0..] linha.

-- Função que exibe o tabuleiro no terminal
mostrarTabuleiro :: Tabuleiro -> IO ()
mostrarTabuleiro tab = do
    putStrLn "   +---+---+---+---+---+---+---+---+" -- Desenha a borda superior do tabuleiro
    mapM_ putStrLn $ zipWith mostrarLinha [8,7..1] tab -- Percorre as linhas do tabuleiro, as exibindo numerando-as
    putStrLn "   +---+---+---+---+---+---+---+---+" -- Desenha a borda inferior do tabuleiro
    putStrLn "     A   B   C   D   E   F   G   H" -- Escreve os nomes das colunas no canto inferior
