module Tabuleiro where

import Posicao

import Control.Monad (guard) -- função guard da biblioteca Control.Monad
-- usado em condições, permitindo que o fluxo prossiga se o retorno for True e gerando Nothing se for False
-- usado em funções com muitas validações, como por exemplo a moverPeca e atualizarCasa

data Peca = PecaJogador1 | PecaJogador2 | DamaJogador1 | DamaJogador2
    deriving (Eq, Show)
-- usa o Eq para poder comparar se dois jogadores são iguais ou diferentes
-- usa o Show para poder converter o texto para String

data Casa = Vazia | Ocupada Peca
    deriving (Eq, Show)
-- usa o Eq para poder comparar se dois jogadores são iguais ou diferentes
-- usa o Show para poder converter o texto para String

type Linha = [Casa]
type Tabuleiro = [Linha]

-- Função que gera o tabuleiro inicial da partida
tabuleiroInicial :: Tabuleiro
tabuleiroInicial =
    [ linhaJogador2 i | i <- [0..2] ] ++ -- posiciona as peças do jogador 2 -- posiciona as peças do jogador 2 -- posiciona as peças do jogador 2 -- posiciona as peças do jogador 2
     -- posiciona as peças do jogador 2
     -- posiciona as peças do jogador 2
     -- posiciona as peças do jogador 2 -- posiciona as peças do jogador 2
     -- posiciona as peças do jogador 2
    replicate 2 linhaVazia ++ -- deixa as duas linhas do meio vazias -- deixa as duas linhas do meio vazias -- deixa as duas linhas do meio vazias -- deixa as duas linhas do meio vazias
    [ linhaJogador1 i | i <- [0..2] ] -- posiciona as peças do jogador 1
  where
    linhaJogador2 i = [if odd (i + j) then Ocupada PecaJogador2 else Vazia | j <- [0..7]]
    linhaJogador1 i = [if even (i + j) then Ocupada PecaJogador1 else Vazia | j <- [0..7]]
    linhaVazia = replicate 8 Vazia

-- Função que exibe o tabuleiro no terminal
mostrarTabuleiro :: Tabuleiro -> IO ()
mostrarTabuleiro tab = do
    putStrLn "   ╔═══╦═══╦═══╦═══╦═══╦═══╦═══╦═══╗" -- Desenha a borda superior do tabuleiro
    mapM_ putStrLn $ zipWith mostrarLinha [8,7..1] tab -- percorre as linhas do tabuleiro mostrando as linhas e colocando a numeracao de cada uma
    putStrLn "   ╚═══╩═══╩═══╩═══╩═══╩═══╩═══╩═══╝" -- Desenha a borda inferior do tabuleiro
    putStrLn "     A   B   C   D   E   F   G   H" -- Escreve os nomes das colunas no canto inferior

-- Função auxiliar para mostrar a linha do tabuleiro
mostrarLinha :: Int -> Linha -> String
mostrarLinha numeroLinha linha =
    " " ++ show numeroLinha ++ " ║" ++ concatMap (mostrarCasa numeroLinha) (zip [0..] linha) -- aqui tem aplicação parcial. ainda não entendi isso direito
    -- Pegue a função mostrarCasa com o primeiro argumento fixado (linhaIndex) e aplique ela a cada elemento da lista gerada por zip [0..] linha.

-- Função auxliar para mostrar a casa do tabuleiro
mostrarCasa :: Int -> (Int, Casa) -> String
mostrarCasa linhaIndex (colIndex, casa) =
    let bg = if even (linhaIndex + colIndex) then bgBranco else bgPreto
        texto = case casa of
                    Vazia -> "   "
                    Ocupada PecaJogador1   -> corTexto "\x1b[34;1m" " o "
                    Ocupada PecaJogador2   -> corTexto "\x1b[33;1m" " o "
                    Ocupada DamaJogador1   -> corTexto "\x1b[34;1m" " D "
                    Ocupada DamaJogador2   -> corTexto "\x1b[33;1m" " D "
    in bg texto ++ "║"

-- Cores e plano de fundo
corTexto :: String -> String -> String
corTexto cor s = cor ++ s ++ "\x1b[0m"

bgPreto :: String -> String
bgPreto s = "\x1b[40m" ++ s ++ "\x1b[0m"

bgBranco :: String -> String
bgBranco s = "\x1b[47m" ++ s ++ "\x1b[0m"
