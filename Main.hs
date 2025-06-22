module Main where

import Tabuleiro
import Data.Char (toUpper)
import Control.Monad (when)

-- Tipo para identificar o jogador atual
data Jogador = Jogador1 | Jogador2
    deriving (Eq, Show)

-- Converte entrada "6 B" para posição (Int, Char)
lerPosicao :: String -> Maybe (Int, Char)
lerPosicao [l,c] 
    | c >= 'a' && c <= 'h' = Just (read [l], toUpper c)
    | c >= 'A' && c <= 'H' = Just (read [l], c)
lerPosicao _ = Nothing

-- Loop principal do jogo
loopJogo :: Tabuleiro -> Jogador -> IO ()
loopJogo tab jogadorAtual = do
    putStrLn $ "\nTurno do " ++ show jogadorAtual
    mostrarTabuleiro tab
    putStrLn "Digite posição origem (ex: 6B): "
    origemStr <- getLine
    putStrLn "Digite posição destino (ex: 5A): "
    destinoStr <- getLine

    case (lerPosicao origemStr, lerPosicao destinoStr) of
        (Just origem, Just destino) -> 
            case moverPeca tab origem destino of
                Just tabNovo -> loopJogo tabNovo (trocarJogador jogadorAtual)
                Nothing -> do
                    putStrLn "Movimento inválido! Tente novamente."
                    loopJogo tab jogadorAtual
        _ -> do
            putStrLn "Entrada inválida! Tente novamente."
            loopJogo tab jogadorAtual

trocarJogador :: Jogador -> Jogador
trocarJogador Jogador1 = Jogador2
trocarJogador Jogador2 = Jogador1

-- Menu inicial do jogo
main:: IO()
main = do
    putStrLn "====================="
    putStrLn "    JOGO DE DAMAS    "
    putStrLn "====================="
    putStrLn "Escoha o modo de jogo:"
    putStrLn "1 - Jogador vs Máquina"
    putStrLn "2 - Máquina vs Máquina"
    putStrLn "3 - Sair"
    putStrLn "Opção: "
    option <- getLine
    case option of
        "1" -> escolherInicio True
        "2" -> escolherInicio False
        "3" -> putStrLn "Saindo do jogo..."
        _   -> do 
            putStrLn "Opção inválida, tente novamente."
            main

-- Função para escolher quem começa o jogo
escolherInicio :: Bool -> IO ()
escolherInicio jogadorVsMaquina = do
    putStrLn "Quem começa o Jogo?"
    putStrLn "1 - Jogador (ou Máquina 1)"
    putStrLn "2 - Máquina (ou máquina 2)"
    putStr "Opção: "
    inicio <- getLine
    case inicio of
        "1" -> iniciarJogo jogadorVsMaquina True
        "2" -> iniciarJogo jogadorVsMaquina False
        _   -> do
            putStrLn "Opção inválida, tente novamente."
            escolherInicio jogadorVsMaquina

-- Função que inicializa o jogo
iniciarJogo :: Bool -> Bool -> IO ()
iniciarJogo jogadorVsMaquina jogadorComeca = do
    putStrLn $ "\nModo de jogo: " ++
        if jogadorVsMaquina then "Jogador vs Máquina" else "Máquina vs Máquina"
    putStrLn $ "Quem começa: " ++ 
        if jogadorComeca then "Jogador (ou Máquina 1)" else "Máquina (ou Máquina 2)"
    loopJogo tabuleiroInicial (if jogadorComeca then Jogador1 else Jogador2)
