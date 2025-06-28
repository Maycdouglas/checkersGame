module Posicao where

import Data.Char (toUpper) -- Importa função toUpper do módulo Data.Char para converter letra minúscula em letra maiúscula | Usado em lerPosicao
import Text.Read (readMaybe) -- Importa função readMaybe para tentar converter uma string para número de forma segura | usado no lerPosicao

-- Converte coluna de Char para índice Int (A=0, B=1, ..., H=7)
colunaParaIndice :: Char -> Maybe Int
colunaParaIndice coluna
    | coluna >= 'A' && coluna <= 'H' = Just (fromEnum coluna - fromEnum 'A')
    | otherwise = Nothing

-- Converte linha (1..8) para índice interno (0..7) invertendo o eixo vertical
linhaParaIndice :: Int -> Maybe Int
linhaParaIndice linha
    | linha >= 1 && linha <= 8 = Just (8 - linha)
    | otherwise = Nothing

-- Função para verificar se a posição está dentro do tabuleiro e se é uma casa preta
ehPosicaoValida :: (Int, Int) -> Bool
ehPosicaoValida (linha, coluna) =
    linha >= 0 && linha < 8 &&
    coluna >= 0 && coluna < 8 &&
    odd (linha + coluna) -- Somente casas pretas

-- Valida posição dada em formato de interface (linha, coluna) 
ehPosicaoValidaInterface :: (Int, Char) -> Bool
ehPosicaoValidaInterface (l, c) = case (linhaParaIndice l, colunaParaIndice c) of
    (Just li, Just ci) -> ehPosicaoValida (li, ci)
    _ -> False

-- Converte entrada "6 B" para posição (Int, Char), ou seja, (6, 'B')
-- Usa o readMaybe para não dar erro em tempo de execução no caso do usuário tentar uma entrada como XB, por exemplo
lerPosicao :: String -> Maybe (Int, Char)
lerPosicao [linha, coluna]
    | coluna >= 'a' && coluna <= 'h' = (,) <$> readMaybe [linha] <*> Just (toUpper coluna)
    | coluna >= 'A' && coluna <= 'H' = (,) <$> readMaybe [linha] <*> Just coluna
lerPosicao _ = Nothing