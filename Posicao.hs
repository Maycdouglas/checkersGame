module Posicao where

import Data.Char (toUpper) -- importa função toUpper do módulo Data.Char para converter letra minúscula em letra maiúscula | Usado em lerPosicao
import Text.Read (readMaybe) -- importa função readMaybe para tentar converter uma string par anúmero de forma segura | usado no lerPosicao

-- Converte coluna de Char para índice Int (A=0, B=1, ..., H=7)
colunaParaIndice :: Char -> Maybe Int
colunaParaIndice c
    | c >= 'A' && c <= 'H' = Just (fromEnum c - fromEnum 'A')
    | otherwise = Nothing

-- Converte linha (1..8) para índice interno (0..7) invertendo o eixo vertical
linhaParaIndice :: Int -> Maybe Int
linhaParaIndice n
    | n >= 1 && n <= 8 = Just (8 - n)
    | otherwise = Nothing

-- Função para verificar se a posição está dentro do tabuleiro e se é uma casa preta
ehPosicaoValida :: (Int, Int) -> Bool
ehPosicaoValida (linha, coluna) =
    linha >= 0 && linha < 8 &&
    coluna >= 0 && coluna < 8 &&
    ((linha + coluna) `mod` 2 == 1)  -- somente casas pretas

-- Valida posição dada em formato de interface (linha, coluna) 
ehPosicaoValidaInterface :: (Int, Char) -> Bool
ehPosicaoValidaInterface (l, c) = case (linhaParaIndice l, colunaParaIndice c) of
    (Just li, Just ci) -> ehPosicaoValida (li, ci)
    _ -> False

-- Converte entrada "6 B" para posição (Int, Char), ou seja, (6, 'B')
-- usa o readMaybe para não dar erro em tempo de execução no caso do usuário tentar uma entrada como XB, por exemplo
lerPosicao :: String -> Maybe (Int, Char)
lerPosicao [l, c]
  | c >= 'a' && c <= 'h' = (,) <$> readMaybe [l] <*> Just (toUpper c)
  | c >= 'A' && c <= 'H' = (,) <$> readMaybe [l] <*> Just c
lerPosicao _ = Nothing