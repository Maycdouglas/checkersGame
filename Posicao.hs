module Posicao where

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