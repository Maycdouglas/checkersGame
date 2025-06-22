module Tabuleiro where

data Peca = PecaJogador | PecaMaquina | DamaJogador | DamaMaquina
    deriving (Eq, Show)

data Casa = Vazia | Ocupada Peca
    deriving (Eq, Show)

type Linha = [Casa]
type Tabuleiro = [Linha]

tabuleiroInicial :: Tabuleiro
tabuleiroInicial =
    [ linhaMaquina i | i <- [0..2] ] ++
    replicate 2 linhaVazia ++
    [ linhaJogador i | i <- [0..2] ]
  where
    linhaMaquina i = [if (i + j) `mod` 2 == 1 then Ocupada PecaMaquina else Vazia | j <- [0..7]]
    linhaJogador i = [if (i + j) `mod` 2 == 0 then Ocupada PecaJogador else Vazia | j <- [0..7]]
    linhaVazia = replicate 8 Vazia

mostrarTabuleiro :: Tabuleiro -> IO ()
mostrarTabuleiro tab = do
    putStrLn "   ╔═══╦═══╦═══╦═══╦═══╦═══╦═══╦═══╗"
    mapM_ putStrLn $ zipWith mostrarLinha [8,7..1] tab
    putStrLn "   ╚═══╩═══╩═══╩═══╩═══╩═══╩═══╩═══╝"
    putStrLn "     A   B   C   D   E   F   G   H"
  where
    mostrarLinha i linha =
        " " ++ show i ++ " ║" ++ concatMap (mostrarCasa i) (zip [0..] linha)

    mostrarCasa linhaIndex (colIndex, casa) =
        let bg = if (linhaIndex + colIndex) `mod` 2 == 0 then bgBranco else bgPreto
            texto = case casa of
                        Vazia -> "   "
                        Ocupada PecaJogador   -> corTexto "\x1b[34m" " o "
                        Ocupada PecaMaquina   -> corTexto "\x1b[31m" " o "
                        Ocupada DamaJogador   -> corTexto "\x1b[94m" " D "
                        Ocupada DamaMaquina   -> corTexto "\x1b[91m" " D "
        in bg texto ++ "║"

-- Cores e plano de fundo
corTexto :: String -> String -> String
corTexto cor s = cor ++ s ++ "\x1b[0m"

bgPreto :: String -> String
bgPreto s = "\x1b[40m" ++ s ++ "\x1b[0m"

bgBranco :: String -> String
bgBranco s = "\x1b[47m" ++ s ++ "\x1b[0m"

posicaoValida :: (Int, Int) -> Bool
posicaoValida (linha, coluna) =
    linha >= 0 && linha < 8 &&
    coluna >= 0 && coluna < 8 &&
    ((linha + coluna) `mod` 2 == 1)  -- somente casas pretas


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

-- Valida posição dada em formato de interface (linha, coluna) 
posicaoValidaInterface :: (Int, Char) -> Bool
posicaoValidaInterface (l, c) = case (linhaParaIndice l, colunaParaIndice c) of
    (Just li, Just ci) -> posicaoValida (li, ci)
    _ -> False

-- Função para consultar o conteúdo de uma posição válido no tabuleiro
obterCasa :: Tabuleiro -> (Int, Char) -> Maybe Casa
obterCasa tab pos@(l, c) =
    if posicaoValidaInterface pos
       then do
           li <- linhaParaIndice l
           ci <- colunaParaIndice c
           Just ((tab !! li) !! ci)
       else Nothing


