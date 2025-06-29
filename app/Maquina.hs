module Maquina where

import Tabuleiro
import Movimento
import Posicao
import Data.List (maximum)
import Data.Maybe (fromJust, fromMaybe)
import System.Random (randomRIO)

-- Executa a jogada da IA
jogadaIa :: Tabuleiro -> Jogador -> IO Tabuleiro
jogadaIa tab jogador = do
    let capturas = fromMaybe [] (melhoresCapturas tab jogador)
    if null capturas -- Se não tem captura possível, realiza movimento sem captura
        then do
            let movimentosSimples = movimentosSimplesValidos tab jogador
            if null movimentosSimples
                then do
                    putStrLn "IA: sem jogadas possíveis."
                    return tab
                else do
                    (origem, destino) <- escolherMelhorMovimentoSimples tab jogador movimentosSimples -- Realiza escolha entre lista de melhores movimentos sem capturas possíveis
                    putStrLn $ "IA: movendo de " ++ show origem ++ " para " ++ show destino
                    case moverPeca tab origem destino of
                        Just tabNovo -> return tabNovo
                        Nothing -> do
                            putStrLn "IA: erro ao mover peça."
                            return tab
        else do
            (origem, melhorSeq) <- escolherMelhorCaptura tab jogador capturas -- Realiza escolha entre lista de melhores capturas possíveis
            putStrLn $ "IA: capturando de " ++ show origem ++ " seguindo " ++ show melhorSeq
            tabFinal <- executarSequenciaCapturas tab origem melhorSeq
            return tabFinal

-- Escolhe uma das melhores capturas identificadas. Quando empate, a escolha é randomizada
escolherMelhorCaptura :: Tabuleiro -> Jogador -> [((Int, Char), [(Int, Char)])] -> IO ((Int, Char), [(Int, Char)])
escolherMelhorCaptura tab jogador capturas = do
    let capturasComPeso = [ (cap, avaliarCaptura tab jogador cap) | cap <- capturas ]
        melhorValor = maximum (map snd capturasComPeso)
        melhores = [ cap | (cap, peso) <- capturasComPeso, peso == melhorValor ]
    idx <- randomRIO (0, length melhores - 1) -- Escolha aleatório adotada para que a IA não faça sempre o mesmo movimento em caso de empate
    return (melhores !! idx)

avaliarCaptura :: Tabuleiro -> Jogador -> ((Int, Char), [(Int, Char)]) -> Int
avaliarCaptura tab jogador (origem, seqMov) =
    let posFinal = if null seqMov then origem else last seqMov
        tabSimulado = simularSequenciaCaptura tab origem seqMov
        linhaFinal = fst posFinal
        adversario = trocarJogador jogador
    in case tabSimulado of
        Just tabNovo ->
            if destinoAmeacado tabNovo posFinal adversario
                then -1000
                else
                    let pesoPromocao = case jogador of
                            Jogador1 -> if linhaFinal == 8 then 100 else 0
                            Jogador2 -> if linhaFinal == 1 then 100 else 0
                    in pesoPromocao + length seqMov
        Nothing -> -1000

-- Simula o resultado de uma sequência de captura sem modificar o tabuleiro original
simularSequenciaCaptura :: Tabuleiro -> (Int, Char) -> [(Int, Char)] -> Maybe Tabuleiro
simularSequenciaCaptura tab origem [] = Just tab
simularSequenciaCaptura tab origem (d:ds) =
    case capturarPecaComPos tab origem d of
        Just (tabNovo, novaPos) -> simularSequenciaCaptura tabNovo d ds
        Nothing -> Nothing

-- Executa a sequência de capturas de fato
executarSequenciaCapturas :: Tabuleiro -> (Int, Char) -> [(Int, Char)] -> IO Tabuleiro
executarSequenciaCapturas tab origem [] = return tab
executarSequenciaCapturas tab origem (destino:resto) = do
    case capturarPecaComPos tab origem destino of
        Just (novoTab, novaPos) -> do
            mostrarTabuleiro novoTab  -- Exibe o tabuleiro após a captura com peça branca, significando semicapturada
            executarCapturasRec novoTab novaPos resto
        Nothing -> return tab
    where
        executarCapturasRec :: Tabuleiro -> (Int, Char) -> [(Int, Char)] -> IO Tabuleiro
        executarCapturasRec t ultimaPos [] = do
            -- Quando acabou a sequência de capturas:
            case obterCasa t ultimaPos of
                Just (Ocupada peca) -> do
                    let novaPeca = avaliarPromocaoParaDama ultimaPos peca -- Avalia se a promoção para dama é possível ao fim da jogada
                    case atualizarCasa t ultimaPos (Ocupada novaPeca) of
                        Just t1 -> return (removerSemicapturadas t1)
                        Nothing -> return (removerSemicapturadas t)
                _ -> return (removerSemicapturadas t)
        executarCapturasRec t atual (prox:resto') = do
            case capturarPecaComPos t atual prox of
                Just (novoTab, novaPos) -> do
                    mostrarTabuleiro novoTab  -- Exibe o tabuleiro após a captura com peça branca, significando semicapturada
                    executarCapturasRec novoTab novaPos resto'
                Nothing -> return t

-- Gera os movimentos sem captura válidos para o jogador
movimentosSimplesValidos :: Tabuleiro -> Jogador -> [((Int, Char), (Int, Char))]
movimentosSimplesValidos tab jogador =
    concatMap movimentosDaPeca (posicoesDoJogador tab jogador)
    where
        -- Obtem os possiveis movimentos de uma peça
        movimentosDaPeca :: (Int, Char) -> [((Int, Char), (Int, Char))]
        movimentosDaPeca origem =
            case obterCasa tab origem of
                Just (Ocupada peca) ->
                    if ehDama peca
                        then destinosDama origem
                        else destinosPecaComum peca origem
                _ -> []
        -- Movimentos possíveis para peças comuns (diagonal para frente)
        destinosPecaComum :: Peca -> (Int, Char) -> [((Int, Char), (Int, Char))]
        destinosPecaComum peca (l, c) =
            let direcoes = case peca of
                    PecaJogador1 -> [(1, -1), (1, 1)]    -- desce
                    PecaJogador2 -> [(-1, -1), (-1, 1)]  -- sobe
                    _ -> []
                possiveis = [ (l + dl, toEnum (fromEnum c + dc)) | (dl, dc) <- direcoes ]
            in [ ((l, c), destino)
            | destino <- possiveis
            , ehPosicaoValidaInterface destino
            , obterCasa tab destino == Just Vazia
            ]
        -- Movimentos possíveis para damas (em qualquer diagonal, até encontrar obstáculo)
        destinosDama :: (Int, Char) -> [((Int, Char), (Int, Char))]
        destinosDama (l, c) =
            let direcoes = [(-1, -1), (-1, 1), (1, -1), (1, 1)]
                li = fromJust (linhaParaIndice l)
                ci = fromJust (colunaParaIndice c)
            in concatMap (caminharDirecao li ci) direcoes
        -- Gera lista de movimentos para cada uma das posições possíveis na direção recebida
        caminharDirecao :: Int -> Int -> (Int, Int) -> [((Int, Char), (Int, Char))]
        caminharDirecao li ci (dl, dc) =
            let posicoes = tail $ zip [li, li+dl ..] [ci, ci+dc ..]
            in takeWhile ehValidaELivre posicoes >>= \ (x, y) ->
                let destino = (8 - x, toEnum (fromEnum 'A' + y))
                in [((8 - li, toEnum (fromEnum 'A' + ci)), destino)]
        -- Verifica se a casa é válida e está livre
        ehValidaELivre :: (Int, Int) -> Bool
        ehValidaELivre (x, y)
            | not (ehPosicaoValida (x, y)) = False
            | otherwise =
                let pos = (8 - x, toEnum (fromEnum 'A' + y))
                in obterCasa tab pos == Just Vazia

-- Escolhe um dos melhores movimentos sem captura identificadas. Quando empate, a escolha é randomizada
escolherMelhorMovimentoSimples :: Tabuleiro -> Jogador -> [((Int, Char), (Int, Char))] -> IO ((Int, Char), (Int, Char))
escolherMelhorMovimentoSimples tab jogador movimentos = do
    let movimentosComPeso = [ (mov, avaliarMovimento tab jogador mov) | mov <- movimentos ]
        melhorValor = maximum (map snd movimentosComPeso)
        melhores = [ mov | (mov, peso) <- movimentosComPeso, peso == melhorValor ]
    idx <- randomRIO (0, length melhores - 1) -- Escolha aleatório adotada para que a IA não faça sempre o mesmo movimento em caso de empate
    return (melhores !! idx)

-- Verifica se uma casa está sob ameaça do adversário após o movimento ser feito
destinoAmeacado :: Tabuleiro -> (Int, Char) -> Jogador -> Bool
destinoAmeacado tab pos jogadorAdversario =
    any (\(_, seqMov) -> pos `elem` seqMov) $ fromMaybe [] (melhoresCapturas tab jogadorAdversario)

-- Atribui uma pontuação para o movimento
avaliarMovimento :: Tabuleiro -> Jogador -> ((Int, Char), (Int, Char)) -> Int
avaliarMovimento tab jogador (origem, destino) =
    case moverPeca tab origem destino of
        Just tabSimulado ->
            let adversario = trocarJogador jogador
                ameacado = destinoAmeacado tabSimulado destino adversario
                linhaDestino = fst destino
                pesoPromocao = case jogador of
                    Jogador1 -> if linhaDestino == 8 then 100 else 0
                    Jogador2 -> if linhaDestino == 1 then 100 else 0
            in if ameacado
                then -1000  -- movimento suicida: fortemente penalizado
                else pesoPromocao
        Nothing -> -1000  -- movimento inválido
