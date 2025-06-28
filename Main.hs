module Main where -- Define um módulo Main

import Tabuleiro -- importa o módulo Tabuleiro criado por mim
import Movimento
import Posicao
import Data.List (maximumBy, intercalate, maximum)
import Data.Ord (comparing)
import Data.Maybe (fromJust, fromMaybe)
import Debug.Trace (trace)

-- Define tipo de dado Jogador
data Jogador = Jogador1 | Jogador2
    deriving (Eq, Show)

-- Verifica se uma peça pertence a um jogador
pecaPertenceAoJogador :: Peca -> Jogador -> Bool
pecaPertenceAoJogador PecaJogador1 Jogador1 = True
pecaPertenceAoJogador DamaJogador1 Jogador1 = True
pecaPertenceAoJogador PecaJogador2 Jogador2 = True
pecaPertenceAoJogador DamaJogador2 Jogador2 = True
pecaPertenceAoJogador _ _ = False

-- Loop principal do jogo

-- Controla o turno dos jogadores
-- Lê as posições de origem e destino
-- Verifica se o movimento é válido (simples ou de captura)
-- Verifica se a peça escolhida é do jogador atual
-- Atualiza o tabuleiro e troca o jogador

-- Executa Loop do Jogo de Damas
loopJogo :: Tabuleiro -> Jogador -> Bool -> IO ()
loopJogo tab jogadorAtual maquinaVsMaquina = do
    let outroJogador = trocarJogador jogadorAtual
    if jogadorSemPecas tab jogadorAtual
        then do
            putStrLn "\n========================="
            putStrLn $ "Fim de jogo! " ++ nomeJogadorColorido outroJogador ++ " venceu!"
            putStrLn "========================="
            mostrarTabuleiro tab
        else if maquinaVsMaquina || jogadorAtual == Jogador2 then do
            putStrLn $ "\nTurno da " ++ nomeJogadorColorido jogadorAtual ++ " (IA)"
            mostrarTabuleiro tab
            tabNovo <- jogadaIa tab jogadorAtual
            loopJogo tabNovo outroJogador maquinaVsMaquina
        else do
            putStrLn $ "\nTurno do " ++ nomeJogadorColorido jogadorAtual
            mostrarTabuleiro tab

            case melhoresCapturas tab jogadorAtual of
                Just sequencias -> do
                    putStrLn "\nVocê deve realizar uma das capturas a seguir:"
                    mapM_ (\(i, (origem, seq)) ->
                        putStrLn $ show i ++ " - Origem: " ++ show origem ++ " -> " ++ intercalate " -> " (map show seq)
                     ) (zip [1..] sequencias)

                    putStrLn "Digite o número da jogada que deseja realizar:"
                    escolhaStr <- getLine
                    case reads escolhaStr of
                        [(idx, "")] | idx >= 1 && idx <= length sequencias -> do
                            let (origem, seq) = sequencias !! (idx - 1)
                            case capturarPecaComPos tab origem (head seq) of
                                Just (tabPrimeiro, novaPos) -> do
                                    mostrarTabuleiro tabPrimeiro
                                    tabFinal <- fazerCapturas tabPrimeiro novaPos (tail seq)
                                    loopJogo tabFinal outroJogador maquinaVsMaquina
                                Nothing -> do
                                    putStrLn "Erro ao executar a primeira captura."
                                    loopJogo tab jogadorAtual maquinaVsMaquina
                        _ -> do
                            putStrLn "Opção inválida. Tente novamente."
                            loopJogo tab jogadorAtual maquinaVsMaquina

                Nothing -> do
                    putStrLn "Digite posição origem (ex: 6B): "
                    origemStr <- getLine
                    putStrLn "Digite posição destino (ex: 5A): "
                    destinoStr <- getLine
                    case (lerPosicao origemStr, lerPosicao destinoStr) of
                        (Just origem, Just destino) -> do
                            if not (ehPosicaoValidaInterface origem)
                                then do
                                    putStrLn "Casa inválida!"
                                    loopJogo tab jogadorAtual maquinaVsMaquina
                                else case obterCasa tab origem of
                                    Just (Ocupada peca)
                                        | pecaPertenceAoJogador peca jogadorAtual -> do
                                            case capturarPecaComPos tab origem destino of
                                                Just (tabApósCaptura, novaPos) -> do
                                                    putStrLn "Captura realizada!"
                                                    mostrarTabuleiro tabApósCaptura
                                                    tabFinal <- loopCapturasSequenciais tabApósCaptura novaPos jogadorAtual
                                                    loopJogo tabFinal outroJogador maquinaVsMaquina
                                                Nothing -> case moverPeca tab origem destino of
                                                    Just tabNovo -> loopJogo tabNovo outroJogador maquinaVsMaquina
                                                    Nothing -> do
                                                        putStrLn "Movimento inválido!"
                                                        loopJogo tab jogadorAtual maquinaVsMaquina
                                    Just Vazia -> do
                                        putStrLn "Não há peça na origem."
                                        loopJogo tab jogadorAtual maquinaVsMaquina
                                    _ -> do
                                        putStrLn "Essa peça não pertence a você."
                                        loopJogo tab jogadorAtual maquinaVsMaquina
                        _ -> do
                            putStrLn "Entrada inválida!"
                            loopJogo tab jogadorAtual maquinaVsMaquina

  where
    fazerCapturas t ultimaPos [] = do
        case obterCasa t ultimaPos of
            Just (Ocupada peca) -> do
                let novaPeca = avaliarPromocaoParaDama ultimaPos peca
                case atualizarCasa t ultimaPos (Ocupada novaPeca) of
                    Just t1 -> return (removerSemicapturadas t1)
                    Nothing -> return (removerSemicapturadas t)
            _ -> return (removerSemicapturadas t)

    fazerCapturas t atual (prox:resto) = do
        case capturarPecaComPos t atual prox of
            Just (novoTab, novaPos) -> do
                mostrarTabuleiro novoTab
                putStrLn $ "Captura para " ++ show novaPos ++ " realizada!"
                fazerCapturas novoTab novaPos resto
            Nothing -> do
                putStrLn $ "Erro ao capturar para " ++ show prox
                return t


-- Colore o nome do Jogador 
nomeJogadorColorido :: Jogador -> String
nomeJogadorColorido Jogador1 = corTexto "\x1b[34;1m" "Jogador 1"  -- Azul
nomeJogadorColorido Jogador2 = corTexto "\x1b[33;1m" "Jogador 2"  -- Amarelo

-- Alterna entre jogadores
trocarJogador :: Jogador -> Jogador
trocarJogador Jogador1 = Jogador2
trocarJogador Jogador2 = Jogador1

-- Menu inicial do jogo
main:: IO() -- declara a função main como uma ação de input e output(IO)
main = do
    putStrLn "====================="
    putStrLn "    JOGO DE DAMAS    "
    putStrLn "====================="
    putStrLn "Escoha o modo de jogo:"
    putStrLn "1 - Jogador vs Máquina"
    putStrLn "2 - Máquina vs Máquina"
    putStrLn "3 - Sair"
    putStrLn "Opção: "
    option <- getLine -- captura a opção do usuário como uma String
    case option of
        "1" -> escolherInicio True -- True pq é jogadorVsMaquina
        "2" -> escolherInicio False -- False pq é maquinaVsMaquina
        "3" -> putStrLn "Saindo do jogo..."
        _   -> do 
            putStrLn "Opção inválida, tente novamente."
            main -- executa essa funcao até o usuário escolher uma opção válida

-- Escolhe quem inicia o jogo
escolherInicio :: Bool -> IO ()
escolherInicio jogadorVsMaquina = do
    putStrLn "Quem começa o Jogo?"
    putStrLn "1 - Jogador (ou Máquina 1)"
    putStrLn "2 - Máquina (ou máquina 2)"
    putStr "Opção: "
    inicio <- getLine
    case inicio of
        "1" -> iniciarJogo jogadorVsMaquina Jogador1
        "2" -> iniciarJogo jogadorVsMaquina Jogador2
        _   -> do
            putStrLn "Opção inválida, tente novamente."
            escolherInicio jogadorVsMaquina

-- Inicializa o jogo
iniciarJogo :: Bool -> Jogador -> IO ()
iniciarJogo jogadorVsMaquina jogadorInicial = do
    putStrLn $ "\nModo de jogo: " ++
        if jogadorVsMaquina then "Jogador vs Máquina" else "Máquina vs Máquina"
    putStrLn $ "Quem começa: " ++ nomeJogadorColorido jogadorInicial
    loopJogo tabuleiroInicial jogadorInicial (not jogadorVsMaquina && True)

-- Retorna as posições das peças do Jogador
posicoesDoJogador :: Tabuleiro -> Jogador -> [(Int, Char)]
posicoesDoJogador tab jogador = 
    [ (8 - li, toEnum (fromEnum 'A' + ci)) 
    | (li, linha) <- zip [0..] tab
    , (ci, casa) <- zip [0..] linha
    , Ocupada peca <- [casa]
    , pecaPertenceAoJogador peca jogador
    ]

-- Gera as melhores capturas possíveis na jogada
melhoresCapturas :: Tabuleiro -> Jogador -> Maybe [((Int, Char), [(Int, Char)])]
melhoresCapturas tab jogador =
    let
        todasPosicoes = posicoesDoJogador tab jogador

        todasCapturas =
            [ (origem, seq)
            | origem <- todasPosicoes
            , seq <- sequenciasCaptura tab origem
            , not (null seq)
            ]
    in
        if null todasCapturas
            then Nothing
            else
                let maxLen = maximum (map (length . snd) todasCapturas)
                in Just $ filter (\(_, seq) -> length seq == maxLen) todasCapturas

-- Executa loop para capturas sequenciais
loopCapturasSequenciais :: Tabuleiro -> (Int, Char) -> Jogador -> IO Tabuleiro
loopCapturasSequenciais tab pos jogador = do
    let capturas = sequenciasCaptura tab pos
    case capturas of
        [] -> do
            putStrLn "Nenhuma captura possível a partir daqui."
            return tab
        _ -> do
            let melhores = filter (\seq -> length seq == maximum (map length capturas)) capturas
            case melhores of
                [] -> return tab
                (melhorSeq:_) -> fazerCapturas tab pos melhorSeq
  where
    fazerCapturas t ultimaPos [] = do
        case obterCasa t ultimaPos of
            Just (Ocupada peca) -> do
                let novaPeca = avaliarPromocaoParaDama ultimaPos peca
                case atualizarCasa t ultimaPos (Ocupada novaPeca) of
                    Just t1 -> return (removerSemicapturadas t1)
                    Nothing -> return (removerSemicapturadas t) -- falha ao atualizar
            _ -> return (removerSemicapturadas t)  -- Ao final, limpa as semicapturadas   
    fazerCapturas t atual (prox:resto) = do
        case capturarPecaComPos t atual prox of
            Just (novoTab, novaPos) -> do
                mostrarTabuleiro novoTab
                putStrLn $ "Captura para " ++ show novaPos ++ " realizada!"
                fazerCapturas novoTab novaPos resto
            Nothing -> do
                putStrLn $ "Erro ao tentar capturar para " ++ show prox
                return t

-- Verifica se o Jogador está sem peças
jogadorSemPecas :: Tabuleiro -> Jogador -> Bool
jogadorSemPecas tab jogador =
    null [()
         | linha <- tab
         , Ocupada peca <- linha
         , pecaPertenceAoJogador peca jogador
         ]



-- Função principal da IA: retorna o tabuleiro atualizado após o movimento da máquina
-- Executa a jogada da IA
jogadaIa :: Tabuleiro -> Jogador -> IO Tabuleiro
jogadaIa tab jogador = do
    let capturas = fromMaybe [] (melhoresCapturas tab jogador)
    if null capturas
        then do
            let movimentosSimples = movimentosSimplesValidos tab jogador
            if null movimentosSimples
                then do
                    putStrLn "IA: sem jogadas possíveis."
                    return tab
                else do
                    let (origem, destino) = escolherMelhorMovimentoSimples tab jogador movimentosSimples
                    putStrLn $ "IA: movendo de " ++ show origem ++ " para " ++ show destino
                    case moverPeca tab origem destino of
                        Just tabNovo -> return tabNovo
                        Nothing -> do
                            putStrLn "IA: erro ao mover peça."
                            return tab
        else do
            let (origem, melhorSeq) = escolherMelhorCaptura tab jogador capturas
            putStrLn $ "IA: capturando de " ++ show origem ++ " seguindo " ++ show melhorSeq
            tabFinal <- executarSequenciaCapturas tab origem melhorSeq
            return tabFinal

-- Escolhe a melhor captura entre as melhores capturas segundo heurística
escolherMelhorCaptura :: Tabuleiro -> Jogador -> [((Int, Char), [(Int, Char)])] -> ((Int, Char), [(Int, Char)])
escolherMelhorCaptura tab jogador capturas =
    maximumBy (comparing (mobilidadePosicaoFinal tab jogador)) capturas

-- Avalia a "mobilidade" (quantidade de movimentos possíveis) da peça após a sequência de capturas
mobilidadePosicaoFinal :: Tabuleiro -> Jogador -> ((Int, Char), [(Int, Char)]) -> Int
mobilidadePosicaoFinal tab jogador (origem, seq) =
    let posFinal = if null seq then origem else last seq
        tabSimulado = simularSequenciaCaptura tab origem seq
    in case tabSimulado of
        Just tabNovo -> length $ sequenciasCaptura tabNovo posFinal -- número de sequências de captura disponíveis na nova posição
        Nothing -> 0

-- Simula o resultado de uma sequência de captura sem modificar o tabuleiro original
simularSequenciaCaptura :: Tabuleiro -> (Int, Char) -> [(Int, Char)] -> Maybe Tabuleiro
simularSequenciaCaptura tab origem [] = Just tab
simularSequenciaCaptura tab origem (d:ds) =
    case capturarPecaComPos tab origem d of
        Just (tabNovo, novaPos) -> simularSequenciaCaptura tabNovo d ds
        Nothing -> Nothing

-- Executa a sequência de capturas
executarSequenciaCapturas :: Tabuleiro -> (Int, Char) -> [(Int, Char)] -> IO Tabuleiro
executarSequenciaCapturas tab origem [] = return tab
executarSequenciaCapturas tab origem (destino:resto) = do
    case capturarPecaComPos tab origem destino of
        Just (novoTab, novaPos) -> do
            mostrarTabuleiro novoTab  -- Exibe o tabuleiro após a captura com peça branca
            executarCapturasRec novoTab novaPos resto
        Nothing -> return tab  -- erro na primeira captura

  where
    executarCapturasRec :: Tabuleiro -> (Int, Char) -> [(Int, Char)] -> IO Tabuleiro
    executarCapturasRec t ultimaPos [] = do
        -- Quando acabou a sequência de capturas:
        case obterCasa t ultimaPos of
            Just (Ocupada peca) -> do
                let novaPeca = avaliarPromocaoParaDama ultimaPos peca
                case atualizarCasa t ultimaPos (Ocupada novaPeca) of
                    Just t1 -> return (removerSemicapturadas t1)
                    Nothing -> return (removerSemicapturadas t)
            _ -> return (removerSemicapturadas t)

    executarCapturasRec t atual (prox:resto') = do
        case capturarPecaComPos t atual prox of
            Just (novoTab, novaPos) -> do
                mostrarTabuleiro novoTab  -- Mostra tabuleiro com peça branca
                executarCapturasRec novoTab novaPos resto'
            Nothing -> return t  -- erro numa das capturas subsequentes

-- Obtem todos movimentos simples válidos (origem,destino)
movimentosSimplesValidos :: Tabuleiro -> Jogador -> [((Int, Char), (Int, Char))]
movimentosSimplesValidos tab jogador =
    concatMap movimentosDaPeca (posicoesDoJogador tab jogador)
  where
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

    caminharDirecao :: Int -> Int -> (Int, Int) -> [((Int, Char), (Int, Char))]
    caminharDirecao li ci (dl, dc) =
        let posicoes = tail $ zip [li, li+dl ..] [ci, ci+dc ..]
        in takeWhile ehValidaELivre posicoes >>= \ (x, y) ->
                let destino = (8 - x, toEnum (fromEnum 'A' + y))
                in [((8 - li, toEnum (fromEnum 'A' + ci)), destino)]

    ehValidaELivre :: (Int, Int) -> Bool
    ehValidaELivre (x, y)
        | not (ehPosicaoValida (x, y)) = False
        | otherwise =
            let pos = (8 - x, toEnum (fromEnum 'A' + y))
            in obterCasa tab pos == Just Vazia

-- Escolhe o melhor movimento simples segundo heurística (exemplo: maior mobilidade no destino)
escolherMelhorMovimentoSimples :: Tabuleiro -> Jogador -> [((Int, Char), (Int, Char))] -> ((Int, Char), (Int, Char))
escolherMelhorMovimentoSimples tab jogador movimentos =
    maximumBy (comparing (avaliarMovimento tab jogador)) movimentos

-- Atribui peso/pontuação a um movimento
avaliarMovimento :: Tabuleiro -> Jogador -> ((Int, Char), (Int, Char)) -> Int
avaliarMovimento tab jogador (origem, destino) =
    let base = length (sequenciasCaptura tab destino)
        -- Adiciona peso para estar na última linha (chance de promover a peça)
        linhaDestino = fst destino
        pesoPromocao = case jogador of
            Jogador1 -> if linhaDestino == 8 then 3 else 0
            Jogador2 -> if linhaDestino == 1 then 3 else 0
        -- Soma a quantidade de casas livres ao redor da nova posição
        mobilidade = length $ movimentosAdjacentesLivres tab destino
    in base * 10 + pesoPromocao + mobilidade

-- Verifica movimentos adjacentes livres 
movimentosAdjacentesLivres :: Tabuleiro -> (Int, Char) -> [(Int, Char)]
movimentosAdjacentesLivres tab (l, c) =
    let direcoes = [(-1,-1), (-1,1), (1,-1), (1,1)]
        vizinhos = [(l + dl, toEnum (fromEnum c + dc)) | (dl, dc) <- direcoes]
    in filter (\pos -> ehPosicaoValidaInterface pos && obterCasa tab pos == Just Vazia) vizinhos



