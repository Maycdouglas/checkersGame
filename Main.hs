module Main where -- Define um módulo Main

import Tabuleiro
import Movimento
import Posicao
import Maquina
import Data.List (intercalate)

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
    if jogadorSemPecas tab jogadorAtual -- Verifica se o jogo terminou
        then do
            putStrLn "\n========================="
            putStrLn $ "Fim de jogo! " ++ nomeJogadorColorido outroJogador ++ " venceu!"
            putStrLn "========================="
            mostrarTabuleiro tab
        else if maquinaVsMaquina || jogadorAtual == Jogador2 then do -- Verifica se é a vez da máquina jogar
            putStrLn $ "\nTurno da " ++ nomeJogadorColorido jogadorAtual ++ " (IA)"
            mostrarTabuleiro tab
            tabNovo <- jogadaIa tab jogadorAtual
            loopJogo tabNovo outroJogador maquinaVsMaquina
        else do
            putStrLn $ "\nTurno do " ++ nomeJogadorColorido jogadorAtual
            mostrarTabuleiro tab
            case melhoresCapturas tab jogadorAtual of -- Verifica as melhores jogadas de captura possíveis
                Just sequencias -> do
                    putStrLn "\nVocê deve realizar uma das capturas a seguir:"
                    mapM_ (\(i, (origem, seq)) ->
                        putStrLn $ show i ++ " - Origem: " ++ show origem ++ " -> " ++ intercalate " -> " (map show seq)
                     ) (zip [1..] sequencias)
                    putStrLn "Digite o número da jogada que deseja realizar:"
                    escolhaStr <- getLine
                    case reads escolhaStr of -- Verifica se a opção de captura selecionada pelo usuário é válida
                        [(idx, "")] | idx >= 1 && idx <= length sequencias -> do
                            let (origem, seq) = sequencias !! (idx - 1)
                            case capturarPecaComPos tab origem (head seq) of
                                Just (tabPrimeiro, novaPos) -> do
                                    mostrarTabuleiro tabPrimeiro
                                    tabFinal <- fazerCapturas tabPrimeiro novaPos (tail seq) -- Realiza as capturas da sequência
                                    loopJogo tabFinal outroJogador maquinaVsMaquina
                                Nothing -> do
                                    putStrLn "Erro ao executar a primeira captura."
                                    loopJogo tab jogadorAtual maquinaVsMaquina
                        _ -> do
                            putStrLn "Opção inválida. Tente novamente."
                            loopJogo tab jogadorAtual maquinaVsMaquina
                Nothing -> do -- Se não existe captura possível, o jogador terá que executar um movimento simples
                    putStrLn "Digite posição origem (ex: 6B): "
                    origemStr <- getLine
                    putStrLn "Digite posição destino (ex: 5A): "
                    destinoStr <- getLine
                    case (lerPosicao origemStr, lerPosicao destinoStr) of -- Verifica se a entrada é válida
                        (Just origem, Just destino) -> do
                            if not (ehPosicaoValidaInterface origem)
                                then do
                                    putStrLn "Casa inválida!"
                                    loopJogo tab jogadorAtual maquinaVsMaquina
                                else case obterCasa tab origem of -- Verifica se casa tem peça do jogador atual
                                    Just (Ocupada peca)
                                        | pecaPertenceAoJogador peca jogadorAtual -> do
                                            case moverPeca tab origem destino of -- Realiza o movimento se possível
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
        fazerCapturas t ultimaPos [] = do -- Momento quando não há mais capturas a se fazer
            case obterCasa t ultimaPos of
                Just (Ocupada peca) -> do
                    let novaPeca = avaliarPromocaoParaDama ultimaPos peca -- Verifica se deve promover a dama após último movimento da sequência
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
    option <- getLine -- Captura a opção do usuário como uma String
    case option of
        "1" -> escolherInicio True -- True pq é jogadorVsMaquina
        "2" -> escolherInicio False -- False pq é maquinaVsMaquina
        "3" -> putStrLn "Saindo do jogo..."
        _   -> do 
            putStrLn "Opção inválida, tente novamente."
            main -- Executa essa funcao até o usuário escolher uma opção válida

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
