# JOGO DE DAMAS EM HASKELL

Este projeto consiste em uma implementação completa do jogo de Damas, desenvolvida em **Haskell**, com suporte a regras oficiais, múltiplas capturas, promoção para dama e uma **IA básica** para jogadas automáticas.

![Menu Inicial](MenuInicialDamas.png)

![Inicio do jogo](InicioJogoDamas.png)

![Fim do jogo](FimJogoDamas.png)

---

## Objetivo

Explorar os conceitos da **programação funcional** aplicados no desenvolvimento de um jogo de tabuleiro clássico. O projeto também serviu como prática em estruturas recursivas, imutabilidade, funções puras e manipulação de listas.

---

## Funcionalidades

- Criação e exibição do tabuleiro
- Movimentação de peças
- Regras de captura obrigatória
- Múltiplas capturas sequenciais
- Promoção de peças a "Damas" (peças que se movimentam em todas as direções)
- Detecção de fim de jogo
- Modo jogador vs jogador ou jogador vs IA
- IA simples com heurística baseada em número de capturas

---

## Estrutura do Projeto

app/

├── Tabuleiro.hs -- Contém a definição dos novos tipos de dados Peca, Casa e Jogador, 
sinônimos Linha (lista de Casas) e Tabuleiro (lista de Linhas), funções 
relacionadas aos tipos Peca, Casa e Jogador e funções de geração e 
exibição do Tabuleiro.

├── Movimento.hs -- Contém funções relacionadas a movimentação e captura das peças do 
jogo.

├── Maquina.hs -- Contém funções relacionadas às jogadas da Máquina, definindo a 
melhor estratégia de jogo de acordo com a situação da partida. 

├── Posicao.hs -- Contém funções relacionadas à leitura e conversão de linhas e colunas, 
de acordo com a entrada do jogador e com a estrutura interna do 
tabuleiro.

└── Main.hs -- Contém o fluxo principal, menu inicial e loop do jogo. 


## Instruções

Certifique-se de ter o **GHC** e **Cabal** instalados.

### Comandos

Execute os comandos abaixo na raiz do projeto:

- cabal build
- cabal repl
- main

## Exemplos de conceitos funcionais aplicados

- Imutabilidade: o tabuleiro é sempre transformado em uma nova versão após cada jogada.
- Recursão: usada para encontrar sequências de capturas múltiplas.
- Funções puras: a maioria das funções não depende de estado global.
- Composição de funções: permite manipular listas e estados do jogo de forma elegante.
- Tipos algébricos (ADTs): utilizados para representar peças, jogadas e estados do jogo.

## Observação

Para mais informações sobre o desenvolvimento, pode acessar o arquivo "Relatório - Jogo de Damas em Haskell" na raiz do projeto
