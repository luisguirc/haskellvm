# Simulando uma máquina virtual em Haskell
## Introdução e motivação
No desenvolvimento de software moderno, a segurança de tipo e a garantia de corretude em todo o sistema são fundamentais. Muitas são as vezes em que um bug é encontrado apenas "em produção": quando um sistema já está cumprindo seu propósito. Isso ocorre, em geral, por que não podemos esperar que o cérebro humano tenha a mesma capacidade de um computador em manter várias abstrações (muitas vezes, de tipos ou classes) e o funcionamento de um sistema em nossa memória simultaneamente.

Haskell, uma linguagem de programação funcional pura, oferece recursos poderosos, como **Tipos de Dados Algébricos Generalizados (GADTs)** e **Famílias de Tipos (Type Families)**, que permitem aos desenvolvedores construir sistemas de tipos complexos e expressivos que podem impor condições e verificações diretamente através do verificador de tipo.

Este tutorial explorará como esses recursos podem ser utilizados de forma eficaz para construir uma máquina virtual (VM) que seja robusta, segura em termos de tipos e compreensível. Nossa jornada abordará a criação de uma VM simples com operações típicas de arquiteturas de processadores – como gerenciamento de registradores, operações aritméticas e controle de fluxo – introduzindo gradualmente cenários mais complexos utilizando as capacidades inerentes do sistema de tipos do Haskell. No final, esta máquina rodará um programa que nos dá o n-ésimo número de Fibonacci.


### Por que usar GADTs?

Os  **Tipos de Dados Algébricos Generalizados**, ou GADTs, representam uma extensão poderosa dos tipos de dados algébricos comuns, disponíveis em muitas linguagens de programação funcional. Em essência, os GADTs permitem que construtores de tipo especializem seus resultados, oferecendo um controle mais refinado sobre os tipos que esses construtores podem adotar. Isso permite aos desenvolvedores incorporar regras e restrições complexas diretamente nos tipos, fortalecendo a segurança de tipo e reduzindo erros.

GADTs são particularmente úteis em situações onde é necessário um forte acoplamento entre os dados e as operações que podem ser executadas sobre eles. Por exemplo, na definição de uma máquina virtual, usando GADTs para definir instruções, você pode especificar claramente quais tipos de operandos são aceitáveis para cada operação. Isso garante que apenas as combinações corretas de operandos e operações sejam possíveis, eliminando uma classe inteira de erros comuns em sistemas menos rigorosos de tipo.

Essa capacidade de restringir e verificar relacionamentos entre tipos em tempo de compilação oferece um aumento significativo na robustez do código. Ao aproveitar os GADTs, você transforma possíveis falhas de tempo de execução, que podem ser difíceis de rastrear e reproduzir, em erros de compilação claros e gerenciáveis.

## Estrutura e funcionamento geral
O desenvolvimento de uma máquina virtual (VM) envolve a criação de vários componentes interconectados que simulam o hardware de um computador real. Esses componentes incluem a unidade de processamento central (CPU), memória (como uma pilha e registros), e o conjunto de instruções que define as operações possíveis. Abaixo há detalhes sobre como cada parte da VM foi concebida e implementada:

### Definindo Componentes Básicos

####  Registros e Operandos
Começamos definindo os tipos  `Register`  e  `Operand`.  `Register`  é um tipo enumerado que lista todos os registros disponíveis na VM.  `Operand`, definido como um GADT, permite representar não só registros, mas também valores imediatos e endereços de memória, facilitando assim operações diretas sobre diferentes tipos de dados. É importante ressaltar que as extensões de linguagem GADTs e StandaloneDeriving foram habilitadas.
```haskell
data Register = R0 | R1 | R2 | R3 deriving (Show, Enum, Eq, Ord)

type Address = Int
newtype Immediate = Imm Int deriving (Show, Eq)

data Operand where
    Reg :: Register -> Operand
    Val :: Immediate -> Operand
    Addr :: Address -> Operand
deriving instance Show Operand
```
#### Instruções
Utilizando GADTs, definimos o tipo  `Instruction`  que representa todas as possíveis instruções que a máquina virtual pode executar. Cada construtor de  `Instruction`  especifica claramente os tipos de operandos que aceita, garantindo que as instruções sejam construídas de maneira segura em termos de tipo. A implementação de cada uma das instruções está contida em outra função (`execute`).

#### Documentação das instruções
| Código | Instrução | Operador 1   | Operador 2    | Operador 3    | Descrição                                                                                                                                                       |
|--------|-----------|--------------|---------------|---------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 10     | MOVR      | Reg. destino | Reg. origem   | -             | Carrega o valor do reg. de origem para o de destino                                                                                                             |
| 11     | MOVV      | Reg. destino | Valor         | -             | Carrega o valor para o registrador de destino                                                                                                                   |
| 20     | ADD       | Reg. destino | Reg. origem 1 | Reg. origem 2 | Soma o valor dos dois registradores de origem e carrega no registrador de destino                                                                               |
| 21     | SUB       | Reg. destino | Reg. origem 1 | Reg. origem 2 | Subtrai o valor do registrador de origem 2 do registrador de origem 1, e carrega no destino                                                                     |
| 30     | PUSH      | Reg. origem  | -             | -             | Empilha o valor no registrador de origem                                                                                                                        |
| 31     | POP       | Reg. destino | -             | -             | Desempilha um valor da pilha, e carrega no registrador de destino                                                                                               |
| 40     | JP        | Endereço     | -             | -             | Continua a execução a partir do endereço especificado                                                                                                           |
| 41     | JL        | Reg. 1       | Reg. 2        | Endereço      | Continua a execução a partir do endereço especificado caso o valor em Reg. 1 seja menor que o valor em Reg. 2. Caso contrário, continua a execução normalmente. |
| 42     | CALL      | Endereço     | -             | -             | Empilha o endereço da instrução imediatamente após CALL                                                                                                         |
| 50     | RET       | -            | -             | -             | Desempilha um valor, assume que é um endereço e executa um JP para este endereço                                                                                |
| 60     | PRINT     | Registrador  | -             | -             | Imprime o valor contido neste registrador.                                                                                                                      |
| 99     | HALT      | -            | -             | -             | Para a máquina virtual.                                                                                                                                         |
```haskell
data Instruction where
    -- Register to Register Operations
    MOVR :: Register -> Register -> Instruction
    MOVV :: Register -> Immediate -> Instruction

    -- Arithmetic Operations
    ADD :: Register -> Register -> Register -> Instruction
    SUB :: Register -> Register -> Register -> Instruction

    -- Stack Operations
    PUSH :: Operand -> Instruction
    POP :: Register -> Instruction

    -- Jump and Call Operations
    JP :: Address -> Instruction
    JL :: Register -> Register -> Address -> Instruction
    CALL :: Address -> Instruction
    
    -- Control Instructions
    RET :: Instruction
    HALT :: Instruction
    
deriving instance Show Instruction
```
Ao longo do desenvolvimento, mais uma instrução foi adicionada: a instrução de `PRINT`. Isso levou a pequenas alterações nas funções de `execute`, `decode`, e o próprio `VMState`.

```haskell
data Instruction where
	...
    PRINT :: Operand -> Instruction
    ...
```

#### Estado da máquina
Define o estado atual da VM, mantendo o controle dos registradores, da pilha, do contador de programa, e se a máquina está em estado de parada.
```haskell
data VMState = VMState {
    pc      :: Int,
    regs    :: [(Register, Int)],
    stack   :: [Operand],
    program :: [Instruction],
    halted  :: Bool,
    output  :: Maybe Int --Adicionado após a instrução de PRINT
} deriving (Show)
```
### Execução da VM
#### Inicialização
Para a máquina inicial, foram definidas funções de inicialização, decodificação, e execução.
A função `initialize` define o estado inicial da máquina, e algumas decisões foram tomadas:
1. Optou-se por ter apenas 4 registradores. Isso foi suficiente para executar o programa-alvo: conseguir o n-ésimo número de Fibonacci.
2. A existência de um campo output diminuiu a complexidade na introdução de uma instrução de PRINT, eliminando da função `execute` a necessidade de IO.
3. O programa é construído a partir de um `code`, que pode ser interpretado como a linguagem de programação da máquina. A função `compile` faz o papel de um compilador: transforma a linguagem do usuário em uma linguagem compreendida pela máquina.

O ponto 3 trás algumas implicações: não é possível fazer verificações necessárias no `code` antes da execução. Por exemplo: é necessário que o código possua ao menos uma instrução de `HALT`para interromper o processamento (normalmente ao final do código), mas essa verificação não ocorre no tempo de compilação. Isso poderia ser inserido ao final de qualquer programa, mas qual seria a graça nisso?

Na próxima sessão deste tutorial, usaremos de tipos para que isso seja garantido. Isso exigirá que o usuário programe usando a linguagem de máquina da nossa VM (usando explicitamente os construtores das instruções).
```haskell
initialize :: [Int] -> VMState
initialize code = VMState {
    pc      = 0,
    regs    = [(r, 0) | r <- [R0 .. R4]],
    stack   = [],
    program = compile code,
    halted  = False,
    output  = Nothing
}
```
#### Compilação
Como dito anteriormente, a função `compile` funciona como um compilador (ou um parser). Percorre o `code` e devolve um programa capaz de ser lido pela nossa VM. Caso um código inválido seja dado como input, compile lançará um erro de execução. Isso poderá ser resolvido na segunda sessão deste tutorial.
```haskell
compile :: [Int] -> [Instruction]
compile [] = []
compile (10 : dst : src : rest)         = MOVR (toEnum dst) (toEnum src) : compile rest
compile (11 : dst : val : rest)         = MOVV (toEnum dst) (Imm val) : compile rest
compile (20 : dst : src1 : src2 : rest) = ADD (toEnum dst) (toEnum src1) (toEnum src2) : compile rest
compile (21 : dst : src1 : src2 : rest) = SUB (toEnum dst) (toEnum src1) (toEnum src2) : compile rest
compile (30 : src : rest)               = PUSH (Reg (toEnum src)) : compile rest
compile (31 : dst : rest)               = POP (toEnum dst) : compile rest
compile (40 : addr : rest)              = JP addr : compile rest
compile (41 : r1 : r2 : addr : rest)    = JL (toEnum r1) (toEnum r2) addr : compile rest
compile (42 : addr : rest)              = CALL addr : compile rest
compile (50 : rest)                     = RET : compile rest
compile (60 : src : rest)               = PRINT (Reg (toEnum src)) : compile rest
compile (99 : rest)                     = HALT : compile rest
compile _                               = error "Program cannot be read. You are using a instruction code that does not exist."
```
#### Execução do programa
Por fim, a função `execute` é o cerne da implementação de cada uma das instruções. Ela usa de algumas funções auxiliares abaixo para extrair tipos de operadores e acessar registradores específicos.
```haskell
execute :: Instruction -> VMState -> VMState
execute HALT state = state { halted = True }

execute (MOVR dst src) state =
    let srcVal = lookupReg src (regs state)
    in state { regs = updateReg dst srcVal (regs state) }
    
execute (MOVV dst (Imm value)) state =
    state { regs = updateReg dst value (regs state) }
    
execute (ADD dst src1 src2) state =
    let val1 = lookupReg src1 (regs state)
        val2 = lookupReg src2 (regs state)
        newVal = val1 + val2
    in state { regs = updateReg dst newVal (regs state) }
    
execute (SUB dst src1 src2) state =
    let val1 = lookupReg src1 (regs state)
        val2 = lookupReg src2 (regs state)
        newVal = val1 - val2
    in state { regs = updateReg dst newVal (regs state) }
    
execute (PUSH op) state =
    state { stack = op : stack state }
    
execute (POP dst) state =
    case stack state of
        [] -> state
        (op:ops) -> state { regs = updateReg dst (extractOperandValue op (regs state)) (regs state), stack = ops }
        
execute (JP addr) state = 
    state { pc = addr }
    
execute (JL r1 r2 addr) state =
    let r1Val = lookupReg r1 (regs state)
        r2Val = lookupReg r2 (regs state)
    in if r1Val < r2Val then state { pc = addr } else state
    
execute (CALL addr) state =
    let returnAddress = pc state
    in state { stack = Addr returnAddress : stack state, pc = addr }
    
execute (RET) state =
    case stack state of
        (Addr retAddr:ops) -> state { pc = retAddr, stack = ops }
        _ -> state { halted = True }
        
execute (PRINT op) state =
    state { output = Just (extractOperandValue op (regs state)) }
```
```haskell
lookupReg :: Register -> [(Register, Int)] -> Int
lookupReg reg regList = maybe 0 id $ lookup reg regList

updateReg :: Register -> Int -> [(Register, Int)] -> [(Register, Int)]
updateReg reg newVal regList = (reg, newVal) : filter ((/= reg) . fst) regList

extractOperandValue :: Operand -> [(Register, Int)] -> Int
extractOperandValue (Reg r) regs = lookupReg r regs
extractOperandValue (Val (Imm i)) _ = i
extractOperandValue (Addr a) _ = a
```
#### Loop de execução da VM
A função `runVM` é a responsável por garantir que todo o programa compilado será executado, e lidar com o output da instrução `PRINT`.
```haskell
runVM :: VMState -> IO VMState
runVM state@VMState { halted = True } = return state
runVM state@VMState { pc = pcVal, program = prog } = do
    let instruction = prog !! pcVal
        newState = execute instruction state { pc = pcVal + 1, output = Nothing }
    case output newState of
        -- There are comments here that can be disabled to help debugging the VM. I have turned them off so the output is not that busy.
        Just val -> print val {- >>  print (sortRegisters $ regs state) -} >> runVM (newState { output = Nothing })
        Nothing  -> {-  print (sortRegisters $ regs state) >> print (state) >> -} runVM newState
```
Como visto no código, há partes desta função que estão comentadas. Estas partes imprimem o estado atual da máquina como um todo, ou ainda apenas os registradores, e podem servir como um mecanismo de debugar o código escrito.

Com a VM inteiramente implementada, nos basta checar como são executados os programas nesta máquina. Caso queira, você pode acessar o código completo da VM [aqui](https://github.com/luisguirc/haskellvm/blob/main/Main.hs).

## Executando um programa
Vimos até agora como a VM foi construída, quais seus componentes, e seu *set* de instruções. Nesta sessão, veremos como a implementação da VM pode ser usada para construir programas simples a sofisticados.

### 1) Soma de inteiros de 1 a N

O objetivo deste programa é calcular a soma dos inteiros de 1 a N, onde N é um número pré-definido. Para simplificar, vamos definir N como 5. Usaremos os registradores
-   **R0**  para armazenar a soma acumulada.
-   **R1**  para armazenar o número atual que está sendo adicionado (que será incrementado a cada iteração do loop).
-   **R2**  para armazenar o número final, N=5, para comparação.

#### Código compilável para a VM
```haskell
programSumN :: [Int]
programSumN =
    [ 11, 0, 0          -- MOVV R0, 0 (Inicializa R0 em 0 para soma)
    , 11, 1, 1          -- MOVV R1, 1 (Inicia o contador em 1)
    , 11, 2, 5          -- MOVV R2, 5 (Define R2 em 5, o limite N)
    , 20, 0, 0, 1       -- ADD R0, R0, R1 (Soma R0 + R1 e armazena em R0)
    , 11, 1, 1          -- MOVV R3, 1 (Armazena 1 em R3 para incremento)
    , 20, 1, 1, 3       -- ADD R1, R1, R3 (Incrementa R1 em 1)
    , 41, 1, 2, 4       -- JL R1, R2, 4 (Se R1 <= R2 pula para a instrução no índice 4)
    , 60, 0             -- PRINT R0 (Imprime a soma de R0)
    , 99                -- HALT (Finaliza a execução)
    ]
```
O resultado é o esperado: 10.
```haskell
> ghci> main
> 10
> [(R0,10),(R1,5),(R2,5),(R3,1),(R4,0)]
```

### 2) Testando as instruções CALL, RET, PUSH e POP
O objetivo deste programa é simplesmente testar o funcionamento das instruções acima. Este programa soma dois números inicializados nos registradores R1 e R2 (3 e 4), os coloca na pilha, desempilha nos registradores R3 e R4, e guarda o resultado da soma em R0, imprimindo ao final. É interessante registrar que a pilha guarda Operadores, ou seja, tanto endereços, quanto valores imediatos.
```haskell
programAddTwoNums :: [Int]
programAddTwoNums =
    [ 11, 1, 3          -- MOVV R1, 3 
    , 11, 2, 4          -- MOVV R2, 4
   
    , 42, 5             -- CALL 5
    , 60, 0
    , 99                -- HALT

    , 30, 1             -- PUSH R1 at 4
    , 30, 2             -- PUSH R2
    , 31, 3             -- POP R3
    , 31, 4             -- POP R4
    , 20, 0, 3, 4       -- ADD  R0, R3, R4
    , 50                -- RET
    ]
```
O resultado é o esperado: 7.
```haskell
> ghci> main
> 7
> [(R0,7),(R1,3),(R2,4),(R3,4),(R4,3)]
```
Isso permitiria, portanto, escrever um programa recursivo, que usa de blocos de código e instruções de CALL, RET, JP e JL, assim como pode ser feito em *Assembly*, por exemplo (por mais que de uma forma muito mais rudimentar).

### 3) N-ésimo número de Fibonacci
Por fim, o objetivo deste programa é calcular o n-ésimo número de Fibonacci, e foi concebido como o objetivo inicial desta máquina virtual em Haskell. O código se encontra a seguir:
```haskell
programFib :: [Int]
programFib = 
    [ 11, 0, 10          -- MOVV R0, 10 (Calcula Fibonacci para n=10)
    , 11, 1, 0           -- MOVV R1, 0 (Fibonacci(0))
    , 11, 2, 1           -- MOVV R2, 1 (Fibonacci(1))
    , 11, 3, 2           -- MOVV R3, 2 (Para checar se o cálculo deve continuar)
    , 41, 0, 3, 12       -- JL R0, R3, Addr 12  (se R0 < R3 pule para o endereço 12 -> PRINT R1)
    , 20, 1, 1, 2        -- ADD R1, R1, R2 (R1 = R1 + R2; novo número de Fibonacci)
    , 10, 3, 2           -- MOVR R3, R2  (R3 = antigo R2)
    , 10, 2, 1           -- MOVR R2, R1  (R2 = antigo R1)
    , 10, 1, 3           -- MOVR R1, R3  (R1 = antigo R2 armazenado em R3)
    , 11, 3, 1           -- MOVV R3, 1   (Para decrementar R0)
    , 21, 0, 0, 3        -- SUB R0, R0, R3 (R0 -= 1)
    , 40, 4              -- JP Addr 4    (Volta para o início do loop)
    , 60, 1              -- PRINT R1
    , 99                 -- HALT
    ]
```
O resultado é o esperado: 55.
```haskell
> ghci> main
> 55
> [(R0,0),(R1,55),(R2,89),(R3,1),(R4,0)]
```
Este programa faz uso de instruções de salto para outras partes do bloco de código, exibindo um comportamento mais sofisticado.

## Melhorando com Type Families

### Por que usar famílias de tipos?

As  Famílias de Tipos, ou **Type Families**, são uma poderosa extensão de Haskell que permite a definição de funções ao nível dos tipos. Em essência, as *Type Families* permitem a especialização de tipos baseados em outros tipos, oferecendo uma forma de realizar computação diretamente nos tipos. Isso oferece aos desenvolvedores a capacidade de codificar condições (constraints) complexas diretamente nos tipos de suas funções, fortalecendo a segurança de tipos e minimizando erros.

As *Type Families* são particularmente úteis quando há a necessidade de estabelecer uma forte correlação entre diferentes tipos de dados, ou quando queremos variar a implementação de algo baseado em particularidades do tipo de dados.

Por exemplo: ao definir uma máquina virtual, poderíamos utilizar uma *Type Familiy* para garantir que o último comando de um programa seja sempre um comando de HALT (como dito anteriormente). Isso evita que um programa seja definido sem uma instrução de parada, garantindo uma classe inteira de erros de tempo de execução possíveis seja eliminada em tempo de compilação.

Essa capacidade de realizar cálculos e verificações em tempo de compilação fornece um aumento significativo na robustez do código. Ao aproveitar as *Type Families*, transformamos potenciais erros de tempo de execução, que podem ser difíceis de rastrear e resolver, em erros de compilação explícitos e gerenciáveis. Essa abordagem traz consigo uma maior confiança na correção do programa antes mesmo de ele ser executado, ou seja, antes de ter consequências mais severas em geral.


### Garantindo que o programa encerra em HALT em tempo de compilação

#### Mudanças necessárias
Para início, devemos definir uma estrutura de Vector que tenha seu tamanho codificado a nível de tipo, para que possamos checar seu último elemento a tempo de compilação. Também definiremos a ordem de prioridade de avaliação, para que possamos usar o construtor (:>) de uma forma mais intuitiva.
```haskell
data Vector :: Nat -> * -> * where
    Nil  :: Vector 0 a
    (:>) :: a -> Vector n a -> Vector (n + 1) a
infixr 5 :>
```
Definimos, agora, a type family responsável por checar se o último elemento de um `Vector` é a instrução HALT. Essa é uma definição recursiva, e levantará um TypeError em tempo de compilação caso o programa não cumpra este `Constraint`.
```haskell
type family LastIsHalt (n :: Nat) (v :: Vector n Instruction) :: Constraint where
    LastIsHalt 1 (HALT :> Nil)  = ()
    LastIsHalt (n + 1) (_ :> v) = LastIsHalt n v
    LastIsHalt _ _             = TypeError ('Text "Program must end with HALT instruction.")
```
Por fim, definimos o programa:
```haskell
type family Program (n :: Nat) (xs :: Vector n Instruction) :: Constraint where
    Program n xs = LastIsHalt n xs
```
Assim, temos o aparato necessário para ao menos escrever nossas instruções. Ou, pelo menos, é o que acreditamos...

#### Limitações do GHC
Por mais que pareça redundante, no contexto do tipo  `Vector`, e das *types families* `LastIsHalt`  e  `Program`, a parte `(n :: Nat)`  das definições é extremamente necessária, pois:

- Deixa explícita a conexão entre o tamanho do vetor (codificado em seu tipo) e o primeiro argumento passado para as *type families*.
- 
-   `(n :: Nat)` permite ao GHC enxergar o tamanho do vetor no nível de tipo. É necessário, por exemplo, para fazer o *pattern matching* com o tamanho do vetor na definição de cada uma das *type families*.

Portanto, já que este "número" está codificado no tipo `Vector`, é necessário para a computação no nível de tipo e verificação que desejamos executar.

De acordo com [GHC+DH Weekly Update #5, 2023-01-25](https://discourse.haskell.org/t/ghc-dh-weekly-update-5-2023-01-25/5662) e [Serokell’s Work on GHC: Dependent Types](https://serokell.io/blog/ghc-dependent-types-in-haskell), há esforços para simplificar estas definições, de forma que `n` neste caso poderia ser diretamente deduzido do tipo `Vector n a`. Infelizmente, por agora, isso ainda não é possível.

Assim, ao tentar compilar o arquivo que faz uso das *Type Families*, obtém-se o seguinte erro:
```haskell
TypeFamilies.hs:14:19: error:
    • Couldn't match kind ‘n’ with ‘1’
      Expected kind ‘Vector n Instruction’,
        but ‘HALT :> Nil’ has kind ‘Vector (0 + 1) Instruction’
    • In the second argument of ‘LastIsHalt’, namely ‘(HALT :> Nil)’
      In the type family declaration for ‘LastIsHalt’
      NB: Type ‘LastIsHalt’ was inferred to use visible dependent quantification.
      Most types with visible dependent quantification are
      polymorphically recursive and need a standalone kind
      signature. Perhaps supply one, with StandaloneKindSignatures.
   |
14 |     LastIsHalt 1 (HALT :> Nil)  = ()
   |                   ^^^^^^^^^^^
Failed, no modules loaded.
``` 
que exibe que o compilador não consegue identificar a equivalência entre alguns tipos.

## Conclusão
Este tutorial explorou uma abordagem prática de desenvolvimento de uma máquina virtual (VM) em Haskell, usando os recursos de Tipos de Dados Algébricos Generalizados (*GADTs*) e Famílias de Tipos (*Type Families*). Os recursos robustos de verificação de tipo do Haskell foram explorados e a utilidade dos *GADTs* e *Type Families* foi demonstrada em situações reais de desenvolvimento.

Entretanto, o tutorial teve seus desafios ao tentar implementar um grau mais alto de verificação de tipo em tempo de compilação usando Famílias de Tipos. Ficou destacada a dificuldade de compilação do código Haskell no GHC, devido à ausência de algumas funcionalidades na linguagem, ou ainda falta de conhecimento técnico na implementação do código.

Como etapas futuras, vejo a tentativa de implementação total de uma VM no nível dos tipos, tendo elementos como a *Stack*, os valores armazenados nos registradores e o próprio programa codificados em nível dos *GADTs* e *Type Families*.

Espero que este tutorial tenha te agradado, e que você tenha sentido vontade de explorar essa VM por conta própria. Sinta-se à vontade em construir seu próprio programa, e me enviar o que codificou!

Luis Crosselli
UFABC
