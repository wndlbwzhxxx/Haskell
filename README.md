# Функциональное программирование — практические работы

Репозиторий с решениями пяти практических работ по курсу «Функциональное программирование» на языке **Haskell**. Проект охватывает базовые конструкции языка, систему типов и классов типов, парсинг, монады и трансформеры монад, а также property-based тестирование с QuickCheck.

## Возможности

- **Работа №1 — основы Haskell:** работа с кортежами и списками (`myFST`, `myHead`, `myZip`, `myUnzip`), функции высшего порядка (`myFilter`, `myMap`, `myZipWith`, `myAll`, `myAny`), свёртки (`myFoldl1`, `myFoldr1`), развёртка (`myUnFoldr`), пользовательский тип `MyList`, а также предметная область «Тортики» с `newtype` (`Grams`, `Pieces`) и обработкой ошибок через `Either String`.
- **Работа №2 — классы типов и парсеры:** тип-сумма `MyEvolution` с ручной реализацией `Show`, `Read`, `Eq`, `Ord`, `Enum`, `Bounded` и аналог с `deriving`; собственные реализации `Foldable`/`Functor`/`Applicative` для `MyTree`, `MyMaybe`, `MyEither`; три варианта парсера выражений (`число*число`, `число+число`) — ручной на `MyMaybe`, на `Parsec` и на `Attoparsec`.
- **Работа №3 — эффекты и RWS:** генератор глитч-эффектов на изображениях (чистые функции + случайность через `IO` и `foldM`); поиск маршрута в лабиринте на монаде `RWS` (Reader — карта, Writer — лог, State — посещённые комнаты) без `do`-нотации, через взаимно рекурсивные функции.
- **Работа №4 — собственный трансформер монад:** реализация `MyStateT` с экземплярами `Functor`, `Applicative`, `Monad`, `MonadTrans`, `MonadIO`; текстовая игра-лабиринт на трёх трансформерах (`MyStateT`, `WriterT`, `ReaderT`) с командами `north`/`south`/`west`/`east`/`quit` и финишем.
- **Работа №5 — тестирование с QuickCheck:** функции `addMod` (сложение по модулю) и `reverseWords` (переворот порядка слов), кастомные генераторы `genWord`/`genSentence` и набор свойств (корректность, нейтральный элемент, коммутативность, инволюция), защита от деления на ноль через `Positive Int`.

## Стек

- **Язык:** Haskell (GHC)
- **Системы сборки:** Cabal, Stack
- **Библиотеки:** `parsec`, `attoparsec`, `QuickCheck`, `random`, `bytestring`, `mtl`
- **Формат документации:** PDF-отчёт (`otchet_lab1-5.pdf`)

## Требования

- Установленный [Stack](https://docs.haskellstack.org/) (или Cabal + GHC).
- Компилятор GHC (устанавливается автоматически через Stack).
- Для работы с изображениями (работа №3) — стандартные пакеты `bytestring` и `random`.

## Структура репозитория

```text
KostinaAVFP/
├── practice_01/          # Основы Haskell
│   ├── Pr01_1.hs         # Кортежи, списки, функции высшего порядка
│   └── pr01_2.hs         # Свёртки, развёртки, тип MyList, предметная область «Тортики»
├── practice_02/          # Классы типов и парсеры
│   ├── MyEvolProject/    # cabal-проект: тип MyEvolution
│   ├── myParser/         # stack-проект: MyTree, MyMaybe, MyEither, 3 парсера
│   ├── pr02_1.hs
│   ├── pr02_2.hs
│   └── pr02_3.hs
├── practice_03/          # Эффекты и RWS
│   ├── glitcher/         # stack-проект: глитч-эффекты
│   ├── labirint/         # stack-проект: поиск пути на RWS
│   ├── pr03_1.hs
│   └── pr03_2.hs
├── practice_04/          # Трансформеры монад
│   ├── labirint-game/    # stack-проект: MyStateT + игра-лабиринт
│   └── pr04_1.hs
├── practice_05/          # QuickCheck
│   ├── quick/            # stack-проект: addMod, reverseWords, тесты
│   └── pr05_1.hs
└── otchet_lab1-5.pdf     # Отчёт по всем работам
```

## Установка и сборка

1. **Клонировать репозиторий**
```bash
git clone https://github.com/wndlbwzhxxx/KostinaAVFP
cd KostinaAVFP
```

2. **Собрать конкретный stack-проект** (например, парсер из работы №2)
```bash
cd practice_02/myParser
stack build
```

3. **Запустить проект**
```bash
stack run
```

Для cabal-проекта (`practice_02/MyEvolProject`):
```bash
cd practice_02/MyEvolProject
cabal build
cabal run
```

Отдельные файлы (`.hs`) из папок `practice_01` и части `practice_02/03` можно загрузить в GHCi:
```bash
ghci Pr01_1.hs
```

## Использование

### Работа №1 — основы
Загрузите `Pr01_1.hs` или `pr01_2.hs` в GHCi и вызывайте функции напрямую:
```haskell
ghci> myZip [1,2,3] "abc"
[(1,'a'),(2,'b'),(3,'c')]
ghci> myUnFoldr (\x -> if x > 0 then Just (x, x-1) else Nothing) 5
[5,4,3,2,1]
```

### Работа №2 — парсеры
В `myParser/Main.hs` реализован единый интерфейс вызова трёх парсеров (ручного, Parsec, Attoparsec). Запуск:
```bash
stack run
```
На вход подаются выражения вида `12*34` или `5+7`.

### Работа №3 — глитчер и лабиринт
```bash
cd practice_03/glitcher
stack run       # генерирует 5 версий глитч-изображений

cd practice_03/labirint
stack run       # ищет путь по лабиринту из Genshin Impact
```

### Работа №4 — игра-лабиринт
```bash
cd practice_04/labirint-game
stack run
```
Команды в игре: `north`, `south`, `west`, `east`, `quit`. При достижении комнаты `finish` игра завершается победой и выводит число ходов и полный лог перемещений.

### Работа №5 — тесты QuickCheck
```bash
cd practice_05/quick
stack test
```
Или в GHCi:
```haskell
ghci> quickCheck prop_addMod_correct
ghci> quickCheck $ verbose prop_addMod_correct        -- подробный вывод
ghci> quickCheck $ withMaxSuccess 5 prop_addMod_correct  -- изменить число тестов
```

## Архитектура

- **`practice_01`** — чистые функции, без внешних зависимостей. Демонстрируют рекурсию, свёртки, работу с `Maybe`/`Either` и пользовательскими типами.
- **`practice_02/MyEvolProject`** — cabal-проект с модулем `MyEvolModule.hs`. Тип `MyEvolution` — цепочка эволюции от LUCA до Humans; все экземпляры классов типов реализованы вручную, а `MyEvolution'` — через `deriving`.
- **`practice_02/myParser`** — stack-проект. Модули `MyTypes/MyTree`, `MyMaybe`, `MyEither` реализуют соответствующие классы типов; `MyParsers/MyParser`, `ParsecParser`, `AttoparsecParser` — три реализации парсера с общим интерфейсом.
- **`practice_03/glitcher`** — чистые функции (`replaceByte`, `sortSection`, `reverseSection`) вынесены в `Glitch.hs`, случайность добавляется в `IO`-обёртках, комбинирование эффектов — через `foldM`.
- **`practice_03/labirint`** — монада `RWS Maze MazeLog MazeState`: Reader хранит карту лабиринта, Writer — лог перемещений, State — текущую комнату, посещённые комнаты, путь и число шагов. Поиск — через `findPathFromCurrent` и `tryPaths` (аналог `some`/`many`), без `do`-нотации.
- **`practice_04/labirint-game`** — `MyStateT s m a = MyStateT { runMyStateT :: s -> m (a, s) }` с полным набором экземпляров. Игровая монада: `MyStateT GameState (WriterT GameLog (ReaderT Maze IO))`. Эффекты поднимаются через `lift`, стек раскручивается в `runGame`.
- **`practice_05/quick`** — `Lib.hs` с функциями, `test/Spec.hs` с генераторами и свойствами; тесты запускаются через `quickCheck`.

## График выполнения работ

| Работа | Дата сдачи | Ключевые коммиты |
|--------|-----------|------------------|
| №1 | 06.03.2026 | `cb83693`, `5b736d4`, `1245c68b` |
| №2 | 03.04.2026 | `1a133d66`, `2d314155`, `2b2322bb`, `25cc50ad`, `24474650` |
| №3 | 17.04.2026 | `b159023`, `3087fbea` |
| №4 | 24.04.2026 | `4b7e0d19`, `439ef964`, `48692e0e` |
| №5 | 15.05.2026 | `5346762a`, `5840a003`, `840a003` |

## Текущий статус

Все пять практических работ выполнены и защищены. Отчёт `otchet_lab1-5.pdf` содержит описание коммитов, вопросы и ответы на защите, а также листинги кода.

## Запланированные улучшения

- Расширить набор свойств QuickCheck для парсеров и трансформеров.
- Добавить CI (GitHub Actions) для автоматической сборки всех stack-проектов и запуска тестов.
- Перенести ручной парсер из работы №2 на общий интерфейс с Parsec/Attoparsec для сравнения производительности.

## Автор

Костина Анастасия, СПбПУ, ИКНК, группа 5130201/40001, 2 курс.

Преподаватель: Моторин Д.Е.
