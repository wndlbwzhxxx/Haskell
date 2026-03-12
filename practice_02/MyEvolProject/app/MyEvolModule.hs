module MyEvolModule
    ( MyEvolution(..)
    , MyEvolution'(..)
    ) where

data MyEvolution = LUCA 
                 | Cyanobacteria 
                 | Trilobite 
                 | Ichthyostega 
                 | Dimetrodon 
                 | Archaeopteryx 
                 | Morganucodon 
                 | Purgatorius 
                 | Australopithecine 
                 | Humans


instance Show MyEvolution where
    show LUCA              = "Last Universal Common Ancestor"
    show Cyanobacteria     = "Synechococcus"
    show Trilobite         = "Paradoxides"
    show Ichthyostega      = "Ichthyostega"
    show Dimetrodon        = "Dimetrodon"
    show Archaeopteryx     = "Archaeopteryx"
    show Morganucodon      = "Morganucodon"
    show Purgatorius       = "Purgatorius"
    show Australopithecine = "Australopithecus Afarensis"
    show Humans            = "Homo Sapiens"


instance Read MyEvolution where
    readsPrec _ str = case str of
        "Last Universal Common Ancestor" -> [(LUCA, "")]
        "Synechococcus"                  -> [(Cyanobacteria, "")]
        "Paradoxides"                    -> [(Trilobite, "")]
        "Ichthyostega"                   -> [(Ichthyostega, "")]
        "Dimetrodon"                     -> [(Dimetrodon, "")]
        "Archaeopteryx"                  -> [(Archaeopteryx, "")]
        "Morganucodon"                   -> [(Morganucodon, "")]
        "Purgatorius"                    -> [(Purgatorius, "")]
        "Australopithecus Afarensis"     -> [(Australopithecine, "")]
        "Homo Sapiens"                   -> [(Humans, "")]
        _                                -> []


instance Eq MyEvolution where
    (==) LUCA LUCA                   = True
    (==) Cyanobacteria Cyanobacteria = True
    (==) Trilobite Trilobite         = True
    (==) Ichthyostega Ichthyostega   = True
    (==) Dimetrodon Dimetrodon       = True
    (==) Archaeopteryx Archaeopteryx = True
    (==) Morganucodon Morganucodon   = True
    (==) Purgatorius Purgatorius     = True
    (==) Australopithecine Australopithecine = True
    (==) Humans Humans               = True
    (==) _ _                         = False
    (/=) x y = not (x == y)


instance Ord MyEvolution where
    compare LUCA LUCA = EQ
    compare LUCA _ = LT
    compare _ LUCA = GT
    
    compare Cyanobacteria Cyanobacteria = EQ
    compare Cyanobacteria _ = LT
    compare _ Cyanobacteria = GT
    
    compare Trilobite Trilobite = EQ
    compare Trilobite _ = LT
    compare _ Trilobite = GT
    
    compare Ichthyostega Ichthyostega = EQ
    compare Ichthyostega _ = LT
    compare _ Ichthyostega = GT
    
    compare Dimetrodon Dimetrodon = EQ
    compare Dimetrodon _ = LT
    compare _ Dimetrodon = GT
    
    compare Archaeopteryx Archaeopteryx = EQ
    compare Archaeopteryx _ = LT
    compare _ Archaeopteryx = GT
    
    compare Morganucodon Morganucodon = EQ
    compare Morganucodon _ = LT
    compare _ Morganucodon = GT
    
    compare Purgatorius Purgatorius = EQ
    compare Purgatorius _ = LT
    compare _ Purgatorius = GT
    
    compare Australopithecine Australopithecine = EQ
    compare Australopithecine _ = LT
    compare _ Australopithecine = GT
    
    compare Humans Humans = EQ


instance Enum MyEvolution where
    toEnum 0 = LUCA
    toEnum 1 = Cyanobacteria
    toEnum 2 = Trilobite
    toEnum 3 = Ichthyostega
    toEnum 4 = Dimetrodon
    toEnum 5 = Archaeopteryx
    toEnum 6 = Morganucodon
    toEnum 7 = Purgatorius
    toEnum 8 = Australopithecine
    toEnum 9 = Humans
    toEnum n = error $ "toEnum: no such constructor " ++ show n
    
    fromEnum LUCA              = 0
    fromEnum Cyanobacteria     = 1
    fromEnum Trilobite         = 2
    fromEnum Ichthyostega      = 3
    fromEnum Dimetrodon        = 4
    fromEnum Archaeopteryx     = 5
    fromEnum Morganucodon      = 6
    fromEnum Purgatorius       = 7
    fromEnum Australopithecine = 8
    fromEnum Humans            = 9


instance Bounded MyEvolution where
    minBound = LUCA
    maxBound = Humans


data MyEvolution' = LUCA' 
                  | Cyanobacteria' 
                  | Trilobite' 
                  | Ichthyostega' 
                  | Dimetrodon' 
                  | Archaeopteryx' 
                  | Morganucodon' 
                  | Purgatorius' 
                  | Australopithecine' 
                  | Humans'
                  deriving (Show, Read, Eq, Ord, Enum, Bounded)