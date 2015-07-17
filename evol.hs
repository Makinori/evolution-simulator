import Data.Maybe
import System.Random
import Control.Applicative
import Control.Monad

-- define Type, Data
data Dir   = UP | SAME | DOWN deriving (Show) -- directioin
type FielX = Int
type FielY = Int
type Life  = Int
type BasePair = (Dir, Dir)
type Gene  = [BasePair]

data Grass = Grass {gX :: FielX,
                    gY :: FielY,
                    gLife :: Life} deriving (Show)
 
data Animal = Animal {aX ::FielX,
                      aY :: FielY,
                      aLife :: Life,
                      gene :: Gene} deriving (Show)

data Field = Field {grasses :: [Grass], 
                    animals :: [Animal]} 

-- constants                    
mapX ,mapY, geneLong, grassLife, errorPercent, sproutGrassNum :: Int
mapX = 60
mapY = 20
geneLong = 12
grassLife = 30
errorPercent = 20
sproutGrassNum = 5


grassXY  :: Grass -> (FielX, FielY)
animalXY :: Animal -> (FielX, FielY)
grassXY  gra = (gX gra, gY gra)
animalXY ani = (aX ani, aY ani)
breedCost = 300

randInt :: Int -> Int -> IO Int
randInt a b = getStdRandom (randomR (a,b))

randDir :: IO Dir
randDir = do
    num <- getStdRandom (randomR (0,2)) :: IO Int
    return (case num of
              0 -> UP
              1 -> SAME
              2 -> DOWN)
    
coordLiving :: FielX -> FielY -> [(FielX, FielY)]-> Bool
coordLiving x y = any ((==) y. snd) .filter ((==) x. fst)


-- field functions 
showField :: Field -> IO()
showField (Field {grasses=gs, animals=as}) = mapM_ (\y -> putStrLn $ map (flip blockLiving y) [1..mapX]) [1..mapY]
  where graXYs = map grassXY gs
        aniXYs = map animalXY as
        blockLiving :: Int -> Int -> Char
        blockLiving x y
            | coordLiving x y aniXYs = 'M'
            | coordLiving x y graXYs = '*'
            | otherwise       = '.'

initField :: IO Field
initField = do
    newAnimal <- initAnimal
    return (Field {grasses=[], animals=[newAnimal]})
  where initAnimal = do
            thisGene <- sequence $ map (const (randDir >>= \a -> randDir >>= return. (,) a)) [1..geneLong]
            return (Animal {aLife=10000, aX=10, aY=5, gene= thisGene})

updateField :: Field -> IO Field
updateField (Field {grasses=gs, animals=as}) = do
    updatedGrasses <- updateGrasses gs
    addedGrass <- sequence $ map (const addGrass) [1..sproutGrassNum]
    newAnimals <- updateAnimals as
    return (Field {grasses=addedGrass++updatedGrasses, animals=newAnimals})
  where
    grassesXYLifeLis :: [((FielX, FielY), Int)] -- int means life
    grassesXYLifeLis = map (\gras -> (grassXY gras, gLife gras)) gs
    animalsXYLis :: [(FielX, FielY)]
    animalsXYLis = map animalXY as

    -- grass update
    updateGrass :: Grass -> Maybe Grass
    updateGrass g
       | gLife g <= 0 = Nothing  -- die
       | coordLiving (gX g) (gY g) animalsXYLis = Nothing  -- eaten by animals
       | otherwise = Just Grass {gX=gX g, gY=gY g, gLife=gLife g-1}
    updateGrasses :: [Grass] -> IO [Grass]
    updateGrasses = return.catMaybes. map updateGrass
    addGrass :: IO Grass
    addGrass = do
        gX' <- randInt 1 mapX
        gY' <- randInt 1 mapY
        return $ Grass {gX=gX', gY=gY', gLife=grassLife}

    -- aniaml update
    updateAnimal :: Animal -> [IO Animal]
    updateAnimal ani@(Animal {aLife=al, aX=ax, aY=ay, gene=gen@(g@(fs,sn):gs)})
        | al <= 0 = []
        | breedCost <= al = [return $ moveAnimal breedCost, bornAnimal ani]
        | otherwise = [return $ moveAnimal 1]
      where moveAnimal :: Int -> Animal
            moveAnimal sharpenLife = Animal {aLife=al+ eat ax ay- sharpenLife, aX=move mapX fs ax, aY=move mapY sn ay, gene=gs++[g]}
            move :: Int -> Dir -> Int -> Int
            move fiel_max whichGo now 
                | fiel_max < nextWhere' = 1
                | 0 >= nextWhere'       = fiel_max
                | otherwise             = nextWhere'
              where nextWhere' = now + case whichGo of 
                      UP   -> -1
                      SAME -> 0
                      DOWN -> 1
            eat :: FielX -> FielY -> Int
            eat x y = sum $ map snd $ filter ((==) y. snd.fst) $ filter ((==) x. fst.fst) grassesXYLifeLis
            bornAnimal :: Animal -> IO Animal
            bornAnimal ani = copyGene (gene ani)>>= \newGene -> return $ Animal {aX=aX ani, aY=aY ani, aLife=breedCost-1, gene=newGene}
            copyGene :: Gene -> IO Gene
            copyGene  = sequence. map (\g -> randInt 1 100 >>= \r -> if r < errorPercent then variation else return g )
            variation :: IO BasePair
            variation = randDir >>= \a -> randDir >>= \b -> return (a, b)
    updateAnimals :: [Animal] -> IO [Animal]
    updateAnimals  = sequence. concatMap updateAnimal


-- UI
game :: Field -> IO()
game fiel@(Field {grasses=gs,animals=as}) = do
    line <- getLine
    newFiel <- updateField fiel
    showField newFiel
    game newFiel
    
main = game =<<initField

