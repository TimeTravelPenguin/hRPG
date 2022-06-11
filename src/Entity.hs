{-# LANGUAGE RecordWildCards #-}

module Entity where

-- ============= Types =============

type AttackStat = Int

type DefenseStat = Int

type SpeedStat = Int

type AccuracyStat = Int

data StatModel = StatModel
  { statAttack :: Int,
    statDefense :: Int,
    statSpeed :: Int,
    statAccuracy :: Int
  }

data Entity = Entity
  { entityName :: String,
    entityStats :: StatModel
  }

data EntityKind
  = Player
  | Enemy

data EnemyKind
  = Slime
  | Skeleton
  | Spider
  | Genie
  deriving (Show)

-- ============= Levelling =============

_lvConstK :: Integer
_lvConstK = 1000

-- Result is an integer iff k is even. Result is floored.
lvToExp' :: Integer -> Integer -> Integer
lvToExp' k lv = floor . fromIntegral $ (k * lv * (lv - 1) + 1) `div` 2

lvToExp :: Integer -> Integer
lvToExp = lvToExp' _lvConstK

-- Result is an integer iff k is even. Result is floored.
expToLv' :: Integer -> Integer -> Integer
expToLv' k exp = floor $ 0.5 + sqrt (1 + 8 * exp' / k') / 2
  where
    exp' = fromIntegral exp
    k' = fromIntegral ((k + 1) `div` 2)

expToLv :: Integer -> Integer
expToLv = expToLv' _lvConstK

-- ============= Builders =============

mkStats :: AttackStat -> DefenseStat -> SpeedStat -> AccuracyStat -> StatModel
mkStats statAttack statDefense statSpeed statAccuracy = StatModel {..}

mkEnemy :: EnemyKind -> Entity
mkEnemy kind =
  let entityName = show kind
      mkStatModel Slime = mkStats 1 1 1 1
      mkStatModel Skeleton = mkStats 1 1 1 1
      mkStatModel Spider = mkStats 1 1 1 1
      mkStatModel Genie = mkStats 1 1 1 1
      entityStats = mkStatModel kind
   in Entity {..}