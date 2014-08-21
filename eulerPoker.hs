{-# LANGUAGE NoMonomorphismRestriction #-}
import Control.Monad
import Data.List
import Control.Applicative
import Data.Monoid
import Data.List.Split
import Control.Arrow ( (***) )

-- {{{ DataTypes 
data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Eq, Ord, Enum, Show)
    
instance Read Value where
    readsPrec _ value = 
        let tbl = zip "23456789TJQKA" [Two .. Ace]
        in case lookup (head value) tbl of
          Just r -> [(r, tail value)]
          Nothing -> error $ "Invalid rank: " ++ value
          
data Suit = Heart | Club | Diamond | Spade
    deriving (Eq, Enum, Show)

instance Read Suit where
    readsPrec _ value = 
        let tbl = zip "HCDS" [Heart .. Spade]
        in case lookup (head value) tbl of
          Just r -> [(r, tail value)]
          Nothing -> error $ "Invalid rank: " ++ value    
    
data HandValue = 
      HighCard Value
    | OnePair Value
    | TwoPair Value Value
    | ThreeOfKind Value
    | Straight Value
    | Flush  -- Suit does not change ord so not including
    | FullHouse Value
    | FourOfKind Value
    | StraightFlush Value -- Suit does not change ord so not including
   deriving (Eq, Ord, Read, Show)
   
data Card = Card {cardValue :: Value, cardSuit :: Suit}
    deriving (Eq, Read, Show)
    
instance Ord Card where
  (Card v1 _) `compare` (Card v2 _) = v1 `compare` v2

type Hand = [Card]
type Round = (Hand,Hand)
-- }}}

{- Run -}

main :: IO Int
main = liftM process $ readFile "poker.txt" 

process :: String -> Int
process = length 
    . filter doesPlayer1Win 
    . map getRound 
    . lines

{- Determine Round winner -}
doesPlayer1WinOld :: Round -> Bool
doesPlayer1WinOld (p1,p2) = (getBestHand p1 `compare` getBestHand p2)
    <> (getHighCard p1 `compare` getHighCard p2) == GT -- fallback on highcard if tie
    
doesPlayer1Win :: Round -> Bool
doesPlayer1Win round =  (isPlayer1Greater getBestHand round <>  isPlayer1Greater getHighCard round)  == GT 
   
isPlayer1Greater :: Ord a1 => (Hand -> a1) -> (Hand,Hand) -> Ordering
isPlayer1Greater rule hands = (uncurry compare) (mapTuple rule hands)
    
getBestHand :: Hand -> HandValue
getBestHand hand = stripMaybe . maximum 
    $ [getHighCard,getPair,getThreeOfKind,getTwoPair, getFullHouse,getFourOfKind,getFlush,getStraight,getStraightFlush] <*> [hand]

    
{- Each poker hand -}
getHighCard,getPair,getThreeOfKind,getTwoPair, getFullHouse,getFourOfKind,getFlush,getStraight,getStraightFlush :: Hand -> Maybe HandValue

getHighCard hand = return . HighCard $ high hand 
    where high hand = cardValue $ maximum hand

getPair hand 
     | null pair = Nothing
     | otherwise = Just $ OnePair (head pair)    
   where pair = getNPair 2 hand

getThreeOfKind hand 
     | null pair = Nothing
     | otherwise = Just $ ThreeOfKind (head pair)    
   where pair = getNPair 3 hand
   
getTwoPair hand 
     | length pair < 2 = Nothing
     | otherwise = Just $ TwoPair (head pair) (pair !! 1)    
   where pair = getNPair 2 hand
   
getFullHouse hand = do 
     OnePair pair <- getPair hand
     ThreeOfKind three <- getThreeOfKind hand
     return $ FullHouse three
     
getFourOfKind hand 
     | null pair = Nothing
     | otherwise = Just $ FourOfKind (head pair)    
   where pair = getNPair 4 hand
   
getFlush hand 
    | allTheSame $ map cardSuit hand = Just Flush
    | otherwise = Nothing
        
getStraight hand 
    | isStraight = Just . Straight . cardValue . last $ hand
    | otherwise = Nothing
   where
        isStraight = values == compareTo
        compareTo = [(head values)..(last values)]
        values = map cardValue . sort $ hand 
        
        
getStraightFlush hand = do 
     Flush <- getFlush hand
     Straight highcard <- getStraight hand
     return $ StraightFlush highcard

{- supporting code for hand determination -}
getNPair :: Int -> Hand ->  [Value]
getNPair n xs = map head 
    . filter (\x -> length x == n) 
    . groupHand $ xs

groupHand :: Hand -> [[Value]]
groupHand hand = group  . sort . map cardValue $ hand

allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = all (== head xs) (tail xs)
stripMaybe :: Maybe t -> t
stripMaybe (Just x) = x

mapTuple :: (b -> c) -> (b, b) -> (c, c)
mapTuple = join (***)

{- Parsing Code -}
getRound :: String -> Round
getRound hand = (first,second)
    where 
        split = splitOn " " hand
        first = getCard <$> take 5 split
        second = getCard <$> drop 5 split

getHand :: String -> Hand
getHand hand = getCard <$> splitOn " " hand

getCard :: String -> Card
getCard c = Card (readChar(head c)) (readChar(last c))

readChar :: Read a => Char -> a
readChar c = read (c:[])

getValueFromChar :: Char -> Value
getValueFromChar c = read (c:[])
