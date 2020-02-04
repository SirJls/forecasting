{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Forecasting.Main
   ( forecast
   )
where

import           System.IO
import           System.FilePath
import           Control.Applicative
import           Options.Applicative
import           Text.Printf
import           GA
import           Data.Csv                       ( (.:)
                                                , FromNamedRecord
                                                , parseNamedRecord
                                                , decodeByName
                                                )
import qualified Data.ByteString.Lazy          as BL
import qualified Data.Vector                   as V
import Debug.Trace

-------------------------------------------------------------------------------
--                      CSV Operations                                       --
-------------------------------------------------------------------------------

data SwordData = SwordData {
     t      :: !Int
   , demand :: !Double
} deriving (Show)

instance FromNamedRecord SwordData where
   parseNamedRecord r = SwordData <$> r .: "t" <*> r .: "Demand"

decodeSwordData :: BL.ByteString -> Either String (V.Vector SwordData)
decodeSwordData = fmap snd . decodeByName

parseCsv :: String -> IO (V.Vector SwordData)
parseCsv dataSet = do
   csvData <- BL.readFile dataSet
   case decodeSwordData csvData of
      Left  _ -> error "Failed to parse the given data set"
      Right v -> return v

-------------------------------------------------------------------------------
--                      CLI operations                                       --
-------------------------------------------------------------------------------

usage :: IO ()
usage =
   putStr
      . unlines
      $ concat ["Usage: ", "Regression", "[OPTION]"]
      : "Options:"
      : " --version        Print the version number"
      : " --help           Print this message"
      : []

version :: IO ()
version = putStr "0.1.0.0"

data Input = ProgramInput
      {
        generate         :: Bool
      , datasetPath      :: String
      , outputScriptName :: String
      }
      | Version

generateFlag :: Parser Input
generateFlag =
   ProgramInput
      <$> switch
             (long "generate" <> short 'g' <> help "Generate the GNUplot script"
             )
      <*> strArgument (metavar "DATASET")
      <*> strArgument (metavar "FILENAME")

versionFlag = flag'
   Version
   (long "version" <> short 'v' <> help "Version number of the program")


-------------------------------------------------------------------------------
--                      Operations                                           --
-------------------------------------------------------------------------------

data ForecastRecord = ForecastRecord {
     ts                  :: V.Vector Int
   , demands             :: V.Vector Double
   , levelEstimates      :: V.Vector Double
   , trends              :: V.Vector Double
   , seasonalAdjustments :: V.Vector Double
   , oneStepForecasts    :: V.Vector Double
   , forecastErrors      :: V.Vector Double
   , squaredErrors       :: V.Vector Double
} deriving (Show)

instance Semigroup ForecastRecord where
   (ForecastRecord a c e g i k m o) <> (ForecastRecord b d f h j l n p) =
      ForecastRecord (a <> b) (c <> d) (e <> f) (g <> h) (i <> j) (k <> l) (m <> n) (o <> p)

instance Monoid ForecastRecord where
   mempty  = ForecastRecord V.empty V.empty V.empty V.empty V.empty V.empty V.empty V.empty
   mappend = (<>)

addOneStepForecast :: Double -> ForecastRecord -> ForecastRecord
addOneStepForecast n fr = fr { oneStepForecasts = V.snoc (oneStepForecasts fr) n }

addEstimate :: Double -> ForecastRecord -> ForecastRecord
addEstimate n fr = fr { levelEstimates = V.snoc (levelEstimates fr) n }

addForecastError :: Double -> ForecastRecord -> ForecastRecord
addForecastError n fr = fr { forecastErrors = V.snoc (forecastErrors fr) n }

addSquaredError :: Double -> ForecastRecord -> ForecastRecord
addSquaredError n fr = fr { squaredErrors = V.snoc (squaredErrors fr) n }

addDemand :: Double -> ForecastRecord -> ForecastRecord
addDemand n fr = fr { demands = V.snoc (demands fr) n }

addTrend :: Double -> ForecastRecord -> ForecastRecord
addTrend n fr = fr { trends = V.snoc (trends fr) n }

addSeasonalAdjustment :: Double -> ForecastRecord -> ForecastRecord
addSeasonalAdjustment n fr = fr { seasonalAdjustments = V.snoc (seasonalAdjustments fr) n }

addSeasonalAdjustments :: V.Vector Double -> ForecastRecord -> ForecastRecord
addSeasonalAdjustments ns fr = fr { seasonalAdjustments = seasonalAdjustments fr <> ns }

addDemands :: V.Vector Double -> ForecastRecord -> ForecastRecord
addDemands ns fr = fr { demands = demands fr <> ns }

addTs :: V.Vector Int -> ForecastRecord -> ForecastRecord
addTs ns fr = fr { ts = ts fr <> ns }

addTsAndDemands :: V.Vector Int -> V.Vector Double -> ForecastRecord -> ForecastRecord
addTsAndDemands as bs  fr = addDemands bs (addTs as fr) 

addT :: Int -> ForecastRecord -> ForecastRecord
addT n fr = fr { ts = V.snoc (ts fr) n }

addToForecastRecordSES
   :: ForecastRecord
   -> Int
   -> Double
   -> Double
   -> Double
   -> Double
   -> Double
   -> ForecastRecord
addToForecastRecordSES fr n1 n2 n3 n4 n5 n6 =
        addSquaredError n6
      . addForecastError n5
      . addOneStepForecast n4
      . addEstimate n3
      . addDemand n2
      . addT n1
      $ fr

addToForecastRecordDES
   :: ForecastRecord
   -> Int
   -> Double
   -> Double
   -> Double
   -> Double
   -> Double
   -> Double
   -> ForecastRecord
addToForecastRecordDES fr n1 n2 n3 n4 n5 n6 n7 =
        addSquaredError n7
      . addForecastError n6
      . addOneStepForecast n5
      . addTrend n4
      . addEstimate n3
      . addDemand n2
      . addT n1
      $ fr

addToForecastRecordTES
   :: ForecastRecord
   -> Int
   -> Double
   -> Double
   -> Double
   -> Double
   -> Double
   -> Double
   -> Double
   -> ForecastRecord
addToForecastRecordTES fr n1 n2 n3 n4 n5 n6 n7 n8 =
        addSquaredError n8
      . addForecastError n7
      . addOneStepForecast n6
      . addSeasonalAdjustment n5
      . addTrend n4
      . addEstimate n3
      . addDemand n2
      . addT n1
      $ fr

configSES :: GA.Config
configSES = GA.Config { GA.generations    = 100
                      , GA.constraint     = Just (Constraint 0 1)
                      , GA.crossoverRate  = 0.75
                      , GA.elitism        = True
                      , GA.mutationRate   = 0.075
                      , GA.populationSize = 20
                      , GA.chromLength    = 1
                      , GA.problemDesc    = Nothing
                      , GA.problemType    = MIN
                      }

configDES :: GA.Config
configDES = GA.Config { GA.generations    = 200
                      , GA.constraint     = Just (Constraint 0 1)
                      , GA.crossoverRate  = 0.85
                      , GA.elitism        = True
                      , GA.mutationRate   = 0.075
                      , GA.populationSize = 40
                      , GA.chromLength    = 2
                      , GA.problemDesc    = Nothing
                      , GA.problemType    = MIN
                      }

configTES :: GA.Config
configTES = GA.Config { GA.generations    = 500
                      , GA.constraint     = Just (Constraint 0 1)
                      , GA.crossoverRate  = 0.95
                      , GA.elitism        = True
                      , GA.mutationRate   = 0.01
                      , GA.populationSize = 40
                      , GA.chromLength    = 3
                      , GA.problemDesc    = Nothing
                      , GA.problemType    = MIN
                      }

initialSolution :: GA.Solution
initialSolution = Solution { GA.bestIndividual  = Nothing
                           , GA.population      = []
                           , GA.populationCount = 0
                           }

objectiveFunctionSES cds predictionOffset betas = 
   let r = ses cds predictionOffset (head betas)
   in standardErrorSES $ squaredErrors r

objectiveFunctionDES cds predictionOffset betas = 
   let [alpha, gamma] = betas
       r = des cds predictionOffset alpha gamma
   in standardErrorDES $ squaredErrors r

objectiveFunctionTES cds predictionOffset numberOfSeasonalFactorEstimates monthOfInterest betas = 
   let [alpha, gamma, delta] = betas
       r = tes cds predictionOffset numberOfSeasonalFactorEstimates monthOfInterest alpha gamma delta
   in standardErrorTES $ squaredErrors r

standardErrorSES :: V.Vector Double -> Double
standardErrorSES sqes =
   let histDemLength = fromIntegral (length sqes)
   in sqrt (sum sqes / (histDemLength - 1))

standardErrorDES :: V.Vector Double -> Double
standardErrorDES sqes =
   let histDemLength = fromIntegral (length sqes)
   in sqrt (sum sqes / (histDemLength - 2))

standardErrorTES :: V.Vector Double -> Double
standardErrorTES sqes =
   let histDemLength = fromIntegral (length sqes)
   in sqrt (sum sqes / (histDemLength - 3))

ses :: V.Vector SwordData -> Int -> Double -> ForecastRecord
ses cs j alpha = calculate 0 (mempty :: ForecastRecord)
 where
  calculate :: Int -> ForecastRecord -> ForecastRecord
  calculate i r 
   | i == 0 = calculate (i + 1) $ addEstimate l0 r
   | i == len = if j > 0 then let dms' = V.replicate j (last (levelEstimates r))
                                  ts'  = V.fromList $ let l = last (ts r) in [ l + x  | x <- [1 .. V.length dms']]
                              in addTsAndDemands ts' dms' r
                else r
   | otherwise = let les             = levelEstimates r
                     dm              = (V.!) dms (i - 1)
                     oneStepForecast = (V.!) les (i - 1)
                     tr              = trends r
                     forecastError   = dm - oneStepForecast
                     levelEstimate   = (V.!) les (i - 1) + alpha * forecastError
                     squaredError    = forecastError ** 2.0
                 in calculate (i + 1) (addToForecastRecordSES r i dm levelEstimate oneStepForecast forecastError squaredError)
  dms = V.map demand cs 
  last = V.head . V.reverse
  len = V.length dms + 1
  l0 = V.sum (V.take j dms) / fromIntegral j

-- Yes this can be optimized by calculating everything at once, no I don't care
trendline :: V.Vector Int -> V.Vector Double -> (Double, Double)
trendline xs ys = let sumX  = fromIntegral $ V.sum xs 
                      sumX2 = fromIntegral $ V.sum $ V.map (^2) xs
                      sumY  = V.sum ys
                      sumXY = V.foldr (\(x, y) acc -> (fromIntegral x * y) + acc) 0.0 (V.zip xs ys)
                      cnt   = fromIntegral $ length xs
                      slope = (sumXY - ((sumX * sumY) / cnt)) / (sumX2 - ((sumX * sumX) / cnt))
                      intercept = (sumY / cnt) - (slope * (sumX / cnt))
                  in (slope, intercept)

des :: V.Vector SwordData -> Int -> Double -> Double -> ForecastRecord
des cs j alpha gamma = calculate 0 (mempty :: ForecastRecord)
 where
  calculate :: Int -> ForecastRecord -> ForecastRecord
  calculate i r 
   | i == 0 = calculate (i + 1) $ addTrend slope (addEstimate intercept r)
   | i == len = if j > 0 then let dms'  = V.replicate j (last (levelEstimates r))
                                  l     = last tss
                                  ts'   = V.fromList $ [ l + x  | x <- [1 .. V.length dms']]
                                  tsdms = V.zip ts' dms'
                                  dms'' = V.map (\(i, d) -> d + fromIntegral (i - l) * last (trends r)) tsdms
                              in addTsAndDemands ts' dms'' r
                else r
   | otherwise = let les             = levelEstimates r
                     trs             = trends r
                     dm              = (V.!) dms (i - 1)
                     oneStepForecast = (V.!) les (i - 1) + (V.!) trs (i - 1)
                     forecastError   = dm - oneStepForecast
                     trend           = (V.!) trs (i - 1) + gamma * alpha * forecastError
                     levelEstimate   = (V.!) les (i - 1) + (V.!) trs (i - 1) +  alpha * forecastError
                     squaredError    = forecastError ** 2.0
                 in calculate (i + 1) (addToForecastRecordDES r i dm levelEstimate trend oneStepForecast forecastError squaredError)
  dms = V.map demand cs 
  last = V.head . V.reverse
  len = V.length dms + 1
  tss = V.map t cs
  l0Dm = fst $ V.splitAt ((len - 1) `div` 2) dms
  l0Ts = fst $ V.splitAt ((len - 1) `div` 2) tss
  (slope, intercept) = trendline l0Ts l0Dm


smooth n xs = V.ifoldr (\i _ acc -> let sv1 = V.slice i 12 xs
                                        sv2 = V.slice (i + 1) 12 xs
                                    in smooth' sv1 sv2 `V.cons` acc ) V.empty (V.fromList [1..n])
 where
  avg xs = let len = V.length xs in V.sum xs / fromIntegral len
  smooth' xs1 xs2 = (avg xs1 + avg xs2) / 2

seasonalFactors mnt end dms smo = let dms' = V.slice (mnt-1) end dms
                                      dmso = V.zip dms' smo
                                  in V.map (uncurry (/)) dmso

initialSeasonalFactors xs = let len = V.length xs
                                (left, right) = V.splitAt (len `div` 2) xs
                                xs' = V.zip left right
                                xs'' = V.map (\(x, y) -> (x + y) / 2.0) xs'
                                (left', right') = V.splitAt (length xs'' `div` 2) xs''
                            in  right' <> left'

tes :: V.Vector SwordData -> Int -> Int -> Int -> Double -> Double -> Double -> ForecastRecord
tes cs j n month alpha gamma delta = calculate 0 $ addSeasonalAdjustments sAdjustments (addTs negativeTs (mempty :: ForecastRecord))
 where
  calculate :: Int -> ForecastRecord -> ForecastRecord
  calculate i r 
   | i == 0 = calculate (i + 1) $ addT 0 (addSeasonalAdjustment sTopAdjustment' (addTrend slope (addEstimate intercept r)))
   | i == len = if j > 0 then let dms'  = V.replicate j (last (levelEstimates r))
                                  adjms = seasonalAdjustments r 
                                  l     = last tss
                                  adjmsIndex = l - n
                                  ts'   = V.fromList $ [ l + x  | x <- [1 .. V.length dms']]
                                  tsdms = V.zip ts' dms'
                                  dms'' = V.imap (\k (i, d) -> (d + fromIntegral (i - l) * last (trends r)) *  (V.!) adjms (adjmsIndex + k) ) tsdms
                              in addTsAndDemands ts' dms'' r
                else r
   | otherwise = let les             = levelEstimates r
                     trs             = trends r
                     sea             = seasonalAdjustments r
                     dm              = (V.!) dms (i - 1)
                     adjustment      = (V.!) sea  (i - 1)
                     oneStepForecast = ((V.!) les (i - 1) + (V.!) trs (i - 1)) * adjustment
                     forecastError   = dm - oneStepForecast
                     trend           = (V.!) trs (i - 1) + gamma * alpha * forecastError / adjustment
                     levelEstimate   = (V.!) les (i - 1) + (V.!) trs (i - 1) +  alpha * forecastError / adjustment
                     sSmoothParam    = adjustment + delta * (1 - alpha) * forecastError / (levelEstimate + trend)
                     squaredError    = forecastError ** 2.0
                 in calculate (i + 1) (addToForecastRecordTES r i dm levelEstimate trend sSmoothParam oneStepForecast forecastError squaredError)
  dms = V.map demand cs 
  last = V.head . V.reverse
  len = V.length dms + 1
  tss = V.map t cs
  smoothed = smooth n dms
  sfa = initialSeasonalFactors (seasonalFactors month n dms smoothed)
  sfaThrice = V.concat . V.toList $ V.replicate (length dms `div` V.length sfa) sfa
  deseasonalizedData = V.map (uncurry (/)) $ V.zip dms sfaThrice
  (slope, intercept) = trendline tss deseasonalizedData
  sAdjustmentsLength = V.length sfa
  (sAdjustments, sTopAdjustment) = V.splitAt (sAdjustmentsLength - 1) sfa
  sTopAdjustment' = V.head sTopAdjustment
  negativeTs = V.fromList [-(V.length sAdjustments)..(-1)]

-------------------------------------------------------------------------------
--                      Rounding / Printing / Writing                        --
-------------------------------------------------------------------------------

data Prefix = COMMENT | COMMENT_BAR | EMPTY deriving (Show, Enum)

fprs :: (Floating a, PrintfArg a) => [a] -> String
fprs = foldr (\s acc -> printf "%.2f" s <> " " <> acc) ""

fprsn :: (Floating a, PrintfArg a) => [a] -> Prefix -> String
fprsn l c = case c of
   COMMENT     -> foldr (\s acc -> "# " <> printf "%.2f" s <> "\n" <> acc) "" l
   COMMENT_BAR -> foldr (\s acc -> "# | " <> printf "%.2f" s <> "\n" <> acc) "" l
   _           -> foldr (\s acc -> printf "%.2f" s <> "\n" <> acc) "" l

fprsn' :: (Show a, Num a, PrintfArg a) => [a] -> Prefix -> String
fprsn' l c = case c of
   COMMENT     -> foldr (\s acc -> "# " <> show s <> "\n" <> acc) "" l
   COMMENT_BAR -> foldr (\s acc -> "# | " <> show s <> "\n" <> acc) "" l
   _           -> foldr (\s acc -> show s <> "\n" <> acc) "" l


fprsn2 :: (Floating a, PrintfArg a) => [(a, a)] -> String
fprsn2 = foldr
   (\(a, b) acc -> printf "%.2f" a <> ", " <> printf "%.2f" b <> "\n" <> acc)
   ""

fprsn2' :: (Show a, Num a, Floating b, PrintfArg b) => [(a, b)] -> String
fprsn2' = foldr
   (\(a, b) acc -> show a <> ", " <> printf "%.2f" b <> "\n" <> acc)
   ""

sesDataReport file record sse standardError histDemands futureDemands alpha = do
   handle <- openFile (file <> ".dat") WriteMode
   hPutStr handle (report <> content)
   hClose handle
 where
  report = ""
        <> "# |---------------------------------------------------------------\n"
        <> "# | SES Report                                                    \n"
        <> "# |---------------------------------------------------------------\n"
        <> "# | Total Months:                      " <> show histLength   <> "\n"               
        <> "# | Forecast Months:                   " <> show futLength    <> "\n"
        <> "# | Level smoothing parameter (alpha): " <> show alpha        <> "\n"
        <> "# | SSE:                               " <> show sse          <> "\n"
        <> "# | Standard Error:                    " <> show standardError<> "\n"
        <> "# |---------------------------------------------------------------\n"
        <> "# | T: " <>                                                      "\n"
        <>      fprsn' (V.toList $ ts record) COMMENT_BAR
        <> "# |---------------------------------------------------------------\n"
        <> "# | Demand: "                                         <>         "\n"
        <>      fprsn (V.toList $ demands record) COMMENT_BAR
        <> "# |---------------------------------------------------------------\n"
        <> "# | Level Estimate: "                                 <>         "\n"
        <>      fprsn (V.toList $ levelEstimates record) COMMENT_BAR
        <> "# |---------------------------------------------------------------\n"
        <> "# | One-step Forecast: "                              <>         "\n"
        <>      fprsn (V.toList $ oneStepForecasts record) COMMENT_BAR
        <> "# |---------------------------------------------------------------\n"
        <> "# | Forecast Error: "                                 <>         "\n"
        <>      fprsn (V.toList $ forecastErrors record) COMMENT_BAR
        <> "# |---------------------------------------------------------------\n"
        <> "# | Squared Error: "                                  <>         "\n"
        <>      fprsn (V.toList $ squaredErrors record) COMMENT_BAR
        <> "# |---------------------------------------------------------------\n"
        <> "# | Future Demand: "                                  <>         "\n"
        <>      fprsn futureDemands COMMENT_BAR
        <> "# |---------------------------------------------------------------\n"
  histLength = length histDemands
  content = fprsn2' $ zip (V.toList $ ts record) (V.toList $ demands record)
  futLength = length futureDemands

desDataReport file record sse standardError histDemands futureDemands alpha gamma = do
   handle <- openFile (file <> ".dat") WriteMode
   hPutStr handle (report <> content)
   hClose handle
 where
  report = ""
        <> "# |---------------------------------------------------------------\n"
        <> "# | DES Report                                                    \n"
        <> "# |---------------------------------------------------------------\n"
        <> "# | Total Months:                      " <> show histLength   <> "\n"               
        <> "# | Forecast Months:                   " <> show futLength    <> "\n"
        <> "# | Level smoothing parameter (alpha): " <> show alpha        <> "\n"
        <> "# | Trend smoothing parameter (gamma): " <> show gamma        <> "\n"
        <> "# | SSE:                               " <> show sse          <> "\n"
        <> "# | Standard Error:                    " <> show standardError<> "\n"
        <> "# |---------------------------------------------------------------\n"
        <> "# | T: " <>                                                      "\n"
        <>      fprsn' (V.toList $ ts record) COMMENT_BAR
        <> "# |---------------------------------------------------------------\n"
        <> "# | Actual Demand: "                                  <>         "\n"
        <>      fprsn (V.toList $ demands record) COMMENT_BAR
        <> "# |---------------------------------------------------------------\n"
        <> "# | Level : "                                         <>         "\n"
        <>      fprsn (V.toList $ levelEstimates record) COMMENT_BAR
        <> "# |---------------------------------------------------------------\n"
        <> "# | Trend : "                                         <>         "\n"
        <>      fprsn (V.toList $ trends record) COMMENT_BAR
        <> "# |---------------------------------------------------------------\n"
        <> "# | One-step Forecast: "                              <>         "\n"
        <>      fprsn (V.toList $ oneStepForecasts record) COMMENT_BAR
        <> "# |---------------------------------------------------------------\n"
        <> "# | Forecast Error: "                                 <>         "\n"
        <>      fprsn (V.toList $ forecastErrors record) COMMENT_BAR
        <> "# |---------------------------------------------------------------\n"
        <> "# | Squared Error: "                                  <>         "\n"
        <>      fprsn (V.toList $ squaredErrors record) COMMENT_BAR
        <> "# |---------------------------------------------------------------\n"
        <> "# | Future Demand: "                                  <>         "\n"
        <>      fprsn futureDemands COMMENT_BAR
        <> "# |---------------------------------------------------------------\n"
  histLength = length histDemands
  content = fprsn2' $ zip (V.toList $ ts record) (V.toList $ demands record)
  futLength = length futureDemands

tesDataReport file record sse standardError histDemands futureDemands alpha gamma delta predictionOffset = do
   handle <- openFile (file <> ".dat") WriteMode
   hPutStr handle (report <> content)
   hClose handle
 where
  report = ""
        <> "# |---------------------------------------------------------------\n"
        <> "# | DES Report                                                    \n"
        <> "# |---------------------------------------------------------------\n"
        <> "# | Total Months:                         " <> show histLength   <> "\n"               
        <> "# | Forecast Months:                      " <> show futLength    <> "\n"
        <> "# | Level smoothing parameter (alpha):    " <> show alpha        <> "\n"
        <> "# | Trend smoothing parameter (gamma):    " <> show gamma        <> "\n"
        <> "# | Seasonal smoothing parameter (delta): " <> show gamma        <> "\n"
        <> "# | SSE:                                  " <> show sse          <> "\n"
        <> "# | Standard Error:                       " <> show standardError<> "\n"
        <> "# |---------------------------------------------------------------\n"
        <> "# | T: " <>                                                      "\n"
        <>      fprsn' (V.toList $ ts record) COMMENT_BAR
        <> "# |---------------------------------------------------------------\n"
        <> "# | Actual Demand: "                                  <>         "\n"
        <>      fprsn (V.toList $ demands record) COMMENT_BAR
        <> "# |---------------------------------------------------------------\n"
        <> "# | Level : "                                         <>         "\n"
        <>      fprsn (V.toList $ levelEstimates record) COMMENT_BAR
        <> "# |---------------------------------------------------------------\n"
        <> "# | Trend : "                                         <>         "\n"
        <>      fprsn (V.toList $ trends record) COMMENT_BAR
        <> "# |---------------------------------------------------------------\n"
        <> "# | Seasonal Adjustment : "                           <>         "\n"
        <>      fprsn (V.toList $ seasonalAdjustments record) COMMENT_BAR
        <> "# |---------------------------------------------------------------\n"
        <> "# | One-step Forecast: "                              <>         "\n"
        <>      fprsn (V.toList $ oneStepForecasts record) COMMENT_BAR
        <> "# |---------------------------------------------------------------\n"
        <> "# | Forecast Error: "                                 <>         "\n"
        <>      fprsn (V.toList $ forecastErrors record) COMMENT_BAR
        <> "# |---------------------------------------------------------------\n"
        <> "# | Squared Error: "                                  <>         "\n"
        <>      fprsn (V.toList $ squaredErrors record) COMMENT_BAR
        <> "# |---------------------------------------------------------------\n"
        <> "# | Future Demand: "                                  <>         "\n"
        <>      fprsn futureDemands COMMENT_BAR
        <> "# |---------------------------------------------------------------\n"
  histLength = length histDemands
  -- content = fprsn2' $ zip (V.toList $ ts record) (V.toList $ demands record)
  ts' = ts record
  dms = demands record
  (_, ts_2) = V.splitAt predictionOffset ts'
  content = fprsn2' $ V.toList (V.zip ts_2 dms)
  futLength = length futureDemands

createSESPlotScript :: FilePath -> IO ()
createSESPlotScript filename = writeFile (filename <> ".plot") content
 where
  content =
     ""
        <> "set object 1 rectangle from screen 0,0 to screen 1,1 fillcolor rgb 'white' behind\n"
        <> "set key fixed left top vertical Right noreverse enhanced autotitle box lt black linewidth 1.000 dashtype solid\n"
        <> "set style increment default\n"
        <> "set title 'Simple Exponential Smoothing Forecast'\n"
        <> "set xlabel 'Time (Months)'\n"
        <> "set ylabel 'Demand (Swords)'\n"
        <> "set xtics 0,6,48\n"
        <> "set ytics 0,50,350\n"
        <> "set xrange [0:48]\n"
        <> "set yrange [0:350]\n"
        <> "plot '" <> (filename <> ".dat") <> "' using 1:2 with linespoint linecolor rgb 'grey' lw 4"

createDESPlotScript :: FilePath -> IO ()
createDESPlotScript filename = writeFile (filename <> ".plot") content
 where
  content =
     ""
        <> "set object 1 rectangle from screen 0,0 to screen 1,1 fillcolor rgb 'white' behind\n"
        <> "set key fixed left top vertical Right noreverse enhanced autotitle box lt black linewidth 1.000 dashtype solid\n"
        <> "set style increment default\n"
        <> "set title 'Simple Exponential Smoothing Forecast'\n"
        <> "set xlabel 'Time (Months)'\n"
        <> "set ylabel 'Demand (Swords)'\n"
        <> "set xtics 0,6,48\n"
        <> "set ytics 0,50,400\n"
        <> "set xrange [0:48]\n"
        <> "set yrange [0:400]\n"
        <> "plot '" <> (filename <> ".dat") <> "' using 1:2 with linespoint linecolor rgb 'grey' lw 4"

createTESPlotScript :: FilePath -> IO ()
createTESPlotScript filename = writeFile (filename <> ".plot") content
 where
  content =
     ""
        <> "set object 1 rectangle from screen 0,0 to screen 1,1 fillcolor rgb 'white' behind\n"
        <> "set key fixed left top vertical Right noreverse enhanced autotitle box lt black linewidth 1.000 dashtype solid\n"
        <> "set style increment default\n"
        <> "set title 'Simple Exponential Smoothing Forecast'\n"
        <> "set xlabel 'Time (Months)'\n"
        <> "set ylabel 'Demand (Swords)'\n"
        <> "set xtics 0,6,48\n"
        <> "set ytics 0,50,400\n"
        <> "set xrange [0:48]\n"
        <> "set yrange [0:400]\n"
        <> "plot '" <> (filename <> ".dat") <> "' using 1:2 with linespoint linecolor rgb 'grey' lw 4"


forecast :: IO ()
forecast = do
   opts <- execParser opts
   case opts of
      Version                -> version
      ProgramInput _ dat scn -> launch dat scn
 where
  opts = info
     (helper <*> versionFlag <|> generateFlag)
     (  fullDesc
     <> progDesc
           "Generate a (GNUplot) datasets & scripts that produces a SES/DES/TES plots"
     <> header "Forecasting - An example of the SES/DES/TES plot script generation"
     )
  launch dat filename = do
     putStrLn ("==> Parsing " <> dat)
     cds <- parseCsv dat
     
     let dms = V.toList (V.map demand cds)
     let predictionOffset = 12
     putStrLn "==> SES: Optimizing for alpha..."
     (ses_bi, _) <- GA.runGA configSES initialSolution
        $ GA.train (objectiveFunctionSES cds predictionOffset)
     let ses_alpha = head . GA.chromosome $ ses_bi
     
     let ses_record = ses cds predictionOffset ses_alpha
     let ses_dms = demands ses_record
     let ses_totalLengthDemands = V.length ses_dms
     let ses_histDemLength = ses_totalLengthDemands - predictionOffset
     let (ses_historicalDemands, ses_futureDemands) = splitAt ses_histDemLength (V.toList ses_dms) 
     let sse = sum (squaredErrors ses_record)
     let stdError = standardErrorSES $ squaredErrors ses_record
     putStrLn ("==> Cooficient found: " <> fprs [ses_alpha])

     putStrLn $ "==> Creating "  <> (sesFile <> ".dat")  <> " dataset"
     putStrLn "==> Creating the SES report"
     sesDataReport sesFile ses_record sse stdError ses_historicalDemands ses_futureDemands ses_alpha
     putStrLn "==> Done!"
     putStrLn $ "==> Creating " <> (sesFile <> ".plot") <> " script"
     createSESPlotScript sesFile
     putStrLn "==> Done!"

     putStrLn "==> DES: Optimizing for alpha & gamma..."
     (des_bi, _) <- GA.runGA configDES initialSolution
        $ GA.train (objectiveFunctionDES cds predictionOffset)
     let desAlphaGamma@[des_alpha, des_gamma] = GA.chromosome des_bi

     let des_record = des cds predictionOffset des_alpha des_gamma
     let des_dms = demands des_record
     let des_totalLengthDemands = V.length des_dms
     let des_histDemLength = des_totalLengthDemands - predictionOffset
     let (des_historicalDemands, des_futureDemands) = splitAt des_histDemLength (V.toList des_dms) 
     let sse = sum (squaredErrors des_record)
     let stdError = standardErrorTES $ squaredErrors des_record
     putStrLn ("==> Cooficient found: " <> fprs desAlphaGamma)

     putStrLn $ "==> Creating "  <> (desFile <> ".dat")  <> " dataset"
     putStrLn "==> Creating the DES report"
     desDataReport desFile des_record sse stdError des_historicalDemands des_futureDemands des_alpha des_gamma
     putStrLn "==> Done!"
     putStrLn $ "==> Creating " <> (desFile <> ".plot") <> " script"
     createDESPlotScript desFile
     putStrLn "==> Done!"

     let numberOfSeasonalFactorEstimates = 24
     let monthOfInterest = 7

     putStrLn "==> TES: Optimizing for alpha, gamma & delta..."
     (tes_bi, _) <- GA.runGA configTES initialSolution
        $ GA.train (objectiveFunctionTES cds predictionOffset numberOfSeasonalFactorEstimates monthOfInterest)
     let tesAlphaGammaDelta@[tes_alpha, tes_gamma, tes_delta] = GA.chromosome tes_bi

     let tes_record = tes cds predictionOffset numberOfSeasonalFactorEstimates monthOfInterest tes_alpha tes_gamma tes_delta
     let tes_dms = demands tes_record
     let tes_totalLengthDemands = V.length tes_dms
     let tes_histDemLength = tes_totalLengthDemands - predictionOffset
     let (tes_historicalDemands, tes_futureDemands) = splitAt tes_histDemLength (V.toList tes_dms) 
     let sse = sum (squaredErrors tes_record)
     let stdError = standardErrorTES $ squaredErrors ses_record
     putStrLn ("==> Cooficient found: " <> fprs tesAlphaGammaDelta)

     putStrLn $ "==> Creating "  <> (tesFile <> ".dat")  <> " dataset"
     putStrLn "==> Creating the DES report"
     tesDataReport tesFile tes_record sse stdError tes_historicalDemands tes_futureDemands tes_alpha tes_gamma tes_delta predictionOffset
     putStrLn "==> Done!"
     putStrLn $ "==> Creating " <> (tesFile <> ".plot") <> " script"
     createDESPlotScript tesFile
     putStrLn "==> Done!"

     putStrLn "INFO: Please make sure GNUplot is installed and is in your $PATH!"
     putStrLn "INFO: To produce the plot(s) run: "
     putStrLn $ "INFO: `gnuplot -persist " <> (sesFile   <> ".plot`") <> " for the SES forecast plot."
     putStrLn $ "INFO: `gnuplot -persist " <> (desFile   <> ".plot`") <> " for the DES forecast plot."
     putStrLn $ "INFO: `gnuplot -persist " <> (tesFile   <> ".plot`") <> " for the TES forecast plot."
      where
       sesFile   = filename <> "_ses"
       desFile   = filename <> "_des"
       tesFile   = filename <> "_tes"
