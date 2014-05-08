module Chart.Distribution (renderContinuousDistribution,
                           renderDiscreteDistribution,
                           continuousDistributionPlot,
                           discreteDistributionPlot) where
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Colour
import Data.Default.Class
import Control.Lens ((&), (.~))
import Data.List (sort, span)

import qualified Data.Map as M

renderContinuousDistribution :: FilePath -> [Double] -> IO (PickFn ())
renderContinuousDistribution file xs = renderableToFile def renderable file
   where renderable = toRenderable $ def
                        & layout_plots .~ [plotBars $ continuousDistributionPlot xs]
                        & layout_left_axis_visibility .~ nolabel
         nolabel = def & axis_show_labels .~ False
                       & axis_show_ticks .~ False

renderDiscreteDistribution :: (Ord k, Show k) => FilePath -> [k] -> IO (PickFn ())
renderDiscreteDistribution file xs = renderableToFile def renderable file
   where renderable = toRenderable $ def
                        & layout_plots .~ [plotBars $ discreteDistributionPlot $ map snd counts]
                        & layout_left_axis_visibility .~ nolabel
                        & layout_x_axis . laxis_generate .~ (autoIndexAxis $ map (show . fst) counts) 
         nolabel = def & axis_show_labels .~ False
                       & axis_show_ticks .~ False
         counts = M.toList $ M.fromListWith (+) $ map (\x->(x,1)) xs

continuousDistributionPlot :: [Double] -> PlotBars Double Double
continuousDistributionPlot xs = def & plot_bars_alignment .~ BarsRight
                          & plot_bars_spacing .~ BarsFixGap 0 0
                          & plot_bars_values .~ (zip binmaxes $ map (:[]) bindensity)
  where sorted = sort xs
        numberofbins = ceiling $ 2 * (fromIntegral $ length xs) ** (0.333) -- rice rule
        min = minimum sorted
        max = maximum sorted
        range = max - min
        binwidths = range / (fromIntegral numberofbins)
        binmaxes = map (\x-> min + (fromIntegral x)*binwidths) [1..numberofbins]
        binranges = zip (min:binmaxes) (binmaxes++[max])
        bincounts = map fromIntegral $ allBinCounts binranges sorted
        bindensity = map (\(x,y) -> y/x) $ zip (repeat binwidths) bincounts

singleBinCount :: (Double,Double) -> [Double] -> (Int,[Double])
singleBinCount (min,max) xs = (length included, rest)
  where (included, rest) = span (<max) xs

allBinCounts :: [(Double,Double)] -> [Double] -> [Int]
allBinCounts [] _ = []
allBinCounts (b:bs) xs = c : (allBinCounts bs rest)
  where (c,rest) = singleBinCount b xs

discreteDistributionPlot :: [Double] -> PlotBars PlotIndex Double
discreteDistributionPlot vals = def
                                & plot_bars_values .~ (addIndexes $ map (:[]) vals)
                                & plot_bars_spacing .~ BarsFixGap 30 5
