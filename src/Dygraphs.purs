
module DyGraphs
  ( DyXValue(), xNumber, xDate
  , DyRange(), range, unRange
  , DyAxis(), y, y2
  , DySeriesOpts()
  , DyGranularity()
  , secondly
  , twoSecondly
  , fiveSecondly
  , tenSecondly
  , thirtySecondly
  , minutely
  , twoMinutely
  , fiveMinutely
  , tenMinutely
  , thirtyMinutely
  , hourly
  , twoHourly
  , sixHourly
  , daily
  , weekly
  , monthly
  , quarterly
  , biannual
  , annual
  , decadal
  , centennial
  , DyAxisOpts()
  , DyData(..)
  , DyLegendMode(), always, never, follow, onMouseOver
  , DyOpts()

  -- Main exports
  , defaultDyOpts
  , newDyGraph
  , RunDyGraph()

  -- extra type
  , Color()
  , DyPattern()
  , DyValueFormatter()
  , DyTicker()
  , DyDrawPointCallback()
  , DyGraph()
  , DyOptsView()
  , DyZoomCallback()
  , DyAxisLabelFormatter()
  , DyOptionValue()
  , Canvas()
  , DyUnhighlightCallback()
  , Event()
  , DyUnderlayCallback()
  , DyArea()
  , DyPointClickCallback()
  , DyPoint()
  , DyHighlightCallback()
  , DySeriesName()
  , DyDrawCallback()
  , DyClickCallback()
  , DyAnnotationHandler()
  , DyAnnotation()
  )
where

-- * Small supporting types

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Function (Fn3, mkFn1, runFn3)
import Control.Monad.Eff (Eff)
import DOM.Node.Types (Node())
import Data.StrMap (StrMap())
import Data.Foreign.OOFFI (method0Eff, method1Eff, method2Eff, method3Eff, instantiate3)
import Data.Foreign.Options (toOptions)

newtype DySeriesName = DySeriesName String

type DyPoint =
  { xval :: Number
  , yval :: Number
  , canvasx :: Number
  , canvasy :: Number
  , name :: DySeriesName
  }

newtype DyXValue = DyXValue (Either Number String)

xNumber :: Number -> DyXValue
xNumber = DyXValue <<< Left

xDate :: String -> DyXValue
xDate = DyXValue <<< Right

type Color = String

foreign import data RunDyGraph :: !
foreign import data DyGraph :: *

-- * Annotations

foreign import data Event :: *

type DyAnnotationHandler = forall eff.
  DyAnnotation ->
  DyPoint ->
  DyGraph ->
  Event ->
  Eff (|eff) Unit

newtype DyAnnotation = DyAnnotation
  { series  :: DySeriesName
  , x       :: DyXValue
  , shortText :: Maybe String
  , text    :: Maybe String
  , icon :: Maybe String
  , width :: Maybe Number
  , height :: Maybe Number
  , cssClass :: Maybe String
  , tickHeight :: Maybe Number
  , attachAtBottom :: Maybe Boolean
  , clickHandler :: Maybe DyAnnotationHandler
  , mouseOverHandler :: Maybe DyAnnotationHandler
  , mouseOutHandler :: Maybe DyAnnotationHandler
  , dblClickHandler :: Maybe DyAnnotationHandler
  }

-- * Per series options

newtype DyRange = DyRange (Array Number)
range :: Number -> Number -> DyRange
range min max | min <= max = DyRange [min, max]
range min max              = DyRange [max, min]

unRange (DyRange a) = a

newtype DyAxis = DyAxis String
y  = DyAxis "y"
y2 = DyAxis "y2"

type DyDrawPointCallback = forall eff.
  DyGraph ->
  String ->
  Canvas ->
  Number ->
  Number ->
  Color ->
  Number ->
  Eff (|eff) Unit

type DyPattern = Array Number

type DySeriesOpts =
  { axis :: DyAxis
  , color :: Maybe Color
  , connectSeparatedPoints :: Maybe Boolean
  , drawGapEdgePoint :: Maybe Boolean
  , drawHighlightPointCallback :: Maybe DyDrawPointCallback
  , drawPointCallback :: Maybe DyDrawPointCallback
  , drawPoints :: Maybe Boolean
  , fillAlpha :: Maybe Number
  , fillGraph :: Maybe Boolean
  , highlightCircleSize :: Maybe Number
  --, plotter :: Maybe [DyPlotter]
  , pointSize :: Maybe Number
  , stepPlot :: Maybe Boolean
  , strokeBorderColor :: Maybe Color
  , strokeBorderWidth :: Maybe Boolean
  , strokePattern :: Maybe DyPattern
  , strokeWidth :: Maybe Number
  }



-- * Per axis options, assigned to axes: x, y, or y2

foreign import data DyOptionValue :: *

type DyOptsView = String -> DyOptionValue

newtype DyGranularity = DyGranularity Number

secondly      = DyGranularity 0.0
twoSecondly   = DyGranularity 1.0
fiveSecondly  = DyGranularity 2.0
tenSecondly   = DyGranularity 3.0
thirtySecondly= DyGranularity 4.0
minutely      = DyGranularity 5.0
twoMinutely   = DyGranularity 6.0
fiveMinutely  = DyGranularity 7.0
tenMinutely   = DyGranularity 8.0
thirtyMinutely= DyGranularity 9.0
hourly        = DyGranularity 10.0
twoHourly     = DyGranularity 11.0
sixHourly     = DyGranularity 12.0
daily         = DyGranularity 13.0
weekly        = DyGranularity 14.0
monthly       = DyGranularity 15.0
quarterly     = DyGranularity 16.0
biannual      = DyGranularity 17.0
annual        = DyGranularity 18.0
decadal       = DyGranularity 19.0
centennial    = DyGranularity 20.0


type DyAxisLabelFormatter =
  Number ->
  DyGranularity ->
  DyOptsView ->
  String

type DyTicker =
  Number ->
  Number ->
  Number ->
  DyOptsView ->
  DyGraph ->
  Maybe (Array Number) ->
  Array {v :: Number, label :: String}

type DyValueFormatter = forall a. a -> String

foreign import data Canvas :: *

type DyAxisOpts =
  { axisLabelColor :: Maybe Color
  , axisLabelFontSize :: Maybe Number
  , axisLabelFormatter :: Maybe DyAxisLabelFormatter
  , axisLabelWidth :: Maybe Number
  , axisLineColor :: Maybe Color
  , axixLineWidth :: Maybe Number
  , drawAxis :: Maybe Boolean
  , drawGrid :: Maybe Boolean
  , gridLineColor :: Maybe Color
  , gridLineWidth :: Maybe Number
  , gridLinePattern :: Maybe DyPattern
  , includeZero :: Maybe Boolean
  , independentTicks :: Maybe Boolean
  , logscale :: Maybe Boolean
  , pixelsPerLabel :: Maybe Number
  , ticker :: Maybe DyTicker
  , valueFormatter :: Maybe DyValueFormatter
  , valueRange :: Maybe DyRange
  -- DySeriesOpts
  -- , axis :: DyAxis -- no axis name for across axes opts
  , color :: Maybe Color
  , connectSeparatedPoints :: Maybe Boolean
  , drawGapEdgePoint :: Maybe Boolean
  , drawHighlightPointCallback :: Maybe DyDrawPointCallback
  , drawPointCallback :: Maybe DyDrawPointCallback
  , drawPoints :: Maybe Boolean
  , fillAlpha :: Maybe Number
  , fillGraph :: Maybe Boolean
  , highlightCircleSize :: Maybe Number
  --, plotter :: Maybe [DyPlotter]
  , pointSize :: Maybe Number
  , stepPlot :: Maybe Boolean
  , strokeBorderColor :: Maybe Color
  , strokeBorderWidth :: Maybe Boolean
  , strokePattern :: Maybe DyPattern
  , strokeWidth :: Maybe Number

  }


-- * Graph wide options

type DyClickCallback e = Event -> Number -> Array DyPoint -> Eff e Unit

type DyDrawCallback e = DyGraph -> Boolean -> Eff e Unit

type DyHighlightCallback e =
  Event ->
  Number ->
  Array DyPoint ->
  Number ->
  String ->
  Eff e Unit

type DyPointClickCallback = forall eff. Event -> DyPoint -> Eff (|eff) Unit

type DyArea = { x :: Number, y :: Number, w :: Number, h :: Number }

type DyUnderlayCallback = forall eff.
  Canvas ->
  DyArea ->
  DyGraph ->
  Eff (|eff) Unit

type DyUnhighlightCallback = forall eff. Event -> Eff (|eff) Unit

type DyZoomCallback = forall eff.
  Number ->
  Number ->
  Array (Array Number) ->
  Eff (|eff) Unit

data DyData = CSV String
            | URL String
            | Array2D (Array (Array Number))

newtype DyLegendMode = DyLegendMode String
always = DyLegendMode "always"
never = DyLegendMode "never"
follow = DyLegendMode "follow"
onMouseOver = DyLegendMode "onmouseover"

type DyOpts a b c =
  { annotationClickHandler :: Maybe DyAnnotationHandler
  , annotationDblClickHandler :: Maybe DyAnnotationHandler
  , annotationMouseOutHandler :: Maybe DyAnnotationHandler
  , annotationMouseOverHandler :: Maybe DyAnnotationHandler
  , displayAnnotations :: Maybe Boolean
  , axisTickSize :: Maybe Number
  , dateWindow :: Maybe DyRange
  , drawAxesAtZero :: Maybe Boolean
  , panEdgeFraction :: Maybe Number
  , xAxisHeight :: Maybe Number
  , xRangePad :: Maybe Number
  , yRangePad :: Maybe Number
  , customBars :: Maybe Boolean
  , delimiter :: Maybe String
  , errorBars :: Maybe Boolean
  , fractions :: Maybe Boolean
  , xValueParser :: Maybe (String -> Number)
  , clickCallback :: Maybe (DyClickCallback a)
  , drawCallback :: Maybe (DyDrawCallback b)
  , highlightCallback :: Maybe (DyHighlightCallback c)
  , pointClickCallback :: Maybe DyPointClickCallback
  , underlayCallback :: Maybe DyUnderlayCallback
  , unhighlightCallback :: Maybe DyUnhighlightCallback
  , zoomCallback :: Maybe DyZoomCallback
  , title :: Maybe String
  , titleHeight :: Maybe Number
  , xLabelHeight :: Maybe Number
  , xlabel :: Maybe String
  , ylabel :: Maybe String
  , y2label :: Maybe String
  , yLabelWidth :: Maybe Number
  --, plugins :: Maybe DyPlugin
  --, dataHandler :: Foreign? Skip this one for now
  , file :: Maybe DyData
  , stackedGraph :: Maybe Boolean
  , stackedGraphNaNFill :: Maybe String
  , visibility :: Maybe (Array Boolean)
  , colorSaturation :: Maybe Number
  , colorValue :: Maybe Number
  , colors :: Maybe (Array Color)
  , timingName :: Maybe String
  , rollPeriod :: Maybe Number
  , sigma :: Maybe Number
  , wilsonInterval :: Maybe Boolean
  , animatedZooms :: Maybe Boolean
  , hideOverlayOnMouseOut :: Maybe Boolean
  , highlightSeriesBackgroundAlpha :: Maybe Number
  , highlightSeriesOpts :: Maybe DySeriesOpts
  --, interactionModel ::
  , rangeSelectorHeight :: Maybe Number
  , rangeSelectorPlotFillColor :: Maybe Color
  , rangeSelectorPlotStrokeColor :: Maybe Color
  , showLabelsOnHighlight :: Maybe Boolean
  , showRangeSelector :: Maybe Boolean
  , showInRangeSelector :: Maybe Boolean
  , showRoller :: Maybe Boolean
  , labels :: Maybe (Array String)
  , labelsDiv :: Maybe Node
  --, labelsDivStyles ::
  , labelsDivWidth :: Maybe Number
  , labelsSeparateLines :: Maybe Boolean
  , labelsShowZeroValues :: Maybe Boolean
  , legend :: Maybe DyLegendMode
  , height :: Maybe Number
  , rightGap :: Maybe Number
  , width :: Maybe Number
  , series :: Maybe (StrMap DySeriesOpts)
  , digitsAfterDecimal :: Maybe Number
  , labelsKMB :: Maybe Boolean
  , labelsKMG2 :: Maybe Boolean
  , labelsUTC :: Maybe Boolean
  , maxNumberWidth :: Maybe Number
  , sigFigs :: Maybe Number
  , isZoomedIgnoreProgrammaticZoom :: Maybe Boolean
  , axes :: Maybe ({ x :: Maybe DyAxisOpts
                   , y :: Maybe DyAxisOpts
                   , y2 :: Maybe DyAxisOpts})

  -- DyAxisOpts
  , axisLabelColor :: Maybe Color
  , axisLabelFontSize :: Maybe Number
  , axisLabelFormatter :: Maybe DyAxisLabelFormatter
  , axisLabelWidth :: Maybe Number
  , axisLineColor :: Maybe Color
  , axixLineWidth :: Maybe Number
  , drawAxis :: Maybe Boolean
  , drawGrid :: Maybe Boolean
  , gridLineColor :: Maybe Color
  , gridLineWidth :: Maybe Number
  , gridLinePattern :: Maybe DyPattern
  , includeZero :: Maybe Boolean
  , independentTicks :: Maybe Boolean
  , logscale :: Maybe Boolean
  , pixelsPerLabel :: Maybe Number
  , ticker :: Maybe DyTicker
  , valueFormatter :: Maybe DyValueFormatter
  , valueRange :: Maybe DyRange

  -- DySeriesOpts
  -- , axis :: DyAxis -- No axis names for across graph opts
  , color :: Maybe Color
  , connectSeparatedPoints :: Maybe Boolean
  , drawGapEdgePoint :: Maybe Boolean
  , drawHighlightPointCallback :: Maybe DyDrawPointCallback
  , drawPointCallback :: Maybe DyDrawPointCallback
  , drawPoints :: Maybe Boolean
  , fillAlpha :: Maybe Number
  , fillGraph :: Maybe Boolean
  , highlightCircleSize :: Maybe Number
  --, plotter :: Maybe [DyPlotter]
  , pointSize :: Maybe Number
  , stepPlot :: Maybe Boolean
  , strokeBorderColor :: Maybe Color
  , strokeBorderWidth :: Maybe Boolean
  , strokePattern :: Maybe DyPattern
  , strokeWidth :: Maybe Number

  }

defaultDyOpts :: forall a b c. DyOpts a b c
defaultDyOpts =
  { annotationClickHandler : Nothing
  , annotationDblClickHandler : Nothing
  , annotationMouseOutHandler : Nothing
  , annotationMouseOverHandler : Nothing
  , displayAnnotations : Nothing
  , axisTickSize : Nothing
  , dateWindow : Nothing
  , drawAxesAtZero : Nothing
  , panEdgeFraction : Nothing
  , xAxisHeight : Nothing
  , xRangePad : Nothing
  , yRangePad : Nothing
  , customBars : Nothing
  , delimiter : Nothing
  , errorBars : Nothing
  , fractions : Nothing
  , xValueParser : Nothing
  , clickCallback : Nothing
  , drawCallback : Nothing
  , highlightCallback : Nothing
  , pointClickCallback : Nothing
  , underlayCallback : Nothing
  , unhighlightCallback : Nothing
  , zoomCallback : Nothing
  , title : Nothing
  , titleHeight : Nothing
  , xLabelHeight : Nothing
  , xlabel : Nothing
  , ylabel : Nothing
  , y2label : Nothing
  , yLabelWidth : Nothing
  --, plugins : Nothing
  --, dataHandler : Nothing?
  , file : Nothing
  , stackedGraph : Nothing
  , stackedGraphNaNFill : Nothing
  , visibility : Nothing
  , colorSaturation : Nothing
  , colorValue : Nothing
  , colors : Nothing
  , timingName : Nothing
  , rollPeriod : Nothing
  , sigma : Nothing
  , wilsonInterval : Nothing
  , animatedZooms : Nothing
  , hideOverlayOnMouseOut : Nothing
  , highlightSeriesBackgroundAlpha : Nothing
  , highlightSeriesOpts : Nothing
  --, interactionModel ::
  , rangeSelectorHeight : Nothing
  , rangeSelectorPlotFillColor : Nothing
  , rangeSelectorPlotStrokeColor : Nothing
  , showLabelsOnHighlight : Nothing
  , showRangeSelector : Nothing
  , showInRangeSelector : Nothing
  , showRoller : Nothing
  , labels : Nothing
  , labelsDiv : Nothing
  --, labelsDivStyles ::
  , labelsDivWidth : Nothing
  , labelsSeparateLines : Nothing
  , labelsShowZeroValues : Nothing
  , legend : Nothing
  , height : Nothing
  , rightGap : Nothing
  , width : Nothing
  , series : Nothing
  , digitsAfterDecimal : Nothing
  , labelsKMB : Nothing
  , labelsKMG2 : Nothing
  , labelsUTC : Nothing
  , maxNumberWidth : Nothing
  , sigFigs : Nothing
  , isZoomedIgnoreProgrammaticZoom : Nothing
  , axes : Nothing

  -- DyAxisOpts
  , axisLabelColor : Nothing
  , axisLabelFontSize : Nothing
  , axisLabelFormatter : Nothing
  , axisLabelWidth : Nothing
  , axisLineColor : Nothing
  , axixLineWidth : Nothing
  , drawAxis : Nothing
  , drawGrid : Nothing
  , gridLineColor : Nothing
  , gridLineWidth : Nothing
  , gridLinePattern : Nothing
  , includeZero : Nothing
  , independentTicks : Nothing
  , logscale : Nothing
  , pixelsPerLabel : Nothing
  , ticker : Nothing
  , valueFormatter : Nothing
  , valueRange : Nothing

  -- DySeriesOpts
  , color : Nothing
  , connectSeparatedPoints : Nothing
  , drawGapEdgePoint : Nothing
  , drawHighlightPointCallback : Nothing
  , drawPointCallback : Nothing
  , drawPoints : Nothing
  , fillAlpha : Nothing
  , fillGraph : Nothing
  , highlightCircleSize : Nothing
  --, plotter : Nothing
  , pointSize : Nothing
  , stepPlot : Nothing
  , strokeBorderColor : Nothing
  , strokeBorderWidth : Nothing
  , strokePattern : Nothing
  , strokeWidth : Nothing

  }

-- * Dygraph object and methods

foreign import unsafeNullToMaybeImpl :: forall a . Fn3 (a -> Maybe a) (Maybe a) a (Maybe a)

unsafeNullToMaybe :: forall a. a -> Maybe a
unsafeNullToMaybe = runFn3 unsafeNullToMaybeImpl Just Nothing

newDyGraph :: forall e a b c. Node -> DyData -> DyOpts a b c -> Eff (runDyGraph::RunDyGraph|e) DyGraph
newDyGraph div (CSV file) opts = instantiate3 "Dygraph" div file (toOptions opts)
newDyGraph div (URL file) opts = instantiate3 "Dygraph" div file (toOptions opts)
newDyGraph div (Array2D file) opts = instantiate3 "Dygraph" div file (toOptions opts)

adjustRoll :: forall e. DyGraph -> Number -> Eff (runDyGraph::RunDyGraph|e) Unit
adjustRoll = method1Eff "adjustRoll"

annotations :: forall e. DyGraph -> Eff (runDyGraph::RunDyGraph|e) (Array DyAnnotation)
annotations = method0Eff "annotations"

clearSelection :: forall e. DyGraph -> Eff (runDyGraph::RunDyGraph|e) Unit
clearSelection = method0Eff "clearSelection"

destroy :: forall e. DyGraph -> Eff (runDyGraph::RunDyGraph|e) Unit
destroy = method0Eff "destroy"

eventToDomCoords :: forall e. DyGraph -> Event -> Eff (runDyGraph::RunDyGraph|e) (Array Number)
eventToDomCoords = method1Eff "eventToDomCoords"

getArea :: forall e. DyGraph -> Eff (runDyGraph::RunDyGraph|e) {x::Number, y::Number, w::Number, h::Number}
getArea = method0Eff "getArea"

getColors :: forall e. DyGraph -> Eff (runDyGraph::RunDyGraph|e) (Array Color)
getColors = method0Eff "getColors"

getHighlightSeries :: forall e. DyGraph -> Eff (runDyGraph::RunDyGraph|e) DySeriesName
getHighlightSeries d = DySeriesName <$> method0Eff "getHighlightSeries" d

getLabels :: forall e. DyGraph -> Eff (runDyGraph::RunDyGraph|e) (Maybe (Array String))
getLabels d = unsafeNullToMaybe <$> method0Eff "getLabels" d

getOption :: forall e. DyGraph -> String -> Maybe DySeriesName -> Eff (runDyGraph::RunDyGraph|e) DyOptionValue
getOption d s Nothing = method1Eff "getOption" d s
getOption d s (Just n)= method2Eff "getOption" d s n

getPropertiesForSeries
  :: forall e. DyGraph
  -> DySeriesName
  -> Eff (runDyGraph::RunDyGraph|e) (Maybe {column::Number, visibility::Boolean, color::Color, axis::Number})
getPropertiesForSeries d (DySeriesName n) =
  unsafeNullToMaybe <$> method1Eff "getPropertiesForSeries" d n

getSelection :: forall e. DyGraph -> Eff (runDyGraph::RunDyGraph|e) (Maybe Number)
getSelection d = do
  sel <- method0Eff "getSelection" d
  return $ if sel == -1.0 then Nothing else Just sel

getValue :: forall e. DyGraph -> Number -> Number -> Eff (runDyGraph::RunDyGraph|e) (Maybe Number)
getValue d r c = unsafeNullToMaybe <$> method2Eff "getValue" d r c

indexFromSetName :: forall e. DyGraph -> DySeriesName -> Eff (runDyGraph::RunDyGraph|e) (Maybe Number)
indexFromSetName d (DySeriesName n) = unsafeNullToMaybe <$> method1Eff "indexFromSetName" d n

isSeriesLocked :: forall e. DyGraph -> Eff (runDyGraph::RunDyGraph|e) Boolean
isSeriesLocked = method0Eff "isSeriesLocked"

isZoomed :: forall e. DyGraph -> Maybe DyAxis -> Eff (runDyGraph::RunDyGraph|e) Boolean
isZoomed d Nothing = method0Eff "isZoomed" d
isZoomed d (Just (DyAxis a)) = method1Eff "isZoomed" d a

numAxes :: forall e. DyGraph -> Eff (runDyGraph::RunDyGraph|e) Number
numAxes = method0Eff "numAxes"

numColumns :: forall e. DyGraph -> Eff (runDyGraph::RunDyGraph|e) Number
numColumns = method0Eff "numColumns"

numRows :: forall e. DyGraph -> Eff (runDyGraph::RunDyGraph|e) Number
numRows = method0Eff "numRows"

ready :: forall a e. DyGraph -> (DyGraph -> Eff (|a) Unit) -> Eff (runDyGraph::RunDyGraph|e) Unit
ready d f =
  let fCallback = mkFn1 f in method1Eff "ready" d fCallback

resetZoom :: forall e. DyGraph -> Eff (runDyGraph::RunDyGraph|e) Unit
resetZoom = method0Eff "resetZoom"

resize :: forall e. DyGraph -> Maybe {width::Number, height::Number} -> Eff (runDyGraph::RunDyGraph|e) Unit
resize d Nothing = method0Eff "resize" d
resize d (Just {width=w, height=h}) = method2Eff "resize" d w h

rollPeriod :: forall e. DyGraph -> Eff (runDyGraph::RunDyGraph|e) Number
rollPeriod = method0Eff "rollPeriod"

setAnnotations :: forall e. DyGraph -> Array DyAnnotation -> Maybe Boolean -> Eff (runDyGraph::RunDyGraph|e) Unit
setAnnotations d as Nothing = method1Eff "setAnnotations" d as
setAnnotations d as (Just b) = method2Eff "setAnnotations" d as b

setSelection :: forall e. Partial => DyGraph -> Number -> Maybe DySeriesName -> Maybe Boolean -> Eff (runDyGraph::RunDyGraph|e) Unit
setSelection d r Nothing Nothing = method1Eff "setSelection" d r
setSelection d r (Just (DySeriesName n)) Nothing = method2Eff "setSelection" d r n
setSelection d r (Just (DySeriesName n)) (Just b) = method3Eff "setSelection" d r n b

setVisibility :: forall e. DyGraph -> Number -> Boolean -> Eff (runDyGraph::RunDyGraph|e) Unit
setVisibility = method2Eff "setVisibility"

toDataCoords :: forall e. DyGraph -> Number -> Number -> Maybe DyAxis -> Eff (runDyGraph::RunDyGraph|e) (Array Number)
toDataCoords d x y Nothing = method2Eff "toDataCoords" d x y
toDataCoords d x y (Just (DyAxis a)) = method3Eff "toDataCoords" d x y a

toDataXCoord :: forall e. DyGraph -> Number -> Eff (runDyGraph::RunDyGraph|e) Number
toDataXCoord = method1Eff "toDataXCoord"

toDataYCoord :: forall e. DyGraph -> Number -> Maybe DyAxis -> Eff (runDyGraph::RunDyGraph|e) Number
toDataYCoord d n Nothing = method1Eff "toDataYCoord" d n
toDataYCoord d n (Just (DyAxis a)) = method2Eff "toDataYCoord" d n a

toDomCoords :: forall e. DyGraph -> Number -> Number -> Maybe DyAxis -> Eff (runDyGraph::RunDyGraph|e) (Array Number)
toDomCoords d x y Nothing = method2Eff "toDomCoords" d x y
toDomCoords d x y (Just (DyAxis a)) = method3Eff "toDomCoords" d x y a

toDomXCoord :: forall e. DyGraph -> Number -> Eff (runDyGraph::RunDyGraph|e) Number
toDomXCoord = method1Eff "toDomXCoord"

toDomYCoord :: forall e. DyGraph -> Number -> Maybe DyAxis -> Eff (runDyGraph::RunDyGraph|e) Number
toDomYCoord d n Nothing = method1Eff "toDomYCoord" d n
toDomYCoord d n (Just (DyAxis a)) = method2Eff "toDomYCoord" d n a

toPercentXCoord :: forall e. DyGraph -> Number -> Eff (runDyGraph::RunDyGraph|e) Number
toPercentXCoord = method1Eff "toPercentXCoord"

toPercentYCoord :: forall e. DyGraph -> Number -> Maybe DyAxis -> Eff (runDyGraph::RunDyGraph|e) Number
toPercentYCoord d n Nothing = method1Eff "toPercentYCoord" d n
toPercentYCoord d n (Just (DyAxis a)) = method2Eff "toPercentYCoord" d n a

toString :: forall e. DyGraph -> Eff (runDyGraph::RunDyGraph|e) String
toString = method0Eff "toString"

updateOptions :: forall e a b c. DyGraph -> DyOpts a b c -> Maybe Boolean -> Eff (runDyGraph::RunDyGraph|e) Unit
updateOptions d o Nothing = method1Eff "updateOptions" d o
updateOptions d o (Just b)= method2Eff "updateOptions" d o b

visibility :: forall e. DyGraph -> Eff (runDyGraph::RunDyGraph|e) (Array Boolean)
visibility = method0Eff "visibility"

xAxisExtremes :: forall e. DyGraph -> Eff (runDyGraph::RunDyGraph|e) DyRange
xAxisExtremes d = DyRange <$> method0Eff "xAxisExtremes" d

xAxisRange :: forall e. DyGraph -> Eff (runDyGraph::RunDyGraph|e) DyRange
xAxisRange d = DyRange <$> method0Eff "xAxisRange" d

yAxisRange :: forall e. DyGraph -> Maybe DyAxis -> Eff (runDyGraph::RunDyGraph|e) DyRange
yAxisRange d Nothing = DyRange <$> method0Eff "yAxisRange" d
yAxisRange d (Just (DyAxis a)) = DyRange <$> method1Eff "yAxisRange" d a

yAxisRanges :: forall e. DyGraph -> Eff (runDyGraph::RunDyGraph|e) (Array DyRange)
yAxisRanges d = (<$>) DyRange <$> method0Eff "yAxisRanges" d
