
module DyGraphs 
  ( DyXValue(), xNumber, xDate
  , DyWindow(), dateWindow
  , DyAxisName(), x, y, y2
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

import Data.Either
import Data.Maybe
import Control.Monad.Eff
import DOM (Node())
import Data.StrMap (StrMap())
import Data.Foreign.OOFFI (instantiate3)
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

newtype DyWindow = DyWindow [Number]
dateWindow min max | min <= max = DyWindow [min, max]
dateWindow min max              = DyWindow [max, min]

newtype DyAxisName = DyAxisName String
x  = DyAxisName "x"
y  = DyAxisName "y"
y2 = DyAxisName "y2"

type DyDrawPointCallback = forall eff.
  DyGraph -> 
  String -> 
  Canvas -> 
  Number -> 
  Number -> 
  Color -> 
  Number -> 
  Eff (|eff) Unit

type DyPattern = [Number]

type DySeriesOpts = 
  { axis :: DyAxisName
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

type DyOptsView = forall a. String -> DyOptionValue

newtype DyGranularity = DyGranularity Number

secondly      = DyGranularity 0
twoSecondly   = DyGranularity 1
fiveSecondly  = DyGranularity 2
tenSecondly   = DyGranularity 3
thirtySecondly= DyGranularity 4
minutely      = DyGranularity 5
twoMinutely   = DyGranularity 6
fiveMinutely  = DyGranularity 7
tenMinutely   = DyGranularity 8
thirtyMinutely= DyGranularity 9
hourly        = DyGranularity 10
twoHourly     = DyGranularity 11
sixHourly     = DyGranularity 12
daily         = DyGranularity 13
weekly        = DyGranularity 14
monthly       = DyGranularity 15
quarterly     = DyGranularity 16
biannual      = DyGranularity 17
annual        = DyGranularity 18
decadal       = DyGranularity 19
centennial    = DyGranularity 20


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
  Maybe [Number] -> 
  [{v :: Number, label :: String}]

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
  , valueRange :: Maybe DyWindow
  -- DySeriesOpts
  -- , axis :: DyAxisName -- no axis name for across axes opts
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

type DyClickCallback = forall eff. Event -> Number -> [DyPoint] -> Eff (|eff) Unit

type DyDrawCallback = forall eff. DyGraph -> Boolean -> Eff (|eff) Unit

type DyHighlightCallback = forall eff. 
  Event -> 
  Number -> 
  [DyPoint] -> 
  Number -> 
  String -> 
  Eff (|eff) Unit

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
  [[Number]] -> 
  Eff (|eff) Unit

data DyData = CSV String 
            | URL String
            | Array2D [[Number]]
            
newtype DyLegendMode = DyLegendMode String
always = DyLegendMode "always"
never = DyLegendMode "never"
follow = DyLegendMode "follow"
onMouseOver = DyLegendMode "onmouseover"

type DyOpts = 
  { annotationClickHandler :: Maybe DyAnnotationHandler
  , annotationDblClickHandler :: Maybe DyAnnotationHandler
  , annotationMouseOutHandler :: Maybe DyAnnotationHandler
  , annotationMouseOverHandler :: Maybe DyAnnotationHandler
  , displayAnnotations :: Maybe Boolean
  , axisTickSize :: Maybe Number
  , dateWindow :: Maybe DyWindow
  , drawAxesAtZero :: Maybe Boolean
  , drawXAxis :: Maybe Boolean
  , drawYAxis :: Maybe Boolean
  , panEdgeFraction :: Maybe Number
  , xAxisHeight :: Maybe Number
  , xAxisLabelWidth :: Maybe Number
  , xRangePad :: Maybe Number
  , yAxisLabelWidth :: Maybe Number
  , yRangePad :: Maybe Number
  , customBars :: Maybe Boolean
  , delimiter :: Maybe String
  , errorBars :: Maybe Boolean
  , fractions :: Maybe Boolean
  , xValueParser :: Maybe (String -> Number)
  , clickCallback :: Maybe DyClickCallback
  , drawCallback :: Maybe DyDrawCallback
  , highlightCallback :: Maybe DyHighlightCallback
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
  , file :: Maybe DyData
  , stackedGraph :: Maybe Boolean
  , stackedGraphNaNFill :: Maybe String
  , visibility :: Maybe [Boolean]
  , colorSaturation :: Maybe Number
  , colorValue :: Maybe Number
  , colors :: Maybe [Color]
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
  , showRoller :: Maybe Boolean
  , labels :: Maybe [String]
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
  , maxNumberWidth :: Maybe Number
  , sigFigs :: Maybe Number
  , isZoomedIgnoreProgrammaticZoom :: Maybe Boolean
  , axes :: Maybe ({ x :: Maybe DyAxisOpts
                   , y :: Maybe DyAxisOpts
                   , z :: Maybe DyAxisOpts})

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
  , valueRange :: Maybe DyWindow

  -- DySeriesOpts
  -- , axis :: DyAxisName -- No axis names for across graph opts
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

defaultDyOpts :: DyOpts
defaultDyOpts = 
  { annotationClickHandler : Nothing
  , annotationDblClickHandler : Nothing
  , annotationMouseOutHandler : Nothing
  , annotationMouseOverHandler : Nothing
  , displayAnnotations : Nothing
  , axisTickSize : Nothing
  , dateWindow : Nothing
  , drawAxesAtZero : Nothing
  , drawXAxis : Nothing
  , drawYAxis : Nothing
  , panEdgeFraction : Nothing
  , xAxisHeight : Nothing
  , xAxisLabelWidth : Nothing
  , xRangePad : Nothing
  , yAxisLabelWidth : Nothing
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

newDyGraph :: forall e. Node -> DyData -> DyOpts -> Eff (runDyGraph :: RunDyGraph | e) DyGraph
newDyGraph div (CSV file) opts = instantiate3 "Dygraph" div file (toOptions opts)
newDyGraph div (URL file) opts = instantiate3 "Dygraph" div file (toOptions opts)
newDyGraph div (Array2D file) opts = instantiate3 "Dygraph" div file (toOptions opts)
