# Module Documentation

## Module DyGraphs

### Types

    data Canvas :: *

    type Color = String

    newtype DyAnnotation

    type DyAnnotationHandler = forall eff. DyAnnotation -> DyPoint -> DyGraph -> Event -> Eff eff Unit

    type DyArea = { h :: Number, w :: Number, y :: Number, x :: Number }

    newtype DyAxis

    type DyAxisLabelFormatter = Number -> DyGranularity -> DyOptsView -> String

    type DyAxisOpts = { strokeWidth :: Maybe Number, strokePattern :: Maybe DyPattern, strokeBorderWidth :: Maybe Boolean, strokeBorderColor :: Maybe Color, stepPlot :: Maybe Boolean, pointSize :: Maybe Number, highlightCircleSize :: Maybe Number, fillGraph :: Maybe Boolean, fillAlpha :: Maybe Number, drawPoints :: Maybe Boolean, drawPointCallback :: Maybe DyDrawPointCallback, drawHighlightPointCallback :: Maybe DyDrawPointCallback, drawGapEdgePoint :: Maybe Boolean, connectSeparatedPoints :: Maybe Boolean, color :: Maybe Color, valueRange :: Maybe DyRange, valueFormatter :: Maybe DyValueFormatter, ticker :: Maybe DyTicker, pixelsPerLabel :: Maybe Number, logscale :: Maybe Boolean, independentTicks :: Maybe Boolean, includeZero :: Maybe Boolean, gridLinePattern :: Maybe DyPattern, gridLineWidth :: Maybe Number, gridLineColor :: Maybe Color, drawGrid :: Maybe Boolean, drawAxis :: Maybe Boolean, axixLineWidth :: Maybe Number, axisLineColor :: Maybe Color, axisLabelWidth :: Maybe Number, axisLabelFormatter :: Maybe DyAxisLabelFormatter, axisLabelFontSize :: Maybe Number, axisLabelColor :: Maybe Color }

    type DyClickCallback e = Event -> Number -> [DyPoint] -> Eff e Unit

    data DyData where
      CSV :: String -> DyData
      URL :: String -> DyData
      Array2D :: [[Number]] -> DyData

    type DyDrawCallback e = DyGraph -> Boolean -> Eff e Unit

    type DyDrawPointCallback = forall eff. DyGraph -> String -> Canvas -> Number -> Number -> Color -> Number -> Eff eff Unit

    newtype DyGranularity

    data DyGraph :: *

    type DyHighlightCallback e = Event -> Number -> [DyPoint] -> Number -> String -> Eff e Unit

    newtype DyLegendMode

    data DyOptionValue :: *

    type DyOpts a b c = { strokeWidth :: Maybe Number, strokePattern :: Maybe DyPattern, strokeBorderWidth :: Maybe Boolean, strokeBorderColor :: Maybe Color, stepPlot :: Maybe Boolean, pointSize :: Maybe Number, highlightCircleSize :: Maybe Number, fillGraph :: Maybe Boolean, fillAlpha :: Maybe Number, drawPoints :: Maybe Boolean, drawPointCallback :: Maybe DyDrawPointCallback, drawHighlightPointCallback :: Maybe DyDrawPointCallback, drawGapEdgePoint :: Maybe Boolean, connectSeparatedPoints :: Maybe Boolean, color :: Maybe Color, valueRange :: Maybe DyRange, valueFormatter :: Maybe DyValueFormatter, ticker :: Maybe DyTicker, pixelsPerLabel :: Maybe Number, logscale :: Maybe Boolean, independentTicks :: Maybe Boolean, includeZero :: Maybe Boolean, gridLinePattern :: Maybe DyPattern, gridLineWidth :: Maybe Number, gridLineColor :: Maybe Color, drawGrid :: Maybe Boolean, drawAxis :: Maybe Boolean, axixLineWidth :: Maybe Number, axisLineColor :: Maybe Color, axisLabelWidth :: Maybe Number, axisLabelFormatter :: Maybe DyAxisLabelFormatter, axisLabelFontSize :: Maybe Number, axisLabelColor :: Maybe Color, axes :: Maybe { y2 :: Maybe DyAxisOpts, y :: Maybe DyAxisOpts, x :: Maybe DyAxisOpts }, isZoomedIgnoreProgrammaticZoom :: Maybe Boolean, sigFigs :: Maybe Number, maxNumberWidth :: Maybe Number, labelsUTC :: Maybe Boolean, labelsKMG2 :: Maybe Boolean, labelsKMB :: Maybe Boolean, digitsAfterDecimal :: Maybe Number, series :: Maybe (StrMap DySeriesOpts), width :: Maybe Number, rightGap :: Maybe Number, height :: Maybe Number, legend :: Maybe DyLegendMode, labelsShowZeroValues :: Maybe Boolean, labelsSeparateLines :: Maybe Boolean, labelsDivWidth :: Maybe Number, labelsDiv :: Maybe Node, labels :: Maybe [String], showRoller :: Maybe Boolean, showInRangeSelector :: Maybe Boolean, showRangeSelector :: Maybe Boolean, showLabelsOnHighlight :: Maybe Boolean, rangeSelectorPlotStrokeColor :: Maybe Color, rangeSelectorPlotFillColor :: Maybe Color, rangeSelectorHeight :: Maybe Number, highlightSeriesOpts :: Maybe DySeriesOpts, highlightSeriesBackgroundAlpha :: Maybe Number, hideOverlayOnMouseOut :: Maybe Boolean, animatedZooms :: Maybe Boolean, wilsonInterval :: Maybe Boolean, sigma :: Maybe Number, rollPeriod :: Maybe Number, timingName :: Maybe String, colors :: Maybe [Color], colorValue :: Maybe Number, colorSaturation :: Maybe Number, visibility :: Maybe [Boolean], stackedGraphNaNFill :: Maybe String, stackedGraph :: Maybe Boolean, file :: Maybe DyData, yLabelWidth :: Maybe Number, y2label :: Maybe String, ylabel :: Maybe String, xlabel :: Maybe String, xLabelHeight :: Maybe Number, titleHeight :: Maybe Number, title :: Maybe String, zoomCallback :: Maybe DyZoomCallback, unhighlightCallback :: Maybe DyUnhighlightCallback, underlayCallback :: Maybe DyUnderlayCallback, pointClickCallback :: Maybe DyPointClickCallback, highlightCallback :: Maybe (DyHighlightCallback c), drawCallback :: Maybe (DyDrawCallback b), clickCallback :: Maybe (DyClickCallback a), xValueParser :: Maybe (String -> Number), fractions :: Maybe Boolean, errorBars :: Maybe Boolean, delimiter :: Maybe String, customBars :: Maybe Boolean, yRangePad :: Maybe Number, xRangePad :: Maybe Number, xAxisHeight :: Maybe Number, panEdgeFraction :: Maybe Number, drawAxesAtZero :: Maybe Boolean, dateWindow :: Maybe DyRange, axisTickSize :: Maybe Number, displayAnnotations :: Maybe Boolean, annotationMouseOverHandler :: Maybe DyAnnotationHandler, annotationMouseOutHandler :: Maybe DyAnnotationHandler, annotationDblClickHandler :: Maybe DyAnnotationHandler, annotationClickHandler :: Maybe DyAnnotationHandler }

    type DyOptsView = forall a. String -> DyOptionValue

    type DyPattern = [Number]

    type DyPoint = { name :: DySeriesName, canvasy :: Number, canvasx :: Number, yval :: Number, xval :: Number }

    type DyPointClickCallback = forall eff. Event -> DyPoint -> Eff eff Unit

    newtype DyRange

    newtype DySeriesName

    type DySeriesOpts = { strokeWidth :: Maybe Number, strokePattern :: Maybe DyPattern, strokeBorderWidth :: Maybe Boolean, strokeBorderColor :: Maybe Color, stepPlot :: Maybe Boolean, pointSize :: Maybe Number, highlightCircleSize :: Maybe Number, fillGraph :: Maybe Boolean, fillAlpha :: Maybe Number, drawPoints :: Maybe Boolean, drawPointCallback :: Maybe DyDrawPointCallback, drawHighlightPointCallback :: Maybe DyDrawPointCallback, drawGapEdgePoint :: Maybe Boolean, connectSeparatedPoints :: Maybe Boolean, color :: Maybe Color, axis :: DyAxis }

    type DyTicker = Number -> Number -> Number -> DyOptsView -> DyGraph -> Maybe [Number] -> [{ label :: String, v :: Number }]

    type DyUnderlayCallback = forall eff. Canvas -> DyArea -> DyGraph -> Eff eff Unit

    type DyUnhighlightCallback = forall eff. Event -> Eff eff Unit

    type DyValueFormatter = forall a. a -> String

    newtype DyXValue

    type DyZoomCallback = forall eff. Number -> Number -> [[Number]] -> Eff eff Unit

    data Event :: *

    data RunDyGraph :: !


### Values

    defaultDyOpts :: forall a b c. DyOpts a b c

    newDyGraph :: forall e a b c. Node -> DyData -> DyOpts a b c -> Eff (runDyGraph :: RunDyGraph | e) DyGraph

    xDate :: String -> DyXValue

    xNumber :: Number -> DyXValue