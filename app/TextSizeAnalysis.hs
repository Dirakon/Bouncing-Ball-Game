module TextSizeAnalysis where
import qualified Data.Maybe
import Graphics.UI.GLUT.Fonts (Font(stringWidth), StrokeFont (Roman))
import Data.Maybe (fromMaybe)
import Graphics.Gloss (Picture (Text), translate)



characterSizes :: [(Char,Float)]
characterSizes = [('1',67.0),('2',78.0),('3',77.0),('4',80.0),('5',78.0),('6',74.0),('7',77.0),('8',78.0),('9',74.0),('M',97.0),('N',89.0),('B',84.0),('V',82.0),('C',84.0),('X',72.0),('Z',74.0),('L',71.0),('K',79.0),('J',60.0),('H',89.0),('G',90.0),('F',79.0),('D',85.0),('S',81.0),('A',80.0),('P',86.0),('O',89.0),('I',21.0),('U',89.0),('Y',80.0),('T',72.0),('R',82.0),('E',78.0),('W',101.0),('Q',88.0),('\\',78.0),('/',82.0),('.',26.0),(',',26.0),('m',124.0),('n',71.0),('b',70.0),('v',61.0),('c',69.0),('x',56.0),('z',62.0),('\'',14.0),(';',26.0),('l',19.0),('k',63.0),('j',36.0),('h',71.0),('g',71.0),('f',39.0),('d',70.0),('s',62.0),('a',67.0),(')',48.0),('(',47.0),(']',46.0),('[',46.0),('p',71.0),('o',72.0),('i',29.0),('u',71.0),('y',66.0),('t',39.0),('r',49.0),('e',69.0),('w',80.0),('q',71.0),(' ',105.0)]

defaultCharacterSize:: Float
defaultCharacterSize = 50

fontHeight:: Float
fontHeight = 152.381

estimateTextWidth :: String -> (Float,Float)
estimateTextWidth [] = (0,fontHeight)
estimateTextWidth (character : others) = 
    (fromMaybe defaultCharacterSize (lookup character characterSizes) + fst (estimateTextWidth others),fontHeight)

alignedCenterText ::  String -> Picture
alignedCenterText text = translate (-textWidth*0.5) (-textHeight*0.5) (Text text)
    where 
        (textWidth,textHeight) = estimateTextWidth text


setTextSizes ::   IO [(Char,Float)]
setTextSizes = do
    (_,sizes)<-recursivelySetSizes characterSizes []
    print sizes
    return sizes
    where
        recursivelySetSizes [] setTable = return ([],setTable)
        recursivelySetSizes ((textToCheck,_):others) setTable = do
            supposedWidth <- stringWidth Roman [textToCheck]
            let newSetTable = (textToCheck, fromIntegral supposedWidth::Float) : setTable
            recursivelySetSizes others newSetTable












-- IO version (temporarily scrapped because render function cannot change state)
-- type TextSizeAnalyzer = ([TextSizeAnalyzerEntry],[TextSizeAnalyzerEntry])
-- type TextSizeAnalyzerEntry = (String, Maybe (Float,Float))

-- tryGetTextSize :: String -> TextSizeAnalyzer -> (TextSizeAnalyzer,Maybe (Float,Float))
-- tryGetTextSize textToCheck (currentLookup,newLookup) = ((currentLookup,updatedNewLookup),foundSize)
--     where
--         updatedNewLookup = (textToCheck,foundSize):newLookup
--         foundSize = Data.Maybe.fromMaybe Nothing (lookup textToCheck currentLookup)

-- setTextSizes :: TextSizeAnalyzer -> IO TextSizeAnalyzer
-- setTextSizes (lookupTable,_) = do
--     recursivelySetSizes lookupTable []
--     where
--         recursivelySetSizes [] setTable = return ([],setTable)
--         recursivelySetSizes ((textToCheck,_):others) setTable = do
--             supposedWidth <- stringWidth Roman textToCheck
--             supposedHeight <- fontHeight Roman
--             let newSetTable = (textToCheck,Just (fromIntegral supposedWidth::Float,supposedHeight::Float)) : setTable
--             recursivelySetSizes others newSetTable


-- emptyTextSizeAnalyzer :: TextSizeAnalyzer
-- emptyTextSizeAnalyzer = ([],[])

-- newTextSizeAnalyzer :: TextSizeAnalyzer -> TextSizeAnalyzer
-- newTextSizeAnalyzer (oldEntries,newEntries) = (newEntries,[])
