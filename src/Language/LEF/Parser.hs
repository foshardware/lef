{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Language.LEF.Parser where

import Control.Applicative (optional)
import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec hiding (option, optional)
import Text.Parsec.String (GenParser)
import Text.Parsec.Pos
import Prelude hiding (null)

import Language.LEF.Lexer
import Language.LEF.Syntax


type Parser = GenParser (Lexer Token) ()

parseLEF :: Text -> Either ParseError LEF
parseLEF = parse lef [] . lexer []

lef :: Parser LEF
lef = LEF
  <$> many1 option
  <*> many layer
  <*> many via
  <*> many viaRule
  <*> spacing
  <*> many site
  <*> many1 macro
  <*  endLibrary
  <?> "lef"

option :: Parser Option
option
  =   version
  <|> cases
  <|> bitChars
  <|> divideChars
  <|> units
  <|> useMinSpacing
  <|> clearanceMeasure
  <|> manufacturingGrid
  <?> "option"


version :: Parser Option
version = version_ >> Version <$> double
    <?> "version"

cases :: Parser Option
cases = namescasesensitive_ >> Cases <$> boolean
    <?> "cases"

bitChars :: Parser Option
bitChars = busbitchars_ >> BitChars <$> stringLiteral
    <?> "bit_chars"

divideChars :: Parser Option
divideChars = dividerchar_ >> DivideChar <$> stringLiteral
    <?> "divide_char"

units :: Parser Option
units = units_ >> Units <$> databaseList <* end_ <* units_
    <?> "units"

databaseList :: Parser DatabaseList
databaseList = database_ >> microns_ >> DatabaseList <$> integer
    <?> "database_list"

useMinSpacing :: Parser Option
useMinSpacing = useminspacing_ >> (obs_ <|> pin_) >> UseMinSpacing <$> boolean
    <?> "use_min_spacing"

clearanceMeasure :: Parser Option
clearanceMeasure = clearancemeasure_ >> ClearanceMeasure <$> ident
    <?> "clearance_measure"

manufacturingGrid :: Parser Option
manufacturingGrid = manufacturinggrid_ >> ManufacturingGrid <$> double
    <?> "manufacturing_grid"

layer :: Parser Layer
layer = Layer
  <$> layerName
  <*> many layerOption
  <*> (end_ *> ident)
  <?> "layer"

layerName :: Parser LayerName
layerName = layer_ *> ident <?> "layer_name"

layerOption :: Parser LayerOption
layerOption
  =   Type         <$> (type_        *> ident ) 
  <|> LayerSpacing <$> (spacing_     *> double) <*> many spacingOption
  <|> Direction    <$> (direction_   *> layerDirection)
  <|> Pitch        <$> (pitch_       *> double)
  <|> Offset       <$> (offset_      *> double <* optional double)
  <|> Thickness    <$> (thickness_   *> double)
  <|> Height       <$> (height_      *> double)
  <|> Width        <$> (width_       *> double)
  <|> Resistance   <$> (resistance_  *> optional ident) <*> double
  <|> SpacingTable <$> (spacingtable_ *> spacingTable)
  <|> Capacitance  <$> (capacitance_ *> ident ) <*> double
  <|> EdgeCapacitance <$> (edgecapacitance_ *> double)
  <?> "layer_option"

spacingOption :: Parser SpacingOption
spacingOption
  =   Range <$> (range_ *> double) <*> double
  <?> "spacing_option"

via :: Parser Via
via = via_ >> Via
  <$> viaName
  <*> many viaLayer
  <*> (end_ *> ident)
  <?> "via"

viaName :: Parser ViaName
viaName = ViaName <$> ident <*> ident <?> "via_name"

viaLayer :: Parser ViaLayer
viaLayer = ViaLayer
  <$> viaLayerName
  <*> many viaRect
  <?> "via_layer"

viaLayerName :: Parser ViaLayerName
viaLayerName = layer_ *> ident <?> "via_layer_name"

viaRect :: Parser ViaRect
viaRect = rect_ >> ViaRect
  <$> double
  <*> double
  <*> double
  <*> double
  <?> "via_rect"

viaRule :: Parser ViaRule
viaRule = ViaRule
  <$> viaRuleName
  <*> many viaRuleLayer
  <*> (end_ *> ident)
  <?> "via_rule"

viaRuleName :: Parser ViaRuleName
viaRuleName = viarule_ >> ViaRuleName
  <$> ident
  <*> ident
  <?> "via_rule_name"

viaRuleLayer :: Parser ViaRuleLayer
viaRuleLayer = ViaRuleLayer
  <$> viaRuleLayerName
  <*> many viaRuleLayerOption
  <?> "via_rule_layer"

viaRuleLayerName :: Parser ViaRuleLayerName
viaRuleLayerName = layer_ *> ident <?> "via_rule_layer_name"

viaRuleLayerOption :: Parser ViaRuleLayerOption
viaRuleLayerOption
  =   ViaRuleLayerOptionDirection     <$> (direction_ *> layerDirection)
  <|> ViaRuleLayerOptionWidth         <$> (width_     *> double) <*> (to_ *> double)
  <|> ViaRuleLayerOptionSpacing       <$> (spacing_   *> double) <*> (by_ *> double)
  <|> ViaRuleLayerOptionEnclosure     <$> (enclosure_ *> double) <*> double
  <|> ViaRuleLayerOptionOverhang      <$> (overhang_  *> double)
  <|> ViaRuleLayerOptionMetalOverhang <$> (metaloverhang_ *> double)
  <|> ViaRuleLayerOptionRect          <$> (rect_ *> double) <*> double <*> double <*> double
  <?> "via_rule_layer_option"


spacing :: Parser Spacing
spacing = spacing_ >> Spacing
  <$> many samenet
  <*  (end_ *> spacing_)
  <?> "spacing"

samenet :: Parser Samenet
samenet = samenet_ >> Samenet
  <$> ident
  <*> ident
  <*> double
  <*  optional stack_
  <?> "samenet" 


site :: Parser Site
site = Site
  <$> siteName
  <*> many siteOption
  <*> (end_ *> ident)
  <?> "site"

siteName :: Parser SiteName
siteName = site_ *> ident <?> "site_name"

siteOption :: Parser SiteOption
siteOption
  =   SiteClass    <$> (class_    *> ident )
  <|> SiteSymmetry <$> (symmetry_ *> ident ) <*> optional ident
  <|> SiteSize     <$> (size_     *> double) <*> (by_ *> double)
  <?> "site_option"

macro :: Parser Macro
macro = Macro
  <$> macroName
  <*> many macroOption
  <*> (end_ *> ident)
  <?> "macro"

macroName :: Parser MacroName
macroName = macro_ *> ident <?> "macro_name"

pinUsage :: Parser PinUsage
pinUsage
  =   Analog <$ analog_
  <|> Clock  <$ clock_
  <|> Ground <$ ground_
  <|> Power  <$ power_
  <|> Signal <$ signal_
  <?> "pin_usage"

macroOption :: Parser MacroOption
macroOption
  =   MacroClass    <$> (class_    *> ident ) <*> optional ident
  <|> MacroForeign  <$> (foreign_  *> ident ) <*> double <*> double
  <|> MacroOrigin   <$> (origin_   *> double) <*> double
  <|> MacroSize     <$> (size_     *> double) <*> (by_ *> double)
  <|> MacroSymmetry <$> (symmetry_ *> ident ) <*> optional ident <*> optional ident
  <|> MacroSite     <$> (site_     *> ident )
  <|> MacroPin      <$> (pin_ *> ident) <*> many macroPinOption <*> (end_ *> ident)
  <|> MacroObs      <$> (obs_ *> many macroObsInfo) <* end_
  <?> "macro_option"

macroObsInfo :: Parser MacroObsInfo
macroObsInfo
  =   MacroObsLayer <$> (layer_ *> ident) <*> many (polygon_ *> many1 double)
  <|> MacroObsRect  <$> (rect_ *> double) <*> double <*> double <*> double
  <?> "macro_obs_info"

macroPinOption :: Parser MacroPinOption
macroPinOption
  =   MacroPinName      <$> (pin_ *> ident)
  <|> MacroPinUse       <$> (use_ *> pinUsage)
  <|> MacroPinDirection <$> (direction_ *> portDirection) <*> optional ident
  <|> MacroPinShape     <$> (shape_ *> ident)
  <|> MacroPinPort      <$> (port_  *> many macroPinPortInfo) <* end_
  <|> MacroPinAntennaPartialMetalArea     <$> (antennapartialmetalarea_ *> double) <*> (layer_ *> ident)
  <|> MacroPinAntennaPartialMetalSideArea <$> (antennapartialmetalsidearea_ *> double) <*> (layer_ *> ident)
  <|> MacroPinAntennaGateArea             <$> (antennagatearea_ *> double)
  <|> MacroPinAntennaDiffArea             <$> (antennadiffarea_ *> double)
  <?> "macro_pin_option"

macroPinPortInfo :: Parser MacroPinPortInfo
macroPinPortInfo
  =   MacroPinPortLayer <$> (layer_ *> ident ) <*> many (polygon_ *> many1 double)
  <|> MacroPinPortRect  <$> (rect_  *> double) <*> double <*> double <*> double
  <|> MacroPinPortClass <$> (class_ *> ident )
  <|> MacroPinPortWidth <$> (width_ *> double)
  <|> MacroPinPortPath  <$> (path_  *> double) <*> double <*> double <*> double
  <?> "macro_pin_port_info"

endLibrary :: Parser ()
endLibrary = end_ *> library_

portDirection :: Parser PortDirection
portDirection
  =   InputOutput <$ inout_
  <|> Output <$ output_
  <|> Input  <$ input_
  <?> "port_direction"

layerDirection :: Parser LayerDirection
layerDirection
  =   Horizontal <$ horizontal_
  <|> Vertical   <$ vertical_
  <?> "layer_direction"

spacingTable :: Parser Table 
spacingTable = parallelrunlength_ >> Table
  <$> many1 double
  <*> many1 (width_ *> many1 double)
  <?> "spacing_table"


boolean :: Parser Bool
boolean = True <$ on_ <|> False <$ off_ <?> "boolean"


double :: Parser Double
double = do
    t <- number
    case (T.head t, T.findIndex (== '.') t) of
      ('-', Nothing) -> pure $ fromIntegral $ negate $ numberValue $ T.tail t
      (_, Nothing) -> pure $ fromIntegral $ numberValue t
      ('-', Just i)
         | (a, b) <- T.splitAt i t -> pure
         $ fromIntegral (negate $ numberValue $ T.tail a)
         - fractionValue (T.tail b)
      (_, Just i)
         | (a, b) <- T.splitAt i t -> pure
         $ fromIntegral (numberValue a)
         + fractionValue (T.tail b)
  <?> "double"

integer :: Parser Integer
integer = do
    t <- number
    case T.head t of
      '-' -> pure $ fromIntegral $ negate $ numberValue $ T.tail t
      _   -> pure $ fromIntegral $ numberValue t
  <?> "integer"


numberValue :: Text -> Int
numberValue = T.foldl (\ x c -> 10 * x + digitToInt c) 0


fractionValue :: Text -> Double
fractionValue
    = uncurry (/)
    . bimap fromIntegral fromIntegral
    . T.foldl (\ (s, p) d -> (p * digitToInt d + s, p * 10)) (0, 1)
    . T.dropWhile (== '0')
    . T.reverse


maybeToken :: (Token -> Maybe a) -> Parser a
maybeToken test = token showT posT testT
  where
  showT (L _ t) = show t
  posT  (L x _) = pos2sourcePos x
  testT (L _ t) = test t
  pos2sourcePos (l, c) = newPos "" l c

ident :: Parser Ident
ident = maybeToken q
  where q (Tok_Ident  t) = Just t
        q _ = Nothing

number :: Parser Text
number = maybeToken q
  where q (Tok_Number t) = Just t
        q _ = Nothing

stringLiteral :: Parser Text
stringLiteral = maybeToken q
  where q (Tok_String t) = Just t
        q _ = Nothing

p :: Token -> Parser ()
p t = maybeToken $ \r -> if r == t then Just () else Nothing
end_ = p Tok_End
library_ = p Tok_Library
version_ = p Tok_Version
edgecapacitance_ = p Tok_EdgeCapacitance
capacitance_ = p Tok_Capacitance
spacingtable_ = p Tok_Spacingtable
parallelrunlength_ = p Tok_Parallelrunlength
samenet_ = p Tok_Samenet
antennadiffarea_ = p Tok_Antennadiffarea
antennagatearea_ = p Tok_Antennagatearea
antennapartialmetalsidearea_ = p Tok_Antennapartialmetalsidearea
antennapartialmetalarea_ = p Tok_Antennapartialmetalarea
resistance_ = p Tok_Resistance
height_ = p Tok_Height
width_ = p Tok_Width
offset_ = p Tok_Offset
pitch_ = p Tok_Pitch
direction_ = p Tok_Direction
spacing_ = p Tok_Spacing
type_ = p Tok_Type
layer_ = p Tok_Layer
units_ = p Tok_Units
dividerchar_ = p Tok_DividerChar
microns_ = p Tok_Microns
database_ = p Tok_Database
busbitchars_ = p Tok_BusBitChars
namescasesensitive_ = p Tok_Namescasesensitive
pin_ = p Tok_Pin
obs_ = p Tok_Obs
useminspacing_ = p Tok_UseMinSpacing
rect_ = p Tok_Rect
metaloverhang_ = p Tok_MetalOverhang
by_ = p Tok_By
to_ = p Tok_To
viarule_ = p Tok_ViaRule
manufacturinggrid_ = p Tok_ManufacturingGrid
clearancemeasure_ = p Tok_ClearanceMeasure
symmetry_ = p Tok_Symmetry
stack_ = p Tok_Stack
class_ = p Tok_Class
size_ = p Tok_Size
site_ = p Tok_Site
use_ = p Tok_Use
origin_ = p Tok_Origin
foreign_ = p Tok_Foreign
macro_ = p Tok_Macro
on_ = p Tok_On
off_ = p Tok_Off
via_ = p Tok_Via
overhang_ = p Tok_Overhang
enclosure_ = p Tok_Enclosure
path_ = p Tok_Path
port_ = p Tok_Port
shape_ = p Tok_Shape
input_ = p Tok_Input
output_ = p Tok_Output
inout_ = p Tok_Inout
horizontal_ = p Tok_Horizontal
vertical_ = p Tok_Vertical
thickness_ = p Tok_Thickness
power_ = p Tok_Power
ground_ = p Tok_Ground
polygon_ = p Tok_Polygon
range_ = p Tok_Range
analog_ = p Tok_Analog
clock_ = p Tok_Clock
signal_ = p Tok_Signal
