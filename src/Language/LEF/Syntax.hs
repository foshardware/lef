
module Language.LEF.Syntax where

import Data.Text (Text)

type Ident = Text

data LEF = LEF [Option] [Layer] [Via] [ViaRule] Spacing [Site] [Macro]
  deriving (Eq, Show)

data Option
  = Version Double
  | Cases Bool
  | BitChars Ident
  | DivideChar Ident
  | Units DatabaseList
  | UseMinSpacing Bool
  | ClearanceMeasure Ident
  | ManufacturingGrid Double
  deriving (Eq, Show)

newtype DatabaseList = DatabaseList Integer
  deriving (Eq, Show)

data Layer = Layer LayerName [LayerOption] Text
  deriving (Eq, Show)

type LayerName = Ident

data LayerOption
  = Type Ident
  | LayerSpacing Double [SpacingOption]
  | Direction LayerDirection
  | Pitch Double
  | Offset Double
  | Thickness Double
  | Height Double
  | Width Double
  | Resistance (Maybe Ident) Double
  | Capacitance Ident Double
  | EdgeCapacitance Double
  | SpacingTable Table
  deriving (Eq, Show)

data SpacingOption
  = Range Double Double
  deriving (Eq, Show)

data PortDirection = Input | Output | InputOutput
  deriving (Eq, Show)

data LayerDirection = Horizontal | Vertical
  deriving (Eq, Show)

data Via = Via ViaName [ViaLayer] Ident
  deriving (Eq, Show)

data ViaName = ViaName Ident Ident
  deriving (Eq, Show)

data ViaLayer = ViaLayer ViaLayerName [ViaRect]
  deriving (Eq, Show)

type ViaLayerName = Ident

data ViaRect = ViaRect Double Double Double Double
  deriving (Eq, Show)

data ViaRule = ViaRule ViaRuleName [ViaRuleLayer] Ident
  deriving (Eq, Show)

data ViaRuleName = ViaRuleName Ident Ident
  deriving (Eq, Show)

data ViaRuleLayer = ViaRuleLayer ViaRuleLayerName [ViaRuleLayerOption]
  deriving (Eq, Show)

type ViaRuleLayerName = Ident

data ViaRuleLayerOption
  = ViaRuleLayerOptionDirection LayerDirection
  | ViaRuleLayerOptionWidth Double Double
  | ViaRuleLayerOptionWidthDiscrete Double Integer
  | ViaRuleLayerOptionOverhang Double
  | ViaRuleLayerOptionEnclosure Double Double
  | ViaRuleLayerOptionOverhangDiscrete Integer
  | ViaRuleLayerOptionMetalOverhang Double
  | ViaRuleLayerOptionMetalOverhangDiscrete Integer
  | ViaRuleLayerOptionRect Double Double Double Double
  | ViaRuleLayerOptionSpacing Double Double
  deriving (Eq, Show)


data Spacing = Spacing [Samenet]
  deriving (Eq, Show)

data Samenet = Samenet Ident Ident Double
  deriving (Eq, Show)


data Site = Site SiteName [SiteOption] Ident
  deriving (Eq, Show)

type SiteName = Ident

data SiteOption
  = SiteClass Ident
  | SiteSymmetry Ident (Maybe Ident)
  | SiteSize Double Double
  deriving (Eq, Show)

data Macro = Macro MacroName [MacroOption] Ident
  deriving (Eq, Show)

data Table = Table [Double] [[Double]]
  deriving (Eq, Show)

type MacroName = Ident

data PinUsage = Analog | Clock | Ground | Power | Signal
  deriving (Eq, Show)

data MacroOption
  = MacroClass Ident (Maybe Ident)
  | MacroForeign Ident Double Double
  | MacroOrigin Double Double
  | MacroSize Double Double
  | MacroSymmetry Ident (Maybe Ident) (Maybe Ident)
  | MacroSite Ident
  | MacroPin Ident [MacroPinOption] Ident
  | MacroObs [MacroObsInfo]
  deriving (Eq, Show)

data MacroPinOption
  = MacroPinName Ident
  | MacroPinUse PinUsage
  | MacroPinDirection PortDirection (Maybe Ident)
  | MacroPinShape Ident
  | MacroPinPort [MacroPinPortInfo]
  | MacroPinAntennaPartialMetalArea Double Ident
  | MacroPinAntennaPartialMetalSideArea Double Ident
  | MacroPinAntennaGateArea Double
  | MacroPinAntennaDiffArea Double
  deriving (Eq, Show)

data MacroPinPortInfo
  = MacroPinPortLayer Ident [[Double]]
  | MacroPinPortRect Double Double Double Double
  | MacroPinPortClass Ident
  | MacroPinPortWidth Double
  | MacroPinPortPath Double Double Double Double
  deriving (Eq, Show)

data MacroObsInfo
  = MacroObsLayer Ident [[Double]]
  | MacroObsRect Double Double Double Double
  deriving (Eq, Show)


