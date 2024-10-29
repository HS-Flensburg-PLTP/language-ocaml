{-# LANGUAGE DeriveDataTypeable #-}

module Language.OCaml.AST
  ( ToplevelPhrase (..),
    Location (..),
    Position (..),
    Loc (..),
    Longident (..),
    Constant (..),
    MutableFlag (..),
    VirtualFlag (..),
    OverrideFlag (..),
    RecFlag (..),
    ClosedFlag (..),
    DirectionFlag (..),
    PrivateFlag (..),
    Pattern (..),
    PatternDesc (..),
    Payload (..),
    Structure,
    StructureItem (..),
    StructureItemDesc (..),
    Attribute (..),
    SignatureItem (..),
    SignatureItemDesc (..),
    ValueDescription (..),
    ExpressionDesc (..),
    Expression (..),
    ValueBinding (..),
    ValueConstraint (..),
    CoreType (..),
    CoreTypeDesc (..),
    ObjectField (..),
    ObjectFieldDesc (..),
    ArgLabel (..),
    Label,
    FunctionParam (..),
    FunctionParamDesc (..),
    FunctionBody (..),
    Case (..),
    TypeConstraint (..),
    ModuleExpr (..),
    ModuleExprDesc (..),
    FunctorParameter (..),
    ModuleType (..),
    ModuleTypeDesc (..),
    WithConstraint (..),
    TypeDeclaration (..),
    ExtensionConstructor (..),
    ExtensionConstructorKind (..),
    ModuleDeclaration (..),
    ModuleSubstitution (..),
    TypeExtension (..),
    TypeException (..),
    ModuleTypeDeclaration (..),
    OpenInfos (..),
    IncludeInfos (..),
    ClassDescription,
    ClassInfos (..),
    ClassType (..),
    ClassTypeDesc (..),
    ClassSignature (..),
    ClassTypeField (..),
    ClassTypeFieldDesc (..),
    TypeKind (..),
    ConstructorDeclaration (..),
    LabelDeclaration (..),
    ClassTypeDeclaration,
    ConstructorArguments (..),
    ModuleBinding (..),
    ClassDeclaration,
    ClassExpr (..),
    ClassExprDesc (..),
    ClassStructure (..),
    ClassField (..),
    ClassFieldDesc (..),
    ClassFieldKind (..),
    Letop (..),
    BindingOp (..),
    Extension,
    RowField (..),
    RowFieldDesc (..),
    DirectiveArgument (..),
    DirectiveArgumentDesc (..),
    ToplevelDirective (..),
  )
where

import Data.Data (Data)

-- Locations

data Position = Position
  { posFname :: String,
    posLnum :: Int,
    posBol :: Int,
    posCnum :: Int
  }
  deriving (Show, Data)

data Location = Location
  { locStart :: Position,
    locEnd :: Position,
    locGhost :: Bool
  }
  deriving (Show, Data)

data Loc a = Loc
  { content :: a,
    location :: Location
  }
  deriving (Show, Data)

-- values of this type are not printed
-- type LocationStack = [Location]

-- Identifiers

data Longident
  = Lident String
  | Ldot Longident String
  | Lapply Longident Longident
  deriving (Show, Data)

type Label = String

data ArgLabel
  = Nolabel
  | Labelled String
  | Optional String
  deriving (Show, Data)

-- Flags

data RecFlag = Nonrecursive | Recursive
  deriving (Show, Data)

data DirectionFlag = Upto | Downto
  deriving (Show, Data)

data PrivateFlag = Private | Public
  deriving (Show, Data)

data MutableFlag = Immutable | Mutable
  deriving (Show, Data)

data VirtualFlag = Virtual | Concrete
  deriving (Show, Data)

data OverrideFlag = Override | Fresh
  deriving (Show, Data)

data ClosedFlag = Closed | Open
  deriving (Show, Data)

-- values of this type are not printed
-- data Variance = Covariant | Contravariant | NoVariance
--   deriving (Show, Data)

-- values of this type are not printed
-- data Injectivity = Injective | NoInjectivity
-- deriving (Show, Data)

-- Attributes

type Attributes = [Attribute]

data Attribute = Attribute
  { -- should be `Loc String`
    attrName :: String,
    attrPayload :: Payload
    -- not printed
    -- attrLoc :: Location
  }
  deriving (Show, Data)

-- should be `(Loc String, Payload)`
type Extension = (String, Payload)

data Payload
  = PStr Structure
  | PSig Signature
  | PTyp CoreType
  | PPat Pattern (Maybe Expression)
  deriving (Show, Data)

-- Types

data CoreType = CoreType
  { ptypDesc :: CoreTypeDesc,
    ptypLoc :: Location,
    -- not printed
    -- ptypLocStack :: LocationStack,
    ptypAttributes :: Attributes
  }
  deriving (Show, Data)

data CoreTypeDesc
  = PtypAny
  | PtypVar String
  | PtypArrow ArgLabel CoreType CoreType
  | PtypTuple [CoreType]
  | PtypConstr (Loc Longident) [CoreType]
  | PtypObject [ObjectField] ClosedFlag
  | PtypClass (Loc Longident) [CoreType]
  | -- should be `Loc String`
    PtypAlias CoreType String
  | PtypVariant [RowField] ClosedFlag (Maybe [Label])
  | -- should be `[Loc String]`
    PtypPoly [String] CoreType
  | PtypPackage PackageType
  | PtypOpen (Loc Longident) CoreType
  | PtypExtension Extension
  deriving (Show, Data)

data RowField = RowField
  { prfDesc :: RowFieldDesc,
    -- not printed
    -- prfLoc :: Location,
    prfAttributes :: Attributes
  }
  deriving (Show, Data)

data RowFieldDesc
  = -- should be `Loc Label`
    Rtag Label Bool [CoreType]
  | Rinherit CoreType
  deriving (Show, Data)

data ObjectField = ObjectField
  { pofDesc :: ObjectFieldDesc,
    pofAttributes :: Attributes
    -- not printed
    -- pofLoc :: Location,
  }
  deriving (Show, Data)

data ObjectFieldDesc
  = -- should be `Loc Label`
    Otag Label CoreType
  | Oinherit CoreType
  deriving (Show, Data)

type PackageType = (Loc Longident, [(Loc Longident, CoreType)])

data TypeConstraint
  = Pconstraint CoreType
  | Pcoerce (Maybe CoreType) CoreType
  deriving (Show, Data)

data TypeKind
  = PtypeAbstract
  | PtypeVariant [ConstructorDeclaration]
  | PtypeRecord [LabelDeclaration]
  | PtypeOpen
  deriving (Show, Data)

data ConstructorDeclaration = ConstructorDeclaration
  { pcdName :: Loc String,
    -- should be `[Loc String]`
    pcdVars :: [String],
    pcdArgs :: ConstructorArguments,
    pcdRes :: Maybe CoreType,
    pcdLoc :: Location,
    pcdAttributes :: Attributes
  }
  deriving (Show, Data)

data ConstructorArguments = PcstrTuple [CoreType] | PcstrRecord [LabelDeclaration]
  deriving (Show, Data)

data ExtensionConstructorKind
  = -- should be `[Loc String]`
    PextDecl [String] ConstructorArguments (Maybe CoreType)
  | PextRebind (Loc Longident)
  deriving (Show, Data)

data LabelDeclaration = LabelDeclaration
  { pldName :: Loc String,
    pldMutable :: MutableFlag,
    pldType :: CoreType,
    pldLoc :: Location,
    pldAttributes :: Attributes
  }
  deriving (Show, Data)

-- Constants

data Constant
  = PconstInteger String (Maybe Char)
  | PconstChar Char
  | PconstString String Location (Maybe String)
  | PconstFloat String (Maybe Char)
  deriving (Show, Data)

-- Patterns

data Pattern = Pattern
  { ppatDesc :: PatternDesc,
    ppatLoc :: Location,
    -- not printed
    -- ppatLocStack :: LocationStack,
    ppatAttributes :: Attributes
  }
  deriving (Show, Data)

data PatternDesc
  = PpatAny
  | PpatVar (Loc String)
  | PpatAlias Pattern (Loc String)
  | PpatConstant Constant
  | PpatInterval Constant Constant
  | PpatTuple [Pattern]
  | PpatConstruct (Loc Longident) (Maybe ([Loc String], Pattern))
  | PpatVariant Label (Maybe Pattern)
  | PpatRecord [(Loc Longident, Pattern)] ClosedFlag
  | PpatArray [Pattern]
  | PpatOr Pattern Pattern
  | PpatConstraint Pattern CoreType
  | PpatType (Loc Longident)
  | PpatLazy Pattern
  | PpatUnpack (Loc (Maybe String))
  | PpatException Pattern
  | PpatExtension Extension
  | PpatOpen (Loc Longident) Pattern
  deriving (Show, Data)

-- Expressions

data Expression = Expression
  { pexpDesc :: ExpressionDesc,
    pexpLoc :: Location,
    -- not printed
    -- pexpLocStack :: LocationStack,
    pexpAttributes :: Attributes
  }
  deriving (Show, Data)

data ExpressionDesc
  = PexpIdent (Loc Longident)
  | PexpConstant Constant
  | PexpLet RecFlag [ValueBinding] Expression
  | PexpFunction [FunctionParam] (Maybe TypeConstraint) FunctionBody
  | PexpApply Expression [(ArgLabel, Expression)]
  | PexpMatch Expression [Case]
  | PexpTry Expression [Case]
  | PexpTuple [Expression]
  | PexpConstruct (Loc Longident) (Maybe Expression)
  | PexpVariant Label (Maybe Expression)
  | PexpRecord [(Loc Longident, Expression)] (Maybe Expression)
  | PexpField Expression (Loc Longident)
  | PexpSetfield Expression (Loc Longident) Expression
  | PexpArray [Expression]
  | PexpIfthenelse Expression Expression (Maybe Expression)
  | PexpSequence Expression Expression
  | PexpWhile Expression Expression
  | PexpFor Pattern Expression Expression DirectionFlag Expression
  | PexpConstraint Expression CoreType
  | PexpCoerce Expression (Maybe CoreType) CoreType
  | -- should be `Loc Label`
    PexpSend Expression Label
  | PexpNew (Loc Longident)
  | PexpSetinstvar (Loc Label) Expression
  | PexpOverride [(Loc Label, Expression)]
  | PexpLetmodule (Loc (Maybe String)) ModuleExpr Expression
  | PexpLetexception ExtensionConstructor Expression
  | PexpAssert Expression
  | PexpLazy Expression
  | PexpPoly Expression (Maybe CoreType)
  | PexpObject ClassStructure
  | -- should be 'Loc String'
    PexpNewtype String Expression
  | PexpPack ModuleExpr
  | PexpOpen OpenDeclaration Expression
  | PexpLetop Letop
  | PexpExtension Extension
  | PexpUnreachable
  deriving (Show, Data)

data Case = Case
  { pcLhs :: Pattern,
    pcGuard :: Maybe Expression,
    pcRhs :: Expression
  }
  deriving (Show, Data)

data Letop = Letop
  { let_ :: BindingOp,
    ands :: [BindingOp],
    body :: Expression
  }
  deriving (Show, Data)

data BindingOp = BindingOp
  { pbopOp :: Loc String,
    pbopPat :: Pattern,
    pbopExp :: Expression,
    pbopLoc :: Location
  }
  deriving (Show, Data)

data FunctionParamDesc
  = PparamVal ArgLabel (Maybe Expression) Pattern
  | -- | should be `Loc String`
    PparamNewtype String
  deriving (Show, Data)

data FunctionParam = FunctionParam
  { pparamLoc :: Location,
    pparamDesc :: FunctionParamDesc
  }
  deriving (Show, Data)

data FunctionBody
  = PfunctionBody Expression
  | PfunctionCases [Case] Location Attributes
  deriving (Show, Data)

-- ClassInfos

data ClassInfos a = ClassInfos
  { pciVirt :: VirtualFlag,
    -- should be `[(CoreType, (Variance, Injectivity))]`
    pciParams :: [CoreType],
    pciName :: Loc String,
    pciExpr :: a,
    pciLoc :: Location,
    pciAttributes :: Attributes
  }
  deriving (Show, Data)

type ClassDescription = ClassInfos ClassType

type ClassTypeDeclaration = ClassInfos ClassType

type ClassDeclaration = ClassInfos ClassExpr

data ClassType = ClassType
  { pctyDesc :: ClassTypeDesc,
    pctyLoc :: Location,
    pctyAttributes :: Attributes
  }
  deriving (Show, Data)

data ClassTypeDesc
  = PctyConstr (Loc Longident) [CoreType]
  | PctySignature ClassSignature
  | PctyArrow ArgLabel CoreType ClassType
  | PctyExtension Extension
  | PctyOpen OpenDescription ClassType
  deriving (Show, Data)

data ClassSignature = ClassSignature
  { pcsigSelf :: CoreType,
    pcsigFields :: [ClassTypeField]
  }
  deriving (Show, Data)

data ClassTypeField = ClassTypeField
  { pctfDesc :: ClassTypeFieldDesc,
    pctfLoc :: Location,
    pctfAttributes :: Attributes
  }
  deriving (Show, Data)

data ClassTypeFieldDesc
  = PctfInherit ClassType
  | -- should be `Loc Label`
    PctfVal Label MutableFlag VirtualFlag CoreType
  | -- should be `Loc Label`
    PctfMethod Label PrivateFlag VirtualFlag CoreType
  | PctfConstraint CoreType CoreType
  | PctfAttribute Attribute
  | PctfExtension Extension
  deriving (Show, Data)

data ClassExpr = ClassExpr
  { pclDesc :: ClassExprDesc,
    pclLoc :: Location,
    pclAttributes :: Attributes
  }
  deriving (Show, Data)

data ClassExprDesc
  = PclConstr (Loc Longident) [CoreType]
  | PclStructure ClassStructure
  | PclFun ArgLabel (Maybe Expression) Pattern ClassExpr
  | PclApply ClassExpr [(ArgLabel, Expression)]
  | PclLet RecFlag [ValueBinding] ClassExpr
  | PclConstraint ClassExpr ClassType
  | PclExtension Extension
  | PclOpen OpenDescription ClassExpr
  deriving (Show, Data)

data ClassStructure = ClassStructure
  { pcstrSelf :: Pattern,
    pcstrFields :: [ClassField]
  }
  deriving (Show, Data)

data ClassField = ClassField
  { pcfDesc :: ClassFieldDesc,
    pcfLoc :: Location,
    pcfAttributes :: Attributes
  }
  deriving (Show, Data)

data ClassFieldDesc
  = PcfInherit OverrideFlag ClassExpr (Maybe (Loc String))
  | PcfVal (Loc Label) MutableFlag ClassFieldKind
  | PcfMethod (Loc Label) PrivateFlag ClassFieldKind
  | PcfConstraint CoreType CoreType
  | PcfInitializer Expression
  | PcfAttribute Attribute
  | PcfExtension Extension
  deriving (Show, Data)

data ClassFieldKind = CfkVirtual CoreType | CfkConcrete OverrideFlag Expression
  deriving (Show, Data)

-- OpenInfos

data OpenInfos a = OpenInfos
  { popenExpr :: a,
    popenOverride :: OverrideFlag,
    -- not printed
    -- popenLoc :: Location,
    popenAttributes :: Attributes
  }
  deriving (Show, Data)

type OpenDescription = OpenInfos (Loc Longident)

type OpenDeclaration = OpenInfos ModuleExpr

data ModuleExpr = ModuleExpr
  { pmodDesc :: ModuleExprDesc,
    pmodLoc :: Location,
    pmodAttributes :: Attributes
  }
  deriving (Show, Data)

data ModuleExprDesc
  = PmodIdent (Loc Longident)
  | PmodStructure Structure
  | PmodFunctor FunctorParameter ModuleExpr
  | PmodApply ModuleExpr ModuleExpr
  | PmodApplyUnit ModuleExpr
  | PmodConstraint ModuleExpr ModuleType
  | PmodUnpack Expression
  | PmodExtension Extension
  deriving (Show, Data)

-- IncludeInfos

data IncludeInfos a = IncludeInfos
  { pinclMod :: a,
    -- not printed
    -- pinclLoc :: Location,
    pinclAttributes :: Attributes
  }
  deriving (Show, Data)

type IncludeDescription = IncludeInfos ModuleType

type IncludeDeclaration = IncludeInfos ModuleExpr

data ModuleType = ModuleType
  { pmtyDesc :: ModuleTypeDesc,
    pmtyLoc :: Location,
    pmtyAttributes :: Attributes
  }
  deriving (Show, Data)

data ModuleTypeDesc
  = PmtyIdent (Loc Longident)
  | PmtySignature Signature
  | PmtyFunctor FunctorParameter ModuleType
  | PmtyWith ModuleType [WithConstraint]
  | PmtyTypeof ModuleExpr
  | PmtyExtension Extension
  | PmtyAlias (Loc Longident)
  deriving (Show, Data)

data WithConstraint
  = PwithType (Loc Longident) TypeDeclaration
  | PwithModule (Loc Longident) (Loc Longident)
  | PwithModtype (Loc Longident) ModuleType
  | PwithModtypesubst (Loc Longident) ModuleType
  | PwithTypesubst (Loc Longident) TypeDeclaration
  | PwithModsubst (Loc Longident) (Loc Longident)
  deriving (Show, Data)

data FunctorParameter = Unit | Named (Loc (Maybe String)) ModuleType
  deriving (Show, Data)

-- Declarations

data TypeDeclaration = TypeDeclaration
  { ptypeName :: Loc String,
    -- should be `[(CoreType, (Variance, Injectivity))]`
    ptypeParams :: [CoreType],
    ptypeCstrs :: [(CoreType, CoreType, Location)],
    ptypeKind :: TypeKind,
    ptypePrivate :: PrivateFlag,
    ptypeManifest :: Maybe CoreType,
    ptypeAttributes :: Attributes,
    ptypeLoc :: Location
  }
  deriving (Show, Data)

data ModuleDeclaration = ModuleDeclaration
  { pmdName :: Loc (Maybe String),
    pmdType :: ModuleType,
    pmdAttributes :: Attributes
    -- not printed
    -- pmdLoc :: Location
  }
  deriving (Show, Data)

data ValueDescription = ValueDescription
  { pvalName :: Loc String,
    pvalType :: CoreType,
    pvalPrim :: [String],
    pvalAttributes :: Attributes,
    pvalLoc :: Location
  }
  deriving (Show, Data)

data TypeExtension = TypeExtension
  { ptyextPath :: Loc Longident,
    -- should be `[(CoreType, (Variance, Injectivity))]`
    ptyextParams :: [CoreType],
    ptyextConstructors :: [ExtensionConstructor],
    ptyextPrivate :: PrivateFlag,
    -- not printed
    -- ptyextLoc :: Location,
    ptyextAttributes :: Attributes
  }
  deriving (Show, Data)

data ExtensionConstructor = ExtensionConstructor
  -- should be `Loc String`
  { pextName :: String,
    pextKind :: ExtensionConstructorKind,
    pextLoc :: Location,
    pextAttributes :: Attributes
  }
  deriving (Show, Data)

data TypeException = TypeException
  { ptyexnConstructor :: ExtensionConstructor,
    -- not printed
    -- ptyexnLoc :: Location,
    ptyexnAttributes :: Attributes
  }
  deriving (Show, Data)

data ModuleSubstitution = ModuleSubstitution
  { pmsName :: Loc String,
    pmsManifest :: Loc Longident,
    pmsAttributes :: Attributes
    -- not printed
    -- pmsLoc :: Location
  }
  deriving (Show, Data)

data ModuleTypeDeclaration = ModuleTypeDeclaration
  { pmtdName :: Loc String,
    pmtdType :: Maybe ModuleType,
    pmtdAttributes :: Attributes
    -- not printed
    -- pmtdLoc :: Location
  }
  deriving (Show, Data)

data ValueBinding = ValueBinding
  { pvbPat :: Pattern,
    pvbExpr :: Expression,
    pvbConstraint :: Maybe ValueConstraint,
    pvbAttributes :: Attributes
    -- not printed
    -- pvbLoc :: Location
  }
  deriving (Show, Data)

data ValueConstraint
  = PvcConstraint {locallyAbstractUnivars :: [Loc String], typ :: CoreType}
  | PvcCoercion {ground :: Maybe CoreType, coercion :: CoreType}
  deriving (Show, Data)

data ModuleBinding = ModuleBinding
  { pmbName :: Loc (Maybe String),
    pmbExpr :: ModuleExpr,
    pmbAttributes :: Attributes
    -- not printed
    -- pmbLoc :: Location
  }
  deriving (Show, Data)

-- Toplevel Structures

type Structure = [StructureItem]

data StructureItem = StructureItem
  { pstrDesc :: StructureItemDesc,
    pstrLoc :: Location
  }
  deriving (Show, Data)

data StructureItemDesc
  = PstrEval Expression Attributes
  | PstrValue RecFlag [ValueBinding]
  | PstrPrimitive ValueDescription
  | PstrType RecFlag [TypeDeclaration]
  | PstrTypext TypeExtension
  | PstrException TypeException
  | PstrModule ModuleBinding
  | PstrRecmodule [ModuleBinding]
  | PstrModtype ModuleTypeDeclaration
  | PstrOpen OpenDeclaration
  | PstrClass [ClassDeclaration]
  | PstrClassType [ClassTypeDeclaration]
  | PstrInclude IncludeDeclaration
  | PstrAttribute Attribute
  | PstrExtension Extension Attributes
  deriving (Show, Data)

type Signature = [SignatureItem]

data SignatureItem = SignatureItem
  { psigDesc :: SignatureItemDesc,
    psigLoc :: Location
  }
  deriving (Show, Data)

data SignatureItemDesc
  = PsigValue ValueDescription
  | PsigType RecFlag [TypeDeclaration]
  | PsigTypesubst [TypeDeclaration]
  | PsigTypext TypeExtension
  | PsigException TypeException
  | PsigModule ModuleDeclaration
  | PsigModsubst ModuleSubstitution
  | PsigRecmodule [ModuleDeclaration]
  | PsigModtype ModuleTypeDeclaration
  | PsigModtypesubst ModuleTypeDeclaration
  | PsigOpen OpenDescription
  | PsigInclude IncludeDescription
  | PsigClass [ClassDescription]
  | PsigClassType [ClassTypeDeclaration]
  | PsigAttribute Attribute
  | PsigExtension Extension Attributes
  deriving (Show, Data)

data ToplevelPhrase
  = PtopDef Structure
  | PtopDir ToplevelDirective
  deriving (Show, Data)

data ToplevelDirective = ToplevelDirective
  { -- should be `Loc String`
    pdirName :: String,
    pdirArg :: Maybe DirectiveArgument
    -- not printed
    -- pdirLoc :: Location
  }
  deriving (Show, Data)

newtype DirectiveArgument = DirectiveArgument
  { pdiraDesc :: DirectiveArgumentDesc
  -- not printed
  -- pdiraLoc :: Location
  }
  deriving (Show, Data)

data DirectiveArgumentDesc
  = PdirString String
  | PdirInt String (Maybe Char)
  | PdirIdent Longident
  | PdirBool Bool
  deriving (Show, Data)
