{-# LANGUAGE OverloadedStrings #-}

module Language.OCaml.Parser (parseFile, interfaceParser, implementationParser, topPhraseParser) where

import Control.Applicative (optional, some)
import Control.Exception (SomeException, catch)
import qualified Data.Char as Char
import Data.Functor (void)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Language.OCaml.AST
import qualified Language.OCaml.AST as AST
import Path (File, Path, Rel)
import qualified Path
import Shelly (lastStderr, run_, shelly, silently)
import Text.Megaparsec
  ( Parsec,
    between,
    errorBundlePretty,
    many,
    noneOf,
    oneOf,
    runParser,
    sepBy,
    sepBy1,
    takeWhileP,
    try,
    (<|>),
  )
import Text.Megaparsec.Char
  ( char,
    digitChar,
    hspace,
    letterChar,
    lowerChar,
    newline,
    string,
    upperChar,
  )
import Text.Megaparsec.Char.Lexer (decimal, hexadecimal)

parseFile :: Path Rel File -> IO (Either String [AST.StructureItem])
parseFile ocamlPath = do
  eitherOutput <- runOCaml ocamlPath
  case eitherOutput of
    Left err -> return (Left err)
    Right output -> do
      writeFile "output.txt" (Text.unpack output)
      let cleanedOutput = unlines (map (dropWhile (== ' ')) (lines (Text.unpack output)))
      case runParser implementationParser (Path.fromRelFile ocamlPath) cleanedOutput of
        Left err -> return (Left (errorBundlePretty err))
        Right ast -> return (Right ast)

runOCaml :: Path Rel File -> IO (Either String Text)
runOCaml modulePath =
  catch
    ( shelly
        ( do
            silently
              ( run_
                  "ocamlc"
                  [ "-dparsetree",
                    "-stop-after",
                    "parsing",
                    Text.pack (Path.fromRelFile modulePath)
                  ]
              )
            fmap Right lastStderr
        )
    )
    handle
  where
    handle :: SomeException -> IO (Either String Text)
    handle e = return (Left ("Failed running ocaml with exception " ++ show e))

-- Generic Combinators

quotedParser :: Parsec Void String a -> Parsec Void String a
quotedParser = between (char '"') (char '"')

data Separator = Newline | NoNewline

optionParser :: Separator -> Parsec Void String a -> Parsec Void String (Maybe a)
optionParser separator parser =
  (Just <$ string "Some" <* newlineSep <* hspace <*> parser)
    <|> (Nothing <$ string "None" <* newlineSep)
  where
    newlineSep :: Parsec Void String ()
    newlineSep = case separator of
      Newline -> void newline
      NoNewline -> return ()

listParser :: Parsec Void String a -> Parsec Void String [a]
listParser parser =
  char '['
    *> ( ([] <$ char ']' <* newline)
           <|> (newline *> many parser <* hspace <* char ']' <* newline)
       )

-- Identifiers

uidentParser :: Parsec Void String String
uidentParser = (:) <$> upperChar <*> many (letterChar <|> digitChar <|> char '_' <|> char '\'')

lidentParser :: Parsec Void String String
lidentParser = (:) <$> (lowerChar <|> char '_') <*> many (letterChar <|> digitChar <|> char '_' <|> char '\'')

symbolOpParser :: Parsec Void String String
symbolOpParser = some (oneOf (symbols ++ extraSymbols))
  where
    symbols :: [Char]
    symbols = "!$%&*+-./:<=>?@^|~#"
    extraSymbols :: [Char]
    extraSymbols = "()[]{}"

nameOpParser :: Parsec Void String String
nameOpParser =
  (\str c1 str2 -> str ++ c1 : str2)
    <$> (string "and" <|> string "let")
    <*> oneOf kwdopchar
    <*> many (oneOf dotsymbolchar)
  where
    kwdopchar :: [Char]
    kwdopchar = "$&*+-/<=>@^|"
    dotsymbolchar :: [Char]
    dotsymbolchar = "!$%&*+-/:=>?@^|"

moduleNameParser :: Parsec Void String String
moduleNameParser = uidentParser <|> string "_"

constrIdentParser :: Parsec Void String String
constrIdentParser = uidentParser <|> string "()" <|> string "::" <|> string "[]" <|> string "true" <|> string "false"

valueIdentParser :: Parsec Void String String
valueIdentParser = try nameOpParser <|> symbolOpParser <|> lidentParser

identParser :: Parsec Void String String
identParser = try nameOpParser <|> symbolOpParser <|> lidentParser <|> uidentParser

attributeNameParser :: Parsec Void String String
attributeNameParser = uidentParser <|> lidentParser <|> ((:) <$> char '%' <*> lidentParser)

labelParser :: Parsec Void String Label
labelParser = lidentParser <|> uidentParser

-- | The original grammar A -> ident | A . ident | A ( A ) is left recursive, therefore, we use
--   `A -> ident A'` and `A' -> . ident A' | ( A ) A' | ε`
longidentParser :: Parsec Void String Longident
longidentParser = quotedParser longidentParserRec
  where
    longidentParserRec =
      (\ident context -> context (Lident ident))
        <$> identParser
        <*> longidentParserFactored
    longidentParserFactored =
      ( (\ident context left -> context (Ldot left ident))
          <$ char '.'
          <*> identParser
          <*> longidentParserFactored
      )
        <|> ( (\longident context left -> context (Lapply left longident))
                <$ char '('
                <*> longidentParserRec
                <* char ')'
                <*> longidentParserFactored
            )
        <|> pure id

-- Locations

positionParser :: Parsec Void String Position
positionParser =
  Position
    <$> takeWhileP Nothing (/= '[')
    <* char '['
    <*> decimal
    <* char ','
    <*> decimal
    <* char '+'
    -- it looks like the compiler uses `-1` as default value if none is available
    <*> (decimal <|> -1 <$ string "-1")
    <* char ']'

locationParser :: Parsec Void String Location
locationParser =
  Location
    <$ char '('
    <*> positionParser
    <* string ".."
    <*> positionParser
    <* char ')'
    <*> ((True <$ try (hspace <* string "ghost")) <|> pure False)

locParser :: Parsec Void String a -> Parsec Void String (Loc a)
locParser parser = Loc <$> parser <* hspace <*> locationParser

locOptionParser :: Parsec Void String a -> Parsec Void String (Loc (Maybe a))
locOptionParser parser =
  (Loc . Just <$> quotedParser parser <* hspace <*> locationParser)
    <|> (Loc Nothing <$ string "_" <* hspace <*> locationParser)

-- Flags

mutableFlagParser :: Parsec Void String MutableFlag
mutableFlagParser = Immutable <$ string "Immutable" <|> Mutable <$ string "Mutable"

virtualFlagParser :: Parsec Void String VirtualFlag
virtualFlagParser = Virtual <$ string "Virtual" <|> Concrete <$ string "Concrete"

overrideFlagParser :: Parsec Void String OverrideFlag
overrideFlagParser = Override <$ string "Override" <|> Fresh <$ string "Fresh"

closedFlagParser :: Parsec Void String ClosedFlag
closedFlagParser = Closed <$ string "Closed" <|> Open <$ string "Open"

recFlagParser :: Parsec Void String RecFlag
recFlagParser = Nonrecursive <$ string "Nonrec" <|> Recursive <$ string "Rec"

directionFlagParser :: Parsec Void String DirectionFlag
directionFlagParser = Upto <$ string "Up" <|> Downto <$ string "Down"

privateFlagParser :: Parsec Void String PrivateFlag
privateFlagParser = Public <$ string "Public" <|> Private <$ string "Private"

--

optionCharParser :: Parsec Void String (Maybe Char)
optionCharParser = optionParser NoNewline letterChar

stringParser :: Parsec Void String String
stringParser = quotedParser (concat <$> many stringParser')
  where
    stringParser' =
      ((: []) <$> noneOf ['"', '\\'])
        <|> ((\c1 c2 -> [c1, c2]) <$> char '\\' <*> (oneOf ['\\', '"', '\'', 'n', 'r', 't', 'b', 'x', 'o', 'u'] <|> digitChar))

extensionParser :: Parsec Void String Extension
extensionParser =
  (,) <$> quotedParser (concat <$> sepBy1 (lidentParser <|> uidentParser) (char '.')) <* newline <*> payloadParser

constantParser :: Parsec Void String Constant
constantParser =
  ( PconstFloat . show
      <$ string "PConst_float"
      <* hspace
      <* char '('
      <*> takeWhileP Nothing (/= ',')
      <* char ','
      <*> optionCharParser
      <* char ')'
  )
    <|> ( PconstInteger . show
            <$ string "PConst_int"
            <* hspace
            <* char '('
            <*> takeWhileP Nothing (/= ',')
            <* char ','
            <*> optionCharParser
            <* char ')'
        )
    <|> (PconstChar . Char.chr <$> (string "PConst_char" *> hspace *> hexadecimal))
    <|> ( PconstString
            <$ string "PConst_string"
            <* hspace
            <* char '('
            <*> stringParser
            <* char ','
            <*> locationParser
            <* char ','
            <*> optionParser NoNewline stringParser
            <* char ')'
        )

argLabelParser :: Parsec Void String ArgLabel
argLabelParser =
  (Labelled <$ string "Labelled" <* hspace <*> stringParser <* newline)
    <|> (Nolabel <$ string "Nolabel" <* newline)
    <|> (Optional <$ string "Optional" <* hspace <*> stringParser <* newline)

typeVarsParser :: Parsec Void String [String]
typeVarsParser = sepBy1 (char '\'' *> (lidentParser <|> uidentParser)) hspace

coreTypeParser :: Parsec Void String CoreType
coreTypeParser =
  (\loc attrs desc -> CoreType desc loc attrs)
    <$ string "core_type"
    <* hspace
    <*> locationParser
    <* newline
    <*> attributesParser
    <*> coreTypeDescParser

coreTypeDescParser :: Parsec Void String CoreTypeDesc
coreTypeDescParser =
  ( flip PtypAlias
      <$ string "Ptyp_alias"
      <* hspace
      <*> stringParser
      <* newline
      <*> coreTypeParser
  )
    <|> (PtypAny <$ string "Ptyp_any" <* newline)
    <|> ( PtypArrow
            <$ string "Ptyp_arrow"
            <* newline
            <*> argLabelParser
            <*> coreTypeParser
            <*> coreTypeParser
        )
    <|> ( PtypClass
            <$ string "Ptyp_class"
            <* hspace
            <*> locParser longidentParser
            <* newline
            <*> listParser coreTypeParser
        )
    <|> ( PtypConstr
            <$ string "Ptyp_constr"
            <* hspace
            <*> locParser longidentParser
            <* newline
            <*> listParser coreTypeParser
        )
    <|> ( curry PtypExtension
            <$ string "Ptyp_extension"
            <* hspace
            <*> stringParser
            <* newline
            <*> payloadParser
        )
    <|> ( flip PtypObject
            <$ string "Ptyp_object"
            <* hspace
            <*> closedFlagParser
            <* newline
            <*> many objectFieldParser
        )
    <|> ( PtypOpen
            <$ string "Ptyp_open"
            <* hspace
            <*> quotedParser (locParser longidentParser)
            <* newline
            <*> coreTypeParser
        )
    <|> ( PtypPackage
            <$ string "Ptyp_package"
            <* hspace
            <*> ( (,)
                    <$> locParser longidentParser
                    <* newline
                    <*> listParser packageWithParser
                )
        )
    <|> ( PtypPoly
            <$ string "Ptyp_poly"
            <* hspace
            <*> typeVarsParser
            <* newline
            <*> coreTypeParser
        )
    <|> ( PtypTuple
            <$ string "Ptyp_tuple"
            <* newline
            <*> listParser coreTypeParser
        )
    <|> ( flip PtypVariant
            <$ string "Ptyp_variant"
            <* hspace
            <* string "closed="
            <*> closedFlagParser
            <* newline
            <*> listParser labelXBoolXCoreTypeListParser
            <*> optionParser Newline (listParser stringParser)
        )
    <|> ( PtypVar
            <$ string "Ptyp_var"
            <* hspace
            <*> (lidentParser <|> uidentParser)
            <* newline
        )

objectFieldParser :: Parsec Void String ObjectField
objectFieldParser =
  ( (\label attrs coreType -> ObjectField {pofDesc = Otag label coreType, pofAttributes = attrs})
      <$ string "method"
      <* hspace
      <*> labelParser
      <* newline
      <*> attributesParser
      <*> coreTypeParser
  )
    <|> ( (\coreType -> ObjectField {pofDesc = Oinherit coreType, pofAttributes = []})
            <$ string "Oinherit"
            <* newline
            <*> coreTypeParser
        )

packageWithParser :: Parsec Void String (Loc Longident, CoreType)
packageWithParser =
  (,)
    <$ string "with type"
    <* hspace
    <*> locParser longidentParser
    <* newline
    <*> coreTypeParser

patternParser :: Parsec Void String Pattern
patternParser =
  (\loc attrs desc -> Pattern desc loc attrs)
    <$ string "pattern"
    <* hspace
    <*> locationParser
    <* newline
    <*> attributesParser
    <*> patternDescParser

patternDescParser :: Parsec Void String PatternDesc
patternDescParser =
  ( flip PpatAlias
      <$ string "Ppat_alias"
      <* hspace
      <*> locParser (quotedParser valueIdentParser)
      <* newline
      <*> patternParser
  )
    <|> (PpatAny <$ string "Ppat_any" <* newline)
    <|> ( PpatArray
            <$ string "Ppat_array"
            <* newline
            <*> listParser patternParser
        )
    <|> ( PpatConstant
            <$ string "Ppat_constant"
            <* hspace
            <*> constantParser
            <* newline
        )
    <|> ( PpatConstruct
            <$ string "Ppat_construct"
            <* hspace
            <*> locParser longidentParser
            <* newline
            <*> optionParser
              Newline
              ( (,)
                  <$> listParser (locParser stringParser <* newline)
                  <*> patternParser
              )
        )
    <|> ( PpatConstraint
            <$ string "Ppat_constraint"
            <* newline
            <*> patternParser
            <*> coreTypeParser
        )
    <|> ( PpatException
            <$ string "Ppat_exception"
            <* newline
            <*> patternParser
        )
    <|> ( PpatExtension
            <$ string "Ppat_extension"
            <* hspace
            <*> extensionParser
        )
    <|> ( PpatInterval
            <$ string "Ppat_interval"
            <* hspace
            <*> constantParser
            <* string ".."
            <*> constantParser
            <* newline
        )
    <|> ( PpatLazy
            <$ string "Ppat_lazy"
            <* newline
            <*> patternParser
        )
    <|> ( PpatOpen
            <$ string "Ppat_open"
            <* hspace
            -- the quotes should probably not be printed this way
            <*> quotedParser (locParser longidentParser)
            <* newline
            <*> patternParser
        )
    <|> ( PpatOr
            <$ string "Ppat_or"
            <* newline
            <*> patternParser
            <*> patternParser
        )
    <|> ( flip PpatRecord
            <$ string "Ppat_record"
            <* hspace
            <*> closedFlagParser
            <* newline
            <*> listParser longidentXPatternParser
        )
    <|> (PpatTuple <$ string "Ppat_tuple" <* newline <*> listParser patternParser)
    <|> ( PpatType
            <$ string "Ppat_type"
            <* newline
            <*> locParser longidentParser
            <* newline
        )
    <|> ( PpatUnpack
            <$ string "Ppat_unpack"
            <* hspace
            <*> locOptionParser (lidentParser <|> uidentParser)
            <* newline
        )
    <|> ( PpatVariant
            <$ string "Ppat_variant"
            <* hspace
            <*> stringParser
            <* newline
            <*> optionParser Newline patternParser
        )
    <|> ( PpatVar
            <$ string "Ppat_var"
            <* hspace
            <*> locParser stringParser
            <* newline
        )

expressionParser :: Parsec Void String Expression
expressionParser =
  (\loc attrs desc -> Expression desc loc attrs)
    <$ string "expression"
    <* hspace
    <*> locationParser
    <* newline
    <*> attributesParser
    <*> expressionDescParser

expressionDescParser :: Parsec Void String ExpressionDesc
expressionDescParser =
  ( PexpApply
      <$ string "Pexp_apply"
      <* newline
      <*> expressionParser
      <*> listParser labelXExpressionParser
  )
    <|> ( PexpArray
            <$ string "Pexp_array"
            <* newline
            <*> listParser expressionParser
        )
    <|> ( PexpAssert
            <$ string "Pexp_assert"
            <* newline
            <*> expressionParser
        )
    <|> ( PexpCoerce
            <$ string "Pexp_coerce"
            <* newline
            <*> expressionParser
            <*> optionParser Newline coreTypeParser
            <*> coreTypeParser
        )
    <|> ( PexpConstant
            <$ string "Pexp_constant"
            <* hspace
            <*> constantParser
            <* newline
        )
    <|> ( PexpConstraint
            <$ string "Pexp_constraint"
            <* newline
            <*> expressionParser
            <*> coreTypeParser
        )
    <|> ( PexpConstruct
            <$ string "Pexp_construct"
            <* hspace
            <*> locParser longidentParser
            <* newline
            <*> optionParser Newline expressionParser
        )
    <|> ( PexpExtension
            <$ string "Pexp_extension"
            <* hspace
            <*> extensionParser
        )
    <|> ( PexpField
            <$ string "Pexp_field"
            <* newline
            <*> expressionParser
            <*> locParser longidentParser
            <* newline
        )
    <|> ( (\directionFlag pattern expr1 expr2 expr3 -> PexpFor pattern expr1 expr2 directionFlag expr3)
            <$ string "Pexp_for"
            <* hspace
            <*> directionFlagParser
            <* newline
            <*> patternParser
            <*> expressionParser
            <*> expressionParser
            <*> expressionParser
        )
    <|> ( PexpFunction
            <$ string "Pexp_function"
            <* newline
            <*> listParser functionParamParser
            <*> optionParser Newline typeConstraintParser
            <*> functionBodyParser
        )
    <|> ( PexpIdent
            <$ string "Pexp_ident"
            <* hspace
            <*> locParser longidentParser
            <* newline
        )
    <|> ( PexpIfthenelse
            <$ string "Pexp_ifthenelse"
            <* newline
            <*> expressionParser
            <*> expressionParser
            <*> optionParser Newline expressionParser
        )
    <|> ( PexpLazy
            <$ string "Pexp_lazy"
            <* newline
            <*> expressionParser
        )
    <|> ( PexpLetexception
            <$ string "Pexp_letexception"
            <* newline
            <*> extensionConstructorParser
            <*> expressionParser
        )
    <|> ( PexpLetmodule
            <$ string "Pexp_letmodule"
            <* hspace
            <*> locOptionParser moduleNameParser
            <* newline
            <*> moduleExprParser
            <*> expressionParser
        )
    <|> ( PexpLetop
            <$ string "Pexp_letop"
            <* newline
            <*> ( Letop
                    <$> bindingOpParser
                    <*> listParser bindingOpParser
                    <*> expressionParser
                )
        )
    <|> ( PexpLet
            <$ string "Pexp_let"
            <* hspace
            <*> recFlagParser
            <* newline
            <*> listParser valueBindingParser
            <*> expressionParser
        )
    <|> ( PexpMatch
            <$ string "Pexp_match"
            <* newline
            <*> expressionParser
            <*> listParser caseParser
        )
    <|> ( PexpNewtype
            <$ string "Pexp_newtype"
            <* hspace
            <*> quotedParser lidentParser
            <* newline
            <*> expressionParser
        )
    <|> ( PexpNew
            <$ string "Pexp_new"
            <* hspace
            <*> locParser longidentParser
            <* newline
        )
    <|> ( PexpObject
            <$ string "Pexp_object"
            <* newline
            <*> classStructureParser
        )
    <|> ( PexpOpen
            <$ string "Pexp_open"
            <* hspace
            -- attributes should be printed
            <*> ( (\flag moduleExpr -> OpenInfos moduleExpr flag [])
                    <$> overrideFlagParser
                    <* newline
                    <*> moduleExprParser
                )
            <*> expressionParser
        )
    <|> ( PexpOverride
            <$ string "Pexp_override"
            <* newline
            <*> listParser stringXExpressionParser
        )
    <|> ( PexpPack
            <$ string "Pexp_pack"
            <* newline
            <*> moduleExprParser
        )
    <|> ( PexpPoly
            <$ string "Pexp_poly"
            <* newline
            <*> expressionParser
            <*> optionParser Newline coreTypeParser
        )
    <|> ( PexpRecord
            <$ string "Pexp_record"
            <* newline
            <*> listParser longidentXExpressionParser
            <*> optionParser Newline expressionParser
        )
    <|> ( flip PexpSend
            <$ string "Pexp_send"
            <* hspace
            <*> quotedParser lidentParser
            <* newline
            <*> expressionParser
        )
    <|> ( PexpSetfield
            <$ string "Pexp_setfield"
            <* newline
            <*> expressionParser
            <*> locParser longidentParser
            <* newline
            <*> expressionParser
        )
    <|> ( PexpSetinstvar
            <$ string "Pexp_setinstvar"
            <* hspace
            <*> locParser (quotedParser lidentParser)
            <* newline
            <*> expressionParser
        )
    <|> ( PexpSequence
            <$ string "Pexp_sequence"
            <* newline
            <*> expressionParser
            <*> expressionParser
        )
    <|> ( PexpTry
            <$ string "Pexp_try"
            <* newline
            <*> expressionParser
            <*> listParser caseParser
        )
    <|> ( PexpTuple
            <$ string "Pexp_tuple"
            <* newline
            <*> listParser expressionParser
        )
    <|> ( PexpUnreachable
            <$ string "Pexp_unreachable"
            -- here should probably be a newline in the printer
            <* hspace
        )
    <|> ( PexpVariant
            <$ string "Pexp_variant"
            <* hspace
            <*> quotedParser identParser
            <* newline
            <*> optionParser Newline expressionParser
        )
    <|> ( PexpWhile
            <$ string "Pexp_while"
            <* newline
            <*> expressionParser
            <*> expressionParser
        )

functionParamParser :: Parsec Void String FunctionParam
functionParamParser =
  ( (\loc argLabel maybeExpr -> FunctionParam loc . PparamVal argLabel maybeExpr)
      <$ string "Pparam_val"
      <* hspace
      <*> locationParser
      <* newline
      <*> argLabelParser
      <*> optionParser Newline expressionParser
      <*> patternParser
  )
    <|> ( (\str loc -> FunctionParam loc (PparamNewtype str))
            <$ string "Pparam_newtype"
            <* hspace
            <*> quotedParser lidentParser
            <* hspace
            <*> locationParser
            <* newline
        )

functionBodyParser :: Parsec Void String FunctionBody
functionBodyParser =
  ( PfunctionBody
      <$ string "Pfunction_body"
      <* newline
      <*> expressionParser
  )
    <|> ( (\loc attributes cases -> PfunctionCases cases loc attributes)
            <$ string "Pfunction_cases"
            <* hspace
            <*> locationParser
            <* newline
            <*> attributesParser
            <*> listParser caseParser
        )

typeConstraintParser :: Parsec Void String TypeConstraint
typeConstraintParser =
  ( Pconstraint
      <$ string "Pconstraint"
      <* newline
      <*> coreTypeParser
  )
    <|> ( Pcoerce
            <$ string "Pcoerce"
            <* newline
            <*> optionParser Newline coreTypeParser
            <*> coreTypeParser
        )

valueDescriptionParser :: Parsec Void String ValueDescription
valueDescriptionParser =
  (\locName loc attrs coreType piAttrs -> ValueDescription locName coreType piAttrs attrs loc)
    <$ string "value_description"
    <* hspace
    <*> locParser (quotedParser valueIdentParser)
    <* hspace
    <*> locationParser
    <* newline
    <*> attributesParser
    <*> coreTypeParser
    <*> listParser (quotedParser attributeNameParser <* newline)

typeParameterParser :: Parsec Void String CoreType
typeParameterParser = coreTypeParser

typeDeclarationParser :: Parsec Void String TypeDeclaration
typeDeclarationParser =
  (\name loc attrs params cstrs kind private manifest -> TypeDeclaration name params cstrs kind private manifest attrs loc)
    <$ string "type_declaration"
    <* hspace
    <*> locParser (quotedParser lidentParser)
    <* hspace
    <*> locationParser
    <* newline
    <*> attributesParser
    <* string "ptype_params ="
    <* newline
    <*> listParser typeParameterParser
    <* string "ptype_cstrs ="
    <* newline
    <*> listParser coreTypeXCoreTypeXLocationParser
    <* string "ptype_kind ="
    <* newline
    <*> typeKindParser
    <* string "ptype_private ="
    <* hspace
    <*> privateFlagParser
    <* newline
    <* string "ptype_manifest ="
    <* newline
    <*> optionParser Newline coreTypeParser

attributeParser :: String -> Parsec Void String Attribute
attributeParser k =
  Attribute
    <$ string k
    <* hspace
    <*> quotedParser (concat <$> sepBy1 attributeNameParser (char '.'))
    <* newline
    <*> payloadParser

attributesParser :: Parsec Void String [Attribute]
attributesParser =
  many
    ( Attribute
        <$ string "attribute"
        <* hspace
        <*> quotedParser (concat <$> sepBy1 attributeNameParser (char '.'))
        <* newline
        <*> payloadParser
    )

payloadParser :: Parsec Void String Payload
payloadParser =
  (PStr <$> try structureParser)
    <|> (PSig <$> signatureParser)
    <|> (PTyp <$> coreTypeParser)
    <|> ( PPat
            <$> patternParser
            <*> optional (try (string "<when>" *> newline *> expressionParser))
        )

typeKindParser :: Parsec Void String TypeKind
typeKindParser =
  (PtypeAbstract <$ string "Ptype_abstract" <* newline)
    <|> ( PtypeVariant
            <$ string "Ptype_variant"
            <* newline
            <*> listParser constructorDeclParser
        )
    <|> ( PtypeRecord
            <$ string "Ptype_record"
            <* newline
            <*> listParser labelDeclParser
        )
    <|> (PtypeOpen <$ string "Ptype_open" <* newline)

typeExtensionParser :: Parsec Void String TypeExtension
typeExtensionParser =
  (\attrs locLongident typeParams extConstrs private -> TypeExtension locLongident typeParams extConstrs private attrs)
    <$ string "type_extension"
    <* newline
    <*> attributesParser
    <* string "ptyext_path ="
    <* hspace
    <*> locParser longidentParser
    <* newline
    <* string "ptyext_params ="
    <* newline
    <*> listParser typeParameterParser
    <* string "ptyext_constructors ="
    <* newline
    <*> listParser extensionConstructorParser
    <* string "ptyext_private ="
    <* hspace
    <*> privateFlagParser
    <* newline

typeExceptionParser :: Parsec Void String TypeException
typeExceptionParser =
  flip TypeException
    <$ string "type_exception"
    <* newline
    <*> attributesParser
    <* string "ptyext_constructor ="
    <* newline
    <*> extensionConstructorParser

extensionConstructorParser :: Parsec Void String ExtensionConstructor
extensionConstructorParser =
  (\loc attrs name kind -> ExtensionConstructor name kind loc attrs)
    <$ string "extension_constructor"
    <* hspace
    <*> locationParser
    <* newline
    <*> attributesParser
    <* string "pext_name ="
    <* hspace
    <*> quotedParser constrIdentParser
    <* newline
    <* string "pext_kind ="
    <* newline
    <*> extensionConstructorKindParser

extensionConstructorKindParser :: Parsec Void String ExtensionConstructorKind
extensionConstructorKindParser =
  ( PextDecl
      <$ string "Pext_decl"
      <* newline
      <*> ((string "vars" *> hspace *> typeVarsParser <* newline) <|> pure [])
      <*> constructorArgumentsParser
      <*> optionParser Newline coreTypeParser
  )
    <|> ( PextRebind
            <$ string "Pext_rebind"
            <* newline
            <*> locParser longidentParser
            <* newline
        )

classTypeParser :: Parsec Void String ClassType
classTypeParser =
  (\loc attrs desc -> ClassType desc loc attrs)
    <$ string "class_type"
    <* hspace
    <*> locationParser
    <* newline
    <*> attributesParser
    <*> classTypeDescParser

classTypeDescParser :: Parsec Void String ClassTypeDesc
classTypeDescParser =
  ( PctyConstr
      <$ string "Pcty_constr"
      <* hspace
      <*> locParser longidentParser
      <* newline
      <*> listParser coreTypeParser
  )
    <|> ( PctySignature
            <$ string "Pcty_signature"
            <* newline
            <*> classSignatureParser
        )
    <|> ( PctyArrow
            <$ string "Pcty_arrow"
            <* newline
            <*> argLabelParser
            <*> coreTypeParser
            <*> classTypeParser
        )
    <|> ( PctyExtension
            <$ string "Pcty_extension"
            <* hspace
            <*> extensionParser
        )
    <|> ( PctyOpen
            <$ string "Pcty_open"
            <* hspace
            -- attributes should be printed
            <*> ( (\overFlag locIdent -> OpenInfos locIdent overFlag [])
                    <$> overrideFlagParser
                    <* hspace
                    <*> locParser longidentParser
                )
            <* newline
            <*> classTypeParser
        )

classSignatureParser :: Parsec Void String ClassSignature
classSignatureParser =
  ClassSignature
    <$ string "class_signature"
    <* newline
    <*> coreTypeParser
    <*> listParser classTypeFieldParser

classTypeFieldParser :: Parsec Void String ClassTypeField
classTypeFieldParser =
  (\loc attrs desc -> ClassTypeField desc loc attrs)
    <$ string "class_type_field"
    <* hspace
    <*> locationParser
    <* newline
    <*> attributesParser
    <*> classTypeFieldDescParser

classTypeFieldDescParser :: Parsec Void String ClassTypeFieldDesc
classTypeFieldDescParser =
  ( PctfInherit
      <$ string "Pctf_inherit"
      <* newline
      <*> classTypeParser
  )
    <|> ( PctfVal
            <$ string "Pctf_val"
            <* hspace
            <*> quotedParser lidentParser
            <* hspace
            <*> mutableFlagParser
            <* hspace
            <*> virtualFlagParser
            <* newline
            <*> coreTypeParser
        )
    <|> ( PctfMethod
            <$ string "Pctf_method"
            <* hspace
            <*> quotedParser lidentParser
            <* hspace
            <*> privateFlagParser
            <* hspace
            <*> virtualFlagParser
            <* newline
            <*> coreTypeParser
        )
    <|> ( PctfConstraint
            <$ string "Pctf_constraint"
            <* newline
            <*> coreTypeParser
            <*> coreTypeParser
        )
    <|> ( PctfAttribute
            <$> attributeParser "Pctf_attribute"
        )
    <|> ( PctfExtension
            <$ string "Pctf_extension"
            <* hspace
            <*> extensionParser
        )

classDescriptionParser :: Parsec Void String ClassDescription
classDescriptionParser =
  (\loc attrs virtFlag typeParams name classType -> ClassInfos virtFlag typeParams name classType loc attrs)
    <$ string "class_description"
    <* hspace
    <*> locationParser
    <* newline
    <*> attributesParser
    <* string "pci_virt ="
    <* hspace
    <*> virtualFlagParser
    <* newline
    <* string "pci_params ="
    <* newline
    <*> listParser typeParameterParser
    <* string "pci_name ="
    <* hspace
    <*> locParser (quotedParser lidentParser)
    <* newline
    <* string "pci_expr ="
    <* newline
    <*> classTypeParser

classTypeDeclarationParser :: Parsec Void String ClassTypeDeclaration
classTypeDeclarationParser =
  (\loc attrs virtFlag typeParams name classType -> ClassInfos virtFlag typeParams name classType loc attrs)
    <$ string "class_type_declaration"
    <* hspace
    <*> locationParser
    <* newline
    <*> attributesParser
    <* string "pci_virt ="
    <* hspace
    <*> virtualFlagParser
    <* newline
    <* string "pci_params ="
    <* newline
    <*> listParser typeParameterParser
    <* string "pci_name ="
    <* hspace
    <*> locParser (quotedParser lidentParser)
    <* newline
    <* string "pci_expr ="
    <* newline
    <*> classTypeParser

classExprParser :: Parsec Void String ClassExpr
classExprParser =
  (\loc attrs desc -> ClassExpr desc loc attrs)
    <$ string "class_expr"
    <* hspace
    <*> locationParser
    <* newline
    <*> attributesParser
    <*> classExprDescParser

classExprDescParser :: Parsec Void String ClassExprDesc
classExprDescParser =
  ( PclApply
      <$ string "Pcl_apply"
      <* newline
      <*> classExprParser
      <*> listParser labelXExpressionParser
  )
    <|> ( PclConstraint
            <$ string "Pcl_constraint"
            <* newline
            <*> classExprParser
            <*> classTypeParser
        )
    <|> ( PclConstr
            <$ string "Pcl_constr"
            <* hspace
            <*> locParser longidentParser
            <* newline
            <*> listParser coreTypeParser
        )
    <|> ( PclExtension
            <$ string "Pcl_extension"
            <* hspace
            <*> extensionParser
        )
    <|> ( PclFun
            <$ string "Pcl_fun"
            <* newline
            <*> argLabelParser
            <*> optionParser Newline expressionParser
            <*> patternParser
            <*> classExprParser
        )
    <|> ( PclLet
            <$ string "Pcl_let"
            <* hspace
            <*> recFlagParser
            <* newline
            <*> listParser valueBindingParser
            <*> classExprParser
        )
    <|> ( PclOpen
            <$ string "Pcl_open"
            <* hspace
            -- attributes should be printed
            <*> ( (\flag expr -> OpenInfos expr flag [])
                    <$> overrideFlagParser
                    <* hspace
                    <*> locParser longidentParser
                    <* newline
                )
            <*> classExprParser
        )
    <|> ( PclStructure
            <$ string "Pcl_structure"
            <* newline
            <*> classStructureParser
        )

classStructureParser :: Parsec Void String ClassStructure
classStructureParser =
  ClassStructure
    <$ string "class_structure"
    <* newline
    <*> patternParser
    <*> listParser classFieldParser

classFieldParser :: Parsec Void String ClassField
classFieldParser =
  (\loc attrs desc -> ClassField desc loc attrs)
    <$ string "class_field"
    <* hspace
    <*> locationParser
    <* newline
    <*> attributesParser
    <*> classFieldDescParser

classFieldDescParser :: Parsec Void String ClassFieldDesc
classFieldDescParser =
  (PcfAttribute <$> attributeParser "Pcf_attribute")
    <|> ( PcfConstraint
            <$ string "Pcf_constraint"
            <* newline
            <*> coreTypeParser
            <*> coreTypeParser
        )
    <|> ( PcfExtension
            <$ string "Pcf_extension"
            <* hspace
            <*> extensionParser
        )
    <|> ( PcfInherit
            <$ string "Pcf_inherit"
            <* hspace
            <*> overrideFlagParser
            <* newline
            <*> classExprParser
            <*> optionParser Newline (locParser (quotedParser lidentParser) <* newline)
        )
    <|> ( PcfInitializer
            <$ string "Pcf_initializer"
            <* newline
            <*> expressionParser
        )
    <|> ( flip PcfMethod
            <$ string "Pcf_method"
            <* hspace
            <*> privateFlagParser
            <* newline
            <*> locParser (quotedParser lidentParser)
            <* newline
            <*> classFieldKindParser
        )
    <|> ( flip PcfVal
            <$ string "Pcf_val"
            <* hspace
            <*> mutableFlagParser
            <* newline
            <*> locParser (quotedParser lidentParser)
            <* newline
            <*> classFieldKindParser
        )

classFieldKindParser :: Parsec Void String ClassFieldKind
classFieldKindParser =
  ( CfkConcrete
      <$ string "Concrete"
      <* hspace
      <*> overrideFlagParser
      <* newline
      <*> expressionParser
  )
    <|> ( CfkVirtual
            <$ string "Virtual"
            <* newline
            <*> coreTypeParser
        )

classDeclarationParser :: Parsec Void String ClassDeclaration
classDeclarationParser =
  (\loc attrs virtFlag typeParams name classExpr -> ClassInfos virtFlag typeParams name classExpr loc attrs)
    <$ string "class_declaration"
    <* hspace
    <*> locationParser
    <* newline
    <*> attributesParser
    <* string "pci_virt ="
    <* hspace
    <*> virtualFlagParser
    <* newline
    <* string "pci_params ="
    <* newline
    <*> listParser typeParameterParser
    <* string "pci_name ="
    <* hspace
    <*> locParser (quotedParser lidentParser)
    <* newline
    <* string "pci_expr ="
    <* newline
    <*> classExprParser

moduleTypeParser :: Parsec Void String ModuleType
moduleTypeParser =
  (\loc attrs desc -> ModuleType desc loc attrs)
    <$ string "module_type"
    <* hspace
    <*> locationParser
    <* newline
    <*> attributesParser
    <*> moduleTypeDescParser

moduleTypeDescParser :: Parsec Void String ModuleTypeDesc
moduleTypeDescParser =
  ( PmtyIdent
      <$ string "Pmty_ident"
      <* hspace
      <*> locParser longidentParser
      <* newline
  )
    <|> ( PmtyAlias
            <$ string "Pmty_alias"
            <* hspace
            <*> locParser longidentParser
            <* newline
        )
    <|> ( PmtySignature
            <$ string "Pmty_signature"
            <* newline
            <*> signatureParser
        )
    <|> ( PmtyFunctor
            <$ string "Pmty_functor"
            <* hspace
            <*> ( (Unit <$ string "()" <* newline)
                    <|> (Named <$> locOptionParser moduleNameParser <* newline <*> moduleTypeParser)
                )
            <*> moduleTypeParser
        )
    <|> ( PmtyWith
            <$ string "Pmty_with"
            <* newline
            <*> moduleTypeParser
            <*> listParser withConstraintParser
        )
    <|> ( PmtyTypeof
            <$ string "Pmty_typeof"
            <* newline
            <*> moduleExprParser
        )
    <|> ( PmtyExtension
            <$ string "Pmty_extension"
            <* hspace
            <*> extensionParser
        )

signatureParser :: Parsec Void String [SignatureItem]
signatureParser = listParser signatureItemParser

signatureItemParser :: Parsec Void String SignatureItem
signatureItemParser =
  flip SignatureItem
    <$ string "signature_item"
    <* hspace
    <*> locationParser
    <* newline
    <*> signatureItemDescParser

signatureItemDescParser :: Parsec Void String SignatureItemDesc
signatureItemDescParser =
  (PsigAttribute <$> attributeParser "Psig_attribute")
    <|> ( PsigClassType
            <$ string "Psig_class_type"
            <* newline
            <*> listParser classTypeDeclarationParser
        )
    <|> ( PsigClass
            <$ string "Psig_class"
            <* newline
            <*> listParser classDescriptionParser
        )
    <|> ( (\name attrs payload -> PsigExtension (name, payload) attrs)
            <$ string "Psig_extension"
            <* hspace
            <*> quotedParser lidentParser
            <* newline
            <*> attributesParser
            <*> payloadParser
        )
    <|> ( PsigException
            <$ string "Psig_exception"
            <* newline
            <*> typeExceptionParser
        )
    <|> ( PsigInclude
            <$ string "Psig_include"
            <* newline
            <*> ( IncludeInfos
                    <$> moduleTypeParser
                    <*> attributesParser
                )
        )
    <|> ( PsigModtypesubst
            <$ string "Psig_modtypesubst"
            <* hspace
            <*> ( (\locName attrs modtype -> ModuleTypeDeclaration locName modtype attrs)
                    <$> locParser (quotedParser (lidentParser <|> uidentParser))
                    <* newline
                    <*> attributesParser
                    <*> modtypeDeclarationParser
                )
        )
    <|> ( PsigModtype
            <$ string "Psig_modtype"
            <* hspace
            <*> ( (\locName attrs modtype -> ModuleTypeDeclaration locName modtype attrs)
                    <$> locParser (quotedParser (lidentParser <|> uidentParser))
                    <* newline
                    <*> attributesParser
                    <*> modtypeDeclarationParser
                )
        )
    <|> ( PsigModsubst
            <$ string "Psig_modsubst"
            <* hspace
            <*> ( ModuleSubstitution
                    <$> locParser (quotedParser uidentParser)
                    <* hspace
                    <* char '='
                    <* hspace
                    <*> locParser longidentParser
                    <* newline
                    <*> attributesParser
                )
        )
    <|> ( PsigModule
            <$ string "Psig_module"
            <* hspace
            <*> ( (\locName attrs moduleType -> ModuleDeclaration locName moduleType attrs)
                    <$> locOptionParser moduleNameParser
                    <* newline
                    <*> attributesParser
                    <*> moduleTypeParser
                )
        )
    <|> ( PsigOpen
            <$ string "Psig_open"
            <* hspace
            <*> ( flip OpenInfos
                    <$> overrideFlagParser
                    <* hspace
                    <*> locParser longidentParser
                    <* newline
                    <*> attributesParser
                )
        )
    <|> ( PsigRecmodule
            <$ string "Psig_recmodule"
            <* newline
            <*> listParser moduleDeclarationParser
        )
    <|> ( PsigTypesubst
            <$ string "Psig_typesubst"
            <* newline
            <*> listParser typeDeclarationParser
        )
    <|> ( PsigTypext
            <$ string "Psig_typext"
            <* newline
            <*> typeExtensionParser
        )
    <|> ( PsigType
            <$ string "Psig_type"
            <* hspace
            <*> recFlagParser
            <* newline
            <*> listParser typeDeclarationParser
        )
    <|> ( PsigValue
            <$ string "Psig_value"
            <* newline
            <*> valueDescriptionParser
        )

modtypeDeclarationParser :: Parsec Void String (Maybe ModuleType)
modtypeDeclarationParser =
  (Just <$> moduleTypeParser) <|> (Nothing <$ string "#abstract")

withConstraintParser :: Parsec Void String WithConstraint
withConstraintParser =
  ( PwithTypesubst
      <$ string "Pwith_typesubst"
      <* hspace
      <*> locParser longidentParser
      <* newline
      <*> typeDeclarationParser
  )
    <|> ( PwithType
            <$ string "Pwith_type"
            <* hspace
            <*> locParser longidentParser
            <* newline
            <*> typeDeclarationParser
        )
    <|> ( PwithModule
            <$ string "Pwith_module"
            <* hspace
            <*> locParser longidentParser
            <* hspace
            <* char '='
            <* hspace
            <*> locParser longidentParser
            <* newline
        )
    <|> ( PwithModsubst
            <$ string "Pwith_modsubst"
            <* hspace
            <*> locParser longidentParser
            <* hspace
            <* char '='
            <* hspace
            <*> locParser longidentParser
            <* newline
        )
    <|> ( PwithModtypesubst
            <$ string "Pwith_modtypesubst"
            <* hspace
            <*> locParser longidentParser
            <* newline
            <*> moduleTypeParser
        )
    <|> ( PwithModtype
            <$ string "Pwith_modtype"
            <* hspace
            <*> locParser longidentParser
            <* newline
            <*> moduleTypeParser
        )

moduleExprParser :: Parsec Void String ModuleExpr
moduleExprParser =
  (\loc attrs desc -> ModuleExpr desc loc attrs)
    <$ string "module_expr"
    <* hspace
    <*> locationParser
    <* newline
    <*> attributesParser
    <*> moduleExprDescParser

moduleExprDescParser :: Parsec Void String ModuleExprDesc
moduleExprDescParser =
  ( PmodApplyUnit
      <$ string "Pmod_apply_unit"
      <* newline
      <*> moduleExprParser
  )
    <|> ( PmodApply
            <$ string "Pmod_apply"
            <* newline
            <*> moduleExprParser
            <*> moduleExprParser
        )
    <|> ( PmodConstraint
            <$ string "Pmod_constraint"
            <* newline
            <*> moduleExprParser
            <*> moduleTypeParser
        )
    <|> ( PmodExtension
            <$ string "Pmod_extension"
            <* hspace
            <*> extensionParser
        )
    <|> ( PmodFunctor
            <$ string "Pmod_functor"
            <* hspace
            <*> functorParameterParser
            <*> moduleExprParser
        )
    <|> ( PmodIdent
            <$ string "Pmod_ident"
            <* hspace
            <*> locParser longidentParser
            <* newline
        )
    <|> ( PmodStructure
            <$ string "Pmod_structure"
            <* newline
            <*> structureParser
        )
    <|> ( PmodUnpack
            <$ string "Pmod_unpack"
            <* newline
            <*> expressionParser
        )

functorParameterParser :: Parsec Void String FunctorParameter
functorParameterParser =
  (Unit <$ string "()" <* newline) <|> (Named <$> locOptionParser moduleNameParser <* newline <*> moduleTypeParser)

structureParser :: Parsec Void String [StructureItem]
structureParser = listParser structureItemParser

structureItemParser :: Parsec Void String StructureItem
structureItemParser =
  flip StructureItem
    <$ string "structure_item"
    <* hspace
    <*> locationParser
    <* newline
    <*> structureItemDescParser

structureItemDescParser :: Parsec Void String StructureItemDesc
structureItemDescParser =
  (PstrAttribute <$> attributeParser "Pstr_attribute")
    <|> ( PstrClassType
            <$ string "Pstr_class_type"
            <* newline
            <*> listParser classTypeDeclarationParser
        )
    <|> ( PstrClass
            <$ string "Pstr_class"
            <* newline
            <*> listParser classDeclarationParser
        )
    <|> (PstrException <$ string "Pstr_exception" <* newline <*> typeExceptionParser)
    <|> ( (\name attrs payload -> PstrExtension (name, payload) attrs)
            <$ string "Pstr_extension"
            <* hspace
            -- should probably be `LongIdent` in the `AST` as well
            <*> (AST.pretty <$> longidentParser)
            <* newline
            <*> attributesParser
            <*> payloadParser
        )
    <|> ( flip PstrEval
            <$ string "Pstr_eval"
            <* newline
            <*> attributesParser
            <*> expressionParser
        )
    <|> ( PstrInclude
            <$ string "Pstr_include"
            <*> ( flip IncludeInfos
                    -- here should probably be a newline in the printer
                    <$ hspace
                    <*> attributesParser
                    <*> moduleExprParser
                )
        )
    <|> ( PstrModtype
            <$ string "Pstr_modtype"
            <* hspace
            <*> ( (\name attrs modtype -> ModuleTypeDeclaration name modtype attrs)
                    <$> locParser (quotedParser (lidentParser <|> uidentParser))
                    <* newline
                    <*> attributesParser
                    <*> modtypeDeclarationParser
                )
        )
    <|> (PstrModule <$ string "Pstr_module" <* newline <*> moduleBindingParser)
    <|> ( PstrOpen
            <$ string "Pstr_open"
            <* hspace
            <*> ( flip OpenInfos
                    <$> overrideFlagParser
                    <* newline
                    <*> moduleExprParser
                    <*> attributesParser
                )
        )
    <|> (PstrPrimitive <$ string "Pstr_primitive" <* newline <*> valueDescriptionParser)
    <|> (PstrTypext <$ string "Pstr_typext" <* newline <*> typeExtensionParser)
    <|> ( PstrType
            <$ string "Pstr_type"
            <* hspace
            <*> recFlagParser
            <* newline
            <*> listParser typeDeclarationParser
        )
    <|> ( PstrRecmodule
            <$ string "Pstr_recmodule"
            <* newline
            <*> listParser moduleBindingParser
        )
    <|> ( PstrValue
            <$ string "Pstr_value"
            <* hspace
            <*> recFlagParser
            <* newline
            <*> listParser valueBindingParser
        )

moduleDeclarationParser :: Parsec Void String ModuleDeclaration
moduleDeclarationParser =
  (\name attrs moduleType -> ModuleDeclaration name moduleType attrs)
    <$> locOptionParser moduleNameParser
    <* newline
    <*> attributesParser
    <*> moduleTypeParser

moduleBindingParser :: Parsec Void String ModuleBinding
moduleBindingParser =
  (\name attrs moduleExpr -> ModuleBinding name moduleExpr attrs)
    <$> locOptionParser moduleNameParser
    <* newline
    <*> attributesParser
    <*> moduleExprParser

coreTypeXCoreTypeXLocationParser :: Parsec Void String (CoreType, CoreType, Location)
coreTypeXCoreTypeXLocationParser =
  (\loc t1 t2 -> (t1, t2, loc))
    <$ string "<constraint>"
    <* hspace
    <*> locationParser
    <* newline
    <*> coreTypeParser
    <*> coreTypeParser

constructorDeclParser :: Parsec Void String ConstructorDeclaration
constructorDeclParser =
  (\loc name vars attrs args res -> ConstructorDeclaration name vars args res loc attrs)
    <$> locationParser
    <* newline
    <*> locParser (quotedParser constrIdentParser)
    <* newline
    <*> ((string "pcd_vars =" *> hspace *> typeVarsParser <* newline) <|> pure [])
    <*> attributesParser
    <*> constructorArgumentsParser
    <*> optionParser Newline coreTypeParser

constructorArgumentsParser :: Parsec Void String ConstructorArguments
constructorArgumentsParser =
  (PcstrTuple <$> try (listParser coreTypeParser)) <|> (PcstrRecord <$> listParser labelDeclParser)

labelDeclParser :: Parsec Void String LabelDeclaration
labelDeclParser =
  (\loc attrs mutable name coreType -> LabelDeclaration name mutable coreType loc attrs)
    <$> locationParser
    <* newline
    <*> attributesParser
    <*> mutableFlagParser
    <* newline
    <*> locParser (quotedParser lidentParser)
    -- here should probably be a newline in the printer
    <* hspace
    <*> coreTypeParser

longidentXPatternParser :: Parsec Void String (Loc Longident, Pattern)
longidentXPatternParser =
  (,) <$> locParser longidentParser <* newline <*> patternParser

caseParser :: Parsec Void String Case
caseParser =
  Case
    <$ string "<case>"
    <* newline
    <*> patternParser
    <*> optional (string "<when>" *> newline *> expressionParser)
    <*> expressionParser

valueBindingParser :: Parsec Void String ValueBinding
valueBindingParser =
  (\attr pattern maybeValue expression -> ValueBinding pattern expression maybeValue attr)
    <$ string "<def>"
    <* newline
    <*> attributesParser
    <*> patternParser
    <*> optional (try valueConstraintParser)
    <*> expressionParser

valueConstraintParser :: Parsec Void String ValueConstraint
valueConstraintParser =
  (PvcConstraint [] <$> coreTypeParser)
    <|> ( PvcConstraint
            <$ string "<type>"
            <* hspace
            <*> sepBy (locParser (quotedParser lidentParser)) newline
            <* char '.'
            <* newline
            <*> coreTypeParser
        )
    <|> ( PvcCoercion
            <$ string "<coercion>"
            <* newline
            <*> optionParser Newline coreTypeParser
            <*> coreTypeParser
        )

bindingOpParser :: Parsec Void String BindingOp
bindingOpParser =
  (\name loc pat expr -> BindingOp name pat expr loc)
    <$ string "<binding_op>"
    <* hspace
    <*> locParser (quotedParser valueIdentParser)
    <* hspace
    <*> locationParser
    -- here should probably be a newline in the printer
    <* hspace
    <*> patternParser
    <*> expressionParser

stringXExpressionParser :: Parsec Void String (Loc String, Expression)
stringXExpressionParser =
  (,) <$ string "<override>" <* hspace <*> locParser (quotedParser lidentParser) <* newline <*> expressionParser

longidentXExpressionParser :: Parsec Void String (Loc Longident, Expression)
longidentXExpressionParser =
  (,) <$> locParser longidentParser <* newline <*> expressionParser

labelXExpressionParser :: Parsec Void String (ArgLabel, Expression)
labelXExpressionParser =
  (,) <$ string "<arg>" <* newline <*> argLabelParser <*> expressionParser

labelXBoolXCoreTypeListParser :: Parsec Void String RowField
labelXBoolXCoreTypeListParser =
  ( (\label bool attrs coreTypes -> RowField (Rtag label bool coreTypes) attrs)
      <$ string "Rtag"
      <* hspace
      <*> quotedParser labelParser
      <* hspace
      <*> (True <$ string "true" <|> False <$ string "false")
      <* newline
      <*> attributesParser
      <*> listParser coreTypeParser
  )
    <|> ( (\coreType -> RowField (Rinherit coreType) [])
            <$ string "Rinherit"
            <* newline
            <*> coreTypeParser
        )

toplevelPhraseParser :: Parsec Void String ToplevelPhrase
toplevelPhraseParser =
  (PtopDef <$ string "Ptop_def" <* newline <*> structureParser)
    <|> ( PtopDir
            <$ string "Ptop_dir"
            <* hspace
            <*> ( ToplevelDirective
                    <$> quotedParser (lidentParser <|> uidentParser)
                    <* newline
                    <*> optional directiveArgumentParser
                )
        )

directiveArgumentParser :: Parsec Void String DirectiveArgument
directiveArgumentParser =
  DirectiveArgument <$> directiveArgumentDescParser

directiveArgumentDescParser :: Parsec Void String DirectiveArgumentDesc
directiveArgumentDescParser =
  ( PdirString
      <$ string "Pdir_string"
      <* hspace
      <*> quotedParser lidentParser
      <* newline
  )
    <|> ( PdirInt
            <$ string "Pdir_int"
            <* hspace
            <*> quotedParser lidentParser
            <*> optional (char '\n' *> lowerChar)
        )
    <|> ( PdirIdent
            <$ string "Pdir_ident"
            <* hspace
            <*> longidentParser
            <* newline
        )
    <|> ( PdirBool
            <$ string "Pdir_bool"
            <* hspace
            <*> (True <$ string "true" <|> False <$ string "false")
            <* newline
        )

interfaceParser :: Parsec Void String [SignatureItem]
interfaceParser = listParser signatureItemParser

implementationParser :: Parsec Void String [StructureItem]
implementationParser = listParser structureItemParser

topPhraseParser :: Parsec Void String ToplevelPhrase
topPhraseParser = toplevelPhraseParser
