module Deslop.AST (
    AstNode (..),
    AstModule (..),
    parseAst,
) where

import Effectful
import Effectful.Reader.Static (Reader)
import Effects.FileSystem (AbsPath (..), RoFileSystem, decodeOsPath)
import TypeScript.CST (TsNode (..), TsProgram (cst, path))
import TypeScript.Config (TsConfig)
import TypeScript.ModuleResolver (ModuleId (..), dropTypeScriptExtension, moduleIdUnsafe, reverseResolve)

data AstNode = ImportNode
    { target :: ModuleId
    , rawStatement :: Text
    }
    deriving stock (Show, Eq)
data AstModule = AstModule
    { id :: ModuleId
    , path :: AbsPath
    , nodes :: [AstNode]
    }
    deriving stock (Show, Eq)

parseAst :: (Reader TsConfig :> es, RoFileSystem :> es) => TsProgram -> Eff es AstModule
parseAst prog = do
    moduleId <- programModuleId
    pure
        AstModule
            { id = moduleId
            , path = prog.path
            , nodes = mapMaybe parseNode prog.cst
            }
  where
    -- A module's own id must come from the same alias mapping its import edges
    -- use, or the two never meet in the graph. Routing the decoded path
    -- through resolveImport only works on POSIX, where an absolute path starts
    -- with '/' and the resolver reads that as file-relative; a Windows path
    -- starts with a drive letter, lands in the alias branch, matches no
    -- mapping and stays a raw backslash path that no edge ever points at.
    -- reverseResolve works from the OsPath directly, which both OSes split
    -- correctly; the raw path remains the fallback for unmapped files.
    programModuleId = do
        maybeAlias <- reverseResolve prog.path
        pure . fromMaybe rawPathId $ maybeAlias
      where
        rawPathId =
            moduleIdUnsafe
                . decodeOsPath
                . dropTypeScriptExtension
                $ prog.path.osPath
    parseNode :: TsNode -> Maybe AstNode
    parseNode (Import pre t suf) =
        Just $
            ImportNode
                { target = moduleIdUnsafe t
                , rawStatement = pre <> t <> suf
                }
    parseNode _ = Nothing
