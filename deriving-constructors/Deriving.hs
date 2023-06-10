-- Usage
-- $(deriveTags ''HydraEvent "Kind" [''Show, ''Eq])
-- deriving stock instance Show HydraEventKind

-- $(deriveMapping ''HydraEvent "Kind" "getKind")

-- TODO

module Deriving where

import Prelude
import Control.Monad
import Language.Haskell.TH

addSuffix :: Name -> String -> Name
addSuffix tyName suff = mkName $ (nameBase tyName) <> suff

reifyDatatype :: Name -> Q (Name, [Con])
reifyDatatype ty = do
  (TyConI tyCon) <- reify ty
  case tyCon of
    DataD _ n _ _ cs _ -> pure (n, cs)
    NewtypeD _ n _ _ cs _ -> pure (n, [cs])
    _ -> fail "deriveConstructors: only 'data' and 'newtype' are supported"

deriveConstructors :: Name -> String -> [Name] -> Q [Dec]
deriveConstructors ty suff classes = do
  (tyName, constructors) <- reifyDatatype ty
  dataDecl <- deriveDataDecl tyName constructors
  projSigDecl <- return $ makeProjSigDecl tyName
  projFunDecl <- deriveProjFunDecl tyName constructors
  pure $ [
    dataDecl,
    projSigDecl,
    projFunDecl]

  where
      mappingName tyName = mkName $ "dataProject" -- tyName `addSuffix`
      consTyName originalTyName = originalTyName `addSuffix` suff
      mapConsName func = \case
            NormalC n _ -> func n
            RecC n _ -> func n
            _ -> error $
              "deriveConstructors: constructor names must be NormalC or RecC"
              <> "(See https://hackage.haskell.org/package/"
              <> "template-haskell-2.20.0.0/docs/src/Language.Haskell.TH.Syntax.html#Con)"
      deriveDataDecl tyName originalConsts = do
          let consts = map (mapConsName perConstructor) originalConsts
          return $ DataD [] (consTyName tyName) [] Nothing consts [deriveClause]
        where
          perConstructor name = NormalC (consTyName name) []
          deriveClause = DerivClause (Just StockStrategy) (ConT <$> classes)
      makeProjSigDecl tyName = SigD
        (mappingName tyName)
        (AppT (AppT ArrowT (ConT tyName)) (ConT (consTyName tyName)))
      deriveProjFunDecl tyName originalConsts = do
        let matches = map (mapConsName makeMatch)  originalConsts
        originalVar <- newName "originalVar"
        return $ FunD
          (consTyName tyName)
          [
            Clause
              [VarP originalVar]
              (NormalB (CaseE (VarE originalVar) matches)) []]
        where
          makeMatch name =
              Match (RecP name []) (NormalB (ConE (consTyName name))) []


